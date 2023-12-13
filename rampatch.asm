
// http://www.unusedino.de/ec64/technical/formats/g64.html - 10 header GCR bytes? but DOS reads only 8 http://unusedino.de/ec64/technical/aay/c1541/ro41f3b1.htm
// http://unusedino.de/ec64/technical/aay/c1541/ro41f4d1.htm

/*
l "c:/Users/Maciejdev/Documents/Projects-shared/15x1-ramexp/rampatch.prg" 0
b f4d1
f 6000 8000 0
r pc=0300
*/

// 6% faster on plain load in C64 mode (emulated)
// +patch $C649 to jsr  into ResetCache (both modes)
// +patch $F4D1 to jump into ReadSector (only 1541 MODE!)
// +patch $EAE5 to NOP NOP (disable ROM checksum check)
// +patch $960D to jump into ReadSector (1571 mode, could be faster to replicate code that starts there?)
	// CR vs 1571: 97EE JMP $AAAC (CR) vs JMP $AAAD (stock) (poza tym, co mnie interesuje)
	// CR vs jiffy: 97E9 JSR $97F9 (CR) vs JMP $B393 (jiffy) (j/w) (dekodowanie GCR w buforze?)
	// 1571 vs jiffy: j/w w CR vs jiffy

// ??? also patch http://unusedino.de/ec64/technical/aay/c1541/ro41d00e.htm 'read block header' - command $B0 to read from decoded headers?
// ??? check headers checksums and mark invalid ones?
// ???

.const HEADER = $16
.const HDRPNT = $32
.const STAB = $24
.const BUFPNT = $30 // (2)
.const DBID = $47

// 1571 
.const CHKSUM = $3A // (1)
.const BTAB = $52 // (4)

// 1541 ROM locations (1571 in 1541 mode)
.const LF556 = $F556 // wait for sync, set Y to 0
.const LF497 = $F497 // decode 8 GCR bytes from $24 into header structure at $16-$1A (track at $18, sector at $19)
.const LF4ED = $F4ED // part of read sector procedure right after GCR sector data is read - decode GCR from BUFPNT+$01BA:FF into BUFPNT
.const LF50A = $F50A // wait for header and then for sync (F556), patched instruction at $F4D1
.const LF4D4 = $F4D4 // next instruction after patch at $F4D1
// 1571 ROM locations
.const L952F = $952F // decode 8 GCR bytes from $24 into header structure at $16-$1A (track at $18, sector at $19)
.const L9600 = $9600 // wait for header and then for sync (9754), patched instruction at $960D
.const L9610 = $9610 // next instruction after patch at $960D
.const L970A = $970A // return 'ok' error through $99B5
.const L9754 = $9754 // wait for sync, set Y to 0
.const L99B5 = $99B5 // return error in A

// DOS unused zp
.const bufpage = $14					// (2) 1541/71 pointer to page GCR data, increase by $0100
.const bufrest = $2c					// (2) 1541 pointer to remainder GCR data, increase by bufrestsize; written to by GCR decoding routine at F6D0 but on 1541 that's after bufpage/bufrest was already used, doesn't appear in patched 1571 at all
.const hdroffs = $1b					// (1) 1541/71 offset to header GCR data at RE_cached_headers during data read and header decoding
.const counter = $1d					// (1) 1541/71 counter of read sectors, saved in RE_max_sector ($4B DOS attempt counter can be used for this); written to by powerup routine at $EBBA, but that's ok
.const hdroffsold = $46					// (1) 1541/71 temp storage needed to compare current header with 1st read header; written to after load in $917D / $918D (after jump to L970A)

// sizes
.const hdrsize = 8						// header size in GCR (8 GCR bytes become 5 header bytes)
.const maxsector = 22					// rather 21 but we have space
.const bufrestsize = $48				// size of GCR data over page size, it's $46 really, but it's rounded up

// actual area used: $6000-7BFF (track 1, 21 sectors)
.const RAMEXP = $6000 // 8K of expanded RAM
.const RAMBUF = $7E00 // last page for various stuff
.const RAMEXP_REST = RAMEXP+(maxsector*$0100)	// this is where remainder GCR data starts, make sure that it doesn't overlap RAMBUF (with sector headers)
.const RE_cached_track = RAMBUF+$ff
.const RE_max_sector = RAMBUF+$fe
.const RE_decoded_headers = RAMBUF // can be the same as RE_cached_headers, separated for debug only
.const RE_cached_headers = RAMBUF+$0100
.const RE_cached_checksums = RAMBUF-$0100 // 1571 only

/////////////////////////////////////		

		// $B700-$BEFF available both on stock and jiffydos
		.pc = $B700 "Patch F4D1"

		jmp ReadSector			// patch F4D1 to JMP $AC00
		jmp ReadSector71		// patch 960D to JMP $AC06
//		jmp ResetCache			// patch C649 to JSR $AC03, this has A=$FF	// XXX reverse the two so that we fall back

/////////////////////////////////////		

ResetCache:
		sta $0298				// patched code, set error flag
ResetOnlyCache:
		sta RE_cached_track		// set invalid values
		sta RE_max_sector
		rts

/////////////////////////////////////		

ReadSector:
// patch $F4D1 to jump in here, required sector number is on (HDRPNT)+1, required track in (HDRPNT), data goes into buffer at (BUFPNT)
		ldy #0
		lda (HDRPNT),y			// is cached track the same as required track?
		cmp RE_cached_track
		beq ReadCache
		jmp ReadTrack			// no - read the track

ReadCache:
		iny						// yes, track is cached, just put GCR data back and jump into ROM
		lda (HDRPNT),y			// needed sector number
		sta hdroffs				// keep it here
		// setup pointers
		lda #>RAMEXP			// pages - first 256 bytes
		sta bufpage+1
		lda #>RAMEXP_REST	// remainders - following bytes until end of sector ($46 but we add $80 each time)
		sta bufrest+1
		lda #0
		sta bufpage
		sta bufrest
		// find sector
		ldx #0
!loop:	lda RE_cached_headers,x
		cmp hdroffs
		beq !found+
		// no, next one
		inc bufpage+1
		lda bufrest
		clc
		adc #bufrestsize
		sta bufrest
		bcc !+
		inc bufrest+1
!:		inx
		cpx RE_max_sector
		bne !loop-
		// not found? fall back on ROM
		jsr LF50A	// replaced instruction
		jmp LF4D4	// next instruction

!found:	// copy GCR data and fall back into ROM		
		ldy #0
!:		lda (bufpage),y
		sta (BUFPNT),y
		iny
		bne !-
		ldx #$ba
		ldy #0
!:		lda (bufrest),y
		sta $0100,x
		iny
		inx
		bne !-
		
		jmp LF4ED	// continue in ROM

/////////////////////////////////////		

ReadTrack:
		sta RE_cached_track		// this will be our new track for caching

		lda #>RAMEXP			// pages - first 256 bytes
		sta bufpage+1
		lda #>RAMEXP_REST	// remainders - following bytes until end of sector ($46 but we add $80 each time)
		sta bufrest+1
		lda #0
		sta bufpage
		sta bufrest
		sta hdroffs				// 8-byte counter for sector headers at RAMBUF
		sta counter				// data block counter

		// (BUFPNT:$0100)+(HEADER:$0046) - GCR sector data, BUFPNT+$100, HEADER+$80
		// HEADER+2 - RAMBUF offset to GCR header data, HEADER+2 + $08
		// end loop when read header is the same as 1st read counter (full revolution) or block counter is 23

ReadHeader:
		jsr	LF556			// ; wait for SYNC, Y=0
		ldx hdroffs
		stx hdroffsold
		//ldy #0			// F556 sets Y to 0
!:		bvc !-
		clv
		lda $1c01
		cmp #$52			// is that header?
		bne ReadHeader		// no, wait until next SYNC
		sta RE_cached_headers,x
		inx
		iny					// yes, read remaining 8 bytes (or 10?)
!:		bvc !-
		clv
		lda $1c01
		sta RE_cached_headers,x
		inx
		iny
		cpy #hdrsize		// whole header?
		bne !-
		stx hdroffs			// new header offset
		// do we have that sector already? (on VICE there is enough time to check it even on fastest speedzone (track 35))
		ldx hdroffsold
		beq ReadGCRSector	// it's first sector, nothing to compare with
		ldy #0
!:		lda RE_cached_headers+1,x	// skip magic value byte
		cmp RE_cached_headers+1,y
		bne ReadGCRSector
		inx
		iny
		cpy #3				// last few bytes are identical too
		bne !-
		jmp DecodeData		// yes, no need to read more

ReadGCRSector:
		jsr LF556			// wait for SYNC, Y=0
!:		bvc !-
		clv
		lda $1c01
		sta (bufpage),y
		iny
		bne !-
		ldx #$BA		// ; from $1BA to $1FF
		ldy #0
!:		bvc !-
		clv
		lda $1c01
		sta (bufrest),y
		iny
		inx
		bne !-

		// adjust pointers
		inc bufpage+1
		inc counter
		lda bufrest
		clc
		adc #bufrestsize
		sta bufrest
		bcc !+
		inc bufrest+1
!:		lda counter
		cmp #maxsector	// all sectors already?
		beq DecodeData  // should never run
		jmp ReadHeader	// no, read next one

DecodeData:
		jsr DecodeHeaders
		// all was said and done, now read the sector from cache
		jmp ReadSector

/////////////////////////////////////		

DecodeHeaders:
		// we don't need to decode GCR sector data right now, but we need those sector numbers
		// so go through all headers and decode them, put them back
		ldx #0
		stx bufrest		// reuse for counter
		stx hdroffs

DecodeLoop:
		ldy hdroffs
		ldx #0
!:		lda RE_cached_headers,y
		sta STAB,x
		iny
		inx
		cpx #hdrsize
		bne !-
		jsr LF497		// // decode 8 GCR bytes from $24 into header structure at $16-$1A (track at $18, sector at $19)
.if (0==1) {
		// debug
		ldy hdroffs
		ldx #0
!:		lda $16,x
		sta RE_decoded_headers,y
		iny
		inx
		cpx #5
		bne !-
		//
}
		// XXX check header checksum here to mark mangled sector headers?
		ldx bufrest
		lda $19
		sta RE_cached_headers,x	// store decoded sector number
		lda hdroffs		// next header
		clc
		adc #hdrsize
		sta hdroffs
		inx
		stx bufrest		// next header
		cpx counter
		bne DecodeLoop
		stx RE_max_sector
		rts

/////////////////////////////////////		

ReadSector71:
// patch $960D to jump in here, required sector number is on (HDRPNT)+1, required track in (HDRPNT), data goes into buffer at (BUFPNT)
		ldy #0
		lda (HDRPNT),y			// is cached track the same as required track?
		cmp RE_cached_track
		beq ReadCache71
		jmp ReadTrack71			// no - read the track in native 71 mode

ReadCache71:
		iny						// yes, track is cached, just put GCR data back and jump into ROM
		lda (HDRPNT),y			// needed sector number
		sta hdroffs				// keep it here
		// setup pointers
		lda #>RAMEXP			// pages - first 256 bytes
		sta bufpage+1
		lda #0
		sta bufpage
		// find sector
		ldx #0
!loop:	lda RE_cached_headers,x
		cmp hdroffs
		beq !found+
		// no, next one
		inc bufpage+1
		inx
		cpx RE_max_sector
		bne !loop-
		// not found? fall back on ROM
		jsr L9600	// replaced instruction
		jmp L9610	// next instruction

!found:	// copy data and fall back into ROM		
		ldy #0
!:		lda (bufpage),y
		sta (BUFPNT),y
		iny
		bne !-

		jmp L970A	// continue in ROM, 'ok' error

ReadTrack71:
		sta RE_cached_track		// this will be our new track for caching

		lda #>RAMEXP			// decoded data pages
		sta bufpage+1
		lda #0
		sta bufpage
		sta hdroffs				// 8-byte counter for sector headers at RAMBUF
		sta counter				// data block counter


ReadHeader71:
		jsr	L9754			// ; wait for SYNC, Y=0
		ldx hdroffs
		stx hdroffsold
		//ldy #0			// 9754 sets Y to 0
		lda #$52			// header magic value
!:		bit $180f
		bmi !-
		clv
		cmp $1c01			// is that header?
		bne ReadHeader71		// no, wait until next SYNC
		sta RE_cached_headers,x
		inx
		iny					// yes, read remaining 8 bytes (or 10?)
!:		bit $180f
		bmi !-
		lda $1c01
		sta RE_cached_headers,x
		inx
		iny
		cpy #hdrsize		// whole header?
		bne !-
		stx hdroffs			// new header offset
		// do we have that sector already? (on VICE there is enough time to check it even on fastest speedzone (track 35))
		ldx hdroffsold
		beq ReadGCRSector71	// it's first sector, nothing to compare with
		ldy #0
!:		lda RE_cached_headers+1,x	// skip magic value byte
		cmp RE_cached_headers+1,y
		bne ReadGCRSector71
		inx
		iny
		cpy #3				// last few bytes are identical too
		bne !-
		jmp DecodeData71	// yes, no need to read more

// GCR decoding tables
.const L9F0D = $9F0D
.const LA00D = $A00D
.const LA10D = $A10D
.const LA20D = $A20D
.const LA30D = $A30D

ReadGCRSector71:
		jsr L9754			// wait for SYNC, Y=0
		// copy from 9610
		// replaced sta (BUFPNT),y by sta (bufpage),y
!:		bit $180f
		bmi !-
		lda $1c01
		tax
		lda	LA00D,x
		sta	BTAB
		txa
		and	#$07
		sta	BTAB + 1

!:		bit $180f
		bmi !-
		lda $1c01
		sta	BTAB + 2
		and	#$C0
		ora	BTAB + 1
		tax
		lda	L9F0D,x
		ora	BTAB
		pha
		jmp	L9667
L963B:
!:		bit $180f
		bmi !-
		lda $1c01
		tax
		lda	LA00D,x
		sta	BTAB
		txa
		and	#$07
		sta	BTAB + 1
!:		bit $180f
		bmi !-
		lda $1c01
		sta	BTAB + 2
		and	#$C0
		ora	BTAB + 1
		tax
		lda	L9F0D,x
		ora	BTAB
		sta	(bufpage),y
		iny
		beq	L96D7					// exit loop, whole block read
L9667:
		lda	BTAB + 2
		tax
		lda	LA10D,x
		sta	BTAB
		txa
		and	#$01
		sta	BTAB + 2
!:		bit $180f
		bmi !-
		lda $1c01
		sta	BTAB + 3
		and	#$F0
		ora	BTAB + 2
		tax
		lda	L9F0D + 2,x
		ora	BTAB
		sta	(bufpage),y
		iny
		lda	BTAB + 3
		and	#$0F
		sta	BTAB + 3
!:		bit $180f
		bmi !-
		lda $1c01
		sta	CHKSUM
		and	#$80
		ora	BTAB + 3
		tax
		lda	L9F0D + 16,x
		sta	BTAB
		lda	CHKSUM
		tax
		lda	LA20D,x
		ora	BTAB
		sta	(bufpage),y
		iny
		txa
		and	#$03
		sta	CHKSUM
!:		bit $180f
		bmi !-
		lda $1c01
		sta	BTAB + 1
		and	#$E0
		ora	CHKSUM
		tax
		lda	L9F0D + 29,x
		sta	BTAB
		lda	BTAB + 1
		tax
		lda	LA30D,x
		ora	BTAB
		sta	(bufpage),y
		iny
		jmp	L963B

L96D7:						// end of loop reached, whole block was read
		lda	BTAB + 2
		tax
		lda	LA10D,x
		sta	BTAB
		txa
		and	#$01
		sta	BTAB + 2
!:		bit $180f
		bmi !-
		lda $1c01
		and	#$F0
		ora	BTAB + 2
		tax
		lda	L9F0D + 2,x
		ora	BTAB
		sta	BTAB + 1
		pla
		cmp	DBID
		bne	L9707		// error 4
		ldx counter
		lda BTAB + 1
		sta RE_cached_checksums,x	// save expected checksum for later, can't do that now
		jmp ReadGCRSector71OK
CheckSumErr:
		ldx #$05		// error 5
		.byte $2c
L9707:	ldx #$04		// error 4
		lda #$ff
		jsr ResetOnlyCache	// nothing is cached
		txa
		jmp	L99B5		// return with error

ReadGCRSector71OK:
		// no checksum error, continue
		// adjust pointers
		inc bufpage+1
		inc counter
		lda counter
		cmp #maxsector		// all sectors already?
		beq DecodeData71  	// should never run
		jmp ReadHeader71	// no, read next one

		// doesn't have to be special for 71, isn't it?
		// almost the same as DecodeData but we need to check sector checksums and fall back into different place - there is enough space to duplicate that code
DecodeData71:
		// check checksums of all sectors
		lda #>RAMEXP			// can reuse bufpage here
		sta bufpage+1
		ldx #0
		// F5E9
CheckLoop:
		lda #0
		tay
!:		eor (bufpage),y
		iny
		bne !-
		cmp RE_cached_checksums,x
		bne CheckSumErr
		inc bufpage+1
		inx
		cpx counter
		bne CheckLoop

		// GCR data was decoded on the fly, but we also need those sector numbers
		// so go through all headers and decode them, put them back
		jsr DecodeHeaders	// decode cached headers (or try to decode them on the fly on 1571)

		// all was said and done, now read the sector from cache
		jmp ReadSector71
