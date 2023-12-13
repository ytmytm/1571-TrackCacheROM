
// http://www.unusedino.de/ec64/technical/formats/g64.html - 10 header GCR bytes? but DOS reads only 8 http://unusedino.de/ec64/technical/aay/c1541/ro41f3b1.htm
// http://unusedino.de/ec64/technical/aay/c1541/ro41f4d1.htm

/*
l "c:/Users/Maciejdev/Documents/Projects-shared/15x1-ramexp/rampatch.prg" 0
b f4d1
f 6000 8000 0
r pc=0300
*/

// 6% faster on plain load in C64 mode
// patch $C649 to jsr  into ResetCache (both modes)
// patch $F4D1 to jump into ReadSector (only 1541 MODE!)
// patch $EAE5 to NOP NOP (disable ROM checksum check)
// patch $960D to jump into ReadSector (1571 mode, could be faster to replicate code that starts there?)
	// XXX works for dir, not burst file load -> $0100 corruption?

// ??? also patch http://unusedino.de/ec64/technical/aay/c1541/ro41d00e.htm 'read block header' - command $B0 to read from decoded headers?
// ??? check headers checksums and mark invalid ones?

.const HEADER = $16
.const HDRPNT = $32
.const STAB = $24

.const BUFPNT = $30

// 1541 ROM locations (1571 in 1541 mode)
.const LF969 = $F969 // error continuation
.const LF556 = $F556 // wait for sync, set Y to 0
.const LF497 = $F497 // decode 8 GCR bytes from $24 into header structure at $16-$1A (track at $18, sector at $19)
.const LF4ED = $F4ED // part of read sector procedure right after GCR sector data is read - decode GCR from BUFPNT+$01BA:FF into BUFPNT
.const LF50A = $F50A // wait for header and then for sync, patched instruction at $F4D1
.const LF4D4 = $F4D4 // next instruction after patch at $F4D1

.const RAMEXP = $6000 // 8K of expanded RAM
.const RAMBUF = $7E00 // last page for various stuff

.const RE_cached_track = RAMBUF+$ff
.const RE_max_sector = RAMBUF+$fe
.const RE_decoded_headers = RAMBUF // can be the same as RE_cached_headers, separated for debug only
.const RE_cached_headers = RAMBUF+$0100

// DOS unused zp
.const bufpage = $14					// (2) pointer to page GCR data, increase by $0100
.const bufrest = $2c					// (2) pointer to remainder GCR data, increase by bufrestsize ($46 or $48)
.const hdroffs = $1b					// (1) offset to header GCR data at RE_cached_headers during data read and header decoding
.const counter = $37					// (1) counter of read sectors, saved in RE_max_sector ($4B DOS attempt counter can be used for this)
.const hdroffsold = $46					// (1) temp storage needed to compare current header with 1st read header

.const hdrsize = 8						// header size in GCR (8 GCR bytes become 5 header bytes)
.const maxsector = 22					// rather 21 but we have space

.const bufrestsize = $48				// size of GCR data over page size, it's $46 really, but it's rounded

.const RAMEXP_REST = RAMEXP+(maxsector*$0100)	// this is where remainder GCR data starts, make sure that it doesn't overlap RAMBUF (with sector headers)

		.pc = $AC00 "Patch F4D1"

ReadSector:
// patch $F4D1 to jump in here, required sector number is on (HDRPNT)+1, required track in (HDRPNT), data goes into buffer at (BUFPNT)
		ldy #0
		lda (HDRPNT),y			// cached track the same as required track?
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
		jsr LF50A
		jmp LF4D4

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
		// XXX check checksum here to mark mangled sector headers?
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

		// all was said and done, now read the sector from cache
		jmp ReadSector
