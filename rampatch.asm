
// 1571 ROM patch for drives with RAM expanded into $6000-$7FFF
// by Maciej 'YTM/Elysium' Witkowiak <ytm@elysium.pl>, 2023-12-03 V1.0

// Configuration
// uncomment one of the definitions below or pass them to KickAss as a command line option, e.g. -define ROM1571

// stock standalone 1571 ROM 310654-05
//#define ROM1571

// stock internal 1571CR ROM 318047-01
//#define ROM1571CR

// JiffyDOS for standalone 1571
//#define ROMJIFFY1571

// JiffyDOS for internal 1571CR
//#define ROMJIFFY1571CR

// JiffyDOS for internal 1571CR, patched for PAL systems with http://dtvforge.i24.cc/j1571dcr/
//#define ROMJIFFY1571CRPAL

// INFO
// - read whole track at once and cache
// - decode headers to get the order of sectors
// - 1541: keep GCR data, decode sectors from cache only when requested
// - 1571: decode GCR data on the fly, return actual data
// - needs only one disk revolution (20ms with 300rpm) to read whole track
// - sector GCR decoding errors are reported normally
// - header GCR decoding errors are not reported - but if sector is not found we fall back on ROM routine which should report it during next disk revolution
// - same patch for all four variations: stock ROM / JiffyDOS, standalone 1571 / 1571CR and also JiffyDOS for C128DCR PAL

// Excellent resources:
// http://www.unusedino.de/ec64/technical/formats/g64.html - 10 header GCR bytes? but DOS reads only 8 http://unusedino.de/ec64/technical/aay/c1541/ro41f3b1.htm
// http://unusedino.de/ec64/technical/aay/c1541
// https://spiro.trikaliotis.net/cbmrom

// ??? also check headers checksums and mark invalid ones?
// ??? decode 1541 sector data from GCR on the fly as in https://www.linusakesson.net/programming/gcr-decoding/index.php ?

.const HEADER = $16
.const HDRPNT = $32
.const STAB = $24
.const BUFPNT = $30 // (2)
.const DBID = $47

// 1571 
.const CHKSUM = $3A // (1)
.const BTAB = $52 // (4)

// 1541 ROM locations (1571 in 1541 mode)
.const LC1D1 = $C1D1 // find drive number, instruction patched at $D005
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
.const L99B5 = $99B5 // return with error code in A

// DOS unused zp
.const bufpage = $14					// (2) 1541/71 pointer to page GCR data, increase by $0100
.const bufrest = $2c					// (2) 1541 pointer to remainder GCR data, increase by bufrestsize; written to by GCR decoding routine at F6D0 but on 1541 that's after bufpage/bufrest was already used, doesn't appear in patched 1571 at all
.const hdroffs = $1b					// (1) 1541/71 offset to header GCR data at RE_cached_headers during data read and header decoding
.const counter = $1d					// (1) 1541/71 counter of read sectors, saved in RE_max_sector (alternatively use $4B DOS attempt counter for header find); written to by powerup routine at $EBBA (write protect drive 1), but that's ok
.const hdroffsold = $46					// (1) 1541/71 temp storage needed to compare current header with 1st read header; written to and decremented, but only after load in $917D / $918D (after jump to L970A)

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
.const RE_lastmode = RAMBUF+$fd
.const RE_decoded_headers = RAMBUF // can be the same as RE_cached_headers, separated for debug only
.const RE_cached_headers = RAMBUF+$0100
.const RE_cached_checksums = RAMBUF-$0100 // 1571 only

/////////////////////////////////////

#if !ROM1571 && !ROM1571CR && !ROMJIFFY1571 && !ROMJIFFY1571CR && !ROMJIFFY1571CRPAL
.error "You have to choose ROM to patch"
#endif

#if ROM1571
.print "Assembling stock 1571 ROM 310654-05"
.segmentdef Combined  [outBin="1571-rom.310654-05-patched.bin", segments="Base,Patch1,Patch2,Patch3,Patch4,Patch5,Patch6,Patch7,Patch8,MainPatch", allowOverlap]
.segment Base [start = $8000, max=$ffff]
	.var data = LoadBinary("rom/1571-rom.310654-05.bin")
	.fill data.getSize(), data.get(i)
#endif

#if ROM1571CR
.print "Assembling stock 1571CR ROM 318047-01"
.segmentdef Combined  [outBin="1571cr-rom.318047-01-patched.bin", segments="Base,Patch1,Patch2,Patch3,Patch4,Patch5,Patch6,Patch7,Patch8,MainPatch", allowOverlap]
.segment Base [start = $8000, max=$ffff]
	.var data = LoadBinary("rom/1571cr-rom.318047-01.bin")
	.fill data.getSize(), data.get(i)
#endif

#if ROMJIFFY1571
.print "Assembling JiffyDOS for standalone 1571 ROM 310654-05"
.segmentdef Combined  [outBin="JiffyDOS_1571_repl310654-patched.bin", segments="Base,Patch1,Patch2,Patch3,Patch4,Patch5,Patch6,Patch7,Patch8,MainPatch", allowOverlap]
.segment Base [start = $8000, max=$ffff]
	.var data = LoadBinary("rom/JiffyDOS_1571_repl310654.bin")
	.fill data.getSize(), data.get(i)
#endif

#if ROMJIFFY1571CR
.print "Assembling C128DCR JiffyDOS 1571CR ROM"
.segmentdef Combined  [outBin="JiffyDOS_1571D-patched.bin", segments="Base,Patch1,Patch2,Patch3,Patch4,Patch5,Patch6,Patch7,Patch8,MainPatch", allowOverlap]
.segment Base [start = $8000, max=$ffff]
	.var data = LoadBinary("rom/JiffyDOS_1571D.bin")
	.fill data.getSize(), data.get(i)
#endif

#if ROMJIFFY1571CRPAL
.print "Assembling C128DCR JiffyDOS 1571CR patched for PAL http://dtvforge.i24.cc/j1571dcr/"
.segmentdef Combined  [outBin="jd71dcr-pal-patched.bin", segments="Base,Patch1,Patch2,Patch3,Patch4,Patch5,Patch6,Patch7,Patch8,MainPatch", allowOverlap]
.segment Base [start = $8000, max=$ffff]
	.var data = LoadBinary("rom/jd71dcr-pal.bin")
	.fill data.getSize(), data.get(i)
#endif

/////////////////////////////////////

.segment Patch1 []
		.pc = $F4D1 "Patch 1541 sector read"
		jmp ReadSector

.segment Patch2 []
		.pc = $960D "Patch 1571 sector read"
		jmp ReadSector71

.segment Patch3 []
		.pc = $C649 "Patch disk change"
		jsr ResetCache

.segment Patch4 []
		.pc = $EAE5 "Patch ROM checksum"
		nop
		nop

.segment Patch5 []
		.pc = $F2C0 "Patch 1541 IRQ routine for disk controller"
		jsr InvalidateCacheForJob

.segment Patch6 []
		.pc = $92CA "Patch 1571 IRQ routine for disk controller"
		jsr InvalidateCacheForJob

.segment Patch7 []
		.pc = $D005 "Patch 'I' command"
		jsr ResetOnlyCache

.segment Patch8[]
		.pc = $9027 "Patch 'U0>M{01}' execution"
		jsr InvalidateCacheForModeChange

/////////////////////////////////////

.segment MainPatch [min=$b700,max=$beff]

		// $B700-$BEFF area available both on stock and jiffydos
		.pc = $B700 "Patch"

/////////////////////////////////////

InvalidateCacheForModeChange: {	// patch for U0>M0 switch that would keep returning read error if called when track 18 is active and currently cached
		jsr ResetOnlyCache
		lda $0204				// patched instruction - take digit of the mode parameter: '0' or '1'
		rts
}

/////////////////////////////////////

InvalidateCacheForJob: {
		tya						// enters with Y as job number (5,4,3,2,1,0), we can change A,X but not Y
		tax
		lda $00,x				// job?
		bpl return				// no job
		cmp #$D0				// execute code?
		beq return				// yes, exec doesn't seek to track
		cmp #$90				// write sector?
		beq resetcache			// yes, always invalidate cache
		tya						// get track
		asl						// *2
		tax
		lda $06,x				// check track parameter for job
		cmp RE_cached_track		// is it cached already?
		beq return				// yes, there will be no track change
resetcache:
		jsr ResetOnlyCache		// invalidate cache
return:
		lda $00,y				// instruction from patched $F2C0 and $92CA, must change CPU flags
		rts
}

/////////////////////////////////////

InitializeAndResetCache:
		jsr ResetOnlyCache
		jmp LC1D1				// instruction from patched $D005

/////////////////////////////////////

ResetCache:						// enters with A=$FF
		sta $0298				// instruction from patched $C649, set error flag
ResetOnlyCache:
		lda #$ff
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
#if ROMJIFFY1571 || ROMJIFFY1571CR || ROMJIFFY1571CRPAL
// JD is making it harder, calls 1571 mode sector read (for fast mode) even when track was cached in 1541 mode already)
		bit RE_lastmode			// was cached data decoded from GCR (on the fly)?
		bmi !+					// no, decode it first
		jmp ReadCache71			// data in cache is there already binary, no need for decoding
!:
#endif
		iny						// yes, track is cached, just put GCR data back and jump into ROM
		lda (HDRPNT),y			// needed sector number
		sta hdroffs				// keep it here
		// setup pointers
		lda #>RAMEXP			// pages - first 256 bytes
		sta bufpage+1
		lda #>RAMEXP_REST		// remainders - following bytes until end of sector ($46 bytes)
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
		// not found? fall back on ROM and try to read it again
		jsr LF50A				// replaced instruction
		jmp LF4D4				// next instruction

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
		
		jmp LF4ED	// we have data as if it came from the disk, continue in ROM: decode and return 'ok' (or sector checksum read error)

/////////////////////////////////////

ReadTrack:
		sta RE_cached_track		// this will be our new track for caching

		lda #>RAMEXP			// pages - first 256 bytes
		sta bufpage+1
		lda #>RAMEXP_REST		// remainders - following bytes until end of sector ($46 but we add $80 each time)
		sta bufrest+1
		lda #0
		sta bufpage
		sta bufrest
		sta hdroffs				// 8-byte counter for sector headers at RAMBUF
		sta counter				// data block counter

		// end loop when header we just read is the same as 1st read counter (full disk revolution) or block counter is 23
ReadHeader:
		jsr	LF556			// ; wait for SYNC, Y=0
		ldx hdroffs
		stx hdroffsold
		//ldy #0			// F556 sets Y to 0
!:		bvc !-
		clv
		lda $1c01
		cmp #$52			// is that a header?
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
		// do we have that sector already? (tested on VICE that there is enough time to check it before sector sync even on the fastest speedzone (track 35))
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
		jmp DecodeHeaders	// yes, no need to read more

ReadGCRSector:
		jsr LF556			// wait for SYNC, will set Y=0
!:		bvc !-
		clv
		lda $1c01
		sta (bufpage),y
		iny
		bne !-
		ldx #$BA			// write rest: in ROM from $1BA to $1FF, here we just count it up
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
		cmp #maxsector		// do we have all sectors already? (should be never equal)
		beq DecodeHeaders	// this jump should be never taken
		jmp ReadHeader		// not all sectors, read the next one

DecodeHeaders:
		// we don't need to decode GCR sector data right now, but we need those sector numbers
		// so go through all headers and decode them, put them back
		ldx #0
		stx bufrest			// reuse for counter
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
		jsr LF497			// decode 8 GCR bytes from $24 into header structure at $16-$1A (track at $18, sector at $19)
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
		lda hdroffs			// next header data offset
		clc
		adc #hdrsize
		sta hdroffs
		inx
		stx bufrest			// next header counter
		cpx counter
		bne DecodeLoop
		stx RE_max_sector
#if ROMJIFFY1571 || ROMJIFFY1571CR || ROMJIFFY1571CRPAL
		// cache in 1541 mode - stored as GCR, needs decoding
		lda #$80
		sta RE_lastmode
#endif

		// all was said and done, now read the sector from cache
		jmp ReadSector

/////////////////////////////////////

ReadSector71:
// patch $960D to jump in here, required sector number is on (HDRPNT)+1, required track in (HDRPNT), data goes into buffer at (BUFPNT)
		ldy #0
		lda (HDRPNT),y			// is cached track the same as required track?
		cmp RE_cached_track
		beq ReadCache71
		jmp ReadTrack71			// no - read the track in native 71 mode

ReadCache71:
#if ROMJIFFY1571 || ROMJIFFY1571CR || ROMJIFFY1571CRPAL
// JD is making it harder, calls 1571 mode sector read (for fast mode) even when track was cached in 1541 mode already)
		bit RE_lastmode			// was cached data already decoded from GCR (on the fly)?
		bpl !+					// yes
		jmp ReadCache			// no, data in cache is GCR-encoded, needs to be decoded first
!:
#endif
		iny						// yes, track is cached, just put data back and jump into ROM
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
		// not found? fall back on ROM and try to read it again
		jsr L9600				// replaced instruction
		jmp L9610				// next instruction

!found:	// copy data and fall back into ROM		
		ldy #0
!:		lda (bufpage),y
		sta (BUFPNT),y
		iny
		bne !-

		jmp L970A				// we have data as if it came from the disk, continue in ROM: decode and return 'ok'

ReadTrack71:
		sta RE_cached_track		// this will be our new track for caching

		lda #>RAMEXP			// decoded data pages
		sta bufpage+1
		lda #0
		sta bufpage
		sta hdroffs				// 8-byte counter for sector headers at RAMBUF
		sta counter				// data block counter

		// end loop when header we just read is the same as 1st read counter (full disk revolution) or block counter is 23
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
		bne ReadHeader71	// no, wait until next SYNC
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
		// do we have that sector already? (tested on VICE that there is enough time to check it before sector sync even on the fastest speedzone (track 35))
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

// GCR decoding tables in 1571 ROM
.const L9F0D = $9F0D
.const LA00D = $A00D
.const LA10D = $A10D
.const LA20D = $A20D
.const LA30D = $A30D

ReadGCRSector71:
		jsr L9754			// wait for SYNC, will set Y=0
		// copied from 9610 onwards: read and decode sector on the fly
		// but replaced sta (BUFPNT),y by sta (bufpage),y
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
		beq	L96D7					// exit loop, whole block was read
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
		bne	L9707			// issue error 4
		ldx counter
		lda BTAB + 1
		sta RE_cached_checksums,x	// save expected checksum for later, we can't checksum that sector now
		jmp ReadGCRSector71OK
CheckSumErr:
		ldx #$05			// issue error 5
		.byte $2c
L9707:	ldx #$04			// issue error 4
		jsr ResetOnlyCache	// any error invalidates cache, nothing is cached
		txa
		jmp	L99B5			// return with error

ReadGCRSector71OK:
		// there were no checksum errors yet, continue reading sectors
		// adjust pointers
		inc bufpage+1
		inc counter
		lda counter
		cmp #maxsector		// do we have all sectors already? (should be never equal)
		beq DecodeData71  	// this jump should be never taken
		jmp ReadHeader71	// not all sectors, read the next one

		// almost the same as DecodeData but we need to validate sector checksums and return back into ReadSector71
		// we use different header decoding ROM routine, probably faster(?)
DecodeData71:
		// check checksums of all sectors
		lda #>RAMEXP			// can reuse bufpage here
		sta bufpage+1
		ldx #0
		// this came from F5E9
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
		// this is the same as DecodeHeaders for 1541, but we use L952F, apparently faster GCR decoder
		ldx #0
		stx bufrest		// reuse for counter
		stx hdroffs

DecodeLoop71:
		ldy hdroffs
		ldx #0
!:		lda RE_cached_headers,y
		sta STAB,x
		iny
		inx
		cpx #hdrsize
		bne !-
		jsr L952F		// decode 8 GCR bytes from $24 into header structure at $16-$1A (track at $18, sector at $19)
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
		lda hdroffs		// next header data offset
		clc
		adc #hdrsize
		sta hdroffs
		inx
		stx bufrest		// next header counter
		cpx counter
		bne DecodeLoop71
		stx RE_max_sector
#if ROMJIFFY1571 || ROMJIFFY1571CR || ROMJIFFY1571CRPAL
		// cache in 1571 mode - already decoded
		lda #$00
		sta RE_lastmode
#endif
		// all was said and done, now read the sector from cache
		jmp ReadSector71
