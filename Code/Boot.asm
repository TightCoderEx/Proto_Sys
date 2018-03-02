%include	'BIOS.inc'

; REFERENCES -> Intel 64 and IA-32 Architectures ORDER # 325462-O47US June 2013

	STK_SIZE	equ	 24		; 1k blocks for stack.
	MEM_SIZE	equ	12H

	%define	MAP_ENTR	BP		; # of entries in E820 map
	%define	MAP_SIZE	BP + 1		; # of bytes / entry
	%define	MAP_ADDR	BP + 2		; Long pointer to base address of map
	%define	A20_Stat	BP + 6   		; Status of A20 activation RED, YELLOW or GREEN

	%define	BOOT_DEV	BP - 10		; Media code was read from.

; For curiosity sake only, contents of registers will be displayed in either protected
; or long mode.

    ; Vol 2B 4-269 pg 1324

	pusha			; AX CX DX BX SP BP SI DI

    ; The order of these is just simply to facilitate routine that is going to display contents.

	push	ds
	push	es
	push	ss
	push	fs
	push	gs
	push	cs

; There are some quirky BIOS floating around that pass CS = 7C0H, but as this code
; originates at zero, CS must be set to that.

	jmp	0x7c0:Begin

; Establish a stack frame of STK_SIZE * 1K pages (Max 64 @ 1024 bytes). Initially,
; this frame will persist even into protected and long modes

    ; Determine how much space is available in conventional memory below EBDA

Begin:	int	MEM_SIZE	; On test machine 27FH = 639 * 1024 = 654,336 bytes
	mov	cx, STK_SIZE	; Size of stack frame in 1K blocks
	sub	ax, cx

    ; Extrapolate base address and frame size from MEM_SIZE & STK_SIZE.

	shl	ax, 6		; Convert pages to segment
	shl	cx, 8

    ; This is so the algorithm that probes stack will be looking for DWORD sequences of -1.

	mov	es, ax
	xor	di, di		; ES:DI points to top of stack frame for fill
	or          eax, -1		; Set fill pattern
	rep	stosd		; Fill frame with -1s

    ; 1/8 of this frame is going to be reserved for misc data and it will be pointed to by BP

	mov	ax, di		; Size of frame in bytes
	shr	ax, 3		; Size /= 8
	mov	bp, di
	sub	bp, ax		; BP = offset from bottom of stack

    ; Setup pointers DS:SI & ES:DI so current contents of stack can be moved to new frame.

	mov	si, sp
	push	ss
	pop	ds		; DS:SI points to top of current stack
	mov	di, bp
	sub	di, 32		; ES:DI points to new bottom of stack

    ; Move 8 DWORDs to new stack frame

	mov	cl, 8
	push	di		; This will be new SP
	rep	movsd
	pop	di

    ; Disable interrupts and set new stack pointer that will contain original contents saved
    ; in prologue.

	cli
	push	es
	pop	ss
	mov	sp, di		; SS:SP = Bottom of stack in new frame
	sti

; Read E820 map into a temporary buffer just above boot sector @ 7E0:0

	mov    	ax, 0x7e0	; So MSB of EAX is nullified
	mov	es, ax
	mov	ds, ax		; So segment overrides are not required
	xor	di, di		; ES:DI = Pointer to base address of map
	mov	bx, di		; Initial continuation value
	mov	edx, 'PAMS'	; Function signature
	push	edx

    ; Top of loop to read first or next map entry

NEnt:	inc	byte [MAP_ENTR]	; Bump number of map entries = 0 first iteration.
    .skip:
	mov	cl, 48		; Let function call know how big entry can be.
	mov	ax, 0xe820	; System Service function number.
	int	15H

    ; Assert the possible error and termination conditions

	jc	.done		; CY = 1 can happen in all cases
	cmp	bl, 1		; Is this the first entry
	jb	.done		; If zero, no more entries
	ja	.J0		; Next code only needs happen on first iteration

    ; This need only happen on first iteration

	pop	edx
	sub	eax, edx		; Does BIOS even support this function
	jz	.J0 - 3
	dec	byte [MAP_ENTR]	; Bump value back to -1
	jmp	.done

	mov	[MAP_SIZE], cl	; Save actual size of entry returned by function.
 .J0:
	jcxz	.skip		; Ignore any null length entries
	cmp	cl, 20
	jbe	.J1

	test	byte [di + 20], 1	; Ignore ACPI entries
	jz	.skip

    ; Test 64 bit value representing length for zero
.J1:
	mov	eax, [di + 8]	; Get low order DWORD of length
	or	eax, [di + 12]	; Determine if QWORD value is zero
	jz	.skip

    ; Bump ES:DI pointer to next entry

	add	di, cx
	jmp	NEnt
.done:
	or	di, di		; Was a map even created
	jnz	.mvMap

	mov	ax, 0xb800	; Point to video
	mov	es, ax
	mov	di, 0x7CE		; Offset to vertical & horizontal center of screen

    ; This will display flashing "[ ]" in yellow with "E" between in high intensity white

	mov	eax, 0xf458e5b
	stosd
	inc	al
	inc	al		; AL = "]"
	stosw
	push	ss		; Define upper for calculating total sectors
	jmp	MoveBlock	; Dont need to move map as it doesnt exist

    ; Move E820 entries immediately below top of stack frame.

      .mvMap:
 	mov	cx, di		; Get copy of total bytes in E820 map
	shr	cx, 2		; CX / 4 = Total DWORDS to move

    ; Only every 4th entry is segment aligned (16 bytes), so offset in DI needs to be
    ; calculated so last entry of map terminates at top of stack frame.

	mov	ax, ss		; Get base of stack frame
	sub	bx, di
	and	bx, 15		; BL = 0, 4, 8, 12
	jz	$ + 3

    ; Because BL <> 0 segment has to be skewed by one

	dec	ax		; Bump back one more segment

    ; Now offset can be saved and moved into DI

	mov	[MAP_ADDR], bx	; Lower half of long pointer
	xchg	di, bx		; Move offset into index
	shr	bx, 4		; BL = Total # of 16 byte segments
	sub	ax, bx
	mov	[MAP_ADDR+2], ax	; Upper half of long pointer
	mov	es, ax		; ES:DI = Destination buffer
	push	ax		; Define upper for calculating total sectors

    ; Establish source pointer and then move CX DWORDS

	xor	si, si		; DS:SI = Source
	rep	movsd

; Often this type of code is referred to as a chain loader. This simply means that in place
; of locating code via a file system, contiguous binary images are read from boot media.
; In this case, as many even sectors as will fit between 60:0 and AX:0 will be read.

    ; First thing that needs to happen, is upper half of boot sector moved out of the way
    ; as this region will be overwritten.

 MoveBlock:
	mov	ax, 50H		; Beginning of destination buffer @ 50:0
	mov	es, ax
	xor	di, di

    ; So code from upper half of boot sector will naturally fall through to subsequent
    ; sectors, boot signature @ 1FE must be replaced with NOPs

	push	cs
	pop	ds
	mov	si, 254
	lodsw			; We know this because ALIGN padds with NOP

	mov	cx, 127		; Move everything except boot signature
	rep	movsw
	stosw			; Replace AA55H with NOPs

; Is one of two values. Either equal to SS or if there no error creating E820, base segment of
; maps first entry. In either case, the total number of full sectors that will fit between
; Segment 60H and DX can now be calculated.

	pop	dx
	sub	dx, 96		; Skew by unavailable space.
	shr	dx, 5		; Segment / 32 is the same as bytes / 512

    ; Padding is only to facilitate development tool chain, but is otherwise irrelevant.

	times	248 - $+$$ db 144	; Force next instruction to start @ 7CF8H

    ; Setting CS to 40H makes it so non-relative pointers will still work after code
    ; is executing at 500H.

	jmp	40H:GetIMG	; Branch to new code segment

	align	128
; =============================================================================================

GetIMG:	push	dx		; Save number of sectors that can be read from boot dev.

; Populate from 600H with as many sectors as can be expected from a low density floppy.

	xor	bx, bx
	mov	ax, 0x60
	mov	es, ax		; ES:BX = Buffer address
	mov	dl, [BOOT_DEV]	; Get device system was booted from 00-02 or 80H - 82H
	mov	dh, 0		; DH = Head
	mov	cx, 2		; Begin at physical sector 2
	mov	ax, 0x20d	; Populate memory up to 2000H
	int	13H
	jc	.error

	sub	[esp], ax		; Skew total records by number already read.
	jmp	DoA20

; Point to the place in video where we want this error to display. In this case center
; vertically & horizontally.

      .error:
	mov	dx, 0b800H
	mov	es, dx
	mov	di, 7CEH		; Offset to center of page 0

    ; Now the error has to be displayed in coded form as there just is not enough room from
    ; here to end of sector to be any more detailed.

	test	ah, 15		; Is the error between 1 and 15
	jz	.J0

    ; There are 8 errors less than 16 so these will be displayed in their equivalent
    ; decimal value, except error 12 that will be displayed as "?".

	cmp	ah, 12		; Is it error 12
	jnz	$ + 5
	add	ah, 3		; Bump up so it will display as ?
	or	ah, 48
	jmp	.J1

    ; There are 4 errors here and they are represented by a single bit. Once shifted
    ; into low nibble, these errors will be converted to A, B, D & H.

      .J0:
	shr	ax, 4
	or	ax, 0x4000	; Bet bit to convert to uppercase alphabetic character

    ; Now that error number or letter is going to be displayed in high intensity white
    ; inside vertical red vertical bars.

      .J1:
	mov	al, 15		; This is bright white
	bswap	eax		; Moves into high order EAX and swaps AH with AL
	mov	ax, 0cddH	; 0C = bright red and DD = left vertical bar
	stosd			; Display left vertical bar with error number or letter
	inc	al		; Convert to right vertical bar
	stosw			; Only display it

    ; This in all likely hood is a fatal error so there is not much to be done beyond this
    ; point other than rebooting.

	hlt
	jmp	$ - 1

; In order to do a wrap-around test, ES:DI must point to the same place in next meg of memory
; as DS:SI. In this case it will be 7D00 & 107D00 (FFFF:7D10)

DoA20:	or	ax, -1
	mov	es, ax		; Set ES to FFFFH
	inc	ax
	mov	ds, ax

    ; Establish a retry count for fast A20.

	mov	cx, ax
	mov	ch, 12		; Set retry count of 3,072 in CX

    ; Initialize appropriate offsets in SI & DI

	mov	ah, 0x7d
	mov	si, ax
	mov	al, 16
	mov	di, ax
	mov	bl, GREEN
      .test:
	mov	ax, [si]
	neg	ax
	mov	[es:di], ax
	cmp	[si], ax
	jnz	.set

	mov	bl, BROWN	; Set status that fast A20 was used to enable
	in	al, 0x92
	or	al, 2
	out	0x92, al
	dec	cx
	jnz	.test

    ; This is where alternate methods of enabling line would go. code should fall through
    ; where BL is set to RED if it fails.

	mov	bl, RED		; Unable to enable A20.
      .set:
	mov	[A20_Stat], bl	; Save status in scratch area above stack

; At this point,  boot sector code is completely volatile and it is by design. As I progress
; through the initialization process, past code can be overwritten with IVTs GDTs, whatever.
; This jump is only because I want CS:IP = 60H:0. Otherwise it would just fall through the
; next 122 NOPs and at beginning of next sector CS:IP = 40H:200.

	times	510- $ + $$	db	144

; Once code in boot sector from 100H has been moved to 500H, this signature has been
; replaced with NOPs

	dw	0AA55H		; Boot signature
