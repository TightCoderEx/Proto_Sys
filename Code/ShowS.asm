%include   'BIOS.inc'

  %define ARG0		BP + 4		; ARG0 = Far pointer to ASCIIZ string.
  %define SafeStk	BP - (FrmSize +  7 * 2)
  %define RVects		BP - 2
  %define StrLen		BP - 4		; Length of most recent calculated string
  
Cursors	   equ	50H	; Base of array of WORDS for cursor position of each page.
   CurPg	   equ	62H	; Current page being displayed. Offset from cursors = 462H in BDA.
      EOS	   equ	  0H	; End Of String
 FrmSize    equ	  4H	; Bytes reserved for local data

; =============================================================================================
;   Display ASCIIZ text on an 80 x 25 x 16 color console. This can be a single or an array
;   of strings, but must be terminated with DOUBLE NULL.

;	ENTER:	ARG1 = Segment portion of far pointer to string
;			ARG0 = Offset and ultimately loaded into DS:SI

;	LEAVE:	  AX = Offset to next string (Only if ZF = 0)
;			  CX = -1 if there was a buffer overrun (string larger than 128 bytes)
;			  DX = Size of memory leak on stack in bytes

;	FLAGS:	  ZF = 1 if at end of array of strings, otherwise there is another string.
; ---------------------------------------------------------------------------------------------

ShowS:	enter	FrmSize, 0

; NOTE: Any changes in this prologue must be representative in SafeStk definition.

	push	es		; 1
	push	ds		; 2
	push	di		; 3
	push	si		; 4
	push	fs		; 5
	push	BDA
	pop	fs		; Establish segment into BDA
	push	cs		; As it is a far call into this procedure, DS might
	pop	ds		; not be pointing to CS
	mov	si, Params	; DS:SI points to the two DWORD parameters
	push	ds		; 6
	push	si		; 7  SafeStk points here

	lodsw			; Get 16 bit offset into desired page.
	mov	di, ax
	lodsw			; Read relative position offset
	mov	[RVects], ax
	mov	[StrLen], ax
	lodsw			; AH = Attribute, AL = Page #
	push	ax
	add	al, 0B8H	; Segments being @ 80x25 16 color
	shl	ax, 8
	mov	es, ax		; ES:DI setup for STOSB or STOSW
	pop	ax		; Retrieve AH = characters attribute

	lds	si, [ARG0]	; Grab far pointer to ASCIIZ string
	mov	cx, 128		; Overflow count
    .next:
	lodsb			; Read next or first char
	cmp	al, EOS
	jz	.done
	cmp	al, ' '
	jb	DoCtl		; Execute handler for control character
     .post:
	or	ah, ah		; Has an attribute been defined
	jnz	.attr
	stosb
	inc	di
	jmp	.attr + 1	; Do overrun check
     .attr:
	stosw
	dec	cx		; Decrement displayable character count.
	jnc	.next		; Return CX = -1 for overflow
     .done:
	mov	dx, sp
	lea	sp, [SafeStk]	; Changes in prologue affects this
	sub	dx, sp
	neg	dx		; CX = Difference in pointers
	shr	ax, 8		; Move characters attribute into low nibble
	push	ax
	push	es
	mov	ax, di		; Save copy of offset
	les	di, [SafeStk]
	stosw			; Save location of next write
	mov	ax, [RVects]
	stosw

; Skewing SP by one, forces attribute back into AH & MSB of video segment into AL

	inc	sp
	pop	ax
	inc	sp
	and	al, 7		; Converts MSB of segment into page # (0-7)
	stosw
	add	sp, 4		; Waste far pointer to procedures parameters

; Test character after this strings terminator to set ZF appropriately.

	test	byte [si], 0FFH	; Test if there is another string.
	mov	ax, si		; Return offset to next string in AX

	pop	fs
	pop	si
	pop	di
	pop	ds
	pop	es

	leave			; Kill procedure frame
	ret	2		; Only waste far pointers offset

	align	4
; ____________________________________________________________________________________________

Params:	dw	 0		; Offset into page in bytes
	dw	 0		; x/y start for relative positioning
	db	 0		; Page to write too (0 - 7)
	db	 0		; Characters attribute
	dw	-1		; Padding to work out to even boundary.

    ; Offset = (DH * 80 + DL) * 2

; To avoid conditional jump of more than 128 bytes, this allows ret to branch back
; to DoCtl from a place than will definitely be more than 128 bytes away. It also serves
; as a convenient entry point for things that apply to each handler such as preserving AX

DoCtl:	push	ax		; Need to preserve attribute
	push	cx		; Save overflow counter
	push	.done		; Set to return address
	jmp	F01		; First function in list
     .done:
	pop	cx		; Restore overflow count
	pop	ax		; and attribute in AH
	jmp	ShowS.next	; Continue reading string

; ============================================================================================
; Convert x/y coordinates to absolute address into video memory.

;	ENTER:	ARG0 = [BP + 4]
;	          Bits 	15 - 08 = Row #, must be < 25
;			07 - 00 = Col #, must be < 80

;	LEAVE:	AX = Absolute offset within video page pointed to by ES.
;		AX = Original value, if zero or beyond x/y extents
;		     All other registers preserved.

;	FLAGS:	NC = Procedure successful
;		CY = When extents exceeded.
;								       2BH = 43 Bytes
; --------------------------------------------------------------------------------------------

XY2A:	push	bp		; Empty frame so CDECL argument can be addressed using BP
	mov	bp, sp
	push	cx		; So procedure is non-destructive.

    ; Comparing a register to an immediate is not allowed, so this method sets carry
    ; whenever row and/or column is out of bounds.

	mov	cx, 184FH	; Set to maximum rows and columns (zero indexed)
	mov	ax, [ARG0]	; Get value passed by caller
	or	ax, ax
	jz	.done		; ZF = 1, nothing to do, at top/left 0,0

    ; Column counts could be greater than 79 as long as rows * cols < 2000. As an example
    ; 00FFH is the same as 030FH, but the intent of algorithm is to stay within logical
    ; bounds.

	cmp	ch, ah		; Is row count > 24
	jc	.done
	cmp	cl, al		; Is column count > 79
	jc	.done

    ; To be compatible with BIOS expectations AH has to be moved into AL and then AL added
    ; to itself again.

	push	dx		; So procedure is non-destructive.
	movzx	dx, al		; Make 16 bit representation of columns
	shr	ax, 8		; Shift row count into low order bits
	inc	cl		; We know CL was already 79
	imul	cl
	add	ax, dx		; This is now offset from 0,0
	shl	ax, 1		; Must be doubled because each character has an attribute.
	pop	dx
      .done:
	pop	cx
	leave			; Kill procedure frame
	ret	2		; Return to calling wasting parameter passed on stack.

; ============================================================================================
; Convert absolute address to x/y vector.

;	ENTER:	ARG0 = Absolute address to be converted (between 0 and 2000).

;	LEAVE:	AX = Coordinates -> AH = Row and AL = Column (zero indexed)
;		AX = Original value, if zero or beyond screen limit.
;		     All other registers persevered.

;	FLAGS:	NC = Procedure successful
;		CY = When value beyond limits > 2000 (FA0H).
;								       25H = 37 Bytes
; --------------------------------------------------------------------------------------------

A2XY:	push	bp		; Empty frame so CDECL argument can be addressed using BP
	mov	bp, sp

	mov	ax, [ARG0]	; Read value to be converted to x/y coordinates
	or	ax, ax
	jz	.done		; Nothing to do if at 0,0.

    ; It is possible address points to attribute instead of character. Shifting
    ; to right (AX /= 2) guarantees x/y will still point to the proper position.

	shr	ax, 1		; AX /= 2 masks out bit 0 in-case address was odd.
	cmp	ax,80*25	; Is address withing bounds of page
	jc	.valid		; Return with error condition (CY = 1)
	stc			; Set condition that value would exceed extents.
	jmp	.done

    ; An 80 column screen is 160 bytes due to attribute character combinations. Dividing
    ; by this value saves the need to divide AX by 2.

    .valid:
	push	cx
	mov	cl, 80		; Number of columns / row (include attribute).
	div	cl

    ; Dividend is in AL and remainder in AH. These need to be exchanged to be compatible
    ; with BIOS routines that expect x/y coordinates in DX.

	mov	cl, ah
	shl	ax, 8		; Move row into AH
	mov	al, cl
	pop	cx
    .done:
	leave			; Kill procedure frame
	ret	2		; Return to calling wasting parameter passed on stack.

; =============================================================================================
;    Change display and/or destination page

;  ENTER: DS:SI +00  -	7 = Use current display page
;		      6-4 = Display page (0-7)
;			3 = Use current destination page
;		      2-0 = Destination page.

;  LEAVE: ES:DI = New pointer if display page (active) was changed
;	     DX = volatile, all others unchanged.

;  FLAGS: Undefined
; ---------------------------------------------------------------------------------------------

F01:	cmp	al, 1
	jnz	.finished + 2

	push	bx		; Preserve only non-volatile being used by procedure.
	mov	bx, CurPg	; Point to address in BDA of active page.
	mov	cl, [fs:bx]	; Get copy of page being displayed from BDA.
	lodsb			; Read single parameter from ASCIIZ string for this function.
	test	al, 128		; Does page being displayed need to change.
	jnz	.ChgDestPg	; Bit 7 on, not changing display page

    ; Change display page, but only if it is not the same as already being displayed.

	push	ax		; Preserve low nibble.
	shr	al, 4		; Shift page to display into low nibble.
	cmp	al, cl		; Does page actually need to be changed
	jz	.noPgChg	; Bounce but AX needs to be restored
	mov	ah, CAP		; BIOS Video function 5, change active page
	int	VIDEO		; execute BIOS video interrupt. 10H
    .noPgChg:
	pop	ax
    .ChgDestPg:
	test	al, 8		; Does display page need to be changed
	jnz	.finished

    ; Again, only change to new page if not the same as current destination.

	and	al, 7		; High nibble can be wasted now
	mov	cx, es
	and	ch, 7
	cmp	al, ch		; If they are the same, then nothing to be done
	jz	.finished

    ; Update ES, get coordinates of caret as that will become new starting point for DI.

	mov	ch, al		; Will be 0-7
	add	ch, 0B8H	; Make 16 bit segment
	mov	es, cx		; Update ES register
	mov	bl, al
	shl	bl, 1
	add	bl, Cursors	; BX points to desired array of x/y vectors in BDA
	mov	dx, [fs:bx]	; Get vectors for selected page
	or	dx, dx		; No calculation required if at 0,0
	jz	.finished - 2
	push	dx
	call	XY2A
	mov	di, ax		; Set offset into page
    .finished:
	pop	bx		; Retrieve non-volatile
	ret			; Carry on reading chars from ASCIIZ string.

; =============================================================================================
; Fill 80 x 25 screen with attribute and/or character with optional positioning of caret and
; next write to page.

; 	       		BYTE  BIT
;	ENTER:	DS:SI   +00 - 7-4 = Reserved for future use
;                      		3 = Set to use current display page
;      		     	      2-0 = Page # (0 - 7), ignored when bit 3 is on.

;			+01 - 7-0 = Fill character

;			+02 -   7 = Set for flashing characters
;     		     	      6-4 = Background color
;  		       		3 = High intensity foreground
;		     	      2-0 = Foreground color

;	LEAVE:	ES:DI preserved
;		DS:SI Points to next character(s) in string.

;	FLAGS:	ZF = 1 if both attribute & fill char where null, NZ otherwise.
; ---------------------------------------------------------------------------------------------

F02:	cmp	al, 2		; Do we want to clear a page.
	jnz	.exit + 3	; ZF=0, Jump to next handler.

	push	es		; In the event writing to a different page
	lodsb			; Read condition bits
	test	al, 1000B	; Write to current destination.
	jnz	.fillPage

	and	al, 7		; Clear any possible erroneous bits
	or	al, 0B8H
	shl	ax, 8
	mov	es, ax

     .fillPage:
	push	di		; Save offset of where we are on active page.
	xor	di, di		; Always start at 0,0
	mov	cx, 80*25	; Number of characters in 80 columns by 25 rows.
	lodsw			; Read fill character and attribute
	test	al, 255		; Has a character been defined
	jnz	.writeWORDS
	inc	di		; Offset so we are writing attribute byte only
	shr	ax, 8		; Shift attribute into AL
	jnz	.writeBYTES	; If both char & attribute are NULL, then nothing to do

    ; Put a flashing red character on bottom right of screen to denote function was selected
    ; but a character and attribute were not specified.

	dec	cx
	shl	cx, 1
	mov	di, cx
	mov	ax, 8D01H	; Flashing red happy face.
	stosw
	jmp	.exit

     .writeWORDS:
	test	ah, 255		; Is attribute NUL
	jz	.writeBYTES
	rep	stosw		; Fill page with specified char & attribute
	jmp	.exit

      .writeBYTES:
	stosb
	inc	di		; Bounce over either char or attribute
	dec	cx
	jnz	.writeBYTES
      .exit:
	pop	di
	pop	es
	ret

	align	16
; =============================================================================================

;	ENTER:	DS:SI   +00 - 7-0 = Col (0 - 79)   NOTE: No bounds checking done
;			+01 - 7-0 = Row (0 -24)

;			+02 - 7-5 = Reserved for future use.

;				4 = Update Vects.
;				3 = Relative to Vects otherwise current position.
;				2 = Calculations relative otherwise absolute.
;				1 = Set caret.
;				0 = Set DI.

;	LEAVE:	ES:DI = New destination offset if bit 0 was specified.
;		DS:SI = Points to next character(s) in string.

;	FLAGS:	Undefined
; ---------------------------------------------------------------------------------------------

F03:	cmp	al, 3
	jnz	.done + 1

    ; Read parameters from ASCIIZ string

	lodsw			; AH = Row(s), AL = Column(s).
	mov	dx, ax		; Move out of the way for condition bits.
	lodsb			; Condition bits.
	test	al, 10011B
	jz	.done		; None of bits 4, 1 or 0 is set

	mov	cl, al
	test	cl, 0100B	; Calculate relative positions?
	jz	.updateVects
	test	cl, 1000B	; Relative to current Vects
	jz	.toCurrent
	add	dx, [RVects]
	jmp	.updateVects
    .toCurrent:
	push	di
	call	A2XY
	add	dx, ax
    .updateVects:
	test	cl, 10000B	; Does DX need to be preserved
	jz	.setCaret
	mov	[RVects], dx	; Update new x/y position
    .setCaret:
	test	cl, 0010B	; Does caret need to be moved to new position
	jz	.postAbs
	mov	bx, es		; Get video pages base segment
	and	bh, 7		; Strip irrelevant bits
	push	cx		; BIOS function trashes this
	mov	ah, SCP		; BIOS Set Cursor Position
	int	VIDEO		; Execute BIOS video service (int 10H)
	pop	cx
    .postAbs:
	test	cl, 0001B	; Does DI need to be updated
	jz	.done
	push	dx		; Pass x/y coordinates to
	call	XY2A		; Conversion routine
	mov	di, ax
      .done:
	ret

; =============================================================================================

;                      BYTE   BITS
;	ENTER:	DS:SI   +00 - 7-6 = Reserved for future use.

;				5 = Update Vects to new position
;				4 = Relative to Vects otherwise current position DI
;			      3-2 = Reserved
;			      1-0 = Text alignment
;   				    1 = Left
;				    2 = Center
;				    3 = Right

;			+01 - 7-0 = Offset +/- from position specified by bit 4

;	LEAVE:	ES:DI = New destination offset if bit 0 was specified.
;		DS:SI = Points to next character(s) in string.

;	FLAGS:	
; ---------------------------------------------------------------------------------------------

F04:	cmp	al, 4
	jnz	.done + 1
	
	lodsw			; AH = Offset AL = condition bits
	test	al, 3		; Some sort of alignment must have been specified
	jz	.done
	
    ; Contingent upon bit 4, set AX to x/y equivalent of DI or read coordinates stored
    ; at RVects.
    
	movzx	dx, ah		; make 16 bit offset in DX from 8 bit param in ASCIIZ string.
	mov	bl, al		; Move condition bits to safer place
	push	di
	call	A2XY		; Calculate x/y vector equivalent of offset in DI
	btr	bx, 4
	jnc	.skew
	mov	ax, [RVects]	; Relative to x/y vectors instead of DI
     .skew:
	xor	cx, cx
	add	dx, ax
	test	bl, 2
	jz	.setDI
	
	push	si
    .L0:	cmp	byte [si], ' '	; Stop at first control character
	jb	.adjCnt
	inc	si
	inc	cx
	jmp	.L0
     .adjCnt:			; CX now equals strings length
	mov	[StrLen], cl	; Save length of this string.
	pop	si
	cmp	bl, 3		; Are we right justifying text
	jz	.setDI
	shr	cx, 1		; CX /= 2 for center alignment
	
    ; Set absolute position without bounds checking.
    
     .setDI:
	sub	dl, cl		; Only modify DL
	push 	dx
	call	XY2A		; Should actually check it will not be over FA0H (2000)
	mov	di, ax
	test	bl, 32
	jz	.done
	mov	[RVects], dx	; Update Vects coordinates
     .done:
	ret
	
F05:	cmp	al, 5
	jnz	.done
	
	lodsb			; Read times character is to be repeated
	mov	cl, al
	test	cl, 255
	jnz	$ + 5
	mov	cl, [StrLen]	; If NULL then use last computer string length
	lodsw			; Read attribute & character
	test	al, 255		; Has a fill character been specified
	jnz	.J0		
	inc	di		; Bump to attribute position
	shr	ax, 8		; Move attribute into AL
	jz	.done		; ZF if neither attribute or character has been specifiedl
    .J0:	test	ah, 255
	jz	.L0
	rep	stosw
	ret
    .L0:	stosb
	inc	di
	dec	cx
	jnz	.L0
	dec	di
    .done:
	ret
