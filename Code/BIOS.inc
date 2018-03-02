%ifndef _BIOS_
%define _BIOS_

	BOOT_SEG	equ		0x7c0
	     BDA	equ		0x40
		 
		VIDEO	equ	16
		  SCP	equ	 2					; Set cursor position
		  GCP	equ	 3					; Get cursor position
		  CAP	equ	 5					; Get Active page
		  SPU	equ	 6
		  RCA	equ	 8					; Read character & attribute
		  WCA	equ	 9
		  
		  TTY	equ	14
		  
		REBOOT	equ	25
      SYS_SERV	equ	21

	        DISKIO	equ	19
	      D_STATUS	equ	 1
			D_READ	equ	 2
	       D_WRITE	equ	 3
	       D_PARAM	equ	 8
			GET_DP	equ	52
	      EXT_READ	equ	66

	KEYBOARD	equ	22
	 GETCHAR	equ	 3
	
	     ESC	equ	27
		 
	BLACK		equ	0
	BLUE		equ	1
	GREEN		equ	2
	CYAN		equ	3
	RED		equ	4
	MAGENTA 	equ	5
	BROWN		equ	6
	LGREY		equ	7
	DGREY		equ	8
	LBLUE		equ	9
	LGREEN		equ	10
	LCYAN		equ	11
	LRED		equ	12
	LMAGENTA	equ	13
	YELLOW		equ	14
	WHITE		equ	15
	
	FLASHING	equ	128
%endif
