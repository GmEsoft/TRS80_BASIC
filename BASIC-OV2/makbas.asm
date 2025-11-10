;MAKBAS - Install basic enhancement package
;
	TITLE <INSTALLB - BASIC ENHANCEMENT INSTALLATION>
;
*GET	SVCMAC
CR	EQU	13
LF	EQU	10
XFER60	EQU	7F0CH		;6.0 basic xfer address
;
	ORG	2600H
;
BEGIN:
	LD	(SAVSTK),SP
	@@FLAGS
	PUSH	IY
	POP	BC
	LD	HL,'S'-'A'	;Get sflag
	ADD	HL,BC
	LD	(SFLAG1),HL	;Need for read open
	LD	HL,SIGNON$
	@@DSPLY
;
	LD	HL,$-$		;Set read only bit
SFLAG1	EQU	$-2
	SET	0,(HL)
	LD	DE,BUFCB	;BU program
	LD	B,1
	LD	HL,BUBUF
	@@OPEN
	JR	Z,OK		;Go no error
	CP	42		;LRL fault
	JR	Z,OK
	JP	BUERR		;Can't find program
OK:	@@PEOF			;End of new program
	CP	28
	JP	NZ,ERROR	;Go if not eof
	@@BKSP
	@@BKSP
	JP	NZ,ERROR	;Should be at xfer addr
	LD	HL,BUBYT	;1 byte buffer
	@@READ
	JP	NZ,ERROR
	LD	A,(HL)
	LD	(NXFER),A	;Save lsb
	@@READ
	JP	NZ,ERROR
	LD	(NXFER+1),A	;Save MSB
	@@REW			;Back to beginning
GETBAS:	LD	HL,BASPMT	;Get drive of BASIC
	@@DSPLY
	LD	HL,KEYBUF
	LD	BC,1<8.OR.0	;Get 1 key
	@@KEYIN
	JP	C,ABORT		;Go on break
	LD	A,B
	DEC	A		;Was 1 key?
	JR	NZ,GETBAS	;Redo if not
	LD	A,(HL)
	CP	'0'		;Valid drive ?
	JR	C,GETBAS
	CP	'8'
	JR	NC,GETBAS	;Bad if out of range
	LD	(BASDRV),A	;Save drive spec
	LD	DE,BASFCB
	LD	HL,BASBUF
	LD	B,1
	@@OPEN
	JR	Z,OK2		;Found basic
	CP	42
	JR	Z,OK2
	LD	HL,NOBAS$
	@@DSPLY
	JR	GETBAS
OK2:
	@@PEOF			;End of BASIC program
	CP	28		;At the end?
	JR	Z,OK3
	JP	ERROR		;Some other wierd error
OK3:	LD	B,4		;Back up 4 bytes
BLP:	@@BKSP
	DJNZ	BLP
	JP	NZ,ERROR	;Bad if can't
	@@LOC			;Save this posn
	PUSH	BC
	LD	HL,BUBYT	;Disk buffer
	LD	B,2		;Ck for xfer load code
RDLP1:	@@READ			;Get a 02
	JP	NZ,ERROR
	LD	A,(HL)
	CP	2
	JP	NZ,LMFERR	;Not BASIC
	DJNZ	RDLP1		;Do twice
	@@READ			;Get xfer address
	LD	C,(HL)		;  LSB
	JP	NZ,ERROR
	@@READ
	LD	B,(HL)
	JP	NZ,ERROR
	LD	HL,XFER60	;6.o xfer address
	OR	A
	SBC	HL,BC		;Are they the same?
	JP	Z,N62ERR	;Go if so
	LD	HL,(NXFER)	;New xfer fm BU
	OR	A
	SBC	HL,BC		;Already installed?
	JP	Z,DUPERR	;Go if so
	POP	BC		;Start of xfer address
	@@POSN			;Go there
;
;	Read a byte, write a byte
;
	PUSH	DE		;Save write FBC
	LD	HL,BUBYT	;1 byte I/O buffer
DOREAD:	LD	DE,BUFCB
	@@READ
	JR	Z,OK4		;Good read
	CP	1CH		;Eof?
	JR	Z,DUNREAD	;Go if so
	CP	1DH
	JP	NZ,ERROR
	JR	DUNREAD
OK4:	POP	DE		;BASIC back
	@@WRITE			;Write the byte
	PUSH	DE
	JP	NZ,ERROR
	JR	DOREAD
;
DUNREAD:
	POP	DE		;BASIC FCB
	@@CLOSE			;Save the change
	JP	NZ,ERROR
	LD	HL,ALLDUN$
	@@DSPLY
	JP	EXIT
;
BUFCB	DB	'NEWB/LMF',3
	DC	32,0
BASFCB	DB	'BASIC/CMD.BASIC:'
BASDRV	DB	' ',3
	DC	32,0
BASBUF	DS	256
BUBUF	DS	256
BUBYT	DB	0
NXFER	DW	0		;Xfer address of BU
KEYBUF	DW	0		;Input buffer for drive #
;
SIGNON$	DB	'Add enhancement package to BASIC',LF
	DB	'Copyright (C) 1984 by Logical Systems, Inc.',LF,CR
BASPMT	DB	'Enter drive containing BASIC/CMD to be modified ',3
;
NOBAS$	DB	'Can''t open BASIC/CMD on that drive',LF,CR
BUERR$	DB	'Can''t find BASIC enhancement file',CR
LMFERR$	DB	'BASIC/CMD: Load module format error',CR
N62ERR$	DB	'Enhancements require BASIC version 01.01.00',CR
DUPERR$	DB	'Enhancements already installed',CR
ALLDUN$	DB	LF,'Enhancements sucessfully installed',CR
;
BUERR:	LD	HL,BUERR$
	DB	0DDH
LMFERR:	LD	HL,LMFERR$
	DB	0DDH
N62ERR:	LD	HL,N62ERR$
	DB	0DDH
DUPERR:	LD	HL,DUPERR$
	@@LOGOT
	JR	ABORT
ERROR:	OR	0C0H		;Short, return
	LD	C,A
	@@ERROR
ABORT:	LD	HL,-1
	JR	EXIT1
EXIT:	LD	HL,0
EXIT1:	LD	SP,$-$
SAVSTK	EQU	$-2
	@@CKBRKC
	RET
;
	END	BEGIN

