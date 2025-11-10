;NEWB/ASM
*LIST OFF
	TITLE	'<NEWB - New additions to BASICG>'
;
;	CAUTION - labels contain @ or $ may be used by the
;	BASICG/OV2 overlay.  If changes are made to this module,
;	it must be xrefed and the /OV2 modules re-assembled.
;
BG103	EQU	1		;Include the BASICG 01.01.03 enhancements
;
;
CR	EQU	0DH
DTOKE	EQU	0AAH		;DELETE token
CURLIN	EQU	721EH		;DOT	Current line number
OLDHI	EQU	71DDH		;MAXMEM	HIGH$ upon entry to BASIC
OLDINIT	EQU	9996H		;INIT	End of BASIC before this
DOSCLIN	EQU	9992H		;CMDPTR	Ptr to DOS cmdline on entry
BASEND	EQU	6F26H		;FILTAB	Eventually, top of free mem
MAXSPAC	EQU	6F28H		;CURLIN
STARTUP	EQU	9976H		;INIT0	Execute BASIC after init
INBUF	EQU	70C5H		;BUF	Input buffer
PGMBGN	EQU	6F2AH		;TXTTAB	Ptr to program start
RENUM	EQU	65F1H		;RESEQ	Entry pt for renum
RENUM1	EQU	6618H		;	Renum intercept for ending line
RENUM2	EQU	6667H		;	Intercept for endline check
SYNERR	EQU	588BH		;SNERR	Print "Syntax error
FUNCERR	EQU	5FEDH		;FCERR	Print "Illegal function call
DERR	EQU	52A1H		;FERROR	Print error returned
OMERR	EQU	4CF8H		;OUTLOD	Out of mem, pgm load only!
;
IMBACK	EQU	975CH		;	Back to basic's keyin
FIND	EQU	5AD6H		;FNDLIN	Find a line
CPHLDE	EQU	4354H		;COMPAR	Compare HL and DE
DOEDIT	EQU	3D27H		;EDTAGN	Edit a line
TOBUFF	EQU	5AF5H		;CRUNCH	Conv toupper and move
NXTCHAR	EQU	5EEBH		;CHRGTR	Get next char fm line buff
NUM2DE	EQU	5FF2H		;LINSPC	Get a number to DE
UNTOKE	EQU	7AA3H		;BUFLIN	Expand a tokenized line
FILEX	EQU	6F24H		;PTRFIL	DCB-2
HIPT	EQU	7204H		;FRETOP	Hi ptr used during load
CLEAR	EQU	4488H		;CLEAR	Entry to CLEAR statement
READY	EQU	5942H		;READY	Print Ready msg
NORMNUM	EQU	6681H		;SCCPTR
OV1	EQU	9841H		;LOADOV1 Re-load /OV1
;
DOSOVR@	EQU	01E00H
OVRLY$	EQU	69H
;
	ORG	59B2H		;NTAUTO	Basic line in intercept
	CALL	IMMED
;
	ORG	59E1H		;Line parse
	CALL	ABBR
;
	ORG	4CB0H		;Program load
	CALL	FASTLD
	JP	4CB7H		;Continue check
;
	ORG	4D95H
	CALL	FASTSAV		;Fast save
	JP	4CEFH		;PRGFIN
;
	ORG	RENUM		;Default last line to FFFFH
	CALL	RENA
;
	ORG	RENUM1		;See if last line entered
	JP	RENB
;
	ORG	RENUM2		;See if past last line
	JP	RENC
;
	ORG	77ABH		;Grab usr routine
	CALL	SVCUSR
;
;	BASICG 01.01.03 enhancement:
;	enable SCREEN 2 overlay mode
;	on GRAFYX and TANDY hi-res boards
;
	IF	BG103		;---> BASICG 01.01.03
	ORG	8EAFH		;SCREEN 0-2
	CP	03H		;
	CALL	SCREENX		;
	ORG	8EB5H		;
	AND	03H		;
	ORG	8EBBH		;
	AND	0FCH		;
	ENDIF			;<--- BASICG 01.01.03
;
	ORG	OLDINIT
;
;	Ck for usr11, the SVC call
;
SVCUSR:	CALL	NXTCHAR		;Get a char
	LD	BC,0
	PUSH	AF		;Save the char
	LD	A,(71D5H)	;CONLO	Get the number
	CP	11		;Is it ours?
	JR	Z,SVC1		;Ok if so
	POP	AF		;If not, return
	JP	77D0H		;Back to basic
SVC1:	POP	AF		;Clean stack
	CALL	NXTCHAR		;Just like basic does
	LD	DE,SVCADDR	;Our execute addr
	RET			;Back to basic
;
SVCADDR	DW	SVCEXEC
SVCEXEC:
	PUSH	DE
	PUSH	HL
	LD	E,(HL)		;Get addr of varptr
	INC	HL
	LD	D,(HL)
	EX	DE,HL		;HL pts to arr(0)
	PUSH	HL
	LD	DE,9
	OR	A
	SBC	HL,DE		;Pt to array type
	LD	A,(HL)
	POP	HL
	CP	2		;Is it int?
	JP	NZ,FUNCERR	;Quit if not
	LD	A,(OVRLY$)	;If /OV2 is resident, remove it
	CP	1
	JR	NZ,SVC2
	XOR	A		;Was in, show none in
	LD	(OVRLY$),A
SVC2:	LD	A,(HL)		;Pt to SVC #
	OR	A		;@IPL?
	JP	Z,FUNCERR
	CP	128		;Too big?
	JP	NC,FUNCERR
	LD	(STAK1),SP	;Save current stack!
	DI
	LD	SP,HL		;Pt to parms
	POP	HL		;Past SVC #
	POP	HL		;Load up registers
	POP	DE
	POP	BC
	POP	IY
	POP	IX
	LD	(RETSTK),SP	;Save for coming back
	LD	SP,$-$		;Get back basic's stack
STAK1	EQU	$-2
	EI
	RST	40		;Do user function
	DI
	LD	SP,$-$		;Top of array
RETSTK	EQU	$-2
	PUSH	IX
	PUSH	IY
	PUSH	BC
	PUSH	DE
	PUSH	HL
	PUSH	AF
	LD	SP,(STAK1)
	EI
	POP	HL
	POP	DE
	RET
;
;	Check if 1st key is arrow, period, or comma
;
IMMED:
	LD	A,($-$)
SFLAG1	EQU	$-2
	BIT	5,A		;JCL?
	JP	NZ,974DH	;SINLIN	Don't do immediate if JCL
	LD	HL,INBUF	;Keyboard input buffer
	CALL	45E9H		;ISFLIO	Chars left check??
	JP	NZ,9774H	;INPFIL	Go if still on last line
	LD	C,0EH		;Cursor on char
	LD	A,2		;@@DSP
	RST	40
	LD	BC,0F900H	;Max line len for keyin
REDO:	PUSH	DE
	LD	A,01H		;@@KEY
	RST	28H
	CALL	9014H		;GOTEXT
	POP	DE
	JP	NZ,DERR		;Device i/o error
	LD	C,0DH		;Default
	CP	80H		;Was it a break?
	JR	Z,WASBC
	LD	C,A		;Get actual char
	CP	0DH
	JR	Z,WASBC		;Treat single enter like break
	CP	1FH		;Shift Clear?
	JR	NZ,NOBRK@	;Go if not
	LD	A,105		;@@CLS
	RST	40
	JR	REDO		;Re-start after clear
WASBC:	PUSH	AF		;Save key value
	LD	A,2		;@@DSP
	RST	40
	POP	AF		;Key value back
	SCF			;Show break fm keyin
	RET			;Back to basic
NOBRK@:	PUSH	DE
	PUSH	AF		;Save flags and char
	LD	DE,(CURLIN)	;Get current line #
	CP	0BH		;Up arrow (list prev)
	JR	Z,CLINE
	CP	0AH		;Down arrow (list next)
	JR	Z,CLINE
	CP	'.'		;List current
	JR	Z,CLINE
	CP	','		;Edit current
	JR	Z,CLINE
	CP	08H		;Left arrow (list first)
	JR	Z,DOFRST
	LD	DE,0FFFFH	;Last line #
	CP	09H		;Right arrow (list last)
	JR	Z,FNDLIN	;Go if so
;
;	Not immediate
;
	CP	20H
	JR	NC,NCTRL	;Go if not control
	POP	AF		;Throw away control chars
	POP	DE
	JR	REDO		;Back to keyin routine
NCTRL:	LD	(HL),A		;Save character
	LD	C,A		;Put in C for dsp
	LD	A,2		;@@DSP
	RST	40
	POP	AF
	POP	DE		;Tempy off stack
	LD	C,B		;Count to C
	DEC	B		;One char gone
	PUSH	DE		;Basic needs on stack
	PUSH	HL		;Save buffer tempy
	LD	HL,IMBACK	;Re-enter basic
	EX	(SP),HL		;This is return address
	PUSH	HL		;Save buffer start
	INC	HL		;Next posn
	PUSH	HL		;Save temp again
	LD	HL,(0112H)	;P/u keyin vector
	INC	HL		;Bypass buffer save
	INC	HL
	EX	(SP),HL		;Get buffer back
	RET			;Go into keyin
;
;	Do immediate key
;
DOFRST:	LD	HL,(PGMBGN)	;Get program start
	JR	USEHL
CLINE:
	LD	DE,(CURLIN)	;Get the current line
FNDLIN:	CALL	FIND		;Find it
	POP	DE		;Get character
	PUSH	DE		;But not flags
	LD	A,D
	JR	NC,NOTFND	;Go if line not found
	CP	'.'		;Doing current list ?
	JR	Z,USEBC		;Go if so
	CP	','		;Editing current?
	JR	Z,USEBC		;Go if so
	CP	0AH		;Down arrow?
	JR	NZ,CKUP		;Go if not
	LD	A,(HL)		;At end of program?
	INC	HL
	OR	(HL)
	DEC	HL
	JR	NZ,USEHL	;Go if not
	JR	USEBC		;Use current if at end
NOTFND:	JR	Z,PREV		;Go if at end of program
CKUP:	CP	0BH		;Prev line?
	JR	NZ,USEBC	;Go if not
PREV:	LD	HL,(PGMBGN)	;Start of program
	AND	A
	SBC	HL,BC		;At start?
	JR	Z,USEBC		;Go if so
	ADD	HL,BC		;Else restore HL
	LD	D,B		;Search for line previous
	LD	E,C
SLOOP:	LD	B,H		;Start of current line to BC
	LD	C,L
	LD	A,(HL)		;P/u ptr to next line
	INC	HL
	LD	H,(HL)		; into HL
	LD	L,A
	CALL	CPHLDE		;See if next = current
	JR	NZ,SLOOP	;Loop if not
;
USEBC:	LD	H,B		;Xfer line # to HL
	LD	L,C
USEHL:	POP	BC		;Recvr char
	POP	AF		;Remove DE
	POP	AF
	LD	A,(HL)		;Is there a line?
	INC	HL
	OR	(HL)
	INC	HL
	JR	NZ,DOLINE	;Go if so
	CALL	41FCH		;CRDO	No lines, do a CR and return
	JP	595DH		;MAIN	
DOLINE:	LD	E,(HL)
	INC	HL
	LD	D,(HL)		;Line # to DE
	INC	HL
	LD	A,B		;Ck for edit or list
	CP	','
	JP	Z,DOEDIT	;Go if edit req.
	CALL	5807H		;PROCHK	Don't know what this does
	LD	(CURLIN),DE	;Save the line # found
	CALL	LISTLIN@	;Display the line
	JP	595DH		;MAIN
;
;	Show a line, used also by /OV2
;
LISTLIN@
	PUSH	HL
	EX	DE,HL		;Line number to HL
	CALL	3139H		;LINPRT	Display the line #
	POP	HL
	LD	A,(HL)
	CP	9
	LD	A,' '
	CALL	NZ,40D8H	;OUTDO	Display space after line
LSTCAL:	CALL	UNTOKE		;Make line into ascii
	LD	HL,INBUF	;Pt to start of line
	CALL	7A9AH		;OUTSTRZ Display line
	JP	41FCH		;CRDO	Dsply CR and return
;
;	Do abbreviated keys
;
ABBR:
	LD	A,($-$)
SFLAG2	EQU	$-2
	BIT	5,A		;JCL?
	JP	NZ,TOBUFF	;If so, back to BASIC
	CALL	TOBUFF		;Upper case and move
	CALL	CKABBR		;Do abbr check
	RET	Z		;Back if not
	INC	HL		;Bypass leading colon
	LD	A,(HL)		;Check if /OV2 req.
	CP	6
	JR	C,MORC		;Go if it is
DOBUF:	CALL	UNTOKE		;  else un tokenize
	LD	HL,INBUF
	PUSH	HL		;Remove any trailing !
DOBUF1:	INC	HL		; on line # > 32767
	LD	A,(HL)
	OR	A		;End of line?
	JR	Z,DOBUF2	;Done if so
	CP	'!'		;Incorrect sng. prec. mark?
	JR	NZ,DOBUF1	;Check more if not
	LD	(HL),0		;Else remove it
DOBUF2:	POP	HL		;Line start back
	JP	TOBUFF		;Let basic interpret
;
CKABBR:	PUSH	BC
	PUSH	DE
	PUSH	HL
	LD	B,0		;Set none found
EVALP	CALL	NXTCHAR		;Get a char of command
	JR	Z,NOTABB	;Back if line end
	CP	8FH		;REM token?
	JR	Z,DOREM		;Back if so
	LD	DE,ABBTBL	;Valid abbrs
	LD	C,A		;Letter to ck for
ABLP:	LD	A,(DE)		;Get a valid abbr
	INC	DE		;Pt to its token
	OR	A		;End of table?
	JR	Z,NOTAB1	;Go if end
	CP	C		;Match out letter?
	JR	Z,ABBMAT	;Go if so
	INC	DE		;Pt to next letter
	JR	ABLP
;
ABBMAT:	PUSH	HL		;Save line posn
ABLP1:	CALL	INXT		;Keep checking line
	JR	Z,ISME		;If end, is an abbr.
	CP	0F1H		;Equal sign found?
	JR	Z,NOTME		;Not abbr if so
	JR	ABLP1
;
ISME:	POP	HL		;Pt to abbr. char in line
	PUSH	HL		;Keep stack right
	LD	A,(DE)		;P/u token for abbr.
	LD	(HL),A		;Replace it in line
	LD	B,1		;To set an NZ return
NOTME:	POP	HL
NOTAB1:	CALL	INXT		;Parse down line
	JR	NZ,NOTAB1
NOTABB	OR	A		;Was it end or a colon?
	JR	NZ,EVALP	;More if colon
DOREM:	INC	B		;Set flags
	DEC	B		;Z=no, NZ=abbr
	POP	HL
	POP	DE
	POP	BC
	RET
;
INXT:	CALL	NXTCHAR		;Get a char
	RET	Z		;Back on end or ":"
	CP	22H		;Quote
	RET	NZ
INXT1:	CALL	NXTCHAR		;Loop for string literal
	OR	A
	RET	Z
	CP	22H
	JR	NZ,INXT1
	OR	A
	RET
;
;
;	/OV2 loader. Handles Copy, Move, Find, Search
;
MORC:
	LD	(SAVA),A	;Save char fm token table
	XOR	A
	CALL	CLEAR		;Clear the memory
	CALL	NORMNUM		;Normalize all line refs
	LD	A,(OVRLY$)	;Get DOS overlay
	CP	1		;Is it us?
	JR	Z,DOOVR		;Go if in
	LD	DE,FCB		;Use this as FCB
	LD	HL,OVRLAY$	;"BASICG/OV2"
	LD	A,78		;@@FSPEC
	RST	40
	XOR	A
	LD	(OVRLY$),A	;Show no DOS overlay
	LD	HL,DOSOVR@	;Pt to buffer
	LD	B,A
	LD	A,59		;@@OPEN
	RST	40
	JP	NZ,DERR
	LD	HL,FCB+4	;Pt to buffer msb
	LD	B,6		;Sector count
MOC1:	LD	A,67		;@@read
	RST	40
	JP	NZ,DERR		;Quit if error
	INC	(HL)		;Advance load point
	DJNZ	MOC1		;Loop thru load
	LD	A,1
	LD	(OVRLY$),A
DOOVR:	LD	A,$-$		;Get function #
SAVA	EQU	$-1
	LD	HL,INBUF	;Pt to extended command
	JP	DOSOVR@		;Execute overlay
;
OVRLAY$	DB	'BASICG/OV2',CR
FCB:	DS	32
;
;
;	Fast program load
;
FASTLD:
	LD	IX,(FILEX)	;FCB-2
	LD	HL,(HIPT)	;Highest available byte
	LD	DE,0FFAAH	;Magic number
	ADD	HL,DE
	LD	DE,(PGMBGN)	;Program load base
	XOR	A
	SBC	HL,DE
	JP	C,OMERR		;Out of memory
	OR	(IX+15)		;ERN < 255
	JP	NZ,OMERR
	LD	B,(IX+14)	;ERN lo byte
	LD	C,A		;0
	SBC	HL,BC		;File too big?
	JP	C,OMERR
	LD	H,(IX+6)	;Get buffer address
	LD	L,(IX+5)
	INC	HL		;Bypass FF header
	LD	B,C
	DEC	C		;Set 255 bytes
	LDIR			;Move first piece
	LD	(IX+6),D	;Set new buffer
	LD	(IX+5),E
	LD	L,E
	PUSH	IX
	POP	DE
	INC	DE		;Pt to FCB
	INC	DE
	LD	(IX+7),B	;Set 0 offset
	RES	7,(IX+3)	;Do sector i/o
LDLOOP:	LD	A,67
	RST	40		;@@READ
	JP	NZ,LDERR	;Some error
	INC	(IX+6)		;Bump buffer ptr
	JR	LDLOOP
LDERR:	CP	1CH		;Eof error?
	JR	Z,LDEOF
	CP	1DH
	JP	NZ,DERR		;Real error, quit
LDEOF:	LD	H,(IX+6)	;Get back buff loc
	XOR	A
	LD	D,A
	LD	E,(IX+10)	;Get eof offset
	DEC	E
	ADD	HL,DE		;Byte after program
	RET			;All done
;
;	Fast save routine
;	A => type byte, FF normal, FE protected
;	DE and HL setup reversed of our needs
;
FASTSAV:
	EX	DE,HL		;DE=start, HL=end
	OR	A
	SBC	HL,DE		;How much to save
	DEC	DE
	LD	(DE),A		;Put in type byte at start
	EX	DE,HL		;Start-1 to HL
	PUSH	DE		;Save byte count
	LD	C,D
	LD	B,0		;Records to BC
	LD	IX,(FILEX)	;DCB-2
	LD	(IX+6),H
	LD	(IX+5),L	;Stuff buffer address
	PUSH	IX
	POP	DE		;Pt DE to FCB
	INC	DE
	INC	DE
	LD	A,66		;@@POSN
	RST	40
	RES	6,(IX+3)
	LD	A,75		;@@WRITE
	RST	40
	JR	NZ,WRBAD
	LD	A,68
	RST	40		;@@REW
	POP	BC		;Len to BC
	INC	C		;Include FF header
	INC	B		;  and last partial rec
WRITIT:	LD	A,75
	RST	40
WRBAD	LD	(HL),0		;Remove FF header
	JP	NZ,DERR		;Go on disk error
	INC	(IX+6)		;Bump buffer ptr
	DJNZ	WRITIT
	LD	(IX+10),C	;Put offset into FCB
	RET
;
RENA:	LD	BC,0FFFFH
	LD	(LSTLIN),BC
	LD	BC,10
	RET
;
RENB:
	JR	Z,RENRET	;Back if no more cmdline
	EX	(SP),HL
	LD	(OLDL),HL	;Save old line start
	EX	(SP),HL
	PUSH	HL
	PUSH	DE
	PUSH	BC
	LD	(INCRE),DE	;Save increment
	CALL	435AH		;SYNCHR	Ck it out
	DB	2CH		;Valid separator
	CALL	5FFCH		;LINGET	Shouldn't be any more
	JP	NZ,SYNERR
	CALL	FIND
	LD	(LSTLIN),BC	;Save line locn
	LD	(ENDPOS),HL
	LD	A,(HL)		;See if "end" is last line
	INC	HL
	OR	(HL)
	JR	Z,RENB1		;Ok always if it is
	INC	HL		;P/u line num after "end"
	LD	A,(HL)
	INC	HL
	LD	H,(HL)		;Put into HL
	LD	L,A
	LD	(ENDCK),HL	;Save end+1
	POP	DE		;Get new line
	PUSH	DE
	EX	DE,HL		;End+1 to DE, new to HL
	SBC	HL,DE		;Must be carry, new < end+1
	JP	NC,FUNCERR
;
;	Now, newline+ (linecount*increment) < end+1
;
	LD	DE,$-$		;Get old line
OLDL	EQU	$-2
	CALL	FIND		;Locate this line
	EXX
	POP	HL		;New line
	PUSH	HL
	LD	BC,$-$
INCRE	EQU	$-2		;Increment
	EXX
	LD	DE,$-$		;Ending posn
ENDPOS	EQU	$-2
ENDLP:	PUSH	HL
	OR	A
	SBC	HL,DE		;See if at end
	POP	HL
	JR	Z,CKEND		;Go if so
	LD	A,(HL)
	LD	C,A		;Save
	INC	HL		;Get nxt ptr
	OR	(HL)		;Pgm end?
	JR	Z,CKEND
	LD	H,(HL)
	LD	L,C
	EXX
	ADD	HL,BC
	EXX
	JR	ENDLP
;
CKEND:	EXX
	LD	DE,$-$		;end+1 line #
ENDCK	EQU	$-2
	OR	A
	SBC	HL,DE
	EXX
	JP	NC,FUNCERR
;
RENB1:	POP	BC
	POP	DE
	POP	HL
RENRET:	JP	661BH		;	Return
;
RENC:	LD	DE,$-$		;Get end line locn
LSTLIN	EQU	$-2
	EX	DE,HL		;End to HL, posn to DE
	OR	A
	SBC	HL,DE
	EX	DE,HL		;Curr posn back to HL
	JP	C,667BH		;SCCALL	Go if past
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	JP	666AH
;
ABBTBL	DB	'A',0ABH	;Auto
	DB	'C',5		;Copy
	DB	'D',0AAH	;Delete
	DB	'E',0A7H	;Edit
	DB	'F',4		;Find
	DB	'L',93H		;List
	DB	'M',2		;Move
	DB	'S',3		;Search
	DB	'!',0BDH	;System
	DW	0
;
LSTTYP$	DB	0		;Last "Search" type
LSTLIN$	DW	0		;Last line during find
LSTNUM$	DW	0		;Last line # ref
LSTKEY$	DB	0		;Last keyword token
LSTVAR$	DC	11,0		;Last variable ref
;
;
;	BASICG 01.01.03 enhancement:
;	enable SCREEN 2 overlay mode
;	on GRAFYX and TANDY hi-res boards
;
	IF	BG103		;---> BASICG 01.01.03
SCREENX	JP	NC,FUNCERR	;FC Error if parameter .GT. 2
	LD	A,0FFH		;GRAFYX flag
GRAFYX	EQU	$-1		;
	OR	A		;
	LD	A,B		;Is set?
	RET	NZ		;Return if so (it's a Grafyx)
	AND	2		;Mask Intermix bit
	RRCA			;To bit 0
	OUT	(8EH),A		;Set Intermix
	LD	A,B		;Mask Intermix bit for port 83H
	AND	1		;
	RET			;Continue normal SCREEN routine
	ENDIF			;<--- BASICG 01.01.03
;
;
;
;	Entry point for initialization
;
BEGIN:	LD	A,(STOR1$)
M82F1	EQU	$-2
	OR	A
	JR	Z,BEGINA
	LD	A,21		;@@ABORT
	RST	28H
BEGINA	INC	A
	LD	(STOR1$),A
	LD	(DOSCLIN),HL
	LD	HL,ENDLOC+0CAH
	LD	(BASEND),HL
	CALL	9841H		;LOADOV1 Load /OV1
	CALL	431EH		;STKINI	Init ptrs, set stack
	LD	A,':'
	LD	(6F85H),A	;KBUF1 (KBUF-1)
	LD	HL,0FFFEH
	LD	(MAXSPAC),HL
	LD	HL,724DH	;PRMSTK
	LD	(72B5H),HL	;PRMPRV
	LD	A,03H
	LD	(6F5AH),A	;NFILES
;
	LD	B,00H		;Set up to get HIGH$
	LD	H,B
	LD	L,B
	LD	A,100		;@@HIGH$
	RST	28H
	LD	(OLDHI),HL	;Save high$ value
	LD	HL,(DOSCLIN)
	LD	DE,INBUF
	LD	A,78		;@@FSPEC
	RST	28H
	JR	NZ,M833C
	LD	(9994H),HL	;CMDTRM
	JR	M833F
M833C	LD	HL,(DOSCLIN)
M833F	LD	DE,PRMTBL
	LD	A,11H		;@@PARAM
	RST	28H
	JP	NZ,ABORT
	LD	A,(M8464)
	OR	A
	JR	Z,M8363
	AND	80H
	JP	Z,ABORT
	LD	HL,(STOR2$)
	LD	DE,0010H
	CALL	CPHLDE
	JP	NC,ABORT
	LD	A,L
	LD	(6F5AH),A	;NFILES
M8363	LD	HL,(OLDHI)	;P/u high$
	LD	A,(M846E)
	OR	A
	JR	Z,M837C
	AND	80H
	JP	Z,ABORT
	LD	DE,(STOR3$)
	CALL	CPHLDE
	JP	C,ABORT
	EX	DE,HL
M837C	CALL	9910H		;CKDOSB
	JP	NC,ABORT
	EX	DE,HL
	LD	HL,M82F1
	LD	(6F2AH),HL	;TXTTAB
	LD	A,E
	SUB	L
	LD	L,A
	LD	A,D
	SBC	A,H
	LD	H,A
	JP	C,ABORT
	LD	B,03H
M8394	OR	A
	LD	A,H
	RRA
	LD	H,A
	LD	A,L
	RRA
	LD	L,A
	DEC	B
	JP	NZ,M8394
	LD	A,H
	CP	02H
	JR	C,M83A7
	LD	HL,0200H
M83A7	LD	A,E
	SUB	L
	LD	L,A
	LD	A,D
	SBC	A,H
	LD	H,A
	LD	(71DFH),HL	;MEMSIZ
	LD	(7204H),HL	;FRETOP
	EX	DE,HL
	LD	(BASEND),HL
	LD	SP,HL
	LD	(721AH),HL	;SAVSTK
	LD	HL,(6F2AH)	;TXTTAB
	EX	DE,HL
	CALL	4274H		;REASON
	CALL	991EH		;INIFDB
;
;	Do flag inits
;
	LD	A,101		;@@FLAGS
	RST	28H
	PUSH	IY
	POP	HL
	INC	HL
	INC	HL
	LD	(95A1H),HL	;CFLAGS	CFLAG locn?
	DI
	LD	A,(HL)		;Set no chng of high$
	OR	01H
	LD	(HL),A
	EI
	LD	BC,8
	ADD	HL,BC
	LD	(959DH),HL	;KFLAGS	KFLAG locn
	DI
	LD	A,(HL)
	AND	0F8H		;Reset all bits
	LD	(HL),A
	EI
	LD	C,08H
	ADD	HL,BC		;SFLAG locn
	LD	(959FH),HL	;SFLAGS
	LD	(SFLAG1),HL
	LD	(SFLAG2),HL
	LD	HL,0		;Disable vectored break
	LD	A,103		;@@BREAK
	RST	28H
;
	CALL	9732H		;CLS
	CALL	9681H		;RSKBRK
	LD	HL,SIGNON$
	CALL	46AFH		;STROUT
	LD	HL,46AFH	;STROUT
	LD	(5953H),HL	;REPINI
	LD	HL,0FFFFH
	LD	(MAXSPAC),HL
	CALL	8C5FH		;GINIT
	IF	BG103		;---> BASICG 01.01.03
	CALL	ISGFYX		;Determine if TANDY or GRAFYX hi-res
	ENDIF			;<--- BASICG 01.01.03
	JP	STARTUP
;
;	BASICG 01.01.03 enhancement:
;	enable SCREEN 2 overlay mode
;	on GRAFYX and TANDY hi-res boards
;
;	Test for GRAFYX or TANDY Hi-Res Adapter
;
	IF	BG103		;---> BASICG 01.01.03
ISGFYX	XOR	A		;Set X=Y=0
	OUT	(80H),A		;
	OUT	(81H),A		;
	LD	A,0FEH		;Set TEXT mode, no increment
	OUT	(83H),A		;
	IN	A,(82H)		;Read data(0,0)
	LD	B,A		;Save it
	IN	A,(83H)		;Read port 83H
	CP	B		;Is it port 82H mirror? (on GRAFYX only)
	JR	NZ,TANDY	;If not, it is a TANDY
	LD	A,B		;Get data byte
	CPL			;Complement it
	OUT	(82H),A		;Write to data(0,0)
	IN	A,(83H)		;Get from mirror port
	CPL			;Complement it
	CP	B		;Is it the same value?
	LD	A,B		;Restore data(0,0)
	OUT	(82H),A		;
	RET	Z		;Return if same, it is a GRAFYX
TANDY	XOR	A		;Otherwise, it is a TANDY
	LD	(GRAFYX),A	;So, clear the GRAFYX flag
	RET			;and return
	ENDIF			;<--- BASICG 01.01.03
;
ABORT	LD	HL,PRMERR$
	CALL	46AFH		;STROUT
	LD	A,21		;@@ABORT
	RST	28H
;
;
PRMERR$	DB	'Bad Command Line P'
	DB	'arameter(s)',0DH
	NOP
PRMTBL	DB	80H
	DB	95H
	DB	'FILES',00H
	DW	STOR2$
	DB	96H
M8464	EQU	$-4
	DB	'MEMORY',00H
	DW	STOR3$
M846E	EQU	$-3
	DB	00H
STOR2$	DW	0
STOR3$	DW	0
STOR1$	DB	0
	IF	BG103		;---> BASICG 01.01.03
SIGNON$	DB	'BASICG 01.01.03 for'
	ELSE			;<--> BASICG 01.01.03
SIGNON$	DB	'BASICG 01.01.02 for'
	ENDIF			;<--- BASICG 01.01.03
	DB	' TRSDOS Version 6',0DH
	DB	'Copyright (c) 1984'
	DB	' by Microsoft, lic'
	DB	'ensed to Tandy Cor'
	DB	'poration.',0DH
	DB	'All rights reserved. ENHANCED by LSI.',CR,CR
	NOP
ENDLOC	EQU	$
	END	BEGIN

