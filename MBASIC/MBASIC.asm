;=============================================================================
;	CP/M BASIC-80 Rev 5.21 (MBASIC.COM) - July 28, 1981
;=============================================================================
;
;	--------- ---- -- ---- ----- --- ---- -----
;	COPYRIGHT 1975 BY BILL GATES AND PAUL ALLEN
;	--------- ---- -- ---- ----- --- ---- -----
;
;	Originally written on the PDP-10 from
;	February 9 to April 9
;
;	Bill Gates wrote the Runtime Stuff.
;	Paul Allen wrote the Non-Runtime Stuff.
;	Monte Davidoff wrote the Math Package.
;
;
	ORG	0100H
START	JP	INIT

	DW	CINT,MAKINT

;=============================================================================
;	IBMRES - IBM COMPATIBLE RESERVED WORDS / MLC
; ## IBMRES.ASM ##
;

;-----------------------------------------------------------------------------
;	MACROS FOR TOKEN DEFINITIONS
;
;	Define token
;	DEFTOK	name[,addr|0]
DEFTOK	MACRO	#W,#A
#W&TK	EQU	QQ
	IF	NUL #A
	 DW	#W
	ELSE
	 IFNE	#A,0
	  DW	#A
	 ENDIF
	ENDIF
QQ	DEFL	QQ+1
	ENDM
;
;	Skip token
;	SKIPTOK	[addr]
SKIPTOK	MACRO	#A
	IF	NUL #A
	ELSE
	 DW	#A
	ENDIF
QQ	DEFL	QQ+1
	ENDM
;
;	Define reserved word
;	TOKEN	word[,name]
TOKEN	MACRO	#W,#T
TOKLEN	DEFL	0
	IRPC	#C,#W
TOKLEN	 DEFL	TOKLEN+1
	ENDM
TOKIDX	DEFL	0
	IRPC	#C,#W
	 IFNE	TOKIDX,0
	  IFLT	TOKIDX,TOKLEN-1
	   DB	"#C"
	  ELSE
	   DB	"#C"+80H
	  ENDIF
	 ENDIF
TOKIDX	 DEFL	TOKIDX+1
	ENDM
	IF	NUL #T
	 DB	#W&TK
	ELSE
	 DB	#T&TK
	ENDIF
	ENDM

;	Define special symbols
;	TOPENSP	'char',name
TOKENSP	MACRO	#S,#T
	DB	#S+80H
	DB	#T&TK
	ENDM

;-----------------------------------------------------------------------------
;	MACROS FOR ERROR CODES
;
;	Define error code ERR#C and message #S
DCL	MACRO	#C,#S
ERR#C	EQU	QQ
QQ	DEFL	QQ+1
	DB	#S,0
	ENDM

;	Define disk error vector #C
;	add #Z=0 for the last vector
DERMAK	MACRO	#C,#Z
DER#C	LD	E,ERR#C
	IF	NUL #Z
	DB	01H
	ENDIF
	ENDM

;	Define BASIC error vector #C
;	add #Z=0 for the last vector
ERRMAK	MACRO	#C,#Z
#C&ERR	LD	E,ERR#C
	IF	NUL #Z
	DB	01H
	ENDIF
	ENDM


;-----------------------------------------------------------------------------
;	CONSTANTS
;

; --- CP/M Specific definitions ---
	IF !BASE
BASE	EQU	0
	ENDIF
CPMWRM	EQU	BASE		;CP/M WARM BOOT ADDR
CPMENT	EQU	BASE+5		;CP/M BDOS CALL ADDR
DIRTMP	EQU	BASE+$0080

; --- Sizes ---

NUMLEV 	EQU	0*20+19+2*5	;NUMBER OF STACK LEVELS RESERVED
				; BY AN EXPLICIT CALL TO GETSTK
NAMLEN 	EQU	40		;MAXIMUM LENGTH NAME -- 3 TO 127

; --- Prefixes, Tokens.. ---

OCTCON 	EQU	11		;$0B - EMBEDED OCTAL CONSTANT
HEXCON 	EQU	12		;$0C - EMBEDED HEX CONSTANT

PTRCON 	EQU	13		;$0D - A LINE REFERENCE CONSTANT
LINCON 	EQU	14		;$0E - A LINE NUMBER UNCONVERTED TO POINTER

IN2CON 	EQU	15		;SINGLE BYTE (TWO BYTE WITH TOKEN) INTEGER
CONCN2 	EQU	16		;TOKEN RETURNED SECOND TYPE CONSTANT IS SCANNED.

ONECON 	EQU	17		;FIRST OF 10 (0-9) INTEGER SPECIAL TOKENS
INTCON 	EQU	28		;REGULAR 16 BIT TWO'S COMPLEMENT INT

CONCON 	EQU	30		;TOKEN RETURNED BY CHRGET AFTER CONSTANT SCANNED
DBLCON 	EQU	31		;DOUBLE PREC (8 BYTE) CONSTANT


;------------------------------------

BUFLEN 	EQU	255		;LONG LINES
KBFLEN 	EQU	BUFLEN/4+BUFLEN	;MAKE KRUNCH BUFFER SOMEWHAT
				; LARGER THAN SOURCE BUFFER (BUF)

LINLN  	EQU	80		; TERMINAL LINE LENGTH

LPTLEN 	EQU	132		; Max column size on printer
CLMWID 	EQU	14		; MAKE COMMA COLUMNS FOURTEEN CHARACTERS
LNCMPS 	EQU	(((LPTLEN/CLMWID)-1)*CLMWID)
				;LAST COMMA FIELD POSIT

DATSPC 	EQU	128    		;NUMBER OF DATA BYTES IN DISK SECTOR



;-----------------------------------------------------------------------------

;	Statements:
QQ	DEFL	81H
STMDSP	DEFTOK	END,ENDST
	DEFTOK	FOR
	DEFTOK	NEXT
	DEFTOK	DATA
	DEFTOK	INPUT
	DEFTOK	DIM
	DEFTOK	READ
	DEFTOK	LET
	DEFTOK	GOTO
	DEFTOK	RUN
	DEFTOK	IF,IFS
	DEFTOK	RESTORE,RESTOR
	DEFTOK	GOSUB
	DEFTOK	RETURN
	DEFTOK	REM
	DEFTOK	STOP
	DEFTOK	PRINT
	DEFTOK	CLEAR
	DEFTOK	LIST,LISTS
	DEFTOK	NEW,SCRATH
	DEFTOK	ON,ONGOTO
	DEFTOK	NULL
	DEFTOK	WAIT,FNWAIT
	DEFTOK	DEF,DEFST
	DEFTOK	POKE
	DEFTOK	CONT
	SKIPTOK	SNERR		;DRAW (GFX)
	SKIPTOK	SNERR		;CIRCLE (GFX)
	DEFTOK	OUT,FNOUT
	DEFTOK	LPRINT
	DEFTOK	LLIST
	SKIPTOK	0		;CLS (VT52)
	DEFTOK	WIDTH,WIDTHS
	DEFTOK	ELSE,REM
	DEFTOK	TRON,TON
	DEFTOK	TROFF,TOFF
	DEFTOK	SWAP
	DEFTOK	ERASE
	DEFTOK	EDIT
	DEFTOK	ERROR,ERRORS
	DEFTOK	RESUME
	DEFTOK	DELETE
	DEFTOK	AUTO
	DEFTOK	RENUM,RESEQ
	DEFTOK	DEFSTR
	DEFTOK	DEFINT
	DEFTOK	DEFSNG
	DEFTOK	DEFDBL
	DEFTOK	LINE
	SKIPTOK	0		;PSET (GFX)
	SKIPTOK	0		;PRESET (GFX)
	DEFTOK	WHILE
	DEFTOK	WEND
	DEFTOK	CALL,CALLST
	DEFTOK	WRITE
	DEFTOK	COMMON,DATA
	DEFTOK	CHAIN
	DEFTOK	OPTION
	DEFTOK	RANDOM
	SKIPTOK	0		;COLOR (GFX)
	DEFTOK	SYSTEM
	SKIPTOK	0		;LOCATE (VT52)
	DEFTOK	OPEN
	DEFTOK	FIELD
	DEFTOK	GET,GETST
	DEFTOK	PUT,PUTST
	DEFTOK	CLOSE
	DEFTOK	LOAD
	DEFTOK	MERGE
	DEFTOK	FILES
	DEFTOK	NAME
	DEFTOK	KILL
	DEFTOK	LSET
	DEFTOK	RSET
	DEFTOK	SAVE
	DEFTOK	RESET
	SKIPTOK			;VPOKE (ZXPLUS3)
	DEFTOK	TO,0
	DEFTOK	THEN,0
	DEFTOK	TAB,0
	DEFTOK	STEP,0
	DEFTOK	USR,0
	DEFTOK	FN,0
	DEFTOK	SPC,0
	DEFTOK	NOT,0
	DEFTOK	ERL,0
	DEFTOK	ERR,0
	DEFTOK	STRING$,0
	DEFTOK	USING,0
	DEFTOK	INSTR,0
	DEFTOK	SNGQ,0
	DEFTOK	VARPTR,0
	DEFTOK	INKEY$,0
	SKIPTOK			;CSRLIN (ZXPLUS3)
	SKIPTOK			;POINT (GFX)

;	Operators:
QQ	DEFL	0EFH
	DEFTOK	GREA,0
	DEFTOK	EQUL,0
	DEFTOK	LESS,0
	DEFTOK	PLUS,0
	DEFTOK	MINU,0
	DEFTOK	MULT,0
	DEFTOK	DIV,0
	DEFTOK	PWR,0
	DEFTOK	AND,0
	DEFTOK	OR,0
	DEFTOK	XOR,0
	DEFTOK	EQV,0
	DEFTOK	IMP,0
	DEFTOK	MOD,0
	DEFTOK	IDIV,0

LSTOPK	EQU	IDIVTK+1-PLUSTK

;	Functions:
QQ	DEFL	01H
ONEFUN	EQU	QQ
FUNDSP	DEFTOK	LEFT$
	DEFTOK	RIGHT$
	DEFTOK	MID$
	DEFTOK	SGN
	DEFTOK	INT
	DEFTOK	ABS
	DEFTOK	SQR
	DEFTOK	RND
	DEFTOK	SIN
	DEFTOK	LOG
	DEFTOK	EXP
	DEFTOK	COS
	DEFTOK	TAN
	DEFTOK	ATN,ATAN
	DEFTOK	FRE
	DEFTOK	INP,FNINP
	DEFTOK	POS
	DEFTOK	LEN
	DEFTOK	STR$
	DEFTOK	VAL
	DEFTOK	ASC
	DEFTOK	CHR$
	DEFTOK	PEEK
	DEFTOK	SPACE$
	DEFTOK	OCT$
	DEFTOK	HEX$
	DEFTOK	LPOS
	DEFTOK	CINT
	DEFTOK	CSNG
	DEFTOK	CDBL
	DEFTOK	FIX
	SKIPTOK	0		;VPEEK (ZXPLUS3)
	SKIPTOK	0
	SKIPTOK	0
	SKIPTOK	0
	SKIPTOK	0
	SKIPTOK	0
	SKIPTOK	0
	SKIPTOK	0
	SKIPTOK	0
	SKIPTOK	0
	SKIPTOK	0
	DEFTOK	CVI
	DEFTOK	CVS
	DEFTOK	CVD
	SKIPTOK	0
	DEFTOK	EOF
	DEFTOK	LOC
	DEFTOK	LOF
	DEFTOK	MKI$
	DEFTOK	MKS$
	DEFTOK	MKD$


;	The following tables are the alphabetic dispatch table
;	followed by the reserved word table itself
ALPTAB	DW	ATAB,BTAB,CTAB,DTAB
	DW	ETAB,FTAB,GTAB,HTAB
	DW	ITAB,JTAB,KTAB,LTAB
	DW	MTAB,NTAB,OTAB,PTAB
	DW	QTAB,RTAB,STAB,TTAB
	DW	UTAB,VTAB,WTAB,XTAB
	DW	YTAB,ZTAB
RESLST1	EQU	$-1
ATAB	TOKEN	AUTO
	TOKEN	AND
	TOKEN	ABS
	TOKEN	ATN
	TOKEN	ASC
	DB	00H
BTAB	DB	00H
CTAB	TOKEN	CLOSE
	TOKEN	CONT
	TOKEN	CLEAR
	TOKEN	CINT
	TOKEN	CSNG
	TOKEN	CDBL
	TOKEN	CVI
	TOKEN	CVS
	TOKEN	CVD
	TOKEN	COS
	TOKEN	CHR$
	TOKEN	CALL
	TOKEN	COMMON
	TOKEN	CHAIN
	DB	00H
DTAB	TOKEN	DELETE
	TOKEN	DATA
	TOKEN	DIM
	TOKEN	DEFSTR
	TOKEN	DEFINT
	TOKEN	DEFSNG
	TOKEN	DEFDBL
	TOKEN	DEF
	DB	00H
ETAB	TOKEN	ELSE
	TOKEN	END
	TOKEN	ERASE
	TOKEN	EDIT
	TOKEN	ERROR
	TOKEN	ERL
	TOKEN	ERR
	TOKEN	EXP
	TOKEN	EOF
	TOKEN	EQV
	DB	00H
FTAB	TOKEN	FOR
	TOKEN	FIELD
	TOKEN	FILES
	TOKEN	FN
	TOKEN	FRE
	TOKEN	FIX
	DB	00H
GTAB	TOKEN	GOTO
	DB	'O T','O'+80H,GOTOTK	;'GO TO'
	TOKEN	GOSUB
	TOKEN	GET
	DB	00H
HTAB	TOKEN	HEX$
	DB	00H
ITAB	TOKEN	INPUT
	TOKEN	IF
	TOKEN	INSTR
	TOKEN	INT
	TOKEN	INP
	TOKEN	IMP
	TOKEN	INKEY$
	DB	00H
JTAB	DB	00H
KTAB	TOKEN	KILL
	DB	00H
LTAB	TOKEN	LPRINT
	TOKEN	LLIST
	TOKEN	LPOS
	TOKEN	LET
	TOKEN	LINE
	TOKEN	LOAD
	TOKEN	LSET
	TOKEN	LIST
	TOKEN	LOG
	TOKEN	LOC
	TOKEN	LEN
	TOKEN	LEFT$
	TOKEN	LOF
	DB	00H
MTAB	TOKEN	MERGE
	TOKEN	MOD
	TOKEN	MKI$
	TOKEN	MKS$
	TOKEN	MKD$
	TOKEN	MID$
	DB	00H
NTAB	TOKEN	NEXT
	TOKEN	NULL
	TOKEN	NAME
	TOKEN	NEW
	TOKEN	NOT
	DB	00H
OTAB	TOKEN	OPEN
	TOKEN	OUT
	TOKEN	ON
	TOKEN	OR
	TOKEN	OCT$
	TOKEN	OPTION
	DB	00H
PTAB	TOKEN	PRINT
	TOKEN	PUT
	TOKEN	POKE
	TOKEN	POS
	TOKEN	PEEK
	DB	00H
QTAB	DB	00H
RTAB	TOKEN	RETURN
	TOKEN	READ
	TOKEN	RUN
	TOKEN	RESTORE
	TOKEN	REM
	TOKEN	RESUME
	TOKEN	RSET
	TOKEN	RIGHT$
	TOKEN	RND
	TOKEN	RENUM
	TOKEN	RESET
	TOKEN	RANDOMIZE,RANDOM
	DB	00H
STAB	TOKEN	STOP
	TOKEN	SWAP
	TOKEN	SAVE
	TOKEN	SPC(,SPC
	TOKEN	STEP
	TOKEN	SGN
	TOKEN	SQR
	TOKEN	SIN
	TOKEN	STR$
	TOKEN	STRING$
	TOKEN	SPACE$
	TOKEN	SYSTEM
	DB	00H
TTAB	TOKEN	THEN
	TOKEN	TRON
	TOKEN	TROFF
	TOKEN	TAB(,TAB
	TOKEN	TO
	TOKEN	TAN
	DB	00H
UTAB	TOKEN	USING
	TOKEN	USR
	DB	00H
VTAB	TOKEN	VAL
	TOKEN	VARPTR
	DB	00H
WTAB	TOKEN	WIDTH
	TOKEN	WAIT
	TOKEN	WHILE
	TOKEN	WEND
	TOKEN	WRITE
	DB	00H
XTAB	TOKEN	XOR
	DB	00H
YTAB	DB	00H
ZTAB	DB	00H
SPCTAB	TOKENSP	'+',PLUS
	TOKENSP	'-',MINU
	TOKENSP	'*',MULT
	TOKENSP	'/',DIV
	TOKENSP	'^',PWR
	TOKENSP	'\',IDIV
	TOKENSP	"'",SNGQ
	TOKENSP	'>',GREA
	TOKENSP	'=',EQUL
	TOKENSP	'<',LESS
	DB	00H

;=============================================================================
;	ROM VERSION INITALIZATION, AND CONSTANTS
; ## GWDATA.ASM:265 ##
;
; 	Arithmetic precedence table
OPTAB	DB	79H		;'+'	token code $F2
	DB	79H		;'-'
	DB	7CH		;'*'
	DB	7CH		;'/'
	DB	7FH		;'^'
	DB	50H		;'AND'
	DB	46H		;'OR'
	DB	3CH		;'XOR'
	DB	32H		;'EQV'
	DB	28H		;'IMP'
	DB	7AH		;'MOD'
	DB	7BH		;'\'	token code $FD

;	Used by assignment code to force the right hand value
;	to correspond to the value type of the variable being
;	assigned to.
;
;	NUMBER TYPES
FRCTBL	DW	CDBL,	0000H,	CINT,	CHKSTR,	CSNG

;
;	These tables are used after the decision has been made
;	to apply an operator and all the necessary conversion has
;	been done to match the two argument types (APPLOP)
;
;	ARITHMETIC OPERATIONS TABLE
;
; 	DBL OPERATIONS TABLE
DBLDSP	DW	DADD,	DSUB,	DMULT,	DDIV,	DCOMPX
; 	SNG OPERATIONS TABLE
SNGDSP	DW	FADD,	FSUB,	FMULT,	FDIV,	FCOMP
;	INT OPERATIONS TABLE
INTDSP	DW	IADD,	ISUB,	IMULT,	INTDIV,	ICOMP

OPCNT	EQU	((SNGDSP-DBLDSP)/2)-1

;-----------------------------------------------------------------------------
;	ERROR MESSAGE TABLE
; ## GWDATA.ASM:326 ##
;
QQ	DEFL	01
ERRTAB	DB	00H
	DCL	NF,'NEXT without FOR'
	DCL	SN,'Syntax error'
	DCL	RG,'RETURN without GOSUB'
	DCL	OD,'Out of DATA'
	DCL	FC,'Illegal function call'
;	"Overflow" 06H
$OVMSG	DCL	OV,'Overflow'
	DCL	OM,'Out of memory'
	DCL	US,'Undefined line number'
	DCL	BS,'Subscript out of range'
	DCL	DD,'Duplicate Definition'
;	"Division by zero" 0B
$DIV0M	DCL	DV0,'Division by zero'
	DCL	ID,'Illegal direct'
	DCL	TM,'Type mismatch'
	DCL	OS,'Out of string space'
	DCL	LS,'String too long'
	DCL	ST,'String formula too complex'
	DCL	CN,"Can't continue"
	DCL	UF,'Undefined user function'
	DCL	NR,'No RESUME'
	DCL	RE,'RESUME without error'
	DCL	UE,'Unprintable error'
	DCL	MO,'Missing operand'
	DCL	LBO,'Line buffer overflow'
	DCL	DTO,'?'
	DCL	DVF,'?'
	DCL	FN,'FOR Without NEXT'
	DCL	OPT,'?'
	DCL	UE4,'?'
	DCL	WH,'WHILE without WEND'
	DCL	WE,'WEND without WHILE'
NONDSK	EQU	QQ

	;First disk error message
QQ	DEFL	50		;DISK ERRORS START AT 50.
DSKERR	EQU	QQ		;FIRST DISK ERROR
DSKER1	EQU	DSKERR-NONDSK	;OFFSET OF DSKERR FROM NONDSK
ERRUE1	EQU	ERRUE+DSKER1	;ERRUE + OFFSET

DKERM1	DCL	FOV,'FIELD overflow'
	DCL	IER,'Internal error'
	DCL	BFN,'Bad file number'
	DCL	FNF,'File not found'
	DCL	BFM,'Bad file mode'
	DCL	FAO,'File already open'
	DCL	LOC,'?'
	DCL	DIO,'Disk I/O error'
	DCL	FAE,'File already exists'
	DCL	UE6,'?'
	DCL	UE7,'?'
	DCL	DFL,'Disk full'
	DCL	RPE,'Input past end'
	DCL	BRN,'Bad record number'
	DCL	IFN,'Bad file name'
	DCL	UE8,'?'
	DCL	FDR,'Direct statement in file'
	DCL	TMF,'Too many files'
LSTERR	EQU	QQ		;LAST DISK ERROR + 1

;-----------------------------------------------------------------------------
;	LOW SEGMENT -- RAM-- IE THIS STUFF IS NOT CONSTANT
; ## GWDATA.ASM:675 ##
;
;
;	This is the "volatile" storage area and none of it
;	can be kept in ROM. Any constants in this area cannot
;	be kept in a ROM, but must be loaded in by the
;	program instructions in ROM.
;
USRTAB	DW	FCERR,FCERR,FCERR,FCERR,FCERR
	DW	FCERR,FCERR,FCERR,FCERR,FCERR
NULLS	DB	01H
NXTKEY	DB	00H
;	Used to save the error number
;	 so EDIT can be called on "SN" err.
ERRFLG	DB	00H,00H
;	Position of LPT print head -initially 0
LPTPOS	DB	00H
;	Whether output goes to LPT
PRTFLG	DB	00H
;	Last col # beyond which no more comma fields (LPRINT)
NLPPOS	DB	LNCMPS
;	Default line printer width (LPRINT)
LPTSIZ	DB	LPTLEN
;	Default console width (PRINT)
TTYSIZ	DB	LINLN
;	Last col # beyond which there are no more comma fields (PRINT)
NTTPOS	DB	(((LINLN/CLMWID)-1)*CLMWID)
;	RUBOUT SWITCH =1 INSIDE THE PROCESSING OF A RUBOUT (INLIN)
RUBSW	DB	00H
;	Suppress Output Flag
;	Non-zero means Suppress
;	Reset by "INPUT",READY and errors
;	Complemented by input of ^O
;	Note: 	No toggle using ^O on TRS-80.
;		This flag seems never set to anything
;		else than zero.
CNTOFL	DB	00H
;	(Original comment from ALTAIR BASIC -- for TOPMEM)
;	Top location to use for the stack
;	initially set up by INIT
;	according to MEMORY SIZE
;	to allow for 50 bytes of string space.
;	Changed by a CLEAR command with
;	an argument.
;
;	Pointer to data block of current file
;	Used by disk and NCR cassette code
PTRFIL	DW	0000H
;	Points to 1st FDB (=STKLOW if no FDB's)
;	Note: in TRS-80, FDBs are pre-allocated using the "FILES="
;	command line parameter. And no STKLOW !
FILTAB	DW	TSTACK+100
;	Current line #
;	Set to 65534 in pure version during INIT execution
;	Set to 65535 when direct statements execute
CURLIN	DW	0FFFEH
;	Pointer to beginning of text
;	Doesn't change after being
;	setup by INIT.
TXTTAB	DW	TSTACK+1
;	Address of message to print (overflow)
OVERRI	DW	$OVMSG
RUNFLG	DB	00H
	;Save NFILES
NFILSSV	DB	00H
;	PUT/GET flag (Non zero=PUT)
PGTFLG	DB	00H,00H
;	TODO (disk files...)
FILPT1	DW	0000H
;	Table of up to 16 FDB's
FDBTAB	DC	32,0
;	Number of files
NFILES	DB	00H
;	The number of character beyond #2 in a var name
NAMCNT	DB	00H
;	Storage for chars beyond #2. Used in PTRGET
NAMBUF	DC	NAMLEN-2,0
;	Temp storage during name save at INDLOP
NAMTMP	DB	00H,00H
;	The File Control Block is a 36-byte data structure
;	 (33 bytes in CP/M 1).
;	Second storage space for the file name (like FILNAM)
FILNA2	DC	16,0
;	-- CP/M 1 FCB structure --
;	The File Control Block is a 36-byte
;	 data structure (33 bytes in CP/M 1).
;
;	Drive. 0 for default, 1-16 for A-P.
FCBDRV	DB	00H
;	File name (8 bytes)
FCBFNAM	DB	00H,00H,00H,00H,00H,00H,00H,00H
;	File type (a.k.s. "file extension", 3 bytes)
FCBFTYP	DB	00H,00H,00H
;	File extent [ie (file pointer % 524288) / 16384],
;	Set to 0 when opening a file
FXBXTND	DB	00H,00H,00H,00H,00H,00H,00H,00H
	DB	00H,00H,00H,00H,00H,00H,00H,00H
	DB	00H,00H,00H,00H,00H
	;CP/M version in BCD format (22)
CPMVER	DB	00H
	;BDOS RD service (1.x:14, 2.x:21)
CPMREA	DB	00H
	;BDOS WR service (1.x:15, 2.x:22)
CPMWRI	DB	00H
	;=KBUF-1, initialized with a ":"
KBUF1	DB	':'		;a colon for restarting input
;	This is the KRUNCH buffer
KBUF	DC	KBFLEN,0
	;=BUF-1
BUFMIN	DB	','		;a comma used, e.g. in "FILSTI"
;	Type in stored here
;	Direct statements execute out of
;	here. Remember "INPUT" smashes BUF.
;	Must be at a lower address
;	than DSCTMP or assignment of string
;	values in direct statements won't copy
;	into string space -- which it must
;	allow for single quote in big line
BUF	DC	BUFLEN+3,0
ENDBUF	DB	00H
;	Store terminal position here
TTYPOS	DB	00H
;	In getting a pointer to a variable
;	it is important to remember whether it
;	is being done for "DIM" or not
;	DIMFLG and VALTYP must be
;	consecutive locations
DIMFLG	DB	00H
;	The type indicator
;	In the 8K 0=numeric 1=string
VALTYP	DB	00H
;	Whether can or can't crunch res'd words
;	Turned on in the 8K when "DATA"
;	being scanned by CRUNCH so unquoted
;	strings won't be crunched.
DORES	DB	00H
;	Flag for CRUNCH =0 means
;	numbers allowed, (Floating,INT, DBL)
;	1 means numbers allowed, KRUNCH by calling LINGET
;	-1 (377) means numbers disallowed
;	(scanning variable name)
DONUM	DB	00H
;	Saved text pointer used by CHRGET
;	to save the text pointer after constant
;	has been scanned.
CONTXT	DB	00H,00H
;	The saved token for a constant
;	after CHRGET has been called
CONSAV	DB	00H
;	Saved constant valtype
CONTYP	DB	00H
;	Saved constant value
;	Extra 4 bytes for DBL precision
CONLO	DB	00H,00H
;	HI constant value
CONHI	DB	00H,00H,00H,00H,00H,00H
;	Highest location in memory
MEMSIZ	DB	00H,00H
;	Pointer at first free Temp Descriptor
;	Initialized to point to TEMPST
TEMPPT	DB	00H,00H
;	Storage for NUMTMP Temp Descriptors
TEMPST	DB	00H,00H,00H,00H,00H,00H,00H,00H
	DB	00H,00H,00H,00H,00H,00H,00H
	;=DEFTBL-'A' (DEFINT...)
DEFT41	DB	00H,00H,00H,00H,00H,00H,00H,00H
	DB	00H,00H,00H,00H,00H,00H,00H
;	String functions build answer descriptor here
;	Must be after TEMPST and before PARM1
DSCTMP	DB	00H
;	DSCTMP+INCSTR(=1)
DSCTMPI	DB	00H,00H
;	Top of String free space
FRETOP	DB	00H,00H
;	Used to store the address of the end of
;	string arrays in garbage collection
;	and used momentarily by FRMEVL
;	Used in extended by FOUT and
;	user defined functions
;	Array variable handling temporary
TEMP3	DB	00H,00H
;	7/3/79 Now used by garbage collection
;	not TEMP3 due to conflict
TEMP8	DB	00H,00H
;	Saved text pointer at end of "FOR" statement
ENDFOR	DB	00H,00H
;	DATA line # -- remember for errors
DATLIN	DB	00H,00H
;	Flag whether subscripted variable allowed
;	"FOR" and user-defined function
;	pointer fetching turn
;	this on before calling PTRGET
;	so arrays won't be detected.
;	STKINI and PTRGET clear it.
SUBFLG	DB	00H
;	Scaned-ARraY-FLag: set by PTRGET when
;	it scans an array element.  Tested by
;	CALL86 so undefined scalers won't
;	be permited after array references
;	in CALL parameter list.
SARYFL	DB	00H
;	Temporary for statement code
;	NEWSTT saves [H,L] here for INPUT and ^C
;	"LET" saves variable
;	pointers here for "FOR"
;	"NEXT" saves its text pointer here
;	CLEARC saves [H,L] here
TEMP	DB	00H,00H
;	=0 if no line numbers converted
;	to pointers, non zero if pointers exist
PTRFLG	DB	00H
;	Flag to inicate AUTO command in
;	progress =0 if not, non-zero if so.
AUTFLG	DB	00H
;	Current line being inserted by AUTO
AUTLIN	DB	00H,00H
;	The AUTO increment
AUTINC	DB	00H,00H
;	Place where NEWSTT saves text pointer
;	for "RESUME" statement
SAVTXT	DB	00H,00H
;	NEWSTT saves stack here before
;	so that error recovery can
;	restore the stack when an
;	error occurs
SAVSTK	DB	00H,00H
;	Line number where last error occured.
ERRLIN	DB	00H,00H
;	Keeps current line for EDIT & LIST
DOT	DB	00H,00H
;	Text pointer for use by "RESUME"
ERRTXT	DB	00H,00H
;	The line to GOTO when an error
;	occurs
ONELIN	DW	0000H
;	ONEFLG=1 if we are executing
;	an error trap routine, otherwise 0
ONEFLG	DB	00H
;	(=$DPADR)
;	Formula evaluator TEMP
;	Must be preserved by operators
;	Used in Extended by FOUT and
;	user-defined functions
;	Array variable handler temporary
TEMP2	DB	00H,00H
;	Old line number (setup by ^C,"STOP"
;	or "END" in a program)
OLDLIN	DB	00H,00H
;	Old text pointer
;	Points at statement to be executed next
OLDTXT	DB	00H,00H
;	Pointer to start of simple
;	variable space
;	Updated whenever the size of the
;	program changes, set to [TXTTAB]+2
;	by SCRATCH ("NEW").
VARTAB	DB	00H,00H
;	Pointer to beginning of array
;	table
;	Incremented by 6 whenever
;	a new simple variable is found, and
;	set to [VARTAB] by CLEARC.
ARYTAB	DB	00H,00H
;	End of storage in use
;	Increased whenever a new array
;	or simple variable is encountered
;	Set to [VARTAB] by CLEARC.
STREND	DB	00H,00H
;	Pointer to DATA. Initialized to point
;	at the zero in front of [TXTTAB]
;	by "RESTORE" which is called by CLEARC
;	Updated by execution of a "READ"
DATPTR	DB	00H,00H
;	This gives the default VALTYP for each
;	letter of the alphabet
;	It is set up by "CLEAR" and changed by
;	"DEFSTR" "DEFINT" "DEFSNG" "DEFDBL" and used
;	by PTRGET when ! # % or $ don't follow
;	a variable name
DEFTBL	DB	00H,00H,00H,00H,00H,00H,00H,00H
	DB	00H,00H,00H,00H,00H,00H,00H,00H
	DB	00H,00H,00H,00H,00H,00H,00H,00H
	DB	00H,00H

;-----------------------------------------------------------------------------
;	RAM STORAGE FOR USER DEFINED FUNCTION PARAMETER INFORMATION
;
PRMSIZ	EQU	100		;NUMBER OF BYTES FOR DEFINITION BLOCK
;	Previous definition block on stack
;	block (for garbage collection)
PRMSTK	DB	00H,00H
;	The number of bytes in the active table
PRMLEN	DB	00H,00H
;	The active parameter definition table
;	Allow INIT to initialize this constant
PARM1	DC	PRMSIZ,0
;	Iniially PRMSTK, the pointer at the previous parameter
;	block (for garbage collection)
PRMPRV	DB	00H,00H
;	Size of parameter block being built
PRMLN2	DB	00H,00H
;	Place to keep parameters being made
PARM2	DC	PRMSIZ,0
;	Used by PTRGET to flag if PARM1 has been searched
PRMFLG	DB	00H
;	Stopping point for simple search
;	(Either [ARYTAB] or PARM1+[PRMLEN])
ARYTA2	DB	00H
	;=ARYTA2+1
ARYTA21	DB	00H
;	Zero if no functions active. Saves time in simple search
NOFUNS	DB	00H
;	Garbage collection temp to chain through parameter blocks
TEMP9	DB	00H,00H
;	Count of active functions
FUNACT	DB	00H,00H
;	Flag telling whether INPUT is scanning first or
;	second time. Zero if first.
INPPAS	DB	00H
;	Used to save text pointer at start of NEXT
NXTTXT	DB	00H,00H
;	Zero if "FOR" is using NEXT code
;	to check for empty loop
NXTFLG	DB	00H
;	Use to store the start value of the loop variable
;	since ANSI says start and end are evaluated
;	before assignment takes place.
FVALSV	DB	00H,00H,00H,00H
;	The line number during scan for "NEXT"
NXTLIN	DB	00H,00H
;	Zero for option base 0 - One for option base 1
OPTVAL	DB	00H
;	Non-zero if "OPTION BASE" has been scanned
OPTFLG	DC	31,0
;	Misc temp used by CALL and LIST
TEMPA	DB	00H
;	??? -- FLAG TO DO "? "
TEMPA1	DB	00H
;	FRETOP saved here by CHAIN
SAVFRE	DB	00H,00H
;	=MAXREC
RECSIZ	DB	00H,00H
;	Non-zero if we have loaded a protected file w/o passwrd
PROFLG	DB	00H
;	Non-zero if CHAIN w/ MERGE in progress
MRGFLG	DB	00H
;	Non-zero if CHAIN w/ MERGE and DELETE in progress
MDLFLG	DB	00H
;	Pointer to end line to DELETE
CMEPTR	DB	00H,00H
;	Pointer to start line to DELETE
CMSPTR	DB	00H,00H
;	Non-zero if CHAIN in progress
CHNFLG	DB	00H
;	Destination line in new program
CHNLIN	DB	00H,00H
;	Value of first "SWAP" variable stored here
;	Enough room for dbl precision
SWPTMP	DB	00H,00H,00H,00H,00H,00H,00H,00H
;	Zero means no trace in progress
TRCFLG	DB	00H

;-------------------------------------------------------------
; THIS IS THE RAM TEMPORARY AREA FOR THE MATH PACKAGE ROUTINES
;-------------------------------------------------------------
;	Four lowest orders for double precision
DFACLO1	DB	00H
DFACLO	DB	00H,00H
;	=DFACLO+2
DFACLO2	DB	00H
;	=DFACLO+3
DFACLO3	DB	00H
;	[Low order of mantissa]
FACLO	DB	00H,00H
;	[Middle order of mantissa]
;	[High order of mantissa]
FACHI	DB	00H
;	[Exponent]
FAC	DB	00H
;	[Temporary complement of sign in MSB]
FAC_1	DB	00H
;	Overflow print flag,=0,1 print
;	Further =1 change to 2
FLGOVC	DB	00H
;	Place to store overflow flag after FIN
OVCSTR	DB	00H
;	Flags input code executing for SCNVAL
FLGSCN	DB	00H
;	[Temporary least significant byte]
ARGLO11	DB	00H
;	[Location of second argument for double precision]
ARGLO	DB	00H,00H,00H,00H,00H,00H
;	=ARG-1 (MSB mantissa)
ARGHI	DB	00H
;	ARG (exponent)
ARG	DB	00H
;	Buffer for FOUT
FBUFFR	DB	00H
	;=FBUFFR+1
FBUFFR1	DB	00H,00H,00H,00H,00H,00H,00H,00H
	DB	00H,00H,00H,00H,00H,00H,00H,00H
	DB	00H,00H,00H,00H,00H,00H,00H,00H
	DB	00H,00H
;	The last 3 locations are temp for ROM fmult
FMLTT1	DB	00H,00H,00H,00H,00H,00H
	;=FMLTT1+6=FBUFFR+21H: END OF FBUFFR
FMLTT16	DB	00H
	;=FMLTT1+7=FBUFFR+22H
FMLTT17	DB	00H,00H,00H,00H,00H,00H,00H,00H
	DB	00H
;
;=============================================================================
;	TEXT CONSTANTS FOR PRINT OUT
; ## GWDATA.ASM:1533 ##
;
;	Needed for messages in all versions
;	Must be stored above DSCTMP or else STRLIT
;	will copy them before STRPRT prints them, this is bad, since if the
;	user is out of string space basic will loop getting "Out of String Space"
;	errors.
INTXT	DB	' in '
	;Empty String
DSEGZ	DB	00H
REDDY	DB	'Ok',0DH,0AH,00H
BRKTXT	DB	'Break',00H


;=============================================================================
;	GWMAIN Copied from BINTRP.MAC
;
;	--------- ---- -- ---- ----- --- ---- -----
;	COPYRIGHT 1975 BY BILL GATES AND PAUL ALLEN
;	--------- ---- -- ---- ----- --- ---- -----
;
;	Originally written on the PDP-10 from
;	February 9 to  April 9 1975
;
;	Bill Gates wrote a lot of stuff.
;	Paul Allen wrote a lot of other stuff and fast code.
;	Monte Davidoff wrote the math package (F4I.MAC).
;
;-----------------------------------------------------------------------------
;	GENERAL STORAGE MANAGEMENT ROUTINES - FNDFOR, BLTU, GETSTK
; ## GWMAIN.SRC:300 ##
;
;	Find a "FOR" entry on the stack with the variable pointer
;	passed in [D,E].
;
FNDFOR	LD	HL,0004H	;IGNORING EVERYONES "NEWSTT" AND THE RETURN
	ADD	HL,SP		; ADDRESS OF THIS SUBROUTINE
;	Look for "FOR" block with same index as specified in D
LOOPER	LD	A,(HL)		;Get block ID (SEE WHAT TYPE OF
				; THING IS ON THE STACK)
	INC	HL		;Point to index address
	CP	WHILETK		;Is it a "WHILE" token
	JP	NZ,STKSRC	;No - check "FOR" as well
	LD	BC,0006H	;WHLSIZ
	ADD	HL,BC
	JP	LOOPER

STKSRC	CP	FORTK		;IS THIS STACK ENTRY A "FOR"?
	RET	NZ		;NO SO OK
	LD	C,(HL)
	INC	HL		;DO EQUIVALENT OF PUSHM / XTHL
	LD	B,(HL)
	INC	HL
	PUSH	HL		;PUT H  ON
	LD	H,B		;PUSH B / XTHL IS SLOWER
	LD	L,C
	LD	A,D		;FOR THE "NEXT" STATEMENT WITHOUT AN ARGUMENT
	OR	E		;WE MATCH ON ANYTHING
	EX	DE,HL		;MAKE SURE WE RETURN [D,E]
	JP	Z,POPGOF	;POINTING TO THE VARIABLE
	EX	DE,HL		;Index back into DE
	CALL	COMPAR		;Compare index with one given
;
;	Note - 8086 versions force stack entries to be an even length
;	so stack accesses won't cross word boundaries.  This is done
;	for speed.  To accomplish this, an extra byte is pushed on
;	top of the FOR token.  This extra byte is NOT reflected in
;	the value of FORSIZ but is taken care of by the code.
;
POPGOF	LD	BC,0010H	;FORSZC: Offset to next block
				; TO WIPE OUT A "FOR" ENTRY
	POP	HL		;Restore pointer to sign
	RET	Z		;Return if block found, WITH [H,L]
				; POINTING THE BOTTOM OF THE ENTRY
	ADD	HL,BC		;Point to next block
	JP	LOOPER		;Keep on looking


;-----------------------------------------------------------------------------
;	ERROR HANDLING
; ## GWMAIN.ASM:372 ##
;
;	This routine is called to reset the stack if BASIC is
;	externally stopped and then restarted.
;
READYR	LD	BC,STPRDY	;ADDRESS GO TO, ALSO POP OFF GARBAGE STACK ENTRY.
	JP	ERESET		;RESET STACK, GOTO READY.

PRGEND	LD	HL,(CURLIN)	;GET CURRENT LINE #
	LD	A,H		;SEE IF DIRECT
	AND	L		;AND TOGETHER
	INC	A		;SET CC'S
	JP	Z,ENDCNJ	;IF DIRECT DONE, ALLOW FOR DEBUGGING PURPOSES
	LD	A,(ONEFLG)	;SEE IF IN ON ERROR
	OR	A		;SET CC
	LD	E,ERRNR		;"NO RESUME" ERROR
	JP	NZ,ERROR	;YES, FORGOT RESUME
ENDCNJ	JP	ENDCON		;NO, LET IT END

	DERMAK	DFL		;Disk Full
	DERMAK	DIO		;Device I/O error
	DERMAK	BFM		;Bad file mode
	DERMAK	FNF		;File not found
	DERMAK	BFN		;Bad file number
	DERMAK	IER		;Internal error
	DERMAK	RPE		;Input past end
	DERMAK	FAO		;File already open
	DERMAK	IFN		;Bad file name
	DERMAK	BRN		;Bad record number
	DERMAK	FOV		;FIELD overflow
	DERMAK	TMF		;Too many files
	DERMAK	FAE,0		;File already exists

	JP	ERROR		;Error handler, E = Error code

;	Get data line, make it current line
DATSNE	LD	HL,(DATLIN)	;GET DATA LINE
	LD	(CURLIN),HL	;MAKE IT CURRENT LINE
	ERRMAK	SN		;Syntax Error
	ERRMAK	DV0		;Division by zero
	ERRMAK	NF		;NEXT without FOR
	ERRMAK	DD		;Duplicate Definition
	ERRMAK	UF		;Undefined user function
	ERRMAK	RE		;RESUME without error
	ERRMAK	OV		;Overflow
	ERRMAK	MO		;Missing operand
	ERRMAK	TM,0		;Type Mismatch

;	Error handler, E = Error code
ERROR	LD	HL,(CURLIN)	;GET CURRENT LINE NUMBER
	LD	(ERRLIN),HL	;SAVE IT FOR ERL VARIABLE
	XOR	A		;CLEAR CHAIN FLAG IN CASE OF ERROR
	LD	(MRGFLG),A	;ALSO MERGE FLAG
	LD	(CHNFLG),A	;SO IT DOESNT TRY TO CHAIN
	LD	A,H		;ONLY SET UP DOT IF IT ISNT DIRECT
	AND	L
	INC	A
	JP	Z,ERRESM
	LD	(DOT),HL	;SAVE IT FOR EDIT OR LIST
ERRESM	LD	BC,ERRMOR	;GET RETURN ADDRESS IN [B,C]
ERESET	LD	HL,(SAVSTK)	;GET A GOOD STACK BACK
	JP	XSTOP1		;JUMP INTO STKINI

;	Error handling continues, err code in E
ERRMOR	POP	BC		;POP OFF FNDFOR STOPPER
	LD	A,E		;[A]=ERROR NUMBER
	LD	C,E		;ALSO SAVE IT FOR LATER RESTORE
	LD	(ERRFLG),A	;SAVE IT SO WE KNOW WHETHER TO CALL "EDIT"
	LD	HL,(SAVTXT)	;GET SAVED TEXT POINTER
	LD	(ERRTXT),HL	;SAVE FOR RESUME.
	EX	DE,HL		;SAVE SAVTXT PTR
	LD	HL,(ERRLIN)	;GET ERROR LINE #
	LD	A,H		;TEST IF DIRECT LINE
	AND	L		;SET CC'S
	INC	A		;SETS ZERO IF DIRECT LINE (65535)
	JP	Z,NTMDCN	;IF DIRECT, DONT MODIFY OLDTXT & OLDLIN
	LD	(OLDLIN),HL	;SET OLDLIN=ERRLIN.
	EX	DE,HL		;GET BACK SAVTXT
	LD	(OLDTXT),HL	;SAVE IN OLDTXT.
NTMDCN	LD	HL,(ONELIN)	;SEE IF WE ARE TRAPPING ERRORS.
	LD	A,H		;BY CHECKING FOR LINE ZERO.
	OR	L
	EX	DE,HL		;PUT LINE TO GO TO IN [D,E]
	LD	HL,ONEFLG	;POINT TO ERROR FLAG
	JP	Z,NOTRAP	;SORRY, NO TRAPPING...
	AND	(HL)		;A IS NON-ZERO, SETZERO IF ONEFLG ZERO
	JP	NZ,NOTRAP	;IF FLAG ALREADY SET, FORCE ERROR
	DEC	(HL)		;IF ALREADY IN ERROR ROUTINE, FORCE ERROR
	EX	DE,HL		;GET LINE POINTER IN [H,L]
	JP	GONE4		;GO DIRECTLY TO NEWSTT CODE

NOTRAP	XOR	A		;A MUST BE ZERO FOR CONTRO
	LD	(HL),A		;RESET ONEFLG
	LD	E,C		;GET BACK ERROR CODE
	LD	(CNTOFL),A	;FORCE OUTPUT
	CALL	CRDONZ		;CRLF
	LD	HL,ERRTAB	;GET START OF ERROR TABLE
	LD	A,E		;GET ERROR CODE
	CP	LSTERR		;IS IT PAST LAST ERROR?
	JP	NC,UPERR	;YES, TOO BIG TO PRINT
	CP	DSKERR		;DISK ERROR?
	JP	NC,NTDER2	;YES
	CP	NONDSK		;IS IT BETWEEN LAST NORMAL & FIRST DISK?
	JP	C,NTDERR	;YES, OK TO PRINT IT
	;Unprintable error?
UPERR	LD	A,ERRUE1	;PRINT "UNPRINTABLE ERROR"
	;Disk error: error message # = (E) - 13H (=19=50-31)
NTDER2	SUB	DSKER1		;FIX OFFSET INTO TABLE OF MESSAGES
	LD	E,A		;SAVE BACK ERROR CODE
	;Skip to end of message (using REM), get (E)th msg
NTDERR	CALL	REM
	INC	HL		;SKIP OVER THIS ERROR MESSAGE
	DEC	E		;DECREMENT ERROR COUNT
	JP	NZ,NTDERR	;Skip to end of message (using REM),
				; get (E)th msg
	PUSH	HL		;SAVE TEXT POINTER
	LD	HL,(ERRLIN)	;GET ERROR LINE NUMBER
	EX	(SP),HL		;GET BACK ERROR TEXT POINTER
ERRFIN	LD	A,(HL)		;GET 1ST CHAR OF ERROR
	CP	'?'		;PADDED ERROR?
	JP	NZ,ERRFN1	;NO,PRINT
	POP	HL		;GET LINE # OFF STACK
	LD	HL,ERRTAB
	JP	UPERR		;MAKE UNPRINTABLE ERROR

;	Print error message
ERRFN1	CALL	STROUT		;PRINT MESSAGE
	POP	HL		;RESTORE LINE NUMBER
	LD	DE,0FFFEH	;IS INIT EXECUTING?
	CALL	COMPAR
	CALL	Z,CRDO		;DO CRLF
	JP	Z,SYSTEMX	;SYSTEM error exit
	LD	A,H		;SEE IF IN DIRECT MODE
	AND	L
	INC	A		;ZERO SAYS DIRECT MODE
	CALL	NZ,INPRT	;PRINT LINE NUMBER IN [H,L]
	DB	3EH		;SKIP THE NEXT BYTE WITH "MVI A,0"
				;NOW FALL INTO MAIN INTERPRETER LOOP

;-----------------------------------------------------------------------------
;	STPRDY, READY, MAIN, CHEAD
; ## GWMAIN.ASM:578 ##
;
;	For "LIST" command stopping
;	and for returning from a failed "CVER"
;	and to correct a direct GOSUB which does INPUT
;
STPRDY	POP	BC
READY	CALL	OUTDOA		;PRINT ANY LEFT OVERS
	XOR	A
	LD	(CNTOFL),A	;FORCE OUTPUT
	CALL	PRGFIN		;FINISH OUTPUT OF A FILE
	CALL	CRDONZ		;IF NOT ALREADY AT LEFT, SEND CRLF
	LD	HL,REDDY	;"OK" CRLF

;	Other versione here have an extra check:
;	"ERRORS IN CP/M INITIALIZATION, RETURN TO CP/M"
	CALL	CPMWRM		;gets STROUT during INIT
REPINI	EQU	$-2
	LD	A,(ERRFLG)	;SEE IF IT WAS A "SYNTAX ERROR"
	SUB	02H
	CALL	Z,ERREDT	;"EDIT" THE BAD LINE
MAIN	LD	HL,0FFFFH
	LD	(CURLIN),HL	;SETUP CURLIN FOR DIRECT MODE
	LD	A,(AUTFLG)	;IN AN AUTO COMMAND?
	OR	A		;SET CC'S
	JP	Z,NTAUTO	;NO, REGULAR MODE
	LD	HL,(AUTLIN)	;GET CURRENT AUTO LINE
	PUSH	HL		;SAVE AWAY FOR LATER USE
	CALL	LINPRT		;PRINT THE LINE #
	POP	DE		;GET IT BACK
	PUSH	DE		;SAVE BACK AGAIN
	CALL	FNDLIN		;SEE IF IT EXISTS
	LD	A,'*'		;CHAR TO PRINT IF LINE ALREADY EXISTS
	JP	C,AUTELN	;DOESN'T EXIST
	LD	A,' '		;PRINT SPACE
AUTELN	CALL	OUTDO		;PRINT CHAR
	CALL	PINLIN		;GET PROGRAM LINE INPUT
	POP	DE
	JP	NC,NTSTOP
	XOR	A
	LD	(AUTFLG),A	;IN CASE OF AUTO, CLEAR IT
	JP	READY

;	TODO: label
NTSTOP0	XOR	A
	LD	(AUTFLG),A	;IN CASE OF AUTO, CLEAR IT
	JP	NTSTOP1

;	TODO: label
NTSTOP	LD	HL,(AUTINC)	;GET INCREMENT
	ADD	HL,DE		;ADD INCREMENT TO THIS LINE
	JP	C,NTSTOP0	;CHECK FOR PATHETIC CASE
	PUSH	DE		;SAVE LINE NUMBER #
	LD	DE,0FFF9H	;CHECK FOR LINE # TOO BIG
	CALL	COMPAR
	POP	DE		;GET BACK LINE #
	JP	NC,NTSTOP0	;IF TOO BIG, QUIT
	LD	(AUTLIN),HL	;SAVE IN NEXT LINE
;	SET NON-ZERO CONDITION CODES (SEE EDIT)
NTSTOP1	LD	A,(BUF)		;GET CHAR FROM BUFFER
	OR	A		;IS IT NULL LINE?
	JP	Z,MAIN		;YES, LEAVE LINE ALONE
	JP	EDTSTL		;JUMP INTO EDIT CODE

NTAUTO	CALL	PINLIN		;GET PROGRAM LINE INPUT
	JP	C,MAIN
	CALL	CHRGTR		;GET THE FIRST
	INC	A		;SEE IF 0 SAVING THE CARRY FLAG
	DEC	A
	JP	Z,MAIN		;IF SO, A BLANK LINE WAS INPUT
	PUSH	AF		;SAVE STATUS INDICATOR FOR 1ST CHARACTER
	CALL	LINGET		;READ IN A LINE #
	CALL	BAKSP		;BACK UP THE POINTER
	LD	A,(HL)
	CP	20H		;CHARACTER A SPACE?
	CALL	Z,INXHRT	;THEN EAT PAST IT
EDENT	PUSH	DE		;SAVE LINE #
	CALL	CRUNCH		;CRUNCH THE LINE DOWN
	POP	DE		;RESTORE LINE #
	POP	AF		;WAS THERE A LINE #?
	LD	(SAVTXT),HL	;FOR RESUMING A DIRECT STMT
	JP	NC,DIRDO	;MAKE SURE WE'RE NOT READING A FILE
	PUSH	DE
	PUSH	BC		;SAVE LINE # AND CHARACTER COUNT
	CALL	PROCHK		;DONT ALLOW ANY FUNNY BUSINESS WITH EXISTING PGM
	CALL	CHRGTR		;REMEMBER IF THIS LINE IS
	OR	A		;SET THE ZERO FLAG ON ZERO
				;LINES THAT START WITH ":" SHOULD NOT BE
				;IGNORED
	PUSH	AF		;BLANK SO WE DON'T INSERT IT
	EX	DE,HL
	LD	(DOT),HL	;SAVE THIS LINE # IN DOT
	EX	DE,HL
	CALL	FNDLIN		;GET A POINTER TO THE LINE
	JP	C,INONLY	;LINE EXISTS, DELETE IT
	POP	AF		;GET FLAG SAYS WHETHER LINE BLANK
	PUSH	AF		;SAVE BACK
	JP	Z,USERR		;TRYING TO DELETE NON-EXISTANT LINE, ERROR
	OR	A		;CLEAR FLAG THAT SAYS LINE EXISTS
INONLY	PUSH	BC		;SAVE REGISTERS
	PUSH	AF
	PUSH	HL		;SAVE [H,L]
	CALL	DEPTR		;GET RID OF PTRS IN PGM
	POP	HL		;GET BACK POINTER TO NEXT LINE
	POP	AF		;GET BACK PSW
	POP	BC		;RESTORE POINTER TO THIS LINE
	PUSH	BC		;SAVE BACK AGAIN
	CALL	C,DEL		;DELETE THE LINE
	POP	DE		;POP POINTER AT PLACE TO INSERT
	POP	AF		;SEE IF THIS LINE HAD
				;ANYTHING ON IT
	PUSH	DE		;SAVE PLACE TO START FIXING LINKS
	JP	Z,FINI		;IF NOT DON'T INSERT
	POP	DE		;GET RID OF START OF LINK FIX
	LD	A,(CHNFLG)	;ONLY CHANGET FRETOP IF NOT CHAINING
	OR	A
	JP	NZ,LEVFRE	;LEAVE FRETOP ALONE
	LD	HL,(MEMSIZ)	;DELETE ALL STRINGS
	LD	(FRETOP),HL	;SO REASON DOESNT USE THEM
LEVFRE	LD	HL,(VARTAB)	;CURRENT END
	EX	(SP),HL		;[H,L]=CHARACTER COUNT. VARTAB
				;ONTO THE STACK
	POP	BC		;[B,C]=OLD VARTAB
	PUSH	HL		;Save count of chars to move
	ADD	HL,BC
	PUSH	HL		;SAVE NEW VARTAB
	CALL	BLTU
	POP	HL		;POP OFF VARTAB
	LD	(VARTAB),HL	;UPDATE VARTAB
	EX	DE,HL
	LD	(HL),H		;FOOL CHEAD WITH NON-ZERO LINK
	POP	BC		;Restore count of chars to move
	POP	DE		;GET LINE # OFF STACK
	PUSH	HL		;SAVE START OF PLACE TO FIX LINKS
	INC	HL		;SO IT DOESN'T THINK
				;THIS LINK IS THE
				;END OF THE PROGRAM
	INC	HL
	LD	(HL),E		;PUT DOWN LINE #
	INC	HL
	LD	(HL),D
	INC	HL
	LD	DE,KBUF		;MOVE LINE FRM KBUF TO PROGRAM AREA
	DEC	BC		;FIX UP COUNT OF CHARS TO MOVE
	DEC	BC		;(DONT INCLUDE LINE # & LINK)
	DEC	BC		;
	DEC	BC
;	NOW TRANSFERING LINE IN FROM BUF
MLOOPR	LD	A,(DE)		;NOW TRANSFERING LINE
	LD	(HL),A		;IN FROM BUF
	INC	HL
	INC	DE
	DEC	BC		;DECREMENT CHAR COUNT BY 1
	LD	A,C		;TEST FOR COUNT EXHAUSTED
	OR	B		;BY SEEING IF [B,C]=0
	JP	NZ,MLOOPR
FINI	POP	DE		;GET START OF LINK FIXING AREA
	CALL	CHEAD		;FIX LINKS
	LD	HL,DIRTMP	;DON'T ALLOW ZERO TO BE CLOSED
	LD	(HL),00H	;NOT SEQUENTIAL OUTPUT
	LD	(FDBTAB),HL
	LD	HL,(PTRFIL)	;GET FILE POINTER, COULD BE ZERO
	LD	(TEMP2),HL	;SAVE IT
	CALL	RUNC		;DO CLEAR & SET UP STACK
	LD	HL,(FILPT1)	;RESET [FILPTR]
	LD	(FDBTAB),HL
	LD	HL,(TEMP2)	;RESET [PTRFIL]
	LD	(PTRFIL),HL
	JP	MAIN		;GO TO MAIN CODE


;	Update interpreter pointers
LINKER	LD	HL,(TXTTAB)
	EX	DE,HL
;
;	CHEAD goes through program storage and fixes
;	up all the links. The end of each
;	line is found by searching for the zero at the end.
;	The double zero link is used to detect the end of the program.
;
CHEAD	LD	H,D		;[H,L]=[D,E]
	LD	L,E
	LD	A,(HL)		;SEE IF END OF CHAIN
	INC	HL		;BUMP POINTER
	OR	(HL)		;2ND BYTE
	RET	Z		;DONE
	INC	HL		;FIX H TO START OF TEXT
	INC	HL
CZLOOP	INC	HL		;BUMP POINTER
	LD	A,(HL)		;GET BYTE
CZLOO2	OR	A		;SET CC'S
	JP	Z,CZLIN		;END OF LINE, DONE.
	CP	DBLCON+1	;EMBEDDED CONSTANT?
	JP	NC,CZLOOP	;NO, GET NEXT
	CP	0BH		;IS IT LINEFEED OR BELOW?
	JP	C,CZLOOP	;THEN SKIP PAST
	CALL	CHRGT2		;GET CONSTANT
	CALL	CHRGTR		;GET OVER IT
	JP	CZLOO2		;GO BACK FOR MORE
CZLIN	INC	HL		;MAKE [H,L] POINT AFTER TEXT
	EX	DE,HL		;SWITCH TEMP
	LD	(HL),E		;STORE FIXUP
	INC	HL
	LD	(HL),D
	JP	CHEAD		;KEEP CHAINING TIL DONE

;-----------------------------------------------------------------------------
;	SCNLIN, FNDLIN - SCAN LINE RANGE AND FIND LINE # IN PROGRAM
; ## GWMAIN.ASM:835 ##
;
;	SCNLIN scans a line range of
;	the form #-# or # or #- or -# or blank
;	and then finds the first line in the range
;
SCNLIN	LD	DE,0000H	;ASSUME START LIST AT ZERO
	PUSH	DE		;SAVE INITIAL ASSUMPTION
	JP	Z,ALLLST	;IF FINISHED, LIST IT ALL
	POP	DE		;WE ARE GOING TO GRAB A #
	CALL	LINSPC		;GET A LINE #. IF NONE, RETURNS ZERO
	PUSH	DE		;SAVE FIRST
	JP	Z,SNGLIN	;IF ONLY # THEN DONE.
	CALL	SYNCHR
	DB	MINUTK		;MUST BE A DASH.
ALLLST	LD	DE,0FFFAH	;ASSUME MAX END OF RANGE
	CALL	NZ,LINSPC	;GET THE END OF RANGE
	JP	NZ,SNERR	;MUST BE TERMINATOR
SNGLIN	EX	DE,HL		;[H,L] = FINAL
	POP	DE		;GET INITIAL IN [D,E]
FNDLN1	EX	(SP),HL		;PUT MAX ON STACK, RETURN ADDR TO [H,L]
	PUSH	HL		;SAVE RETURN ADDRESS BACK
				;FALL INTO FNDLIN
;
;	FNDLIN searches the program text for the line
;	whose line # is passed in [D,E]. [D,E] is preserved.
;	There are three possible returns:
;
;		1) Zero flag set. Carry not set. Line not found.
;		   No line in program greater than one sought.
;		   [B,C] points to two zero bytes at end of program.
;		   [H,L]=[B,C]
;
;		2) Zero, Carry set.
;		   [B,C] points to the link field in the line
;		   which is the line searched for.
;		   [H,L] points to the link field in the next line.
;
;		3) Non-Zero, Carry not set.
;		   Line not found, [B,C] points to line in program
;		   greater than one searched for.
;		   [H,L] points to the link field in the next line.
;
FNDLIN	LD	HL,(TXTTAB)	;GET POINTER TO START OF TEXT
LOOP	LD	B,H		;IF EXITING BECAUSE OF END OF PROGRAM,
	LD	C,L		;SET [B,C] TO POINT TO DOUBLE ZEROES.
	LD	A,(HL)		;GET WORD POINTER TO
	INC	HL		;BUMP  POINTER
	OR	(HL)		;GET 2ND BYTE
	DEC	HL		;GO BACK
	RET	Z		;IF ZERO THEN DONE
	INC	HL		;SKIP PAST AND GET THE LINE #
	INC	HL
	LD	A,(HL)		;INTO [H,,L] FOR COMPARISON WITH
	INC	HL		;THE LINE # BEING SEARCHED FOR
	LD	H,(HL)		;WHICH IS IN [D,E]
	LD	L,A
	CALL	COMPAR		;SEE IF IT MATCHES OR IF WE'VE GONE TOO FAR
	LD	H,B		;MAKE [H,L] POINT TO THE START OF THE
	LD	L,C		;LINE BEYOND THIS ONE, BY PICKING
	LD	A,(HL)		;UP THE LINK THAT [B,C] POINTS AT
	INC	HL
	LD	H,(HL)
	LD	L,A
	CCF			;TURN CARRY ON
	RET	Z		;EQUAL RETURN
	CCF			;MAKE CARRY ZERO
	RET	NC		;NO MATCH RETURN (GREATER)
	JP	LOOP		;KEEP LOOPING

;
;	All "reserved" words are translated into single
;	bytes with the MSB on. This saves space and time
;	by allowing for table dispatch during execution.
;	Therefore all statements appear together in the
;	reserved word list in the same
;	order they appear in STMDSP.
;
;	Numeric constants are also converted to their internal
;	binary representation to improve execution speed
;	Line numbers are also preceeded by a special token
;	so that line numbers can be converted to pointers at execution
;	time.
CRUNCH	XOR	A		;SAY EXPECTING FLOATING NUMBERS
	LD	(DONUM),A	;SET FLAG ACORDINGLY
	LD	(DORES),A	;ALLOW CRUNCHING
	LD	BC,KBFLEN-3	;GET LENGTH OF KRUNCH BUFFER
				;MINUS THREE BECAUSE OF ZEROS AT END
	LD	DE,KBUF		;SETUP DESTINATION POINTER
KLOOP	LD	A,(HL)		;GET CHARACTER FROM BUF
	OR	A		;END OF LINE?
	JP	NZ,NCRDON	;NO, CONTINUE
CRDONE	LD	HL,KBFLEN+2	;GET OFFSET
	LD	A,L		;GET COUNT TO SUBTRACT FROM
	SUB	C		;SUBTRACT
	LD	C,A
	LD	A,H
	SBC	A,B
	LD	B,A		;BC:=# OF CHARS CRUNCHED
	LD	HL,KBUF1	;GET POINTER TO CHAR BEFORE KBUF
				;AS "GONE" DOES A CHRGET
	XOR	A		;GET A ZERO
	LD	(DE),A		;NEED THREE 0'S ON THE END
	INC	DE		;ONE FOR END-OF-LINE
	LD	(DE),A		;AND 2 FOR A ZERO LINK
	INC	DE		;SINCE IF THIS IS A DIRECT STATEMENT
	LD	(DE),A		;ITS END MUST LOOK LIKE THE END OF A PROGRAM
	RET			;END OF CRUNCHING

NCRDON	CP	'"'		;QUOTE SIGN?
	JP	Z,STRNG		;YES, GO TO SPECIAL STRING HANDLING
	CP	' '		;SPACE?
	JP	Z,STUFFH	;JUST STUFF AWAY
	LD	A,(DORES)	;IN DATA STATEMENT AND NO CRUNCH?
	OR	A
	LD	A,(HL)		;GET THE CHARACTER AGAIN
	JP	Z,NTDATA	;IF NO CRUNCHING JUST STORE
				;THE CHARACTER
STUFFH	INC	HL		;ENTRY TO BUMP [H,L]
	PUSH	AF		;SAVE CHAR AS KRNSAV CLOBBERS
	CALL	KRNSAV		;SAVE CHAR IN KRUNCH BUFFER
	POP	AF		;RESTORE CHAR
	SUB	':'		;SEE IF IT IS A COLON
	JP	Z,COLIS		;IF SO ALLOW CRUNCHING AGAIN
	CP	DATATK-':'
	JP	NZ,NODATT	;SEE IF IT IS A DATA TOKEN
	LD	A,01H		;SET LINE NUMBER ALLOWED FLAG
				;KLUDGE AS HAS TO BE NON-ZERO.
COLIS	LD	(DORES),A	;SETUP FLAG
	LD	(DONUM),A	;SET NUMBER ALLOWED FLAG
NODATT	SUB	REMTK-':'
	JP	NZ,KLOOP	;KEEP LOOPING
	PUSH	AF		;SAVE TERMINATOR ON STACK
STR1	LD	A,(HL)		;GET A CHAR
	OR	A		;SET CONDITION CODES
	EX	(SP),HL		;GET AL BACK WITHOUT AFFECTING PSW
	LD	A,H
	POP	HL
	JP	Z,CRDONE	;IF END OF LINE THEN DONE
	CP	(HL)		;COMPARE CHAR WITH THIS TERMINATOR
	JP	Z,STUFFH	;IF YES, DONE WITH STRING
STRNG	PUSH	AF		;SAVE TERMINATOR
	LD	A,(HL)		;GET BACK LINE CHAR
STRNG2	INC	HL		;INCREMENT TEXT POINTER
	CALL	KRNSAV		;SAVE CHAR IN KRUNCH BUFFER
	JP	STR1		;KEEP LOOPING

;	Now check hghbit chars, ? for PRINT
NTDATA	CP	'?'		;A QMARK?
	LD	A,PRINTTK
	PUSH	DE		;SAVE STORE POINTER
	PUSH	BC		;SAVE CHAR COUNT
	JP	Z,NOTFN2	;THEN USE A "PRINT" TOKEN
				;***5.11 DONT ALLOW FOLLOWING LINE #***
	LD	DE,SPCTAB	;ASSUME WE'LL SEARCH SPECIAL CHAR TABLE
	CALL	MAKUPL		;TRANSLATE THIS CHAR TO UPPER CASE
	CALL	ISLET2		;LETTER?
	JP	C,TSTNUM	;NOT A LETTER, TEST FOR NUMBER
	PUSH	HL		;SAVE TEXT POINTER
;	ANSI SAYS YOU CAN USE "GO TO" AND "GO SUB" WITH ANY NUMBER OF SPACES
	LD	DE,GOSTEX	;CHECK FOR "GO "
	CALL	CHKRES		;THAT'S IT?
	JP	NZ,NOTGOS	;NOPE
	CALL	CHRGTR		;SKIP ANY NUMBER OF SPACES
	LD	DE,TOTEX	;IS IT TO?
	CALL	CHKRES		;CHECK
	LD	A,GOTOTK	;ASSUME SO
	JP	Z,GPUTRS	;USE IT
	LD	DE,SUBTEX	;"GO SUB"
	CALL	CHKRES
	JP	NZ,NOTGOS	;NO
GPUTRS	LD	A,GOSUBTK
	POP	BC		;POP OFF THE OLD TEXT POINTER
	JP	NOTFN2		;STORE THE RESERVED WORD

;	Check memory pointed to by text pointer with
;	a specified partial resrword
CHKRES	LD	A,(DE)		;FETCH FROM CODE SEGMENT
	OR	A		;END OF MATCHED PARTIAL RESWORD?
	RET	Z		;YES, DONE
	LD	C,A		;SAVE CHAR
	CALL	MAKUPL		;GET CHAR FROM TEXT
	CP	C		;CHARS THE SAME?
	RET	NZ		;DONE
	INC	HL		;BUMP POINTER INTO TEXT
	INC	DE		;AND RESWORD TEXT
	JP	CHKRES		;LOOP TILL DONE

GOSTEX	DB	'GO ',00H
TOTEX	DB	'TO',00H
SUBTEX	DB	'UB',00H

NOTGOS	POP	HL
	CALL	MAKUPL		;GET BACK THE CHARACTER
	PUSH	HL		;RESAVE THE TEXT POINTER
;	Now search alpha character tables
;	Take the first character and use it as an index into the
;	twenty-six tables that contain the reserved words for each
;	letter
;
	LD	HL,ALPTAB	;GET POINTER TO ALPHA DISPATCH TABLE
	SUB	'A'		;SUBTRACT ALPHA OFFSET
	ADD	A,A		;MULTIPLY BY TWO
	LD	C,A		;SAVE OFFSET IN [C] FOR DAD.
	LD	B,00H		;MAKE HIGH PART OF OFFSET ZERO
	ADD	HL,BC		;ADD TO TABLE ADDRESS
	LD	E,(HL)		;GET POINTER IN [D,E]
	INC	HL
	LD	D,(HL)
	POP	HL		;GET BACK SOURCE POINTER
	INC	HL		;POINT TO CHAR AFTER FIRST ALPHA
TRYAGA	PUSH	HL		;SAVE TXTPTR TO START OF SEARCH AREA
LOPPSI	CALL	MAKUPL		;TRANSLATE THIS CHAR TO UPPER CASE
	LD	C,A		;SAVE CHAR IN [C]
	LD	A,(DE)		;FETCH FROM CODE SEGMENT
	AND	7FH		;GET RID OF HIGH BIT
	JP	Z,NOTRFN	;IF=0 THEN END OF THIS CHARS RESLT
	INC	HL		;BUMP SOURCE POINTER
	CP	C		;COMPARE TO CHAR FROM SOURCE LINE
	JP	NZ,LOPSKP	;IF NO MATCH, SEARCH FOR NEXT RESWRD
	LD	A,(DE)		;FETCH FROM CODE SEGMENT
	INC	DE		;BUMP RESLST POINTER
	OR	A		;SET CC'S
	JP	P,LOPPSI	;SEE IF REST OF CHARS MATCH
	LD	A,C		;GET LAST CHAR OF RESWRD
	CP	28H		;IF TAB( OR SPC(, SPACE NEED NOT FOLLOW
	JP	Z,ISRESW	;IS A RESWORD
	LD	A,(DE)		;FETCH FROM CODE SEGMENT
	CP	FNTK		;FUNCTION?
	JP	Z,ISRESW	;THEN NO SPACE NEED AFTERWARD
	CP	USRTK		;OR USR DEFINITION?
	JP	Z,ISRESW
	CALL	MAKUPL		;GET NEXT CHAR IN LINE (MC 6/22/80)
	CP	'.'		;IS IT A DOT
	JP	Z,ISVARS	;YES
	CALL	TSTANM		;IS IT A LETTER IMMEDIATELY FOLLOWING RESWRD
ISVARS	LD	A,00H		;SET DONUM TO -1
	JP	NC,NOTRFN	;IF ALPHA, CANT BE RESERVED WORD
ISRESW	POP	AF		;GET RID OF SAVED [H,L]
	LD	A,(DE)		;FETCH FROM CODE SEGMENT
	OR	A		;SET CC'S
	JP	M,NOTFNT	;IF MINUS, WASNT FUNCTION TOKEN
	POP	BC		;GET CHAR COUNT OFF STACK
	POP	DE		;GET DEPOSIT POINTER OFF STACK
	OR	80H		;MAKE HIGH ORDER BIT ONE
	PUSH	AF		;SAVE FN CHAR
	LD	A,0FFH		;GET BYTE WHICH PRECEEDS FNS
	CALL	KRNSAV		;SAVE IN KRUNCH BUFFER
	XOR	A		;MAKE A ZERO
	LD	(DONUM),A	;TO RESET DONUM (FLOATINGS ALLOWED)
	POP	AF		;GET FUNCTION TOKEN
	CALL	KRNSAV		;STORE IT
	JP	KLOOP		;KEEP KRUNCHING

LOPSKP	POP	HL		;RESTORE UNDEFILED TEXT POINTER
LOPSK2	LD	A,(DE)		;FETCH FROM CODE SEGMENT
	INC	DE		;BUMP RESLST POINTER
	OR	A		;SET CC'S
	JP	P,LOPSK2	;NOT END OF RESWRD, KEEP SKIPPING
	INC	DE		;POINT AFTER TOKEN
	JP	TRYAGA		;TRY ANOTHER RESWRD

;	Check to see if reserved word matches a list of
;	reserved words that have line numbers following them instead
;	of floating point numbers. If a match is found, set DONUM to
;	indicate that if a number occurs, it is crunched as a line number.
NOTFNT	DEC	HL		;FIX TEXT POINTER
NOTFN2	PUSH	AF		;SAVE CHAR TO BE SAVED IN KRUNCH BUFFER
	LD	DE,LINRES	;POINT TO RESERVED WORDS THAT HAVE LINE ARGS
	LD	C,A		;SAVE CHARACTER TO MATCH IN [C]
NOTFN3	LD	A,(DE)		;FETCH FROM CODE SEGMENT
	OR	A		;END OF LIST?
	JP	Z,NOTFN4	;YES, DONE
	INC	DE		;POINT TO NEXT RESWORD
	CP	C		;SAME AS ONE WERE LOOKING AT?
	JP	NZ,NOTFN3	;KEEP LOOKING
	JP	NOTRS2		;DOESNT HAVE LINE # ARG

;	List of commands (tokens) requiring program line numbers
LINRES
	DB	RESTORETK	;RESTORE CAN HAVE FOLLOWING LINE NUMBER
	DB	AUTOTK		;AUTO COMMAND
	DB	RENUMTK		;RENUMBER?
	DB	DELETETK	;DELETE?
	DB	EDITTK		;EDIT?
	DB	RESUMETK	;RESUME?
	DB	ERLTK		;ERROR LINE
				;SO THAT IF "ERL=...THEN"
				;WILL RESEQUENCE PROPERLY
				;THIS CAN MAKE STATEMENTS LIKE
				;"PRINT ERL,1E20" DO STRANGE THINGS
	DB	ELSETK		;IF ELSE CRUNCH FOLLOWING LINE #
	DB	RUNTK		;RUN?
	DB	LISTTK		;LIST?
	DB	LLISTTK		;LPT LIST?
	DB	GOTOTK		;IF GOTO, CRUNCH LINE #
	DB	THENTK		;CRUNCH LINE #'S AFTER 'THEN'
	DB	GOSUBTK		;IF GOSUB, CRUNCH LINE #'S
	DB	00H

NOTFN4	XOR	A		;GET A ZERO (EXPECT USUALLY NUMBERS)
	DB	0C2H
NOTRS2	LD	A,01H		;SAY LINE #'S ALLOWED.
NOTRS6	LD	(DONUM),A	;SAVE IN FLAG
	POP	AF		;RESTORE CHARACTER TO SAVE IN KRUNCH BUFFER
	POP	BC		;GET BACK THE CHARACTER COUNT
	POP	DE		;GET STUFF POINTER BACK
	CP	ELSETK		;HAVE TO PUT A HIDDEN
				;COLON IN FRONT OF "ELSE"S
	PUSH	AF		;SAVE CURRENT CHAR ($ELSE)
	CALL	Z,KRNSVC	;SAVE ":" IN CRUNCH BUFFER
	POP	AF		;GET BACK TOKEN
	CP	WHILETK		;SEE IF WHILE TO IN A PLUS TO AVOID
	JP	NZ,CKSNGQ	;Brif not
	CALL	KRNSAV		;Emit WHILE Token, then Plus (+)
	LD	A,PLUSTK	;PLUS SIGN IS OK AND AVOIDS CONSTANT
CKSNGQ	CP	SNGQTK		;SINGLE QUOATATION MARK?
	JP	NZ,NTSNGT
	PUSH	AF		;SAVE SNGQTK
	CALL	KRNSVC		;SAVE ":" IN CRUNCH BUFFER
	LD	A,REMTK		;STORE ":$REM" IN FRONT FOR EXECUTION
	CALL	KRNSAV		;SAVE IT
	POP	AF		;GET SNGQTK BACK
	PUSH	AF		;SAVE IT
	JP	STRNG2		;STUFF THE REST OF THE LINE WITHOUT CRUNCHING

TSTNUM	LD	A,(HL)		;GET CHAR
	CP	'.'		;TEST FOR START OF FLOATING #
	JP	Z,NUMTRY	;TRY INPUTTING IT AS CONSTANT
	CP	':'		;IS IT A DIGIT?
	JP	NC,SRCSPC	;NO, TRY OTHER THINGS
	CP	'0'		;TRY LOWER END
	JP	C,SRCSPC	;NO TRY OTHER POSSIBILITIES
NUMTRY	LD	A,(DONUM)	;TEST FOR NUMBERS ALLOWED
	OR	A		;SET CC'S
	LD	A,(HL)		;GET CHAR IF GOING TO STUFFH
	POP	BC		;RESTORE CHAR COUNT
	POP	DE		;RESTORE DEP. POINTER
	JP	M,STUFFH	;NO, JUST STUFF IT (!)
	JP	Z,FLTGET	;IF DONUM=0 THEN FLOATING #'S ALLOWED
	CP	'.'		;IS IT DOT?
	JP	Z,STUFFH	;YES, STUFF IT FOR HEAVENS SAKE! (EDIT .)
	LD	A,LINCON	;GET LINE # TOKEN
	CALL	KRNSAV		;SAVE IT
	PUSH	DE		;SAVE DEPOSIT POINTER
	CALL	LINGET		;GET THE LINE #.
	CALL	BAKSP		;BACK UP POINTER TO AFTER LAST DIGIT
SAVINT	EX	(SP),HL		;EXCHANGE CURRENT [H,L] WITH SAVED [D,E]
	EX	DE,HL		;GET SAVED [D,E] IN [D,E]
SAVI	LD	A,L		;GET LOW BYTE OF VALUE RETURNED BY LINGET
	CALL	KRNSAV		;SAVE THE LOW BYTE OF LINE #
	LD	A,H		;GET HIGH BYTE
POPSTF	POP	HL		;RESTORE [H,L]
	CALL	KRNSAV		;SAVE IT TOO
	JP	KLOOP		;EAT SOME MORE

FLTGET	PUSH	DE		;SAVE DEPOSIT POINTER
	PUSH	BC		;SAVE CHAR COUNT
	LD	A,(HL)		;FIN ASSUMES CHAR IN [A]
	CALL	FIN		;READ THE #
	CALL	BAKSP		;BACK UP POINTER TO AFTER LAST DIGIT
	POP	BC		;RESTORE CHAR COUNT
	POP	DE		;RESTORE DEPOSIT POINTER
	PUSH	HL		;SAVE TEXT POINTER
	LD	A,(VALTYP)	;GET VALUE TYPE
	CP	02H		;INTEGER?
	JP	NZ,NTINTG	;NO
	LD	HL,(FACLO)	;GET IT
	LD	A,H		;GET HIGH PART
	OR	A		;IS IT ZERO?
	LD	A,02H		;RESTORE INT VALTYP
	JP	NZ,NTINTG	;THEN ISNT SINGLE BYTE INT
	LD	A,L		;GET LOW BYTE
	LD	H,L		;GET LOW BYTE IN HIGH BYTE TO STORE
	LD	L,0FH		;GET CONSTANT FOR 1 BYTE INTS
	CP	0AH		;IS IT TOO BIG FOR A SINGLE BYTE CONSTANT?
	JP	NC,SAVI		;TOO BIG, USE SINGLE BYTE INT
	ADD	A,ONECON	;MAKE SINGLE BYTE CONSTANT
	JP	POPSTF		;POP H & STUFF AWAY CHAR

NTINTG	PUSH	AF		;SAVE FOR LATER
	RRCA			;DIVIDE BY TWO
	ADD	A,1BH		;ADD OFFSET TO GET TOKEN
	CALL	KRNSAV		;SAVE THE TOKEN
	LD	HL,FACLO	;GET START POINTER
	CALL	GETYPR		;SET CC'S ON VALTYPE
	JP	C,NTDBL		;IF NOT DOUBLE, START MOVING AT FACLO
	LD	HL,DFACLO	;DOUBLE, START MOVING AT DFACLO
NTDBL	POP	AF
MOVCON	PUSH	AF		;SAVE BYTE MOVE COUNT
	LD	A,(HL)		;GET A BYTE
	CALL	KRNSAV		;SAVE IT IN KRUNCH BUFFER
	POP	AF		;GET BACK COUNT
	INC	HL		;BUMP POINTER INTO FAC
	DEC	A		;MOVE IT DOWN
	JP	NZ,MOVCON	;KEEP MOVING IT
	POP	HL		;GET BACK SAVED TEXT POINTER
	JP	KLOOP		;KEEP LOOPING

SRCSPC	LD	DE,ZTAB		;GET POINTER TO SPECIAL CHARACTER TABLE
SRCSP2	INC	DE		;MOVE POINTER AHEAD
	LD	A,(DE)		;FETCH FROM CODE SEGMENT
	AND	7FH		;MASK OFF HIGH BIT
	JP	Z,NOTRS5	;IF END OF TABLE, STUFF AWAY, DONT CHANGE DONUM
	INC	DE		;BUMP POINTER
	CP	(HL)		;IS THIS SPECIAL CHAR SAME AS CURRENT TEXT CHAR?
	LD	A,(DE)		;FETCH FROM CODE SEGMENT
	JP	NZ,SRCSP2	;IF NO MATCH, KEEP LOOKING
	JP	NOTRS1		;FOUND, SAVE AWAY AND SET DONUM=1.

NTSNGT	CP	'&'		;OCTAL CONSTANT?
	JP	NZ,STUFFH	;JUST STUFF IT AWAY
	PUSH	HL		;SAVE TEXT POINTER
	CALL	CHRGTR		;GET NEXT CHAR
	POP	HL		;RESTORE TEXT POINTER
	CALL	MAKUPS		;MAKE CHAR UPPER CASE
	CP	'H'		;HEX CONSTANT?
	LD	A,OCTCON	;ASSUME OCTAL CONSTANT
	JP	NZ,WUZOCT	;YES, IT WAS
	LD	A,HEXCON	;NO, WAS HEX
WUZOCT	CALL	KRNSAV		;SAVE IT
	PUSH	DE		;SAVE CURRENT DEPOSIT POINTER
	PUSH	BC		;SAVE COUNT
	CALL	OCTCNS		;GET THE VALUE
	POP	BC		;RESTORE [B,C]
	JP	SAVINT		;SAVE THE INTEGER IN THE KRUNCH BUFFER

KRNSVC	LD	A,':'		;GET COLON
KRNSAV	LD	(DE),A		;SAVE BYTE IN KRUNCH BUFFER
	INC	DE		;BUMP POINTER
	DEC	BC		;DECREMENT COUNT OF BYTES LEFT IN BUFFER
	LD	A,C		;TEST IF IT WENT TO ZERO
	OR	B		;BY SEEING IF DOUBLE BYTE ZERO.
	RET	NZ		;ALL DONE IF STILL SPACE LEFT
LBOERR	LD	E,ERRLBO	;GET ERROR CODE
	JP	ERROR		;JUMP TO ERROR ROUTINE

NOTRFN	POP	HL		;GET BACK POINTER TO ORIGINAL CHAR
	DEC	HL		;NOW POINT TO FIRST ALPHA CHAR
	DEC	A		;SET A TO MINUS ONE
	LD	(DONUM),A	;FLAG WERE IN VARIABLE NAME
	POP	BC		;GET BACK CHAR COUNT
	POP	DE		;GET BACK DEPOSIT POINTER
	CALL	MAKUPL		;GET CHAR FROM LINE, MAKE UPPER CASE
KRNVAR	CALL	KRNSAV		;SAVE CHAR
	INC	HL		;INCREMENT SOURCE POINTER
	CALL	MAKUPL		;MAKE UPPER CASE (?)
	CALL	ISLET2		;IS IT A LETTER?
	JP	NC,KRNVAR	;YES, EAT
	CP	':'		;DIGIT?
	JP	NC,JKLOOP	;NO, TOO LARGE
	CP	'0'
	JP	NC,KRNVAR	;YES, EAT
	CP	'.'		;IS IT DOT
	JP	Z,KRNVAR	;YES, DOTS OK IN VAR NAMES
JKLOOP	JP	KLOOP		;DONE LOOKING AT VARIABLE NAME

NOTRS5	LD	A,(HL)		;GET CHAR FROM LINE
	CP	' '		;SPACE OR HIGHER ?
	JP	NC,NOTRS1	;YES = SAVE IT
	CP	09H		;TAB ?
	JP	Z,NOTRS1	;YES = THAT'S OK
	CP	0AH		;ALSO ALLOW...
	JP	Z,NOTRS1	;...LINE FEEDS
	LD	A,' '		;FORCE REST TO SPACES
NOTRS1	PUSH	AF		;SAVE THIS CHAR
	LD	A,(DONUM)	;GET NUMBER OK FLAG
	INC	A		;SEE IF IN A VARIABLE NAME.
	JP	Z,NOTRS11	;IF SO & SPECIAL CHAR SEEN, RESET DONUM
	DEC	A		;OTHERWISE LEAVE DONUM UNCHANGED.
NOTRS11	JP	NOTRS6

; 	Routine to back up pointer after # eaten
BAKSP	DEC	HL		;POINT TO PREVIOUS CHAR
	LD	A,(HL)		;GET THE CHAR
	CP	' '		;A SPACE?
	JP	Z,BAKSP		;YES, KEEP BACKING UP
	CP	09H		;TAB?
	JP	Z,BAKSP		;YES, BACK UP
	CP	0AH		;LF?
	JP	Z,BAKSP
	INC	HL		;POINT TO CHAR AFTER LAST NON-SPACE
	RET			;ALL DONE.


;-----------------------------------------------------------------------------
;	"FOR" STATEMENT
; ## GWMAIN.ASM:1423 ##
;
;	NOTE:
;
;	A FOR entry on the stack has the following format:
;
;	Low address
;		Token (FORTK in high byte) 			1 byte
;		A pointer to the loop variable 			2 bytes
;		A pointer to the matching "NEXT"		2 bytes
;		A byte reflecting the sign of the increment	1 byte
;		A byte -1 for INT and +1 for SNG "FOR"		1 byte
;		The STEP 					4 bytes
;		The upper value 				4 bytes
;		The line # of the "FOR" statement 		2 bytes
;		A text pointer into the "FOR" statement 	2 bytes
;	High address
;
;	Total 19 bytes
;
FOR	LD	A,64H		; =100
	LD	(SUBFLG),A	;DONT RECOGNIZE SUBSCRIPTED VARIABLES
	CALL	PTRGET		;GET POINTER TO LOOP VARIABLE
	CALL	SYNCHR
	DB	EQULTK		;SKIP OVER ASSIGNMENT "="
	PUSH	DE		;SAVE THE VARIABLE POINTER ON STACK
	EX	DE,HL		;AND IN TEMP
	LD	(TEMP),HL	; FOR USE LATER ON
	EX	DE,HL
	LD	A,(VALTYP)	;REMEMBER THE LOOP VARIABLE TYPE
	PUSH	AF
	CALL	FRMEVL		;GET THE START VALUE
	POP	AF		;REGET THE LOOP TYPE
	PUSH	HL		;SAVE THE TEXT POINTER
	CALL	DOCNVF		;FORCE CONVERSION TO LOOP TYPE
	LD	HL,FVALSV	;PLACE TO SAVE THE VALUE
	CALL	$MOVMF		;STORE FOR USE IN "NEXT"
	POP	HL		;GET BACK THE TEXT POINTER
	POP	DE		;GET BACK THE VARIABLE POINTER
	POP	BC		;GET RID OF THE NEWSTT RETURN
	PUSH	HL		;SAVE THE TEXT POINTER
	CALL	DATA		;SET [H,L]=END OF STATEMENT
	LD	(ENDFOR),HL	;SAVE FOR COMPARISON
	LD	HL,0002H	;SET UP POINTER INTO STACK
	ADD	HL,SP
LPFORM	CALL	LOOPER		;MUST HAVE VARIABLE POINTER IN [D,E]
	JP	NZ,NOTOL	;IF NO MATCHING ENTRY, DON'T
				;ELIMINATE ANYTHING
	ADD	HL,BC		;IN THE CASE OF "FOR"
				;WE ELIMINATE THE MATCHING ENTRY
				;AS WELL AS EVERYTHING AFTER IT
	PUSH	DE		;SAVE THE VARIABLE POINTER
	DEC	HL		;SEE IF END TEXT POINTER OF MATCHING ENTRY
	LD	D,(HL)		;MATCHES THE FOR WE ARE HANDLING
	DEC	HL		;PICK UP THE END OF THE "FOR" TEXT POINTER
	LD	E,(HL)		;FOR THE ENTRY ON THE STACK
	INC	HL		;WITHOUT CHANGING [H,L]
	INC	HL
	PUSH	HL		;SAVE THE STACK POINTER FOR THE COMPARISON
	LD	HL,(ENDFOR)	;GET ENDING TEXT POINTER FOR THIS "FOR"
	CALL	COMPAR		;SEE IF THEY MATCH
	POP	HL		;GET BACK THE STACK POINTER
	POP	DE		;GET BACK THE VARIABLE POINTER
	JP	NZ,LPFORM	;KEEP SEARCHING IF NO MATCH
	POP	DE		;GET BACK THE TEXT POINTER
	LD	SP,HL		;DO THE ELIMINATION
	LD	(SAVSTK),HL	;UPDATE SAVED STACK
				;SINCE A MATCHING ENTRY WAS FOUND
	DB	0EH		;SKIP MVI C,(POP D)
NOTOL	POP	DE
	EX	DE,HL		;[H,L]=TEXT POINTER
	LD	C,08H		;MAKE SURE 16 BYTES ARE AVAILABLE
	CALL	GETSTK		;OFF OF THE STACK
				;/BEM: SHOULD CHECK FOR 19 BYTES !
	PUSH	HL		;REALLY SAVE THE TEXT POINTER
	LD	HL,(ENDFOR)	;PICK UP POINTER AT END OF "FOR"
				;JUST BEYOND THE TERMINATOR
	EX	(SP),HL		;PUT [H,L] POINTER TO TERMINATOR ON THE STACK
				;AND RESTORE [H,L] AS TEXT POINTER AT
				;VARIABLE NAME
	PUSH	HL		;PUSH THE TEXT POINTER ONTO THE STACK
	LD	HL,(CURLIN)	;[H,L] GET THE CURRENT LINE #
	EX	(SP),HL		;NOW THE CURRENT LINE # IS ON THE STACK AND
				;[H,L] IS THE TEXT POINTER
	CALL	SYNCHR
	DB	TOTK		;"TO" IS NECESSARY
	CALL	GETYPR		;SEE WHAT TYPE THIS VALUE HAS
	JP	Z,TMERR		;GIVE STRINGS A "TYPE MISMATCH"
	JP	NC,TMERR	;AS WELL AS DOUBLE-PRECISION
	PUSH	AF		;SAVE THE INTEGER/FLOATING FLAG
	CALL	FRMEVL		;EVALUATE THE TARGET VALUE FORMULA
	POP	AF		;POP OFF THE FLAG
	PUSH	HL		;SAVE THE TEXT POINTER
	JP	P,SNGFOR	;POSITIVE MEANS SINGLE PRECISION "FOR"-LOOP
	CALL	CINT		;COERCE THE FINAL VALUE
	EX	(SP),HL		;SAVE IT ON THE STACK AND REGET THE
				;TEXT POINTER
	LD	DE,0001H	;DEFAULT THE STEP TO BE 1
	LD	A,(HL)		;SEE WHAT CHARACTER IS NEXT
	CP	STEPTK		;IS THERE A "STEP" CLAUSE?
	CALL	Z,GETINT	;IF SO, READ THE STEP INTO [D,E]
	PUSH	DE		;PUT THE STEP ONTO THE STACK
	PUSH	HL		;SAVE THE TEXT POINTER
	EX	DE,HL		;STEP INTO [H,L]
	CALL	ISIGN		;THE SIGN OF THE STEP INTO [A]
	JP	STPSGN		;FINISH UP THE ENTRY
				;BY PUTTING THE SIGN OF THE STEP
				;AND THE DUMMY ENTRIES ON THE STACK

;	Single precision FOR index
SNGFOR	CALL	CSNG
	CALL	$MOVRF		;GET THE STUFF
	POP	HL		;REGAIN TEXT POINTER
	PUSH	BC		;OPPOSITE OF PUSHR
	PUSH	DE		;SAVE THE SIGN OF THE INCREMENT
	LD	BC,8100H
	LD	D,C
	LD	E,D		;GET 1.0 IN THE REGISTERS
	LD	A,(HL)		;GET TERMINATING CHARACTER
	CP	STEPTK		;DO WE HAVE "STEP" ?
	LD	A,01H		;SETUP DEFAULT SIGN
	JP	NZ,ONEON	;PUSH SOME CONSTANTS ON IF NOT
	CALL	FRMCHK		;DON'T NEED TO CHECK THE TYPE
	PUSH	HL
	CALL	CSNG
	CALL	$MOVRF		;SET UP THE REGISTERS
	CALL	SIGN		;GET THE SIGN OF THE INCREMENT
STPSGN	POP	HL		;POP OFF THE TEXT POINTER
ONEON	PUSH	BC		;PUT VALUE ON BACKWARDS
	PUSH	DE		;OPPOSITE OF PUSHR

;	If a step of zero is specified, change the sign of the step from
;	0 to 2.  This will ensure an infinite loop which is what ANSI
;	requires.  This works because the test NEXT uses for loop
;	termination is SGN(current value-target value)-SGN(step)=0, which
;	could never be true for SGN(step)=2.
;	Prior to installation of this code, a FOR loop whose initial and
;	target values were equal, and whose step was zero was not executed
;	even once since the above test was met when NEXT was called to
;	scan for the matching NEXT.
;
;	(seems unimplemented in MBASIC)
;	OR	A		;Is the sign of the step zero?
;	JR	NZ,NT0STP	;No, then the step is not zero.
;	LD	A,02H		;Yes, force an infinite loop by making
;				;the sign of step a value such that
;				;the loop termination test can never
;				;be met.
;NT0STP:
	LD	C,A		;[C]=SIGN OF STEP
	CALL	GETYPR		;MUST PUT ON INTEGER/SINGLE-PRECISION FLAG
				;MINUS IS SET FOR INTEGER CASE
	LD	B,A		;HIGH BYTE = INTEGER/SINGLE PRECISION FLAG
	PUSH	BC		;SAVE FLAG AND SIGN OF STEP BOTH
	DEC	HL		;MAKE SURE THE "FOR" ENDED PROPERLY
	CALL	CHRGTR
	JP	NZ,SNERR
	CALL	NXTSCN		;SCAN UNTIL THE MATCHING "NEXT" IS FOUND
	CALL	CHRGTR		;FETCH FIRST CHARACTER OF "NEXT"
	PUSH	HL		;MAKE THE NEXT TXTPTR PART OF THE ENTRY
	PUSH	HL
	LD	HL,(NXTLIN)	;GET THE LINE NUMBER OF NEXT
	LD	(CURLIN),HL	;MAKE IT THE CURRENT LINE
	LD	HL,(TEMP)	;GET THE POINTER TO THE VARIABLE BACK
	EX	(SP),HL		;PUT THE POINTER TO THE VARIABLE
				;ONTO THE STACK AND RESTORE THE TEXT POINTER
	LD	B,FORTK		;FINISH UP "FOR"
	PUSH	BC
	INC	SP
	PUSH	AF		;SAVE THE CHARACTER
	PUSH	AF		;MAKE A STACK ENTRY TO SUBSTITUTE FOR "NEWSTT"
	JP	NEXTS		;GO EXECUTE "NEXT" WITH NXTFLG ZERO

NXTCON	LD	B,FORTK		;PUT A 'FOR' TOKEN ONTO THE STACK
	PUSH	BC
	INC	SP
;
;-----------------------------------------------------------------------------
;	NEW STATEMENT FETCHER
; ## GWMAIN.ASM:1624 ##
;
;	Back here for NEW statement. Character pointed to by [H,L]
;	":" or End-Of-Line. The address of this location is
;	left on the stack when a statement is executed so
;	it can merely do a return when it is done.
NEWSTT	PUSH	HL
	CALL	0000H		;BIOS "GET CONSOLE STATUS"
VCONST3	EQU	$-2
	POP	HL		;RESTORE ALL REGISTERS
	OR	A		;SET CC'S - 0 FALSE - NO CHAR TYPED
	CALL	NZ,STALL	;SEE IF ITS CONTROL-C.
				;IF SO, CHECK FOR CONTRL-C
	LD	(SAVTXT),HL	;Save code address for break
				;USED BY CONTINUE AND INPUT AND
				; CLEAR AND PRINT USING
	EX	DE,HL		;SAVE TEXT POINTER
	LD	HL,0000H	;SAVE STACK POINTER
	ADD	HL,SP		;COPY TO [H,L]
	LD	(SAVSTK),HL	;SAVE IT TO REMEMBER HOW TO RESTART
				; THIS STATEMENT
	EX	DE,HL		;GET CURRENT TEXT POINTER BACK IN [H,L]
				; TO SAVE BYTES & SPEED
	LD	A,(HL)		;GET CURRENT CHARACTER
				; WHICH TERMINATED THE LAST STATEMENT
	CP	':'		;IS IT A COLON?
	JP	Z,GONE		;Yes - Execute Multi statement line
	OR	A		;SHOULD BE ZERO AT EOL
	JP	NZ,SNERR	;MUST BE A ZERO
	INC	HL
GONE4	LD	A,(HL)		;CHECK POINTER TO SEE IF
				;IT IS ZERO, IF SO WE ARE AT THE
				;END OF THE PROGRAM
	INC	HL
	OR	(HL)		;OR IN HIGH PART
	JP	Z,PRGEND	;FIX SYNTAX ERROR IN UNENDED ERROR ROUTINE
	INC	HL
	LD	E,(HL)
	INC	HL
	LD	D,(HL)		;GET LINE # IN [D,E]
	EX	DE,HL		;[H,L]=LINE #
	LD	(CURLIN),HL	;SETUP CURLIN WITH THE CURRENT LINE #
	LD	A,(TRCFLG)	;TRACING?
	OR	A		;NON-ZERO MEANS YES
	JP	Z,NOTTRC	;SKIP THIS PRINTING
;	If "TRACE" is ON, then print current line number between brackets
;	[0000] <<<-- print line number being executed
	PUSH	DE		;SAVE THE TEXT POINTER
	LD	A,'['		;FORMAT THE LINE NUMBER
	CALL	OUTDO		;OUTPUT IT
				;PUT LINE NUMBER TO PRINT IN [H,L]
	CALL	LINPRT		;PRINT THE LINE # IN [H,L]
	LD	A,']'		;SOME MORE FORMATING
	CALL	OUTDO
	POP	DE		;[D,E]=TEXT POINTER
NOTTRC	EX	DE,HL		;RESTORE THE TEXT POINTER
GONE	CALL	CHRGTR		;GET THE STATEMENT TYPE
	LD	DE,NEWSTT	;PUSH ON A RETURN ADDRESS OF NEWSTT
	PUSH	DE		;STATEMENT
	RET	Z		;IF A TERMINATOR TRY AGAIN
				;"IF" COMES HERE
GONE2	SUB	ENDTK		;"ON ... GOTO" AND "ON ... GOSUB" COME HERE
	JP	C,LET		;MUST BE A LET
	CP	RESETTK+1-ENDTK	;END to RESET ?
	JP	NC,ISMID$	;SEE IF LHS MID$ CASE
	RLCA			;TURN BYTE INTO OFFSET
	LD	C,A		;GET OFFSET INTO [BC]
	LD	B,00H
	EX	DE,HL
	LD	HL,STMDSP	;aka FNCTAB: STATEMENT DISPATCH TABLE
	ADD	HL,BC		;ADD ON OFFSET
	LD	C,(HL)		;PUSH THE ADDRESS TO GO TO ONTO
	INC	HL		;THE STACK
	LD	B,(HL)		;PUSHM SAVES BYTES BUT NOT SPEED
	PUSH	BC		;PUSH ADDRESS STATEMENT ON STACK
	EX	DE,HL		;RESTORE THE TEXT POINTER
;
;	NEWSTT falls into CHRGET. This fetches the first char after
;	the statement token and the CHRGET's "RET" dispatches to statement
;

;-----------------------------------------------------------------------------
;	CHRGET - THE NEXT CHARACTER SCAN ROUTINE
; ## GWMAIN.ASM:1714 ##
;
CHRGTR	INC	HL		;DUPLICATION OF CHRGET RST FOR SPEED
CHRGT2	LD	A,(HL)		;SEE CHRGET RST FOR EXPLANATION
	CP	'9'+1
	RET	NC		;NC if > "9"
;
;	CHRCON is the continuation of the CHRGET RST
;
;	In Extended, check for inline constant and if one
;	move it into the FAC & set VALTYP appropriately
;
CHRCON	CP	' '		;MUST SKIP SPACES
	JP	Z,CHRGTR	;GET ANOTHER CHARACTER
	JP	NC,NOTLFT	;SOME FUNNY THING
	OR	A		;NULL AT EOL?
	RET	Z		;YES, ALL DONE
	CP	OCTCON		;IS IT INLINE CONSTANT?
	JP	C,NOTCON	;NO, SHOULD BE TAB OR LF
	CP	CONCON		;ARE WE TRYING TO RE-SCAN A CONSTANT?
	JP	NZ,NTRSCC	;NO.
	LD	A,(CONSAV)	;GET THE SAVED CONSTANT TOKEN
	OR	A		;SET NON-ZERO, NON CARRY CC'S
	RET			;ALL DONE

NTRSCC	CP	CONCN2		;GOING TO SCAN PAST EMBEDDED CONSTANT?
	JP	Z,CONSCN	;YES SCAN AND GO ON
	PUSH	AF		;SAVE TOKEN TO RETURN
	INC	HL		;POINT TO NUMBER
	LD	(CONSAV),A	;SAVE CURRENT TOKEN
	SUB	INTCON		;IS IT LESS THAN INTEGER CONSTANT?
	JP	NC,MAKTKN	;NO, NOT LINE NUMBER CONSTANT
	SUB	ONECON-INTCON	;<ONECON-INTCON>&^O377
				;LESS THAN EMBEDDED 1 BYTER
	JP	NC,ONEI		;WAS ONE BYTER
	CP	IN2CON-ONECON	;IS IT TWO BYTER?
	JP	NZ,FRCINC	;NOPE, NORMAL INT
	LD	A,(HL)		;GET EMBEDED INT
	INC	HL		;POINT AFTER CONSTANT
ONEI	LD	(CONTXT),HL	;SAVE TEXT POINTER
	LD	H,00H		;GET UPPER BYTE OF ZERO
ONEI2	LD	L,A		;GET VALUE
	LD	(CONLO),HL	;SAVE CONSTANT VALUE
	LD	A,02H		;GET VALTYPE
	LD	(CONTYP),A	;SET IT UP IN SAVE PLACE
	LD	HL,NUMCON	;POINT TO NUMBER RE-SCANNER
	POP	AF		;GET BACK TOKEN
	OR	A		;MAKE SURE NUMBER FLAG RE-SET
	RET			;RETURN TO CALLER

FRCINC	LD	A,(HL)		;GET LOW BYTE OF CONSTANT
	INC	HL		;POINT PAST IT
	INC	HL		;TO NEXT THING
	LD	(CONTXT),HL	;SAVE POINTER PAST
	DEC	HL		;BACK TO HIGH BYTE
	LD	H,(HL)		;GET HIGH BYTE
	JP	ONEI2		;FINISH SCANNING

CONFAC	CALL	CONFC1		;SCAN FLOATING CONSTANT
CONSCN	LD	HL,(CONTXT)	;GET SAVED TEXT POINTER
	JP	CHRGT2		;AND SCAN THING AFTER CONSTANT

MAKTKN	INC	A		;CALCULATE VALTYPE
	RLCA			;*2 TO GET VALTYPE 0=2, 1=4, 3=8
	LD	(CONTYP),A	;CONTYPE NOW SETUP
	PUSH	DE		;SAVE SOME RGS
	PUSH	BC
	LD	DE,CONLO	;PLACE TO STORE SAVED CONSTANT
	EX	DE,HL		;GET TEXT POINTER IN [D,E]
	LD	B,A		;SETUP COUNTER IN [B]
	CALL	MOVE1		;MOVE DATA IN
	EX	DE,HL		;GET TEXT POINTER BACK
	POP	BC		;RESTORE [B,C]
	POP	DE
	LD	(CONTXT),HL	;SAVE THE GOOD TEXT POINTER
	POP	AF		;RESTORE TOKEN
	LD	HL,NUMCON	;GET POINTER TO FAKE TEXT
	OR	A		;CLEAR CARRY SO OTHERS DONT THINK ITS A NUMBER
				;AND SET NON-ZERO SO NOT TERMINATOR
	RET			;ALL DONE

NOTCON	CP	09H		;LINE FEED OR TAB?
	JP	NC,CHRGTR	;YES, EAT.
NOTLFT	CP	30H		;ALL CHARACTERS GREATER THAN
				;"9" HAVE RETURNED, SO SEE IF NUMERIC
	CCF			;MAKE NUMERICS HAVE CARRY ON
	INC	A		;SET ZERO IF [A]=0
	DEC	A
	RET

;	These fake tokens force CHRGET
;	to effectively re-scan the embedded constant
NUMCON	DB	CONCON     	;TOKEN RETURNED BY CHRGET AFTER CONSTANT SCANNED
	DB	CONCN2     	;TOKEN RETURNED SECOND TYPE CONSTANT IS SCANNED.

;	This routine moves the saved constant into the FAC
CONFC1	LD	A,(CONSAV)	;GET CONSTANT TOKEN
	CP	LINCON+1	;LINE# CONSTANT? (ERL=#)
	JP	NC,NTLINE	;NO
	CP	PTRCON		;LINE POINTER CONSTANT?
	JP	C,NTLINE	;NO
	LD	HL,(CONLO)	;GET VALUE
	JP	NZ,FLTLIN	;MUST BE LINE NUMBER, NOT POINTER
	INC	HL		;POINT TO LINE #
	INC	HL
	INC	HL
	LD	E,(HL)		;GET LINE # IN [D,E]
	INC	HL
	LD	D,(HL)		;GET HIGH PART
	EX	DE,HL		;VALUE TO [H,L]
FLTLIN	JP	MAKSNG		;FLOAT IT

;	Not line
NTLINE	LD	A,(CONTYP)	;GET SAVED CONSTANT VALTYP
	LD	(VALTYP),A	;SAVE IN REAL VALTYP
	CP	08H		;DOUBLE PRECISION
	JP	Z,CONFDB	;YES
	LD	HL,(CONLO)	;GET LOW TWO BYTES OF FAC
	LD	(FACLO),HL	;SAVE THEM
	LD	HL,(CONHI)	;GET NEXT TWO BYTES
	LD	(FACHI),HL	;SAVE THEM
	RET

CONFDB	LD	HL,CONLO	;GET POINTER TO SAVED CONSTANT AREA
	JP	VMOVFM		;MOVE INTO FAC


;-----------------------------------------------------------------------------
;	DEFSTR, DEFINT, DEFSNG, DEFDBL, INTIDX
; ## GWMAIN.ASM:1871 ##
;
DEFSTR	LD	E,03H		;DEFAULT SOME LETTERS TO STRING
	DB	01H		;"LXI B," OVER THE NEXT 2 BYTES
DEFINT	LD	E,02H		;DEFAULT SOME LETTERS TO INTEGER
	DB	01H		;"LXI B," OVER THE NEXT 2 BYTES
;	a.k.a. DEFREA in GW-BASIC
DEFSNG	LD	E,04H		;DEFAULT SOME LETTERS TO SINGLE PRECISION
	DB	01H		;"LXI B," OVER THE NEXT 2 BYTES
DEFDBL	LD	E,08H		;DEFAULT SOME LETTERS TO DOUBLE PRECISION
DEFCON	CALL	ISLET		;MAKE SURE THE ARGUMENT IS A LETTER
	LD	BC,SNERR	;PREPARE "SYNTAX ERROR" RETURN
	PUSH	BC
	RET	C		;RETURN IF THERES NO LETTER
	SUB	41H		;MAKE AN OFFSET INTO DEFTBL
	LD	C,A		;SAVE THE INITIAL OFFSET
	LD	B,A		;ASSUME IT WILL BE THE FINAL OFFSET
	CALL	CHRGTR		;GET THE POSSIBLE DASH
	CP	MINUTK		;A RANGE ARGUMENT?
	JP	NZ,NOTRNG	;IF NOT, JUST ONE LETTER
	CALL	CHRGTR		;GET THE FINAL POSITION
	CALL	ISLET		;CHECK FOR A LETTER
	RET	C		;GIVE A SYNTAX ERROR IF IMPROPER
	SUB	41H		;MAKE IT AN OFFSET
	LD	B,A		;PUT THE FINAL IN [B]
	CALL	CHRGTR		;GET THE TERMINATOR
NOTRNG	LD	A,B		;GET THE FINAL CHARACTER
	SUB	C		;SUBTRACT THE START
	RET	C		;IF IT'S LESS THATS NONSENSE
	INC	A		;SETUP THE COUNT RIGHT
	EX	(SP),HL		;SAVE THE TEXT POINTER AND GET RID
				;OF THE "SYNTAX ERROR" RETURN
	LD	HL,DEFTBL	;POINT TO THE TABLE OF DEFAULTS
	LD	B,00H		;SETUP A TWO-BYTE STARTING OFFSET
	ADD	HL,BC		;MAKE [H,L] POINT TO THE FIRST ENTRY
				;TO BE MODIFIED
LPDCHG	LD	(HL),E		;MODIFY THE DEFAULT TABLE
	INC	HL
	DEC	A		;COUNT DOUNT THE NUMBER OF CHANGES TO MAKE
	JP	NZ,LPDCHG
	POP	HL		;GET BACK THE TEXT POINTER
	LD	A,(HL)		;GET LAST CHARACTER
	CP	','		;IS IT A COMMA?
	RET	NZ		;IF NOT STATEMENT SHOULD HAVE ENDED
	CALL	CHRGTR		;OTHERWISE SET UP TO SCAN NEW RANGE
	JP	DEFCON

;	INTIDX reads a formula from the current position and
;	turns it into a positive integer
;	leaving the result in [D,E].  Negative arguments
;	are not allowed. [H,L] points to the terminating
;	character of the formula on return.
INTIDX	CALL	CHRGTR
INTID2	CALL	GETIN2		;READ A FORMULA AND GET THE
				;RESULT AS AN INTEGER IN [D,E]
				;ALSO SET THE CONDITION CODES BASED ON
				;THE HIGH ORDER OF THE RESULT
	RET	P		;DON'T ALLOW NEGATIVE NUMBERS
FCERR	LD	E,ERRFC		;TOO BIG. FUNCTION CALL ERROR
	JP	ERROR

;	Linspc is the same as LINGET except in allows the
;	current line (.) specifier
LINSPC	LD	A,(HL)		;GET CHAR FROM MEMORY
	CP	'.'		;IS IT CURRENT LINE SPECIFIER
	EX	DE,HL
	LD	HL,(DOT)	;GET CURRENT LINE #
	EX	DE,HL
	JP	Z,CHRGTR	;ALL DONE.
;
;	LINGET reads a line # from the current text position
;
;	Line numbers range from 0 to 65529
;
;	Answer returned in [D,E].
;	[H,L] is updated to point to the terminating character
;	and [A] contains the terminating character with condition
;	codes set up to reflect its value.
;
LINGET	DEC	HL		;BACKSPACE PTR
LINGT2	CALL	CHRGTR		;FETCH CHAR (GOBBLE LINE CONSTANTS)
	CP	LINCON		;EMBEDDED LINE CONSTANT?
	JP	Z,LINGT3	;YES, RETURN DOUBLE BYTE VALUE
	CP	PTRCON		;ALSO CHECK FOR POINTER
LINGT3	EX	DE,HL
	LD	HL,(CONLO)	;GET EMBEDDED LINE #
	EX	DE,HL
	JP	Z,CHRGTR	;EAT FOLLOWING CHAR
	XOR	A		;SET FLAG THAT NO CONSTANT WAS SEEN SO
	LD	(CONSAV),A	;GOTO2 DOESN'T CHANGE LINCON TO PTRCON
	DEC	HL		;BACK UP POINTER
	LD	DE,0000H	;ZERO ACCUMULATED LINE #
MORLIN	CALL	CHRGTR
	RET	NC
	PUSH	HL
	PUSH	AF
	LD	HL,6552		;SEE IF THE LINE # IS TOO BIG
	CALL	COMPAR
	JP	C,POPHSR	;YES, DON'T SCAN ANY MORE DIGITS IF SO
				;FORCE CALLER TO SEE DIGIT AND GIVE SYNTAX ERROR
				;CAN'T JUST GO TO SYNTAX ERROR BECAUSE OF NON-FAST
				;RENUM WHICH CAN'T TERMINATE
	LD	H,D		;SAVE [D,E]
	LD	L,E
	ADD	HL,DE
	ADD	HL,HL
	ADD	HL,DE
	ADD	HL,HL		;PUTTING [D,E]*10 INTO [H,L]
	POP	AF
	SUB	'0'
	LD	E,A
	LD	D,00H
	ADD	HL,DE		;ADD THE NEW DIGIT
	EX	DE,HL
	POP	HL		;GET BACK TEXT POINTER
	JP	MORLIN

POPHSR	POP	AF		;GET OFF TERMINATING DIGIT
	POP	HL		;GET BACK OLD TEXT POINTER
	RET


;-----------------------------------------------------------------------------
;	RUN, GOTO, GOSUB, RETURN, DATA, REM
; ## GWMAIN.ASM:2016 ##
;
RUN	JP	Z,RUNC		;NO LINE # ARGUMENT
	CP	LINCON		;LINE NUMBER CONSTANT?
	JP	Z,CONRUN	;YES
	CP	PTRCON		;LINE POINTER (RATHER UNLIKELY)
	JP	NZ,LRUN
CONRUN:				;CLEAN UP,SET [H,L]=[TXTTAB]-1 AND
				;RETURN TO NEWSTT
	CALL	CLEARC		;CLEAN UP -- RESET THE STACK
				;DATPTR,VARIABLES ...
				;[H,L] IS THE ONLY THING PRESERVED
	LD	BC,NEWSTT
	JP	RUNC2		;PUT "NEWSTT" ON AND FALL INTO "GOTO"

;
;	A GOSUB entry on the stack has the following format
;
;	Low address
;
;	  A token equal to GOSUTK               1 byte
;         Pointer to event flag if On Event Gosub - else 0000H
;	  The line # of the the GOSUB statement 2 bytes
;	  A pointer into the text of the GOSUB  2 bytes
;
;	High address
;
;	Total 5 bytes (?)
;
GOSUB	LD	C,03H		;"GOSUB" ENTRIES ARE 5 BYTES LONG
	CALL	GETSTK		;MAKE SURE THERE IS ROOM
	CALL	LINGET		;MUST SCAN LINE NUMBER NOW
	POP	BC		;POP OFF RETURN ADDRESS OF "NEWSTT"
	PUSH	HL		;REALLY PUSH THE TEXT POINTER
	PUSH	HL		;SAVE TEXT POINTER
	LD	HL,(CURLIN)	;GET THE CURRENT LINE #
	EX	(SP),HL		;PUT CURLIN ON THE STACK AND [H,L]=TEXT PTR
	LD	A,GOSUBTK	;PUT GOSUB TOKEN ON THE STACK
	PUSH	AF		;
	INC	SP
	PUSH	BC		;SAVE NEWSTT ON STACK
	JP	GOTO2		;HAVE NOW GRAB LINE # PROPERLY

RUNC2	PUSH	BC
;
;	In the 4K version we start at the beginning
;	and search. In the 8K we start where we
;	are if we are going to a forward location.
;
GOTO	CALL	LINGET		;PICK UP THE LINE #
				;AND PUT IT IN [D,E]
GOTO2	LD	A,(CONSAV)	;GET TOKEN FOR LINE # BACK
	CP	PTRCON		;WAS IT A POINTER
	EX	DE,HL		;ASSUME SO
	RET	Z		;IF IT WAS, GO BACK TO NEWSTT
				;WITH [H,L] AS TEXT PTR
	CP	LINCON		;MAKE SURE IT IS A LINE NUMBER CONSTANT
	JP	NZ,SNERR	;IF NOT, BAD ARGUEMMENT PRESENT
	EX	DE,HL		;FLIP BACK IF NOT
	PUSH	HL		;SAVE CURRENT TEXT PTR ON STACK
	LD	HL,(CONTXT)	;GET POINTER TO RIGHT AFTER CONSTANT
	EX	(SP),HL		;SAVE ON STACK, RESTORE CURRENT TEXT PTR
	CALL	REM		;SKIP TO THE END OF THIS LINE
	INC	HL		;POINT AT THE LINK BEYOND IT
	PUSH	HL		;SAVE THE POINTER
	LD	HL,(CURLIN)	;GET THE CURRENT LINE #
	CALL	COMPAR		;[D,E] CONTAINS WHERE WE ARE GOING
				;[H,L] CONTAINS THE CURRENT LINE #
				;SO COMPARING THEM TELLS US WHETHER TO
				;START SEARCHING FROM WHERE WE ARE OR
				;TO START SEARCHING FROM THE BEGINNING
				;OF TXTTAB
	POP	HL		;[H,L]=CURRENT POINTER
	CALL	C,LOOP		;SEARCH FROM THIS POINT
	CALL	NC,FNDLIN	;SEARCH FROM THE BEGINNING -- ACTUALLY
				;SEARCH AGAIN IF ABOVE SEARCH FAILED
	JP	NC,USERR	;LINE NOT FOUND, DEATH
	DEC	BC		;POINT TO ZERO AT END OF PREVIOUS LINE
	LD	A,PTRCON	;POINTER CONSTANT
	LD	(PTRFLG),A	;SET PTRFLG
	POP	HL		;GET SAVED POINTER TO RIGHT AFTER CONSTANT
	CALL	CONCH2		;CHANGE LINE # TO PTR
	LD	H,B		;[H,L]= POINTER TO THE START OF THE
	LD	L,C		;MATCHED LINE
				;NOW POINTING AT THE FIRST BYTE OF THE POINTER
				;TO THE START OF THE NEXT LINE
	RET			;GO TO NEWSTT

USERR	LD	E,ERRUS
	JP	ERROR		;C=MATCH, SO IF NO MATCH WE
				;GIVE A "US" ERROR

;
;	See "GOSUB" for the format of the stack entry
;	"RETURN" restores the line number and text pointer on the stack
;	after eliminating all the "FOR" entries in front of the "GOSUB"
;	entry
;
RETURN	RET	NZ
	LD	D,0FFH		;MAKE SURE THIS VARIABLE POINTER
				;IN [D,E] NEVER GETS MATCHED
	CALL	FNDFOR		;GO PAST ALL THE "FOR" ENTRIES
	LD	SP,HL		;UPDATE THE STACK
	LD	(SAVSTK),HL	;UPDATE SAVED STACK
	CP	GOSUBTK
	LD	E,ERRRG		;ERROR ERRRG IS "RETURN WITHOUT GOSUB"
	JP	NZ,ERROR
	POP	HL
	LD	(CURLIN),HL	;SET CURLIN FOR GOSUB
	LD	HL,NEWSTT
	EX	(SP),HL
				;SKIP OVER SOME CHARACTERS
				;SINCE WHEN "GOSUB" STUCK THE TEXT POINTER
				;ONTO THE STACK THE LINE # ARGUMENT HADN'T
				;BEEN READ IN YET.
	LD	A,0E1H	; SKIP  ;"MVI A," AROUND POP H.
DATA	DB	01H	; SKIP  ;"LXI B," TO PICK UP ":" INTO C AND SKIP
	DB	':'		;"DATA" TERMINATES ON ":"
				;AND 0. ":" ONLY APPLIES IF
				;QUOTES HAVE MATCHED UP
;
;	Note: REM must preserve [D,E] because of "GO TO" and ERROR
;
REM	LD	C,00H		;THE ONLY TERMINATOR IS ZERO
	LD	B,00H		;INSIDE QUOTES THE ONLY TERMINATOR IS ZERO
EXCHQT	LD	A,C		;WHEN A QUOTE IS SEEN THE SECOND
	LD	C,B		;TERMINATOR IS TRADED, SO IN "DATA"
	LD	B,A		;COLONS INSIDE QUOTATIONS WILL HAVE NO EFFECT
REMER	DEC	HL		;NOP THE INX H IN CHRGET
REMER1	CALL	CHRGTR		;GET A CHAR
	OR	A		;ZERO IS ALWAYS A TERMINATOR
	RET	Z
	CP	B		;TEST FOR THE OTHER TERMINATOR
	RET	Z
	INC	HL
	CP	'"'		;IS IT A QUOTE?
	JP	Z,EXCHQT	;IF SO TIME TO TRADE
;
;	When an "IF" takes a false branch it must find the appropriate "ELSE"
;	to start execution at. "DATA" counts the number of "IF"s
;	it sees so that the "ELSE" code can match "ELSE"s with
;	"IF"s. The count is kept in [D]
;
;	Because THEN's have no colon
;	Multiple IFs can be found in a single statement scan
;	This causes a problem for 8-BIT data
;	in unquoted string DATA because $IF might
;	be matched. Fix is to have FALSIF ignore changes
;	in [D] if its a DATA statement
	INC	A		;FUNCTION TOKEN?
	JP	Z,REMER1	;THEN IGNORE FOLLOWING FN NUMBER
	SUB	IFTK+1		;IS IT AN "IF"
	JP	NZ,REMER	;IF NOT, CONTINUE ON
	CP	B		;SINCE "REM" CAN'T SMASH
				;[D,E] WE HAVE TO BE CAREFUL
				;SO ONLY IF B DOESN'T EQUAL
				;ZERO WE INCREMENT D. (THE "IF" COUNT)
	ADC	A,D		;CARRY ON IF [B] NOT ZERO
	LD	D,A		;UPDATE [D]
	JP	REMER


;-----------------------------------------------------------------------------
;	"LET"
; ## GWMAIN.ASM:2267 ##
;
;	LETCON is LET entry point with VALTYP-3 in [A]
;	because GETYPR has been called
LETCON	POP	AF		;GET VALTYPE OFF STACK
	ADD	A,03H		;MAKE VALTYPE CORRECT
	JP	LETCN2		;CONTINUE

LET	CALL	PTRGET		;GET THE POINTER TO THE VARIABLE
				;NAMED IN TEXT AND PUT
				;IT INTO [D,E]
	CALL	SYNCHR
	DB	EQULTK		;CHECK FOR "="
	EX	DE,HL		;MUST SET UP TEMP FOR "FOR"
	LD	(TEMP),HL	;UP HERE SO WHEN USER-FUNCTIONS
				; CALL REDINP, TEMP DOESN'T GET CHANGED
	EX	DE,HL
	PUSH	DE
	LD	A,(VALTYP)
	PUSH	AF
	CALL	FRMEVL		;GET THE VALUE OF THE FORMULA
	POP	AF		;GET THE VALTYP OF THE
				;VARIABLE INTO [A]
				;INTO FAC
LETCN2	EX	(SP),HL		;[H,L]=POINTER TO VARIABLE
				;TEXT POINTER TO ON TOP OF STACK
INPCOM	LD	B,A		;SAVE VALTYP
	LD	A,(VALTYP)	;GET PRESENT VALTYPE
	CP	B		;COMPARE THE TWO
	LD	A,B		;GET BACK CURRENT
	JP	Z,LETCN5	;VALTYPE ALREADY SET UP, GO!
	CALL	DOCNVF		;FORCE VALTPES TO BE [A]'S
LETCN4	LD	A,(VALTYP)	;GET VALTYPE
LETCN5	LD	DE,FACLO	;ASSUME THIS IS WHERE TO START MOVEING
	CP	05H		;IS IT?
	JP	C,LETCN6	;YES
	LD	DE,DFACLO	;NO, USE D.P. FAC
LETCN6	PUSH	HL		;SAVE THE POINTER AT THE VALUE POSITION
	CP	03H		;STRING?
	JP	NZ,COPNUM	;NUMERIC, SO FORCE IT AND COPY
; The following code makes a new copy of the string iff:
; - the string data resides in keyboard or File buffers (since data
;    in these buffers is temporary).
; - the string data resides in string space (each string var must have
;   its own copy of its string data)
; - the source string descriptor is not a temporary descriptor.
;
	LD	HL,(FACLO)	;GET POINTER TO THE DESCRIPTOR OF THE RESULT
	PUSH	HL		;SAVE THE POINTER AT THE DESCRIPTOR
	INC	HL		;Skip over length
	LD	E,(HL)		;LSB of string address
	INC	HL
	LD	D,(HL)		;MSB of string address
	LD	HL,(TXTTAB)	;IF THE DATA IS IN BUF, OR IN DISK
	CALL	COMPAR		;SINCE BUF CHANGES ALL THE TIME
	JP	NC,INBUFC	;GO COPY, IF DATA REALLY IS IN BUF
	LD	HL,(STREND)	;SEE IF IT POINTS INTO STRING SPACE
	CALL	COMPAR		;IF NOT DON'T COPY
	POP	DE		;GET BACK THE POINTER AT THE DESCRIPTOR
	JP	NC,DNTCPY	;DON'T COPY LITERALS
	LD	HL,DSCTMP
	CALL	COMPAR		;IS THE DESCRIPTOR A TEMP?
				;NO, MUST POINT TO VARIABLE(COPY IT!)
	JP	NC,DNTCPY	;YES, DON'T COPY
	DB	3EH		;SKIP THE NEXT BYTE WITH A "MVI A,"
INBUFC	POP	DE		;GET THE POINTER TO THE DESCRIPTOR
				;IN [D,E]
	CALL	FRETMS		;FREE UP A TEMORARY POINTING INTO BUF
	EX	DE,HL		;STRCPY COPIES [H,L]
	CALL	STRCPY		;COPY VARIABLES IN STRING SPACE OR
				;STRINGS WITH DATA IN BUF
DNTCPY	CALL	FRETMS		;FREE UP THE TEMPORARY WITHOUT
				;FREEING UP ANY STRING SPACE
	EX	(SP),HL		;[H,L]=PLACE TO STORE THE DESCRIPTOR
				;LEAVE A NONSENSE ENTRY ON THE STACK,
				;SINCE THE "POP D" DOESN'T EVER
				;MATTER IN THIS CASE
COPNUM	CALL	VMOVE		;COPY A DESCRIPTOR OR A VALUE
	POP	DE		;FOR "FOR" POP OFF A POINTER
				;AT THE LOOP VARIABLE INTO [D,E]
	POP	HL		;GET THE TEXT POINTER BACK
	RET


;-----------------------------------------------------------------------------
;	ON..GOTO, ON ERROR GOTO CODE
; ## GWMAIN.ASM:2355 ##
;
ONGOTO	CP	ERRORTK		;"ON...ERROR"?
	JP	NZ,NTOERR	;NO.
	CALL	CHRGTR		;GET NEXT THING
	CALL	SYNCHR
	DB	GOTOTK		;MUST HAVE ...GOTO
	CALL	LINGET		;GET FOLLOWING LINE #
	LD	A,D		;IS LINE NUMBER ZERO?
	OR	E
	JP	Z,RESTRP	;IF ON ERROR GOTO 0, RESET TRAP
	CALL	FNDLN1		;SEE IF LINE EXISTS (SAVE [H,L] ON STACK)
	LD	D,B		;GET POINTER TO LINE IN [D,E]
	LD	E,C		;(LINK FIELD OF LINE)
	POP	HL		;RESTORE [H,L]
	JP	NC,USERR	;ERROR IF LINE NOT FOUND
RESTRP	EX	DE,HL		;SAVE POINTER TO LINE OR ZERO IF 0.
	LD	(ONELIN),HL
	EX	DE,HL
	RET	C		;YOU WOULDN'T BELIEVE IT IF I TOLD YOU
	LD	A,(ONEFLG)	;ARE WE IN AN "ON...ERROR" ROUTINE?
	OR	A		;SET CONDITION CODES
	LD	A,E		;WANT AN EVEN STACK PTR. FOR 8086
	RET	Z		;IF NOT, HAVE ALREADY DISABLED TRAPPING.
	LD	A,(ERRFLG)	;GET ERROR CODE
	LD	E,A		;INTO E.
	JP	ERRESM		;FORCE THE ERROR TO HAPPEN

	;TODO: => NTONGS
NTOERR	CALL	GETBYT		;GET VALUE INTO [E]
	LD	A,(HL)		;GET THE TERMINATOR BACK
	LD	B,A		;SAVE THIS CHARACTER FOR LATER
	CP	GOSUBTK		;AN "ON ... GOSUB" PERHAPS?
	JP	Z,ISGOSU	;YES, SOME FEATURE USE
	CALL	SYNCHR
	DB	GOTOTK		;OTHERWISE MUST BE "GOTO"
	DEC	HL		;BACK UP CHARACTER POINTER
ISGOSU	LD	C,E		;GET COUNT INTO  [C]
LOOPON	DEC	C		;SEE IF ENOUGH SKIPS
	LD	A,B		;PUT DISPATCH CHARACTER IN PLACE
	JP	Z,GONE2		;IF DONE, GO OFF
	CALL	LINGT2		;SKIP OVER A LINE #
	CP	','		;A COMMA
	RET	NZ		;IF A COMMA DOESN'T DELIMIT THE END OF
				;THE CURRENT LINE # WE MUST BE THE END OF THE LINE
	JP	LOOPON		;CONTINUE GOBBLING LINE #S


;-----------------------------------------------------------------------------
;	RESUME, ERROR STATEMENT CODE
; ## GWMAIN.ASM:2470 ##
;
RESUME	LD	DE,ONEFLG	;Get flag
	LD	A,(DE)		;Were we called by an ON ERROR?
	OR	A
	JP	Z,REERR		;Not in an ON ERROR routine
	INC	A
	LD	(ERRFLG),A	;CLEAR ERROR FLAG SO ^C DOESN'T GIVE ERROR
	LD	(DE),A
	LD	A,(HL)		;GET CURRENT CHAR BACK
	CP	NEXTTK		;RESUME NEXT?
	JP	Z,RESNXT	;YUP.
	CALL	LINGET		;GET FOLLOWING LINE #
	RET	NZ		;SHOULD TERMINATE
	LD	A,D		;IS LINE NUMBER ZERO?
	OR	E		;Yep, go set non-zero CC's
	JP	NZ,GOTO2	;Go find line
	INC	A
	JP	RESTXT

RESNXT	CALL	CHRGTR		;MUST TERMINATE
	RET	NZ		;BLOW HIM UP
RESTXT	LD	HL,(ERRTXT)	;GET POINTER INTO LINE.
	EX	DE,HL		;SAVE ERRTXT IN [D,E]
	LD	HL,(ERRLIN)	;GET LINE #
	LD	(CURLIN),HL	;SAVE IN CURRENT LINE #
	EX	DE,HL
	RET	NZ		;GO TO NEWSTT IF JUST "RESUME"
	LD	A,(HL)		;GET ":" OR LINE HEADER
	OR	A		;SET CC
	JP	NZ,NOTBGL	;#0 MEANS MUST BE ":"
	INC	HL		;SKIP HEADER
	INC	HL
	INC	HL
	INC	HL
NOTBGL	INC	HL		;POINT TO START OF THIS STATEMENT
	JP	DATA

; 	'ERROR' BASIC command
;
;	This is the ERROR <CODE> statement which forces
;	an error of type <CODE> to occur
;	<CODE> must be .GE. 0 and .LE. 255
ERRORS	CALL	GETBYT		;GET THE PARAM
	RET	NZ		;SHOULD HAVE TERMINATED
	OR	A		;ERROR CODE 0?
	JP	Z,FCERR		;YES, ERROR IN ITSELF
	JP	ERROR		;FORCE AN ERROR


;-----------------------------------------------------------------------------
;	AUTO COMMAND
; ## GWMAIN.ASM:2538 ##
;
;	The AUTO [BEGINNING LINE[,[INCREMENT]]]
;	command is used to automatically generate line numbers
;	for lines to be inserted. BEGINNING LINE is
;	used to specify the initial line (10 is assumed if omitted)
;	and the INCREMENT is used to specify the increment used
;	to generate the next line #. If only a comma is used after the
;	BEGINNING LINE, the old INCREMENT is used.
AUTO	LD	DE,000AH	;ASSUME INITIAL LINE # OF 10
	PUSH	DE		;SAVE IT
	JP	Z,SNGAUT	;IF END OF COMMAND USE 10,10
	CALL	LINSPC		;GET LINE #, ALLOW USE OF . FOR CURRENT LINE
	EX	DE,HL		;GET TXT PTR IN [D,E]
	EX	(SP),HL		;PUT INIT ON STACK, GET 10 IN [H,L]
	JP	Z,SNGAU1	;IF TERMINATOR, USE INC OF 10
	EX	DE,HL		;GET TEXT PTR BACK IN [H,L]
	CALL	SYNCHR
	DB	','		;COMMA MUST FOLLOW
	EX	DE,HL
	LD	HL,(AUTINC)	;GET PREVIOUS INC
	EX	DE,HL
	JP	Z,SNGAUT	;USE PREVIOUS INC IF TERMINATOR
	CALL	LINGET		;GET INC
	JP	NZ,SNERR	;SHOULD HAVE FINISHED.
SNGAUT	EX	DE,HL		;GET INC IN [H,L]
SNGAU1	LD	A,H		;SEE IF ZERO
	OR	L
	JP	Z,FCERR		;ZERO INC GIVES FCERR
	LD	(AUTINC),HL	;SAVE INCREMENT
	LD	(AUTFLG),A	;SET FLAG TO USE AUTO IN MAIN CODE.
	POP	HL		;GET INITIAL LINE #
	LD	(AUTLIN),HL	;SAVE IN INTIAL LINE
	POP	BC		;GET RID OF NEWSTT ADDR
	JP	MAIN		;JUMP INTO MAIN CODE (FOR REST SEE AFTER MAIN:)


;-----------------------------------------------------------------------------
;	IF ... THEN CODE
; ## GWMAIN.ASM:2538 ##
;
IFS	CALL	FRMEVL		;EVALUATE A FORMULA
	LD	A,(HL)		;GET TERMINATING CHARACTER OF FORMULA
	CP	','
	CALL	Z,CHRGTR	;IF SO SKIP IT
	CP	GOTOTK		;ALLOW "GOTO" AS WELL
	JP	Z,OKGOTO
	CALL	SYNCHR
	DB	THENTK		;MUST HAVE A THEN
	DEC	HL
OKGOTO	PUSH	HL		;SAVE THE TEXT POINTER
	CALL	VSIGN
	POP	HL		;GET BACK THE TEXT POINTER
	JP	Z,FALSIF	;HANDLE POSSIBLE "ELSE"
DOCOND	CALL	CHRGTR		;PICK UP THE FIRST LINE # CHARACTER
	RET	Z		;RETURN FOR "THEN :" OR "ELSE :"
	CP	LINCON		;LINE NUMBER CONSTANT?
	JP	Z,GOTO		;DO A "GOTO"
	CP	PTRCON		;POINTER CONSTANT
	JP	NZ,GONE2	;EXECUTE STATEMENT, NOT GOTO
	LD	HL,(CONLO)	;GET TEXT POINTER
	RET			;FETCH NEW STATEMENT

;
;	"ELSE" handler. Here on false "IF" condition
;
FALSIF	LD	D,01H		;NUMBER OF "ELSE"S THAT MUST
				;BE SEEN. "DATA" INCREMENTS THIS
				;COUNT EVERY TIME AN "IF" IS SEEN
SKPMRF	CALL	DATA		;SKIP A STATEMENT
				;":" IS STUCK IN FRONT OF "ELSE"S
				;SO THAT "DATA" WILL STOP BEFORE "ELSE" CLAUSES
	OR	A		;END OF LINE?
	RET	Z		;IF SO, NO "ELSE" CLAUSE
	CALL	CHRGTR		;SEE IF WE HIT AN "ELSE"
	CP	ELSETK
	JP	NZ,SKPMRF	;NO, STILL IN THE "THEN" CLAUSE
	DEC	D		;DECREMENT THE NUMBER OF "ELSE"S THAT
				;MUST BE SEEN
	JP	NZ,SKPMRF	;SKIP MORE IF HAVEN'T SEEN
				;ENOUGH
	JP	DOCOND		;FOUND THE RIGHT "ELSE" -- GO EXECUTE


;=============================================================================
;	LPRINT, PRINT Statements
; ## GIO86.ASM 416 ##
;
LPRINT	LD	A,01H		;future output will go to Line Printer
	LD	(PRTFLG),A
	JP	NEWCHR

PRINT	LD	C,02H		;setup output file
	CALL	FILGET		;Look for '#' channel specifier and put
				; the associated file buffer in BC

;	This Syntax variant was enabled on the Tandy Radio Shack models.
;	It required a single parameter to position the cursor on the screen
;	wrapping up the display rows into a single virtual vector.
;	Working on it to adjust programs coming from computers with a different
;	text resolution would probably be more difficult than fiddling with LOCATE.
;	CP	'@'
;	CALL	Z,PRINT@
NEWCHR	DEC	HL		; DEC 'cos GETCHR INCs
	CALL	CHRGTR		;get another character
	CALL	Z,CRDO		;print CR if end without punctuation
PRINTC	JP	Z,FINPRT	;branch if end of statement
	CP	USINGTK		;is it "print using" ?
	JP	Z,PRINUS	;IF SO, USE A SPECIAL HANDLER
	CP	TABTK
	JP	Z,TABER		;the TAB function?
	CP	SPCTK
	JP	Z,TABER		;the SPC function?
	PUSH	HL		;save the text pointer
	CP	','
	JP	Z,COMPRT	;Print Comma
	CP	';'		;is it a ";"
	JP	Z,NOTABR
	POP	BC		;get rid of old text pointer
	CALL	FRMEVL		;evaluate the formula
	PUSH	HL		;save text pointer
	CALL	GETYPR		;see if we have a string
	JP	Z,STRDON	;if so, print special
	CALL	FOUT		;make a number into a string
	CALL	STRLIT		;make it a string
	LD	(HL),' '	;put a space at the end
	LD	HL,(FACLO)	;AND INCREASE SIZE BY 1
	INC	(HL)		;SIZE BYTE IS FIRST IN DESCRIPTOR
;	Output string contents (a.k.a. PRNTST)
;	USE FOLDING FOR STRINGS AND #S
STRDON	CALL	ISFLIO		;DISK OUTPUT?  IF SO, DON'T EVER FORCE A CRLF
	JP	NZ,LINCH2
	LD	HL,(FACLO)	;GET THE POINTER
	LD	A,(PRTFLG)
	OR	A
	JP	Z,ISTTY		;LPT OR TTY?
	LD	A,(LPTSIZ)	;GET WIDTH OF PRINTER
	LD	B,A		;SAVE IN [B]
	INC	A		;IS IT INFINITE? (255="infinite")
	JP	Z,LINCH2	;THEN JUST PRINT
	LD	A,(LPTPOS)	;Get cursor position
	OR	A		;DON'T DO A CRLF IF STRING LONGER THAN LINE
	JP	Z,LINCH2	;LENGTH IF POSITION IS 0
	ADD	A,(HL)		;Add length of string
	CCF			;SET NC IF OVERFLOW ON CHECK
	JP	NC,LINCHK	;START ON A NEW LINE
	DEC	A		;Adjust it
	CP	B		;Will output fit on this line?
	JP	LINCHK

ISTTY	LD	A,(TTYSIZ)	;Get width of line
	LD	B,A		;To B
	INC	A		;NO OVERFLOW LINE WIDTH?
	JP	Z,LINCH2	;YES
	LD	A,(TTYPOS)	;SEE WHERE WE ARE
	OR	A		;don't CR if string longer than line
	JP	Z,LINCH2	;  length if position is 0
	ADD	A,(HL)		;[AL]=column + string size
	CCF			;set nc if overflow on check
	JP	NC,LINCHK	;start on a new line if overflow
	DEC	A
	CP	B		;check for overlap
				;branch if still on current line
LINCHK	CALL	NC,CRDO		;else output CR
LINCH2	CALL	STRPRT		;PRINT THE string/number
	POP	HL		;restore text pointer
	JP	NEWCHR		;print some more

;	PRINT comma (text pointer stacked)
;
COMPRT	LD	BC,0028H	;(NMLO.C) if file output, SPECIAL PRINT
				; POSITION SHOULD BE FETCHED FROM FILE DATA
	LD	HL,(PTRFIL)
	ADD	HL,BC		;[H,L] POINT AT POSITION
	CALL	ISFLIO		;OUTPUTING INTO A FILE?
	LD	A,(HL)		;IF FILE IS ACTIVE
	JP	NZ,COMPRT3
	LD	A,(PRTFLG)	;OUTPUT TO THE LINE PRINTER?
	OR	A		;NON-ZERO MEANS YES
	JP	Z,COMPRT1	;NO, DO TELETYPE COMMA
	LD	A,(NLPPOS)	;Get comma width
	LD	B,A		;Save in B
	INC	A		;TEST
	LD	A,(LPTPOS)	;GET LINE PRINTER POSITION
	JP	Z,COMPRT3	;ALWAYS DO MODULUS IF WIDTH=255
	CP	B		;CHECK IF NO MORE COMMA FIELDS
	JP	COMPRT2		;USE TELETYPE CHECK

COMPRT1	LD	A,(NTTPOS)	;POSITION BEYOND WHICH
	LD	B,A		; THERE ARE NO MORE COMMA FIELDS
	LD	A,(TTYPOS)	;[AL]=file's current column position
	CP	0FFH		;infinite width?
	JP	Z,COMPRT3	;do modulus
	CP	B		;compare current with last comma column
				;branch if not beyond last comma col
COMPRT2	CALL	NC,CRDO		;start new line
	JP	NC,NOTABR	;AND QUIT IF BEYOND LAST COMMA FIELD
COMPRT3	SUB	CLMWID		;[AL]=MODULUS CLMWID
	JP	NC,COMPRT3
	CPL			;fill the print position out
				;to an even CLMWID, so
				;we print CLMWID-[AL] MOD CLMWID spaces
	JP	ASPA2		;go print [AL]+1 spaces

;	PRINT TAB(N) & SPC(N)
;
TABER	PUSH	AF		;remember IF [A]=SPCTK or TABTK
	CALL	CHRGTR
	CALL	GETIN2		;EVALUATE THE ARGUMENT
	POP	AF		;SEE IF ITS SPC OR TAB
	PUSH	AF
	CP	SPCTK		;IF SPACE LEAVE ALONE
	JP	Z,SPCNDC
	DEC	DE		;offset TAB by 1
SPCNDC	LD	A,D
	OR	A		;MAKE SURE ITS NOT NEGATIVE
	JP	P,TBNONG
	LD	DE,0000H
TBNONG	PUSH	HL		;SAVE THE TEXT POINTER
	CALL	ISFLIO		;SEE IF GOING TO DISK FILE
	JP	NZ,LNOMOD	;DONT MOD
	LD	A,(PRTFLG)	;GOING TO PRINTER?
	OR	A		;SET FLAGS
	LD	A,(LPTSIZ)	;GET SIZE
	JP	NZ,TABER12	;WAS LPT, MOD BY ITS SIZE
	LD	A,(TTYSIZ)	;GET THE LINE LENGTH
TABER12	LD	L,A		;[L]=file width
	INC	A		;test for width of 255 (no folding)
	JP	Z,LNOMOD	;if so, don't mod
	LD	H,00H		;MOD out by line length
	CALL	IMOD		;[BX]=[DX] MOD filewidth
	EX	DE,HL		;set [DL] = position to go to
LNOMOD	POP	HL		;get back the text pointer
	CALL	SYNCHR
	DB	')'		; Make sure ")" follows
	DEC	HL		; Back space on to ")"
	POP	AF		;get back SPCTK or TABTK
	SUB	SPCTK		;was it SPCTK?
	PUSH	HL		;save the text pointer
	JP	Z,DOSIZT	;value in [AL]
	LD	BC,0028H	;(NMLO.C) if file output, SPECIAL PRINT
				; POSITION SHOULD BE FETCHED FROM FILE DATA
	LD	HL,(PTRFIL)
	ADD	HL,BC		;[H,L] POINT AT POSITION
	CALL	ISFLIO		;OUTPUTING INTO A FILE?
				;(IF SO, [PTRFIL] .NE. 0)
	LD	A,(HL)		;IF FILE IS ACTIVE
	JP	NZ,DOSIZT	;DO TAB CALCULATION NOW
	LD	A,(PRTFLG)	;LINE PRINTER OR TTY?
	OR	A		;NON-ZERO MEANS LPT
	JP	Z,TABTTY
	LD	A,(LPTPOS)	;GET LINE PRINTER POSITION
	JP	DOSIZT		;value in [AL]

;	TAB on TTY
TABTTY	LD	A,(TTYPOS)	;[AL]=file position
DOSIZT	CPL			;print [E]-[A] spaces
	ADD	A,E
	JP	C,ASPA2		;print if past current
	INC	A
	JP	Z,NOTABR	;do nothing if at current
	CALL	CRDO		;go to a new line
	LD	A,E		;get the position to go to
	DEC	A
	JP	M,NOTABR
;	output (A) spaces
ASPA2	INC	A
	LD	B,A		;[B]=number of spaces to print
	LD	A,' '		;[A]=space
REPOUT	CALL	OUTDO		;PRINT [AL]
	DEC	B		;decrement the count
	JP	NZ,REPOUT
;	Move to next item in the PRINT list
NOTABR	POP	HL		;pick up text pointer
	CALL	CHRGTR		;and the next character
	JP	PRINTC		;and since we just printed
				;spaces, don't call crdo
				;if it's the end of the line

;	FINPRT is called at the end of every BASIC statement and after ERROR.
;	It resets the current file to be Keyboard/Crt.
;	It also frees the File-Data-Block pointed to by FREFDB if it is non-zero.
;	This is useful for Device-Open routines (xxxOPN).  After calling
;	INIFDB to allocate an FDB, they can set FREFDB to point to allocated FDB.
;	When the file gets completely opened, they can reset FREFDB to 0.
;	Then if some error occurs in between, FINPRT will release the FDB.
;	Exit  - AX, SI, DI, FLAGS used, all other registers preserved.

;	FINISH 'PRINT' BY RESETTING FLAGS
;	(IN WHICH CASE A TERMINATOR DOES NOT MEAN WE SHOULD TYPE A CRLF BUT JUST RETURN)
FINPRT	XOR	A
	LD	(PRTFLG),A
	PUSH	HL		;SAVE THE TEXT POINTER
	LD	H,A		;[H,L]=0
	LD	L,A
	LD	(PTRFIL),HL	;ZERO OUT PTRFIL
				;(disabling eventual output redirection)
	POP	HL		;GET BACK THE TEXT POINTER
	RET


;=============================================================================
;	LINE INPUT, INPUT AND READ CODE
; ## GWMAIN.ASM:2635 ##
;
LINE:
	CALL	SYNCHR
	DB	INPUTTK
	CP	'#'		;SEE IF THERE IS A FILE NUMBER
	JP	Z,DLINE		;DO DISK INPUT LINE
	CALL	SCNSEM		;SCAN SEMICOLON FOR NO-CR
	CALL	QTINP		;PRINT QUOTED STRING IF ONE
	CALL	PTRGET		;READ STRING TO STORE INTO
	CALL	CHKSTR		;MAKE SURE ITS A STRING
	PUSH	DE		;SAVE POINTER AT VARIABLE
	PUSH	HL		;SAVE TEXT POINTER
	CALL	SINLIN		;READ A LINE OF INPUT
	POP	DE		;GET TEXT POINTER
	POP	BC		;GET POINTER AT VARIABLE
	JP	C,STPEND1	;IF CONTROL-C, STOP
	PUSH	BC		;SAVE BACK VARIABLE POINTER
	PUSH	DE		;SAVE TEXT POINTER
	LD	B,00H		;SETUP ZERO AS ONLY TERMINATOR
	CALL	STRLT3		;LITERALIZE THE INPUT
	POP	HL		;RESTORE [H,L]=TEXT POINTER
	LD	A,03H		;SET THREE FOR STRING
	JP	LETCN2		;DO THE ASSIGNMENT

TRYAGN	DB	'?Redo from start',0DH,0AH,00H

;
;	Here when passing over string literal in subscript of variable in INPUT list
;	on the first pass of INPUT checking for type match and number
;
SCNSTR	INC	HL		;LOOK AT THE NEXT CHARACTER
	LD	A,(HL)		;FETCH IT
	OR	A		;END OF LINE?
	JP	Z,SNERR		;ENDING IN STRING IN SUBSCRIPT IS BAD SYNTAX
	CP	'"'		;ONLY OTHER TERMINATOR IS QUOTE
	JP	NZ,SCNSTR	;CONTINUE UNTIL QUOTE OR 0 IS FOUND
	JP	SCNCON		;CONTINUE MATCHING PARENS SINCE STRING ENDED

INPBAK	POP	HL		;GET RID OF PASS1 DATA POINTER
	POP	HL		;GET RID OF PASS2 DATA POINTER
	JP	RDOIN2		;GET RID OF PASS2 VARLST POINTER AND RETRY

;
;	Here when the data that was typed in or in "DATA" Statements
;	is improperly formatted. For "INPUT" we start again.
;	For "DATA" we give a syntax error at the DATA line
;
TRMNOK	LD	A,(SARYFL)	;WAS IT READ OR INPUT?
	OR	A		;ZERO=INPUT
	JP	NZ,DATSNE	;GIVE ERROR AT DATA LINE
RDOIN2	POP	BC		;GET RID OF THE POINTER INTO THE VARIABLE LIST
	LD	HL,TRYAGN
	CALL	STROUT		;PRINT "?REDO FROM START"
	LD	HL,(SAVTXT)	;GET SAVED TEXT POINTER
	RET			;GO BACK TO NEWSTT
				;OF THE "INPUT" STATEMENT
FILSTI	CALL	FILINP
	PUSH	HL		;PUT THE TEXT POINTER ON THE STACK
	LD	HL,BUFMIN	;POINT AT A COMMA
	JP	INPCN3

INPUT	CP	'#'
	JP	Z,FILSTI
	CALL	SCNSEM		;SCAN SEMICOLON FOR NO-CR
	LD	BC,NOTQTI	;WHERE TO GO
	PUSH	BC		;WHEN DONE WITH QUOTED STRING
QTINP	CP	'"'		;IS IT A QUOTE?
	LD	A,00H		;BE TALKATIVE
	LD	(CNTOFL),A	;FORCE OUTPUT
	LD	A,0FFH		;MAKE NON-ZERO VALUE
	LD	(TEMPA1),A	;FLAG TO DO "? "
	RET	NZ		;JUST RETURN
	CALL	STRLT1		;MAKE THE MESSAGE A STRING
	LD	A,(HL)		;GET CHAR
	CP	','		;COMMA?
	JP	NZ,NTICMA	;NO
	XOR	A		;FLAG NOT TO DO IT
	LD	(TEMPA1),A	;=TEMPA+1 (in GW-BASIC)
	CALL	CHRGTR		;FETCH NEXT CHAR
	JP	INPCMA		;CONTINUE

NTICMA	CALL	SYNCHR
	DB	';'		;MUST END WITH SEMI-COLON
INPCMA	PUSH	HL		;REMEMBER WHERE IT ENDED
	CALL	STRPRT		;PRINT IT OUT
	POP	HL		;GET BACK SAVED TEXT PTR
	RET			;ALL DONE
NOTQTI	PUSH	HL
	LD	A,(TEMPA1)	;DO "? "
	OR	A
	JP	Z,SUPPRS	;THEN SUPPRESS "?"
	LD	A,'?'		;TYPE "? " AND INPUT A LINE OF TEXT
	CALL	OUTDO
	LD	A,' '
	CALL	OUTDO
SUPPRS	CALL	SINLIN
	POP	BC		;TAKE OFF SINCE MAYBE LEAVING
	JP	C,STPEND1	;IF EMPTY LEAVE
	PUSH	BC		;PUT BACK  SINCE DIDN'T LEAVE
;
;	This is the first pass dictated by ANSI requirment than no values be assigned
;	before checking type and number. The variable list is scanned without evaluating
;	subscripts and the INPUT is scanned to get its type. No assignment
;	is done
;
	XOR	A		;FLAG AS INPUT SO SCNVAL WILL WORK
	LD	(SARYFL),A	;WHEN SCANNING STRINGS WITH ":"
	LD	(HL),','	;PUT A COMMA IN FRONT OF BUF
	EX	DE,HL		;SAVE DATA POINTER IN [D,E]
	POP	HL		;GET THE VARLST POINTER INTO [H,L]
	PUSH	HL		;RESAVE THE VARLST POINTER
	PUSH	DE		;SAVE A COPY OF THE DATA POINTER FOR PASS2
	PUSH	DE		;SAVE THE DATA POINTER FOR PASS1
	DEC	HL		;READ THE FIRST VARIABLE NAME
VARLOP	LD	A,80H		;DON'T ALLOW SUBSCRIPTS -- RETURN POINTING TO "("
	LD	(SUBFLG),A
	CALL	CHRGTR		;ADVANCE TEXT POINTER
	CALL	PTRGTN		;SCAN NAME AND RETURN POINTER IN [D,E]
	LD	A,(HL)		;SEE IF IT ENDED ON "("
	DEC	HL		;RESCAN THE TERMINATOR
	CP	'['		;allow brackets also
	JP	Z,SCNBKT
	CP	'('		;ARRAY OR NOT?
	JP	NZ,ENDSCN	;IF NOT, VARIABLE NAME IS DONE
SCNBKT	INC	HL		;NOW SCAN THE SUBSCRIPT EXPRESSION
	LD	B,00H		;INITIALIZE THE PAREN COUNT
SCNOPN	INC	B		;UP THE COUNT FOR EVERY "("
SCNCON	CALL	CHRGTR		;GET THE NEXT CHARACTER
	JP	Z,SNERR		;SHOULDN'T END STATEMENT IN EXPRESSION
	CP	'"'		;IS THERE A QUOTED STRING CONSTANT
	JP	Z,SCNSTR	;GO SCAN THE ENDTIRE CONSTANT (MAY CONTAIN PARENS)
	CP	'('		;ANOTHER LEVEL OF NESTING?
	JP	Z,SCNOPN	;INCREMENT COUTN AND KEEP SCANNING
	CP	'['		;left bracket?
	JP	Z,SCNCON	;yes, ok
	CP	']'		;left bracket?
	JP	Z,LEFPRN	;yes
	CP	')'		;ONE LESS LEVEL OF PARENS?
	JP	NZ,SCNCON	;NO, KEEP SCANNING
				;DECREMENT PAREN COUNT. OUT OF SUBSCRIPT?
LEFPRN	DEC	B
	JP	NZ,SCNCON	;IF NOT AT ZERO LEVEL, KEEP SCANNING
ENDSCN	CALL	CHRGTR		;GET TERMINATING CHARACTER
	JP	Z,OKVLST	;LAST VARIABLE IN INPUT LIST
	CP	','		;OTHERWISE IT MUST BE A COMMA
	JP	NZ,SNERR	;BADLY FORMED INPUT -- SYNTAX ERROR
OKVLST	EX	(SP),HL		;SAVE THE VARLST POINTER
				;GET THE DATA POINTER INTO [H,L]
	LD	A,(HL)		;DATA SHOULD ALWAYS HAVE A LEADING COMMA
	CP	','		;IS IT PROPERLY FORMED?
	JP	NZ,INPBAK	;NO, ASK FOR COMPLETE REINPUT
	LD	A,01H		;SET OVCSTR=1
	LD	(OVCSTR),A
	CALL	SCNVAL		;GO INTO PASS2 CODE AND SCAN A VALUE
	LD	A,(OVCSTR)	;SEE IF IT WAS TOO BIG
	DEC	A
	JP	NZ,INPBAK
	PUSH	HL		;SAVE THE RETURNED DATA POINTER
	CALL	GETYPR		;RELEASE STRING
	CALL	Z,FREFAC
	POP	HL
	DEC	HL		;SKIP OVER SPACES LEFT AFTER VALUE SCAN
	CALL	CHRGTR
;
;	Note check for overflow of INPUT value here
;
	EX	(SP),HL		;SAVE THE DATA POINTER
				;[H,L]=DATA LIST POINTER
	LD	A,(HL)		;DID VARIABLE LIST CONTINUE?
	CP	','		;MUST HAVE HAD A COMMA
	JP	Z,VARLOP	;GO CHECK ANOTHER
	POP	HL		;GET FINAL DATA POINTER
	DEC	HL		;SKIP OVER ANY TRAILING SPACES
	CALL	CHRGTR
	OR	A		;IS IT A TRUE END?
	POP	HL		;GET THE START OF DATA POINTER FOR PASS2
	JP	NZ,RDOIN2	;IF DATA ENDED BADLY ASK FOR REINPUT
INPCN3	LD	(HL),','	;SETUP COMMA AT BUFMIN
	JP	INPCON

READ	PUSH	HL		;SAVE THE TEXT POINTER
	LD	HL,(DATPTR)	;GET LAST DATA LOCATION
	DB	0F6H		;"ORI" TO SET [A] NON-ZERO
INPCON	XOR	A		;SET FLAG THAT THIS IS AN INPUT
	LD	(SARYFL),A	;STORE THE FLAG
;	In the processing of DATA and READ statements:
;	one pointer points to the data (ie the numbers being fetched)
;	and another points to the list of variables
;
;	The pointer into the data always starts pointing to a
;	terminator -- a "," ":" or End-Of-Line
	EX	(SP),HL		;[H,L]=VARIABLE LIST POINTER
				;DATA POINTER GOES ON THE STACK
	JP	LOPDAT
LOPDT2	CALL	SYNCHR
	DB	','		;MAKE SURE THERE IS A ","
LOPDAT	CALL	PTRGET		;READ THE VARIABLE LIST
				;AND GET THE POINTER TO A VARIABLE INTO [D,E]
				;XTHL
	EX	(SP),HL		;PUT THE VARIABLE LIST POINTER ONTO THE
				;STACK AND TAKE THE
				;DATA LIST POINTER OFF
;	Note at this point we have a variable which wants DATA
;	and so we must get DATA or complain
	PUSH	DE		;SAVE THE POINTER TO THE VARIABLE WE
				;ARE ABOUT TO SET UP WITH A VALUE
	LD	A,(HL)		;SINCE THE DATA LIST POINTER ALWAYS POINTS
				;AT A TERMINATOR LETS READ THE
				;TERMINATOR INTO [A] AND SEE WHAT
				;IT IS
	CP	','
	JP	Z,DATBK		;A COMMA SO A VALUE MUST FOLLOW
	LD	A,(SARYFL)	;SEE WHAT TYPE OF STATEMENT THIS WAS
	OR	A
				;SEARCH FOR ANOTHER DATA STATEMENT
	JP	NZ,DATLOP
;
;	THE DATA NOW STARTS AT THE BEGINNING OF THE BUFFER
;	AND QINLIN LEAVES [H,L]=BUF
DATBK	DB	0F6H		;SET A NON-ZERO
SCNVAL	XOR	A		;SET ZERO FLAG IN [A]
	LD	(INPPAS),A	;STORE SO EARLY RETURN CHECK WORKS
	CALL	ISFLIO		;SEE IF A FILE READ
	JP	NZ,FILIND	;IF SO, SPECIAL HANDLING
	CALL	GETYPR		;IS IT A STRING?
	PUSH	AF		;SAVE THE TYPE INFORMATION
	JP	NZ,NUMINS	;IF NUMERIC, USE FIN TO GET IT
				;ONLY THE VARIABLE TYPE IS
				;CHECKED SO AN UNQUOTED STRING
				;CAN BE ALL DIGITS
	CALL	CHRGTR
	LD	D,A		;ASSUME QUOTED STRING
	LD	B,A		;SETUP TERMINATORS
	CP	'"'		;QUOTE ?
	JP	Z,NOWGET	;TERMINATORS OK
	LD	A,(SARYFL)	;INPUT SHOULDN'T TERMINATE ON ":"
	OR	A		;SEE IF READ OR INPUT
	LD	D,A		;SET D TO ZERO FOR INPUT
	JP	Z,NCOLST
	LD	D,':'		;UNQUOTED STRING TERMINATORS
NCOLST	LD	B,','		;ARE COLON AND COMMA
				;NOTE: ANSI USES [B]=44 AS A FLAG TO
				;TRIGGER TRAILING SPACE SUPPRESSION
	DEC	HL		;BACKUP SINCE START CHARACTER MUST BE INCLUDED
				;IN THE QUOTED STRING CASE WE DON'T WANT TO
				;INCLUDE THE STARTING OR ENDING QUOTE
;	MAKE A STRING DESCRIPTOR FOR THE VALUE AND COPY IF NECESSARY
NOWGET	CALL	STRLT2		;MAKE A STRING DESCRIPTOR FOR THE VALUE
				;AND COPY IF NECESSARY
DOASIG	POP	AF		;POP OFF THE TYPE INFORMATION
	ADD	A,03H		;MAKE VALTYPE CORRECT
	LD	C,A		;SAVE VALUE TYPE IN [C]
	LD	A,(INPPAS)	;SEE IF SCANNING VALUES FOR PASS1
	OR	A		;ZERO FOR PASS1
	RET	Z		;GO BACK TO PASS1
	LD	A,C		;RECOVER VALTYP
	EX	DE,HL		;[D,E]=TEXT POINTER
	LD	HL,STRDN2	;RETURN LOC
	EX	(SP),HL		;[H,L]=PLACE TO STORE VARIABLE VALUE
	PUSH	DE		;TEXT POINTER GOES ON
	JP	INPCOM		;DO ASSIGNMENT

NUMINS	CALL	CHRGTR
	POP	AF		;GET BACK VALTYPE OF SOURCE
	PUSH	AF		;SAVE BACK
				;REGEN CC'S
	LD	BC,DOASIG	;ASSIGNMENT IS COMPLICATED
				;EVEN FOR NUMERICS SO USE THE "LET" CODE
	PUSH	BC		;SAVE ON STACK
	JP	C,FIN		;IF NOT DOUBLE, CALL USUAL # INPUTTER
	JP	FINDP		;ELSE CALL SPECIAL ROUTINE WHICH EXPECTS DOUBLES

;	Where to go after LETSTR
STRDN2	DEC	HL		;DEC 'cos GETCHR INCs
	CALL	CHRGTR		;Get next character
	JP	Z,TRMOK		;End of line - More needed?
	CP	','		;Another value?
	JP	NZ,TRMNOK	;ENDED PROPERLY?
TRMOK	EX	(SP),HL
	DEC	HL		;LOOK AT TERMINATOR
	CALL	CHRGTR		;AND SET UP CONDITION CODES
	JP	NZ,LOPDT2	;NOT ENDING, CHECK FOR COMMA
				;AND GET ANOTHER VARIABLE
				;TO FILL WITH DATA
	POP	DE		;POP OFF THE POINTER INTO DATA
	LD	A,(SARYFL)	;FETCH THE STATEMENT TYPE FLAG
	OR	A		;IF ZERO, INPUT STATEMENT
	EX	DE,HL
	JP	NZ,RESFIN	;UPDATE DATPTR
	PUSH	DE		;SAVE THE TEXT POINTER
	POP	HL		;GET BACK THE TEXT POINTER
	JP	FINPRT

;	Find next DATA statement
;
;	The search for DATA statements is made by using the execution code
;	for DATA to skip over statements. The start word of each statement
;	is compared with $DATA. Each new line number
;	is stored in DATLIN so that if an error occurs while reading
;	data the error message will give the line number of the
;	ill-formatted DATA
;
DATLOP	CALL	DATA		;Get next statement
	OR	A		;End of line?
	JP	NZ,NOWLIN	;No - See if DATA statement
	INC	HL
	LD	A,(HL)		;End of program?
	INC	HL
	OR	(HL)		;IFE XERPAG
	LD	E,ERROD		;NO DATA IS ERROR ERROD
	JP	Z,ERROR		;IF SO COMPLAIN
	INC	HL		;SKIP PAST LINE #
	LD	E,(HL)		;GET DATA LINE #
	INC	HL
	LD	D,(HL)
	EX	DE,HL
	LD	(DATLIN),HL	;SAVE DATA LINE NUMBER
	EX	DE,HL
NOWLIN	CALL	CHRGTR		;GET THE STATEMENT TYPE
	CP	DATATK		;IS IS "DATA"?
	JP	NZ,DATLOP	;NOT DATA SO LOOK SOME MORE
	JP	DATBK		;CONTINUE READING

;=============================================================================
;	GWEVAL Copied from BINTRP.MAC
;-----------------------------------------------------------------------------
;	FORMULA EVALUATION CODE
; ## GWEVAL.ASM:271
;
;	The formula evaluator starts with
;	[H,L] pointing to the first character of the formula.
;	At the end [H,L] points to the terminator.
;	The result is left in the FAC.
; 	On return [A] does not reflect the terminating character
;
;	The formula evaluator uses the operator table (OPTAB)
;	to determine precedence and dispatch addresses for
;	each operator.
;	A temporary result on the stack has the following format
;
;		The address of 'RETAOP' -- the place to return on completion
;		of operator application
;
;		The floating point temporary result
;
;		The address of the operator routine
;
;		The precedence of the operator
;
;	Total 10 bytes

;	Check for = sign before evaluating the formula
FRMEQL	CALL	SYNCHR
	DB	EQULTK		;CHECK FOR EQUAL SIGN
	JP	FRMEVL

;	Check for left paren before evaluating the formula
FRMPRN	CALL	SYNCHR
	DB	'('		;GET PAREN BEFORE FORMULA
;	Main Formula Evaluator
FRMEVL	DEC	HL		;BACK UP CHARACTER POINTER
FRMCHK	LD	D,00H		;INITIAL DUMMY PRECEDENCE IS 0
LPOPER	PUSH	DE		;SAVE PRECEDENCE
	LD	C,01H		;EXTRA SPACE NEEDED FOR RETURN ADDRESS
	CALL	GETSTK		;MAKE SURE THERE IS ROOM FOR RECURSIVE CALLS
	CALL	EVAL		;EVALUATE SOMETHING
				;RESET OVERFLOW PRINTING BACK TO NORMAL
	XOR	A		;(SET TO 1 AT FUNDSP TO SUPPRESS
	LD	(FLGOVC),A	;MULTIPLE OVERFLOW MESSAGES)

;	Evaluate expression until precedence break
TSTOP	LD	(TEMP2),HL	;SAVE TEXT POINTER
RETAOP	LD	HL,(TEMP2)	;RESTORE TEXT PTR
	POP	BC		;POP OFF THE PRECEDENCE OF OLDOP
	LD	A,(HL)		;GET NEXT CHARACTER
	LD	(TEMP3),HL	;SAVE UPDATED CHARACTER POINTER
	CP	GREATK		;IS IT AN OPERATOR?
	RET	C		;NO, ALL DONE (THIS CAN RESULT IN OPERATOR
				;APPLICATION OR ACTUAL RETURN)
	CP	LESSTK+1	;SOME KIND OF RELATIONAL?
	JP	C,DORELS	;YES, DO IT
	SUB	PLUSTK		;SUBTRACT OFFSET FOR FIRST ARITHMETIC
	LD	E,A		;MUST MULTIPLY BY 3 SINCE
				;OPTAB ENTRIES ARE 3 LONG
	JP	NZ,NTPLUS	;NOT ADDITION OP
	LD	A,(VALTYP)	;SEE IF LEFT PART IS STRING
	CP	03H		;SEE IF ITS A STRING
	LD	A,E		;REFETCH OP-VALUE
	JP	Z,CAT
NTPLUS	CP	LSTOPK		;HIGHER THAN THE LAST OP?
	RET	NC		;YES, MUST BE TERMINATOR
	LD	HL,OPTAB	;CREATE INDEX INTO OPTAB
	LD	D,00H		;MAKE HIGH BYTE OF OFFSET=0
	ADD	HL,DE		;ADD IN CALCULATED OFFSET
	LD	A,B		;[A] GETS OLD PRECEDENCE
				;CODE SEGMENT FETCH
	LD	D,(HL)		;REMEMBER NEW PRECEDENCE
	CP	D		;OLD-NEW
	RET	NC		;MUST APPLY OLD OP
				;IF HAS GREATER OR = PRECEDENCE
				;NEW OPERATOR
	PUSH	BC		;SAVE THE OLD PRECEDENCE
	LD	BC,RETAOP	;PUT ON THE ADDRESS OF THE
	PUSH	BC		;PLACE TO RETURN TO AFTER OPERATOR APPLICATION
	LD	A,D		;SEE IF THE OPERATOR IS EXPONENTIATION
	CP	7FH		;WHICH HAS PRECEDENCE 127
	JP	Z,EXPSTK	;IF SO, "FRCSNG" AND MAKE A SPECIAL STACK ENTRY
	CP	51H		;SEE IF THE OPERATOR IS "AND" OR "OR"
	JP	C,ANDORD	;AND IF SO "FRCINT" AND
				;MAKE A SPECIAL STACK ENTRY
	AND	0FEH		;MAKE 123 AND 122 BOTH MAP TO 122
	CP	7AH		;MAKE A SPECIAL CHECK FOR "MOD" AND "IDIV"
	JP	Z,ANDORD	;IF SO, COERCE ARGUMENTS TO INTEGER
;	This code pushes the current value in the FAC
;	onto the stack, except in the case of strings in which it calls
;	type mismatch error. [D] and [E] are preserved.
NUMREL	LD	HL,FACLO	;FIND OUT WHAT TYPE OF VALUE WE ARE SAVING
	LD	A,(VALTYP)	;SETUP THE CONDITION CODES
	SUB	03H		;SET ZERO FOR STRINGS
	JP	Z,TMERR		;Type Mismatch
	OR	A
	LD	C,(HL)		;PUSH FACLO
	INC	HL
	LD	B,(HL)
	PUSH	BC
	JP	M,VPUSHD	;ALL DONE IF THE DATA WAS AN INTEGER
	INC	HL		;PUSH FAC-1,0 ON THE STACK
	LD	C,(HL)
	INC	HL
	LD	B,(HL)
	PUSH	BC
	JP	PO,VPUSHD	;ALL DONE IF WE HAD A SNG
	LD	HL,DFACLO	;PUSH ON LOW BYTES OF DP FAC
	LD	C,(HL)
	INC	HL
	LD	B,(HL)
	INC	HL
	PUSH	BC
	LD	C,(HL)		;PUSH ON NEXT TWO BYES OF DP FAC
	INC	HL
	LD	B,(HL)
	PUSH	BC
VPUSHD	ADD	A,03H		;FIX [A] TO BE THE VALTYP OF THE NUMBER
				;JUST PUSHED ON THE STACK
	LD	C,E		;[C]=OPERATOR NUMBER
	LD	B,A		;[B]=TYPE OF VALUE ON THE STACK
	PUSH	BC		;SAVE THESE THINGS FOR APPLOP
	LD	BC,APPLOP	;GENERAL OPERATOR APPLICATION
				;ROUTINE -- DOES TYPE CONVERSIONS
FINTMP	PUSH	BC		;SAVE PLACE TO GO
	LD	HL,(TEMP3)	;REGET THE TEXT POINTER
	JP	LPOPER
DORELS	LD	D,00H		;ASSUME NO RELATION OPS
				;ALSO SETUP THE HIGH ORDER OF THE INDEX INTO OPTAB
LOPREL	SUB	GREATK		;IS THIS ONE RELATION?
	JP	C,FINREL	;RELATIONS ALL THROUGH
	CP	LESSTK-GREATK+1	;IS IT REALLY RELATIONAL?
	JP	NC,FINREL	;NO JUST BIG
	CP	01H		;SET UP BITS BY MAPPING
	RLA			;0 TO 1 1 TO 2 AND 2 TO 4
	XOR	D		;BRING IN THE OLD BITS
	CP	D		;MAKE SURE RESULT IS BIGGER
	LD	D,A		;SAVE THE MASK
	JP	C,SNERR		;DON'T ALLOW TWO OF THE SAME
	LD	(TEMP3),HL	;SAVE CHARACTER POINTER
	CALL	CHRGTR		;GET THE NEXT CANDIDATE
	JP	LOPREL

;	For exponentiation we want to force the current value in the FAC
;	to be single precision. When application time comes we force
;	the right hand operand to single precision as well
EXPSTK	CALL	CSNG		;COERCE LEFT HAND OPERAND
	CALL	PUSHF		;PUT IT ON THE STACK
	LD	BC,FPWRQ	;PLACE TO COERCE RIGHT HAND
				;OPERAND AND DO EXPONENTIATION
	LD	D,7FH		;RESTORE THE PRECEDENCE
	JP	FINTMP		;FINISH ENTRY AND EVALUATE MORE FORMULA

;	For "AND" and "OR" we want to force the current value in the
;	FAC to be an integer, and at application time force the right
;	hand operand to be an integer
ANDORD	PUSH	DE		;SAVE THE PRECEDENCE
	CALL	CINT
	POP	DE		;[D]=PRECEDENCE
	PUSH	HL		;PUSH THE LEFT HAND OPERAND
	LD	BC,DANDOR	;"AND" AND "OR" DOER
	JP	FINTMP		;PUSH ON THIS ADDRESS,PRECEDENCE
				;AND CONTINUE EVALUATION

;	Here to build an entry for a relational operator
;	Strings are treated specially. Numeric compares are different
;	from most operator entries only in the fact that at the
;	bottom instead of having RETAOP, DOCMP and the relational
;	bits are stored. Strings have STRCMP, the pointer at the string descriptor,
;	DOCMP and the relational bits.
FINREL	LD	A,B		;[A]=OLD PRECEDENCE
	CP	100		;RELATIONALS HAVE PRECEDENCE 100
	RET	NC		;APPLY EARLIER OPERATOR IF IT HAS
				;HIGHER PRECEDENCE
	PUSH	BC		;SAVE THE OLD PRECEDENCE
	PUSH	DE		;SAVE [D]=RELATIONAL BITS
	LD	DE,100*256+OPCNT;[D]=PRECEDENCE=100
				;[E]=DISPATCH OFFSET FOR
				;COMPARES IN APPLOP=4
				;IN CASE THIS IS A NUMERIC COMPARE
	LD	HL,DOCMP	;ROUTINE TO TAKE COMPARE ROUTINE RESULT
				;AND RELATIONAL BITS AND RETURN THE ANSWER
	PUSH	HL		;DOES A JMP TO RETAOP WHEN DONE
	CALL	GETYPR		;SEE IF WE HAVE A NUMERIC COMPARE
	JP	NZ,NUMREL	;YES, BUILD AN APPLOP ENTRY
	LD	HL,(FACLO)	;GET THE POINTER AT THE STRING DESCRIPTOR
	PUSH	HL		;SAVE IT FOR STRCMP
	LD	BC,STRCMP	;STRING COMPARE ROUTINE
	JP	FINTMP		;PUSH THE ADDRESS, REGET THE TEXT POINTER
				;SAVE THE PRECEDENCE AND SCAN
				;MORE OF THE FORMULA

;
;	APPLOP is returned to when it is time to apply an arithmetic
;	or numeric comparison operation.
;	The stack has a double byte entry with the operator
;	number and the VALTYP of the value on the stack.
;	APPLOP decides what value level the operation
;	will occur at, and converts the arguments. APPLOP
;	uses different calling conventions for each value type.
;	Integers: Left in [D,E]     Right in [H,L]
;	Singles:  Left in [B,C,D,E] Right in the FAC
;	Doubles:  Left in FAC       Right in ARG
;
APPLOP	POP	BC		;[B]=STACK OPERAND VALUE TYPE
				;[C]=OPERATOR OFFSET
	LD	A,C		;SAVE IN MEMORY SINCE THE STACK WILL BE BUSY
	LD	(DORES),A	;A RAM LOCATION
	LD	A,(VALTYP)	;GET VALTYP OF FAC
	CP	B		;ARE VALTYPES THE SAME?
	JP	NZ,VALNSM	;NO
	CP	02H		;INTEGER?
	JP	Z,INTDPC	;YES, DISPATCH!!
	CP	04H		;SINGLE?
	JP	Z,SNGDPC	;YES, DISPATCH!!
	JP	NC,DBLDPC	;MUST BE DOUBLE, DISPATCH!!
VALNSM	LD	D,A		;SAVE IN [D]
	LD	A,B		;CHECK FOR DOUBLE
	CP	08H		;PRECISION ENTRY ON THE STACK
	JP	Z,STKDBL	;FORCE FAC TO DOUBLE
	LD	A,D		;GET VALTYPE OF FAC
	CP	08H		;AND IF SO, CONVERT THE STACK OPERAND
	JP	Z,FACDBL	;TO DOUBLE PRECISION
	LD	A,B		;SEE IF THE STACK ENTRY IS SINGLE
	CP	04H		;PRECISION AND IF SO, CONVERT
	JP	Z,STKSNG	;THE FAC TO SINGLE PRECISION
	LD	A,D		;SEE IF THE FAC IS SINGLE PRECISION
	CP	03H		;AND IF SO CONVERT THE STACK TO SINGLE
	JP	Z,TMERR		;BLOW UP ON RIGHT HAND STRING OPERAND
	JP	NC,FACSNG	;AND IF SO CONVERT THE STACK TO SINGLE
				;PRECISION
INTDPC				;NOTE: THE STACK MUST BE INTEGER AT THIS POINT
	LD	HL,INTDSP	;INTEGER INTEGER CASE
	LD	B,00H		;SPECIAL DISPATCH FOR SPEED
	ADD	HL,BC		;[H,L] POINTS TO THE ADDRESS TO GO TO
	ADD	HL,BC
	LD	C,(HL)		;[B,C]=ROUTINE ADDRESS
	INC	HL
	LD	B,(HL)
	POP	DE		;[D,E]=LEFT HAND OPERAND
	LD	HL,(FACLO)	;[H,L]=RIGHT HAND OPERAND
	PUSH	BC		;DISPATCH
	RET

;
;	The stack operand is double precision, so
;	the FAC must be forced to double precision, moved into ARG
;	and the stack value poped into the FAC
;
STKDBL	CALL	CDBL		;MAKE THE FAC DOUBLE PRECISION
DBLDPC	CALL	VMOVAF		;MOVE THE FAC INTO ARG
	POP	HL		;POP OFF THE STACK OPERAND INTO THE FAC
	LD	(DFACLO2),HL
	POP	HL
	LD	(DFACLO),HL	;STORE LOW BYTES AWAY
SNGDBL	POP	BC		;POP OFF A FOUR BYTE VALUE
	POP	DE
	CALL	$MOVFR		;INTO THE FAC
SETDB1	CALL	CDBL		;MAKE SURE THE LEFT OPERAND IS
				;DOUBLE PRECISION
	LD	HL,DBLDSP	;DISPATCH TO A DOUBLE PRECISION ROUTINE
DODSP	LD	A,(DORES)	;RECALL WHICH OPERAND IT WAS
	RLCA			;CREATE A DISPATCH OFFSET, SINCE
				;TABLE ADDRESSES ARE TWO BYTES
	ADD	A,L		;ADD LOW BYTE OF ADDRESS
	LD	L,A		;SAVE BACK
	ADC	A,H		;ADD HIGH BYTE
	SUB	L		;SUBTRACT LOW
	LD	H,A		;RESULT BACK
	LD	A,(HL)		;FETCH THE ADDRESS
	INC	HL
	LD	H,(HL)
	LD	L,A
	JP	(HL)		;AND PERFORM THE OPERATION, RETURNING
				;TO RETAOP, EXCEPT FOR COMPARES WHICH
				;RETURN TO DOCMP

;
;	The FAC is double precision and the stack is either
;	integer or single precision and must be converted
;
FACDBL	LD	A,B		;GET THE VALUE TYPE INTO [A]
	PUSH	AF		;SAVE THE STACK VALUE TYPE
	CALL	VMOVAF		;MOVE THE FAC INTO ARG
	POP	AF		;POP THE STACK VALUE TYPE INTO [A]
	LD	(VALTYP),A	;PUT IT IN VALTYP FOR THE FORCE
				;ROUTINE
	CP	04H		;SEE IF ITS SINGLE, SO WE KNOW
				;HOW TO POP THE VALUE OFF
	JP	Z,SNGDBL	;IT'S SINGLE PRECISION
				;SO DO A POPR / CALL MOVFR
	POP	HL		;POP OFF THE INTEGER VALUE
	LD	(FACLO),HL	;SAVE IT FOR CONVERSION
	JP	SETDB1		;SET IT UP

;
;	This is the case where the stack is single precision
;	and the FAC is either single precision or integer
;
STKSNG	CALL	CSNG		;CONVERT THE FAC IF NECESSARY
SNGDPC	POP	BC		;PUT THE LEFT HAND OPERAND IN THE REGISTERS
	POP	DE
SNGDO	LD	HL,SNGDSP	;SETUP THE DISPATCH ADDRESS
				;FOR THE SINGLE PRECISION OPERATOR ROUTINES
	JP	DODSP		;DISPATCH

;	This is the case where the FAC is single precision and the stack
;	is an integer.
FACSNG	POP	HL		;POP OFF THE INTEGER ON THE STACK
	CALL	PUSHF		;SAVE THE FAC ON THE STACK
	CALL	CONSIH		;CONVERT [H,L] TO A SINGLE PRECISION
				;NUMBER IN THE FAC
	CALL	$MOVRF		;PUT THE LEFT HAND OPERAND IN THE REGISTERS
	POP	HL		;RESTORE THE FAC
	LD	(FACHI),HL	;FROM THE STACK
	POP	HL
	LD	(FACLO),HL
	JP	SNGDO		;PERFORM THE OPERATION

;	Here to do integer division. Since we want 1/3 to be
;	.333333 and not zero we have to force both arguments
;	to be single-precision floating point numbers
;	and use FDIV
INTDIV	PUSH	HL		;SAVE THE RIGHT HAND ARGUMENT
	EX	DE,HL		;[H,L]=LEFT HAND ARGUMENT
	CALL	CONSIH		;CONVERT [H,L] TO A SINGLE-PRECISION
				;NUMBER IN THE FAC
	POP	HL		;GET BACK THE RIGHT HAND ARGUMENT
	CALL	PUSHF		;PUSH THE CONVERTED LEFT HAND ARGUMENT
				;ONTO THE STACK
	CALL	CONSIH		;CONVERT THE RIGHT HAND ARGUMENT TO A
				;SINGLE PRECISION NUMBER IN THE FAC
	JP	$FDIVS?
				;REGISTERS THE LEFT HAND ARGUMENT

;-----------------------------------------------------------------------------
;	EVAL - EVALUATE VARIABLE, CONSTANT, FUNCTION CALL

;	Get next expression value (a.k.a. "OPRND" !)
EVAL	CALL	CHRGTR
	JP	Z,MOERR		;TEST FOR MISSING OPERAND - IF NONE GIVE ERROR
	JP	C,FIN		;IF NUMERIC, INTERPRET CONSTANT
	CALL	ISLET2		;VARIABLE NAME?
	JP	NC,ISVAR	;AN ALPHABETIC CHARACTER MEANS YES
	CP	DBLCON+1	;IS IT AN EMBEDED CONSTANT
	JP	C,CONFAC	;RESCAN THE TOKEN & RESTORE OLD TEXT PTR
	INC	A		;IS IT A FUNCTION CALL (PRECEDED BY 377)
	JP	Z,ISFUN		;YES, DO IT
	DEC	A		;FIX A BACK
	CP	PLUSTK		;IGNORE "+"
	JP	Z,EVAL
	CP	MINUTK		;NEGATION?
	JP	Z,DOMIN
	CP	'"'		;STRING CONSTANT?
	JP	Z,STRLT1	;IF SO BUILD A DESCRIPTOR IN A TEMPORARY
				;DESCRIPTOR LOCATION AND PUT A POINTER TO THE
				;DESCRIPTOR IN FACLO.
	CP	NOTTK		;CHECK FOR "NOT" OPERATOR
	JP	Z,NOTER
	CP	'&'		;OCTAL CONSTANT?
	JP	Z,OCTCNS
	CP	ERRTK
	JP	NZ,NTERC	;NO, TRY OTHER POSSIBILITIES
	CALL	CHRGTR		;GRAB FOLLOWING CHAR
	LD	A,(ERRFLG)	;GET THE ERROR CODE.
	PUSH	HL		;SAVE TEXT POINTER
	CALL	SNGFLT		;RETURN THE VALUE
	POP	HL		;RESTORE TEXT POINTER
	RET			;ALL DONE.

NTERC	CP	ERLTK		;ERROR LINE NUMBER VARIABLE.
	JP	NZ,NTERL	;NO, TRY MORE THINGS.
;	'ERL' BASIC function
	CALL	CHRGTR		;GET FOLLOWING CHARACTER
	PUSH	HL		;SAVE TEXT POINTER
	LD	HL,(ERRLIN)	;GET THE OFFENDING LINE #
	CALL	MAKSNG		;FLOAT 2 BYTE UNSINGED INT
	POP	HL		;RESTORE TEXT POINTER
	RET			;RETURN

NTERL	CP	VARPTRTK	;VARPTR CALL?
	JP	NZ,NTVARP	;NO
	CALL	CHRGTR		;EAT CHAR AFTER
	CALL	SYNCHR
	DB	'('		;EAT LEFT PAREN
	CP	'#'		;WANT POINTER TO FILE?
	JP	NZ,NVRFIL	;NO, MUST BE VARIABLE
	CALL	GTBYTC		;READ FILE #
	PUSH	HL		;SAVE TEXT PTR
	CALL	GETPTR		;GET PTR TO FILE
	POP	HL		;RESTORE TEXT PTR
	JP	VARRET

NVRFIL	CALL	PTRGTN		;GET ADDRESS OF VARIABLE
VARRET	CALL	SYNCHR
	DB	')'		;EAT RIGHT PAREN
	PUSH	HL		;SAVE TEXT POINTER
	EX	DE,HL		;GET VALUE TO RETURN IN [H,L]
	LD	A,H		;MAKE SURE NOT UNDEFINED VAR
	OR	L		;SET CC'S. ZERO IF UNDEF
	JP	Z,FCERR		;ALL OVER IF UNDEF (DONT WANT
				;USER POKING INTO ZERO IF HE'S
				;TOO LAZY TO CHECK
	CALL	MAKINT		;MAKE IT AN INT
	POP	HL		;RESTORE TEXT POINTER
	RET

;	(continued)..get next expression value
NTVARP	CP	USRTK		;USER ASSEMBLY LANGUAGE ROUTINE??
	JP	Z,USRFN		;GO HANDLE IT
	CP	INSTRTK		;IS IT THE INSTR FUNCTION??
	JP	Z,INSTR
	CP	INKEY$TK	;INKEY$ FUNCTION?
	JP	Z,INKEY$
	CP	STRING$TK	;STRING FUNCTION?
	JP	Z,STRNG$
	CP	INPUTTK		;FIXED LENGTH INPUT?
	JP	Z,FIXINP
	CP	FNTK		;USER-DEFINED FUNCTION?
	JP	Z,FNDOER	;NUMBERED CHARACTERS ALLOWED
				;SO THERE IS NO NEED TO CHECK
				;THE UPPER BOUND
;	Only possibility left is a formula in parentheses
PARCHK	CALL	FRMPRN		;RECURSIVELY EVALUATE THE FORMULA
	CALL	SYNCHR
	DB	')'
	RET

;	'-', deal with minus sign
DOMIN	LD	D,7DH		;A PRECEDENCE BELOW ^
				;BUT ABOVE ALL ELSE
	CALL	LPOPER		;SO ^ GREATER THAN UNARY MINUS
	LD	HL,(TEMP2)	;GET TEXT POINTER
	PUSH	HL
	CALL	VNEG
LABBCK:				;FUNCTIONS THAT DON'T RETURN
				;STRING VALUES COME BACK HERE
	POP	HL
	RET

;	Got a variable name
;	(a.k.a. CONVAR)
ISVAR	CALL	PTRGET		;GET A POINTER TO THE
				;VARIABLE IN [D,E]
RETVAR	PUSH	HL		;SAVE THE TEXT POINTER
	EX	DE,HL		;PUT THE POINTER TO THE VARIABLE VALUE
				;INTO [H,L]. IN THE CASE OF A STRING
				;THIS IS A POINTER TO A DESCRIPTOR AND NOT
				;AN ACTUAL VALUE
	LD	(FACLO),HL	;IN CASE IT'S STRING STORE THE POINTER
				;TO THE DESCRIPTOR IN FACLO.
	CALL	GETYPR		;FOR STRINGS WE JUST LEAVE
	CALL	NZ,VMOVFM	;A POINTER IN THE FAC
				;THE FAC USING [H,L] AS THE POINTER.
	POP	HL		;RESTORE THE TEXT POINTER
	RET

;	Get char from (HL) and make upper case
MAKUPL	LD	A,(HL)		;GET CHAR FROM MEMORY
;	Make char in 'A' upper case
MAKUPS	CP	'a'		;IS IT LOWER CASE RANGE
	RET	C		;LESS
	CP	'z'+1		;GREATER
	RET	NC		;TEST
	AND	5FH		;MAKE UPPER CASE
	RET			;DONE

;	This entry point seems unreferenced.
CNSGET	CP	'&'		;OCTAL PERHAPS?
	JP	NZ,LINGET

;	OCTAL, HEX or other specified base (ASCII) to FP number
OCTCNS	LD	DE,0000H	;INITIALIZE TO ZERO AND IGNORE OVERFLOW
	CALL	CHRGTR		;GET FIRST CHAR
	CALL	MAKUPS		;MAKE UPPER IF NESC.
	CP	'O'		;OCTAL?
	JP	Z,LOPOCT	;IF SO, DO IT
	CP	'H'		;HEX?
	JP	NZ,LOPOC2	;THEN DO IT
	LD	B,05H		;INIT DIGIT COUNT
LOPHEX	INC	HL		;BUMP POINTER
	LD	A,(HL)		;GET CHAR
	CALL	MAKUPS		;MAKE UPPER CASE
	CALL	ISLET2		;FETCH CHAR, SEE IF ALPHA
	EX	DE,HL		;SAVE [H,L]
	JP	NC,ALPTST	;YES, MAKE SURE LEGAL HEC
	CP	'9'+1		;IS IT BIGGER THAN LARGEST DIGIT?
	JP	NC,HOCFIN	;YES, BE FORGIVING & RETURN
	SUB	'0'		;CONVERT DIGIT, MAKE BINARY
	JP	C,HOCFIN	;BE FORGIVING IF NOT HEX DIGIT
	JP	NXTHEX		;ADD IN OFFSET

ALPTST	CP	'F'+1		;IS IT LEGAL HEX?
	JP	NC,HOCFIN	;YES, TERMINATE
	SUB	'A'-10		;MAKE BINARY VALUE
NXTHEX	ADD	HL,HL		;SHIFT RIGHT FOUR BITS
	ADD	HL,HL
	ADD	HL,HL
	ADD	HL,HL
	OR	L		;OR ON NEW DIGIT
	LD	L,A		;SAVE BACK
	EX	DE,HL		;GET TEXT POINTER BACK IN [H,L]
	DEC	B
	JP	NZ,LOPHEX	;KEEP EATING IF NOT TOO MANY DIGITS

	JP	OVERR		;Overflow

LOPOC2	DEC	HL		;REGET LAST CHARACTER
LOPOCT	CALL	CHRGTR		;READ A DIGIT
	EX	DE,HL		;RESULT INTO [H,L]
	JP	NC,HOCFIN	;OUT OF DIGITS MEANS DONE
	CP	38H		;IS THIS AN OCTAL DIGIT
	JP	NC,SNERR	;NO, TOO BAD YOU WILL LOSE
	LD	BC,OVERR	;WHERE TO GO ON OVERFLOW ERROR
	PUSH	BC		;SAVE ADDR ON STACK
	ADD	HL,HL		;MULTIPLY BY EIGHT
	RET	C		;OVERFLOW ERROR
	ADD	HL,HL
	RET	C		;OVERFLOW ERROR
	ADD	HL,HL
	RET	C		;OVERFLOW ERROR
	POP	BC		;GET RID OF OVERR ADDR
	LD	B,00H		;SETUP TO ADD [B,C]
	SUB	'0'
	LD	C,A
	ADD	HL,BC		;ADD IN THE DIGIT
	EX	DE,HL		;PUT TEXT POINTER BACK IN [H,L]
	JP	LOPOCT		;SCAN MORE DIGITS

HOCFIN	CALL	MAKINT		;SAVE AS AN INTEGER
	EX	DE,HL		;[H,L]-TEXT POINTER
	RET

ISFUN	INC	HL		;BUMP SOURCE TEXT POINTER
	LD	A,(HL)		;GET THE ACTUAL TOKEN FOR FN
	SUB	80H+ONEFUN	;MAKE INTO OFFSET
	CP	RNDTK-ONEFUN
	JP	NZ,NTMRND
	PUSH	HL
	CALL	CHRGTR
	CP	28H
	POP	HL
	JP	NZ,RNDMON
	LD	A,RNDTK-ONEFUN
NTMRND	LD	B,00H
	RLCA			;MULTIPLY BY 2
	LD	C,A
	PUSH	BC		;SAVE THE FUNCTION # ON THE STACK
	CALL	CHRGTR
	LD	A,C		;LOOK AT FUNCTION #
	CP  (MID$TK-ONEFUN)*2+1	;IS IT PAST LASNUM?
	JP	NC,OKNORM	;NO, MUST BE A NORMAL FUNCTION
;
;	MOST FUNCTIONS TAKE A SINGLE ARGUMENT.
;	THE RETURN ADDRESS OF THESE FUNCTIONS IS A SMALL ROUTINE
;	THAT CHECKS TO MAKE SURE VALTYP IS 0 (NUMERIC) AND POPS OFF
;	THE TEXT POINTER. SO NORMAL FUNCTIONS THAT RETURN STRING RESULTS (I.E. CHR$)
;	MUST POP OFF THE RETURN ADDRESS OF LABBCK, AND POP OFF THE
;	TEXT POINTER AND THEN RETURN TO FRMEVL.
;
;	THE SO CALLED "FUNNY" FUNCTIONS CAN TAKE MORE THAN ONE ARGUMENT.
;	THE FIRST OF WHICH MUST BE STRING AND THE SECOND OF WHICH
;	MUST BE A NUMBER BETWEEN 0 AND 256. THE TEXT POINTER IS
;	PASSED TO THESE FUNCTIONS SO ADDITIONAL ARGUMENTS
;	CAN BE READ. THE TEXT POINTER IS PASSED IN [D,E].
;	THE CLOSE PARENTHESIS MUST BE CHECKED AND RETURN IS DIRECTLY
;	TO FRMEVL WITH [H,L] SETUP AS THE TEXT POINTER POINTING BEYOND THE ")".
;	THE POINTER TO THE DESCRIPTOR OF THE STRING ARGUMENT
;	IS STORED ON THE STACK UNDERNEATH THE VALUE OF THE INTEGER
;	ARGUMENT (2 BYTES)
;
;	FIRST ARGUMENT ALWAYS STRING -- SECOND INTEGER
;
	CALL	FRMPRN		;EAT OPEN PAREN AND FIRST ARG
	CALL	SYNCHR
	DB	','		;TWO ARGS SO COMMA MUST DELIMIT
	CALL	CHKSTR		;MAKE SURE THE FIRST ONE WAS STRING
	EX	DE,HL		;[D,E]=TXTPTR
	LD	HL,(FACLO)	;GET PTR AT STRING DESCRIPTOR
	EX	(SP),HL		;GET FUNCTION #
				;SAVE THE STRING PTR
	PUSH	HL		;PUT THE FUNCTION # ON
	EX	DE,HL		;[H,L]=TXTPTR
	CALL	GETBYT		;[E]=VALUE OF FORMULA
	EX	DE,HL		;TEXT POINTER INTO [D,E]
				;[H,L]=INT VALUE OF SECOND ARGUMENT
	EX	(SP),HL		;SAVE INT VALUE OF SECOND ARG
				;[H,L]=FUNCTION NUMBER
	JP	FINGO		;DISPATCH TO FUNCTION

;	a.k.a. FNVAL
OKNORM	CALL	PARCHK		;CHECK OUT THE ARGUMENT
				;AND MAKE SURE ITS FOLLOWED BY ")"
	EX	(SP),HL		;[H,L]=FUNCTION # AND SAVE TEXT POINTER
;
;	Check if special coercion must be done for one of the transcendental
;	functions (RND, SQR, COS, SIN, TAN, ATN, LOG, and EXP)
;	These functions do not look at VALTYP, but rather assume the
;	argument passed in the FAC is single precision, so FRCSNG
;	must be called before dispatching to them.
;
	LD	A,L		;[A]=FUNCTION NUMBER
	CP     (SQRTK-ONEFUN)*2	;LESS THAN SQUARE ROOT?
	JP	C,NOTFRF	;DON'T FORCE THE ARGUMENT
	CP   (ATNTK-ONEFUN)*2+1	;BIGGER THAN ARC-TANGENT?
	PUSH	HL		;SAVE THE FUNCTION NUMBER
	CALL	C,CSNG		;IF NOT, FORCE FAC TO SINGLE-PRECISION
	POP	HL		;RESTORE THE FUNCTION NUMBER
NOTFRF	LD	DE,LABBCK	;RETURN ADDRESS
	PUSH	DE		;MAKE THEM REALLY COME BACK
	LD	A,01H		;FUNCTION SHOULD ONLY PRINT OVERFLOW ONCE
	LD	(FLGOVC),A
FINGO	LD	BC,FUNDSP	;FUNCTION DISPATCH TABLE
DISPAT	ADD	HL,BC		;ADD ON THE OFFSET
	LD	C,(HL)		;FASTER THAN PUSHM
	INC	HL
	LD	H,(HL)
	LD	L,C
	JP	(HL)		;DISPATCH

;	The following routine is called from FIN in F4
;	to scan leading signs for numbers. It was moved
;	to F3 to eliminate byte externals
;
;	test '+', '-'..
MINPLS	DEC	D		;SET SIGN OF EXPONENT FLAG
	CP	MINUTK		;NEGATIVE EXPONENT?
	RET	Z
	CP	'-'
	RET	Z
	INC	D		;NO, RESET FLAG
	CP	'+'
	RET	Z
	CP	PLUSTK		;IGNORE "+"
	RET	Z
	DEC	HL		;CHECK IF LAST CHARACTER WAS A DIGIT
	RET			;RETURN WITH NON-ZERO SET


;-----------------------------------------------------------------------------
;	MORE FORMULA EVALUATION - LOGICAL, RELATIONAL OPS
; ## GWEVAL.ASM:1024 ##
;
DOCMP	INC	A		;SETUP BITS
	ADC	A,A		;4=LESS 2=EQUAL 1=GREATER
	POP	BC		;WHAT DID HE WANT?
	AND	B		;ANY BITS MATCH?
	ADD	A,0FFH		;MAP 0 TO 0
	SBC	A,A		;AND ALL OTHERS TO 377
	CALL	CONIA		;CONVERT [A] TO AN INTEGER SIGNED
	JP	RETAPG		;RETURN FROM OPERATOR APPLICATION

;	'NOT' boolean expression
NOTER	LD	D,5AH		;"NOT" HAS PRECEDENCE 90, SO
	CALL	LPOPER		;FORMULA EVALUATION IS ENTERED WITH A DUMMY
				;ENTRY OF 90 ON THE STACK
	CALL	CINT		;COERCE THE ARGUMENT TO INTEGER
	LD	A,L		;NOT [H,L]
	CPL
	LD	L,A
	LD	A,H
	CPL
	LD	H,A
	LD	(FACLO),HL	;UPDATE THE FAC
	POP	BC		;FRMEVL, AFTER SEEING THE PRECEDENCE
				;OF 90 THINKS IT IS APPLYING AN OPERATOR
				;SO IT HAS THE TEXT POINTER IN TEMP2 SO
RETAPG	JP	RETAOP		;RETURN TO REFETCH IT

;	Get the VALTYP and set condition codes as follows:
;	CONDITION CODE		TRUE SET	FALSE SET
;
;	Sign			INT=2		STR,SNG,DBL
;	Zero			STR=3		INT,SNG,DBL
;	Odd Parity		SNG=4		INT,STR,DBL
;	No Carry		DBL=8		INT,STR,SNG
GETYPR	LD	A,(VALTYP)	;REPLACEMENT FOR "GETYPE" RST
	CP	08H
	JP	NC,CGETYP	;SPLIT OFF NO CARRY CASE
	SUB	03H		;SET A CORRECTLY
	OR	A		;NOW SET LOGICAL'S OK
	SCF			;CARRY MUST BE SET
	RET			;ALL DONE
;
; 	Continuation of GETYPE RST
;
CGETYP	SUB	03H		;SET THE OTHER CONDITION CODES CORRECTLY
	OR	A		; WITHOUT AFFECTING CARRY
	RET			;RETURN

;	DANDOR applies the "AND" and "OR" operators
;	and should be used to implement all logical operators.
;	Whenever an operator is applied, its precedence is in [B].
;	This fact is used to distinguish between "AND" and "OR".
;	The right hand argument is coerced to integer, just as
;	the left hand one was when it was pushed on the stack.
DANDOR	LD	A,B		;GET THE PRECEDENCE
	PUSH	AF		;SAVE THE PRECEDENCE "OR"=70
	CALL	CINT		;COERCE RIGHT HAND ARGUMENT TO INTEGER
	POP	AF		;GET BACK THE PRECEDENCE TO DISTINGUISH
				;"AND" AND "OR"
	POP	DE		;POP OFF THE LEFT HAND ARGUMENT
	CP	7AH		;IS THE OPERATOR "MOD"?
	JP	Z,IMOD		;IF SO, USE MONTE'S SPECIAL ROUTINE
	CP	7BH		;IS THE OPERATOR "IDIV"?
	JP	Z,IDIV		;LET MONTE HANDLE IT
	LD	BC,GIVINT	;PLACE TO RETURN WHEN DONE
	PUSH	BC		;SAVE ON STACK
	CP	46H		;SET ZERO FOR "OR"
	JP	NZ,NOTOR
;	'OR' boolean expression
	LD	A,E
	OR	L		;OR L,E
	LD	L,A
	LD	A,H
	OR	D		;OR H,D
	RET			;RETURN THE INTEGER [A,L]

NOTOR	CP	50H		;AND?
	JP	NZ,NOTAND
;	'AND' boolean expression
	LD	A,E
	AND	L		;AND L,E
	LD	L,A
	LD	A,H
	AND	D		;AND H,D
	RET			;RETURN THE INTEGER [A,L]

NOTAND	CP	3CH		;XOR?
	JP	NZ,NOTXOR	;NO
;	'XOR' boolean expression
	LD	A,E
	XOR	L		;XOR L,E
	LD	L,A
	LD	A,H
	XOR	D		;XOR H,D
	RET			;RETURN THE INTEGER [A,L]

;	For "EQV" use A EQV B = NOT(A XOR B)
NOTXOR	CP	32H		;EQV?
	JP	NZ,NOTEQV	;NO
;	'EQV' boolean expression
	LD	A,E
	XOR	L		;XOR L,E
	CPL			;NOT L
	LD	L,A
	LD	A,H
	XOR	D		;XOR H,D
	CPL			;NOT H
	RET			;RETURN THE INTEGER [A,L]

;	For "IMP" use A IMP B = NOT(A AND NOT(B))
NOTEQV	LD	A,L
	CPL			;NOT L
;	'IMP' expression
	AND	E		;AND L,E
	CPL			;NOT L
	LD	L,A
	LD	A,H
	CPL			;NOT H
	AND	D		;AND H,D
	CPL			;NOT H
	RET			;RETURN THE INTEGER [A,L]

;	This routine subtracts [D,E] from [H,L]
;	and floats the result leaving it in FAC.
GIVDBL	LD	A,L		;[H,L]=[H,L]-[D,E]
	SUB	E
	LD	L,A
	LD	A,H
	SBC	A,D
	LD	H,A
	JP	MAKSNG		;FLOAT 2 BYTE UNSIGNED INT

;	'LPOS' BASIC command
;	Return printer position in line
LPOS	LD	A,(LPTPOS)	;MAKE [A] AN UNSIGNED INTEGER
	JP	POSC

;	'POS' BASIC instruction
;	Return console position in line
POS	LD	A,(TTYPOS)	;GET TELETYPE POSITION
;	SEE WHERE WE ARE  (LPT or TTY)
POSC	INC	A		;IN ADDS VERSION TAB POSITIONS START AT COLUMN 1.
;	Exit from function, result in A
SNGFLT	LD	L,A		;MAKE [A] AN UNSIGNED INTEGER
	XOR	A
GIVINT	LD	H,A
	JP	MAKINT		;or CONISS: convert result to INT


;-----------------------------------------------------------------------------
;	USER DEFINED (USR) ASSEMBLY LANGUAGE FUNCTION CODE
; ## GWEVAL.ASM:1123 ##
;
;	Note: no check for direct mode in protected environment !!
USRFN	CALL	SCNUSR		;SCAN THE USR#
	PUSH	DE		;SAVE POINTER
	CALL	PARCHK		;EAT LEFT PAREN AND FORMULA
	EX	(SP),HL		;SAVE TEXT POINTER & GET INDEX INTO USRTAB
	LD	E,(HL)		;GET DISPATCH ADDRESS
	INC	HL		;BUMP POINTER
	LD	D,(HL)		;PICK UP 2ND BYTE OF ADDRESS
	LD	HL,POPHRT	;GET ADDRESS OF POP H RET
	PUSH	HL		;PUSH ON SEGMENT ADDRESS OF SUBROUTINE
	PUSH	DE		;SAVE ADDRESS OF USR ROUTINE
	LD	A,(VALTYP)	;GET ARGUMENT TYPE IN [A]
	PUSH	AF		;SAVE VALTYP
	CP	03H		;STRING??
	CALL	Z,FREFAC	;FREE IT UP
	POP	AF		;GET BACK VALTYP
	EX	DE,HL		;MOVE POSSIBLE DESC. POINTER TO [D,E]
	LD	HL,FACLO	;POINTER TO FAC IN [H,L]
	RET			;RETURN

SCNUSR	CALL	CHRGTR		;GET A CHAR
	LD	BC,0000H	;ASSUME USR0
	CP	ONECON+10	;SINGLE BYTE INT EXPECTED
	JP	NC,NOARGU	;NO, MUST BE DEFAULTING TO USR0
	CP	ONECON		;IS IT SMALLER THAN ONECON?
	JP	C,NOARGU	;YES, ASSUME TRYING TO DEFAULT TO USR0
	CALL	CHRGTR		;SCAN PAST NEXT CHAR
	LD	A,(CONLO)	;GET VALUE OF 1 BYTER
	OR	A		;MAKE SURE CARRY IS OFF
	RLA			;MULTIPLY BY 2
	LD	C,A		;SAVE OFFSET IN [C]
NOARGU	EX	DE,HL		;SAVE TEXT POINTER IN [D,E]
	LD	HL,USRTAB	;GET START OF TABLE
	ADD	HL,BC		;ADD ON OFFSET
	EX	DE,HL		;RESTORE TEXT POINTER, ADDRESS TO [D,E]
	RET			;RETURN FROM SCAN ROUTINE

DEFUSR	CALL	SCNUSR		;SCAN THE USR NAME
	PUSH	DE		;SAVE POINTER TO USRTAB ENTRY
	CALL	SYNCHR
	DB	EQULTK		;MUST HAVE EQUAL SIGN
	CALL	FRMQNT		;GET THE ADDRESS
	EX	(SP),HL		;TEXT POINTER TO STACK, GET ADDRESS
	LD	(HL),E		;SAVE USR CALL ADDRESS
	INC	HL		;BUMP POINTER
	LD	(HL),D		;SAVE HIGH BYTE OF ADDRESS
	POP	HL		;RESTORE TEXT POINTER
	RET			;RETURN TO NEWSTT


;-----------------------------------------------------------------------------
;	SIMPLE-USER-DEFINED-FUNCTION CODE
; ## GWEVAL.ASM:1190 ##
;
;	In the 8K version (SEE LATER COMMENT FOR EXTENDED)
;	Note Only single arguments are allowed to functions
;	and functions must be of the single line form:
;	DEF FNA(X)=X^2+X-2
;	No strings can be involved with these functions
;
;	Idea: Create a funny simple variable entry
;	whose first character (second word in memory)
;	has the 200 bit set.
;	The value will be:
;
;	      A TXTPTR to the formula
;	      The name of the parameter variable
;
;	Function names can be like "FNA4"
;
DEFST	CP	USRTK		;DEFINING THE CALL ADDRESS OF USR ROUTINE?
	JP	Z,DEFUSR	;YES, DO IT
	CALL	GETFNM		;GET A POINTER TO THE FUNCTION NAME
	CALL	ERRDIR		;DEF IS "ILLEGAL DIRECT"
	EX	DE,HL		;[D,E] = THE TEXT POINTER AFTER THE FUNCTION
	LD	(HL),E		;NAME AND [H,L] = POINTER AT PLACE TO STORE
	INC	HL		;VALUE OF THE FUNCTION VARIABLE
	LD	(HL),D		;SAVE THE TEXT POINTER AS THE VALUE
	EX	DE,HL		;RESTORE THE TEXT POINTER TO [H,L]
	LD	A,(HL)		;GET NEXT CHAR
	CP	'('		;DOES THIS FUNCTION HAVE ARGS?
	JP	NZ,DATA		;NO
	CALL	CHRGTR
SCNLIS	CALL	PTRGET		;GET POINTER TO DUMMY VAR(CREATE VAR)
	LD	A,(HL)		;GET TERMINATOR
	CP	')'		;END OF ARG LIST?
	JP	Z,DATA		;YES
	CALL	SYNCHR
	DB	','		;"," MUST FOLLOW THEN
	JP	SCNLIS

FNDOER	CALL	GETFNM		;GET A POINTER TO
	LD	A,(VALTYP)	;FIND OUT WHAT KIND OF FUNCTION IT IS
	OR	A		;PUSH THIS [A] ON WITH A PSW WITH CARRY OFF
				;SO THAT WHEN VALUES ARE BEING POPPED OFF
				;AND RESTORED TO PARAMETERS WE WILL KNOW
				;WHEN TO STOP
				;WHEN A VALTYP IS POPPED OFF WITH
				;CARRY OFF
	PUSH	AF		;SAVE SO THAT THE FINAL RESULT WILL
				;BE COERCED TO THE FUNCTION TYPE
	LD	(TEMP2),HL	;SAVE THE TEXT POINTER THAT POINTS PAST
				;THE FUNCTION NAME IN THE CALL
	EX	DE,HL		;[H,L]=A POINTER TO THE VALUE OF FUNCTION
	LD	A,(HL)		;[H,L]=VALUE OF THE FUNCTION
	INC	HL		;WHICH IS A TEXT POINTER AT THE FORMAL
	LD	H,(HL)		;PARAMETER LIST IN THE DEFINITION
	LD	L,A
	LD	A,H		;A ZERO TEXT POINTER MEANS THE FUNCTION
	OR	L		;WAS NEVER DEFINED
	JP	Z,UFERR		;IF SO, GIVEN AN "UNDEFINED FUNCTION" ERROR
	LD	A,(HL)		;SEE IF THERE ARE ANY PARAMETERS
	CP	'('		;PARAMETER LIST STARTS WITH "(""
	JP	NZ,FINVLS	;SKIP OVER PARAMETER SETUP
	CALL	CHRGTR		;GO PAST THE "("
	LD	(TEMP3),HL	;SAVE THE TEXT POINTER TO THE START OF THE
	EX	DE,HL		;PARAMETER LIST.
	LD	HL,(TEMP2)	;NOW GET THE TEXT-POINTER FROM THE CALL
				;WHICH IS POINTING JUST PAST THE
				;FUNCTION NAME AT THE ARGUMENT LIST
	CALL	SYNCHR
	DB	'('		;MAKE SURE THE ARGUMENT LIST IS THERE
	XOR	A		;INDICATE END OF VALUES TO ASSIGN
	PUSH	AF
	PUSH	HL		;SAVE THE CALLERS TEXT POINTER
	EX	DE,HL		;GET THE POINTER TO THE BEGINNING OF THE
				;PARAMETER LIST
ASGMOR	LD	A,80H		;OUTLAW ARRAYS WHEN SCANNING
	LD	(SUBFLG),A	;PARAMETERS
	CALL	PTRGET		;READ A PARAMETER
	EX	DE,HL		;[D,E]=PARAMETER LIST TEXT,[H,L]=VARIABLE POINTER
	EX	(SP),HL		;SAVE THE VARIABLES POSITION AND
				;GET THE POINTER AT THE ARG LIST
	LD	A,(VALTYP)	;AND ITS TYPE (FOR COERCION)
	PUSH	AF
	PUSH	DE
	CALL	FRMEVL		;SAVE THE TEXT POINTER INTO THE PARAMETER
	LD	(TEMP2),HL	;EVALUATE THE ARGUMENT
	POP	HL		;SAVE THE ARGUMENT LIST POINTER
	LD	(TEMP3),HL	;AND THE PARAMETER LIST POINTER
	POP	AF
	CALL	DOCNVF		;GET THE VALUE TYPE
	LD	C,04H		;COERCE THE ARGUMENT
	CALL	GETSTK		;MAKE SURE THERE IS ROOM FOR THE VALUE
	LD	HL,0FFF8H
	ADD	HL,SP		;SAVE EIGHT PLACES
	LD	SP,HL
	CALL	VMOVMF
	LD	A,(VALTYP)	;PUT VALUE INTO RESERVED PLACE IN STACK
	PUSH	AF		;SAVE TYPE FOR ASSIGNMENT
	LD	HL,(TEMP2)
	LD	A,(HL)		;REGET THE ARGUMENT LIST POINTER
	CP	')'		;SEE WHAT COMES AFTER THE ARGUMENT FORMULA
	JP	Z,POPASG	;IS THE ARGUMENT LIST ENDING?
	CALL	SYNCHR		;MAKE SURE THE ARGUMENT LIST ALSO ENDED
	DB	','
	PUSH	HL		;SKIP OVER ARGUMENT COMMA
	LD	HL,(TEMP3)	;SAVE THE ARGUMENT LIST TEXT POINTER
				;GET THE TEXT POINTER INTO THE DEFINTION'S
	CALL	SYNCHR		;PARAMETER LIST
	DB	','
	JP	ASGMOR		;SKIP OVER THE PARAMETER LIST COMMA
				;AND BIND THE REST OF THE PARAMETERS
POPAS2	POP	AF		;IF ASSIGNMENT IS SUCESSFUL UPDATE PRMLN2
	LD	(PRMLN2),A	;INDICATE NEW VARIABLE IS IN PLACE
POPASG	POP	AF		;GET THE VALUE TYPE
	OR	A
	JP	Z,FINASG	;ZERO MEANS NO MORE LEFT TO POP AND ASSIGN
	LD	(VALTYP),A
	LD	HL,0000H	;POINT INTO STACK
	ADD	HL,SP		;TO GET SAVED VALUE
	CALL	VMOVFM		;PUT VALUE INTO FAC
	LD	HL,0008H	;FREE UP STACK AREA
	ADD	HL,SP
	LD	SP,HL
	POP	DE		;GET PLACE TO STORE TO
	LD	L,03H		;CALCULATE THE SIZE OF THE LOOKS (NAME)
LPSIZL	INC	L		;INCREMENT SIZE
	DEC	DE		;POINT AT PREVIOUS CHARACTER
	LD	A,(DE)		;SEE IF IT IS THE LENGTH OR ANOTHER CHARACTER
	OR	A
	JP	M,LPSIZL	;HIGH BIT INDICATES STILL PART OF NAME
	DEC	DE		;BACK UP OVER LOOKS
	DEC	DE
	DEC	DE
	LD	A,(VALTYP)	;GET SIZE OF VALUE
	ADD	A,L		;ADD ON SIZE OF NAME
	LD	B,A		;SAVE TOTAL LENGTH IN [B]
	LD	A,(PRMLN2)	;GET CURRENT SIZE OF BLOCK
	LD	C,A		;SAVE IN [C]
	ADD	A,B		;GET POTENTIAL NEW SIZE
	CP	PRMSIZ		;CAN'T EXCEED ALLOCATED STORAGE
	JP	NC,FCERR
	PUSH	AF		;SAVE NEW SIZE
	LD	A,L		;[A]=SIZE OF NAME
	LD	B,00H		;[B,C]=SIZE OF PARM2
	LD	HL,PARM2	;BASE OF PLACE TO STORE INTO
	ADD	HL,BC		;[H,L]=PLACE TO START THE NEW VARIABLE
	LD	C,A		;[B,C]=LENGTH OF NAME OF VARIABLE
	CALL	BCTRAN		;PUT IN THE NEW NAME
	LD	BC,POPAS2	;PLACE TO RETURN AFTER ASSIGNMENT
	PUSH	BC
	PUSH	BC		;SAVE EXTRA ENTRY ON STACK
	JP	LETCN4		;PERFORM ASSIGNMENT ON [H,L] (EXTRA POP D)

FINASG	LD	HL,(TEMP2)	;GET ARGUMENT LIST POINTER
	CALL	CHRGTR		;SKIP OVER THE CLOSING PARENTHESIS
	PUSH	HL		;SAVE THE ARGUMENT TEXT POINTER
	LD	HL,(TEMP3)	;GET THE PARAMETER LIST TEXT POINTER
	CALL	SYNCHR
	DB	')'		;MAKE SURE THE PARAMETER LIST
				;ENDED AT THE SAME TIME
	DB	3EH		;SKIP THE NEXT BYTE WITH "MVI AL,"
FINVLS	PUSH	DE		;HERE WHEN THERE WERE NO ARGUMENTS
				;OR PARAMETERS
				;SAVE THE TEXT POINTER OF THE CALLER
	LD	(TEMP3),HL	;SAVE THE TEXT POINTER OF THE FUNCTION
	LD	A,(PRMLEN)	;PUSH PARM1 STUFF ONTO THE STACK
	ADD	A,04H		;WITH PRMLEN AND PRMSTK (4 BYTES EXTRA)
	PUSH	AF		;SAVE THE NUMBER OF BYTES
	RRCA			;NUMBER OF TWO BYTE ENTRIES IN [A]
	LD	C,A
	CALL	GETSTK		;IS THERE ROOM ON THE STACK?
	POP	AF		;[A]=AMOUNT TO PUT ONTO STACK
	LD	C,A
	CPL			;COMPLEMENT [A]
	INC	A
	LD	L,A
	LD	H,0FFH
	ADD	HL,SP
	LD	SP,HL		;SET UP NEW STACK
	PUSH	HL		;SAVE THE NEW VALUE FOR PRMSTK
	LD	DE,PRMSTK	;FETCH DATA FROM HERE
	CALL	BCTRAN
	POP	HL
	LD	(PRMSTK),HL	;LINK PARAMETER BLOCK FOR GARBAGE COLLECTION
	LD	HL,(PRMLN2)	;NOW PUT PARM2 INTO PARM1
	LD	(PRMLEN),HL	;SET UP LENGTH
	LD	B,H		;[B,C]=TRANSFER COUNT
	LD	C,L
	LD	HL,PARM1
	LD	DE,PARM2
	CALL	BCTRAN
	LD	H,A		;CLEAR OUT PARM2
	LD	L,A
	LD	(PRMLN2),HL
	LD	HL,(FUNACT)	;INCREMENT FUNCTION COUNT
	INC	HL
	LD	(FUNACT),HL
	LD	A,H
	OR	L		;SET UP ACTIVE FLAG NON-ZERO
	LD	(NOFUNS),A
	LD	HL,(TEMP3)	;GET BACK THE FUNCTION DEFINITION TEXT POINTER
				;SKIP OVER THE "=" IN THE DEFINITION
	CALL	FRMEQL		;AND EVALUATE THE DEFINITION FORMULA
				;CAN HAVE RECURSION AT THIS POINT
	DEC	HL
	CALL	CHRGTR		;SEE IF THE STATEMENT ENDED RIGHT
	JP	NZ,SNERR	;THIS IS A CHEAT, SINCE THE LINE
				;NUMBER OF THE ERROR WILL BE THE CALLERS
				;LINE # INSTEAD OF THE DEFINITIONS LINE #
	CALL	GETYPR		;SEE IT THE RESULT IS A STRING
	JP	NZ,NOCPRS	;WHOSE DESCRIPTOR IS ABOUT TO BE WIPED OUT
				;BECAUSE IT IS SITTING IN PARM1 (THIS
				; HAPPENS IT THE FUNCTION IS A PROJECTION
				; FUNCTION ON A STRING ARGUMENT)
	LD	DE,DSCTMP	;DSCTMP IS PAST ALL THE TEMP AREA
	LD	HL,(FACLO)	;GET THE ADDRESS OF THE DESCRIPTOR
	CALL	COMPAR
	JP	C,NOCPRS	;RESULT IS A TEMP - NO COPY NESC
	CALL	STRCPY		;MAKE A COPY IN DSCTMP
	CALL	PUTTMP		;PUT RESULT IN A TEMP AND MAKE FACLO POINT AT IT
NOCPRS	LD	HL,(PRMSTK)	;GET PLACE TO RESTORE PARM1 FROM STACK
	LD	D,H
	LD	E,L
	INC	HL		;POINT AT LENGTH
	INC	HL
	LD	C,(HL)		;[B,C]=LENGTH
	INC	HL
	LD	B,(HL)
	INC	BC		;INCLUDE EXTRA BYTES
	INC	BC
	INC	BC
	INC	BC
	LD	HL,PRMSTK	;PLACE TO STORE INTO
	CALL	BCTRAN
	EX	DE,HL		;[D,E]=PLACE TO RESTORE STACK TO
	LD	SP,HL
	LD	HL,(FUNACT)	;DECREASE ACTIVE FUNCTION COUNT
	DEC	HL
	LD	(FUNACT),HL
	LD	A,H
	OR	L		;SET UP FUNCTION FLAG
	LD	(NOFUNS),A
	POP	HL		;GET BACK THE CALLERS TEXT POINTER
	POP	AF		;GET BACK THE TYPE OF THE FUNCTION
DOCNVF	PUSH	HL		;SAVE THE TEXT POINTER
	AND	07H		;SETUP DISPATCH TO FORCE
				;FORMULA TYPE TO CONFORM
				;TO THE VARIABLE ITS BEING ASSIGNED TO
	LD	HL,FRCTBL	;TABLE OF FORCE ROUTINES
	LD	C,A		;[B,C]=TWO BYTE OFFSET
	LD	B,00H
	ADD	HL,BC
	CALL	DISPAT		;DISPATCH
	POP	HL		;GET BACK THE TEXT POINTER
	RET
;
;	Block transfer routine with source in [D,E] destination in [H,L]
;	and count in [B,C]. Transfer is forward.
;
BCTRAL	LD	A,(DE)
	LD	(HL),A
	INC	HL
	INC	DE
	DEC	BC
BCTRAN	LD	A,B
	OR	C
	JP	NZ,BCTRAL
	RET

;
;	Subroutine to see if we are in direct mode and
;	complain if so
;
;
;	Check for a running program (Z if so).  If a program is not running,
;	generate an Illegal Direct (ID) error.
ERRDIR	PUSH	HL		;SAVE THEIR [H,L]
	LD	HL,(CURLIN)	;SEE WHAT THE CURRENT LINE IS
	INC	HL		;DIRECT IS 65,535 SO NOW 0
	LD	A,H		;IS IT ZERO NOW?
	OR	L
	POP	HL
	RET	NZ		;RETURN IF NOT
	LD	E,ERRID		;"ILLEGAL DIRECT" ERROR
	JP	ERROR

;
;
;	Subroutine to get a pointer to a function name
;
GETFNM	CALL	SYNCHR		;Make sure "FN" follows and get FN name
	DB	FNTK		;MUST START WITH "FN"
	LD	A,80H		;DONT ALLOW AN ARRAY
	LD	(SUBFLG),A	;DON'T RECOGNIZE THE "(" AS
				;THE START OF AN ARRAY REFEREENCE
	OR	(HL)		;PUT FUNCTION BIT ON
	LD	C,A		;GET FIRST CHARACTER INTO [C]
	JP	PTRGT2


;-----------------------------------------------------------------------------
;	STRING FUNCTIONS - LEFT HAND SIDE MID$
; ## GWEVAL.ASM:1521 ##
;
ISMID$	CP	0FFH-ENDTK	;FUNCTION? (FF - $END)
	JP	NZ,SNERR	;NO, ERROR.
	INC	HL		;POINT TO NEXT CHAR
	LD	A,(HL)		;GET FN DESCRIPTOR
	INC	HL		;POINT TO CHAR AFTER
	CP	MID$TK+80H	;IS IT MID?
	JP	Z,LHSMID
	JP	SNERR


;-----------------------------------------------------------------------------
;	INP, OUT, WAIT, CONSOLE, WIDTH
; ## GWEVAL.ASM:1540 ##
;
;	The following functions allow the
;	user full access to the 8080/Z80 I/O ports
;	INP(CHANNEL#) returns an integer which is the status
;	of the channel. OUT CHANNEL#,VALUE puts out the integer
;	VALUE on CHANNEL#. It is a statement, not a function.
FNINP	CALL	CONINT		;MAKE ARGUMENT AN INTEGER CHANNEL NUMBER
	LD	(INPP),A
	IN	A,(00H)		;READ BYTE INTO A FROM PORT
INPP	EQU	$-1
	JP	SNGFLT

;	'OUT' BASIC command
FNOUT	CALL	SETIO
	OUT	(00H),A		;OUTPUT TO PORT FROM [A]
OUTP	EQU	$-1
	RET

;	'WAIT' BASIC command
;
;	The WAIT CHANNEL#,MASK,MASK2 waits until the status
;	returned by CHANNEL# is non zero when XORed with MASK2
;	and then ANDed with MASK. If MASK2 is not present it is assumed
;	to be zero.
;
FNWAIT	CALL	SETIO		;SET UP FOR WAIT
	PUSH	AF		;SAVE THE MASK
	LD	E,00H		;DEFAULT MASK2 TO ZERO
	DEC	HL
	CALL	CHRGTR		;SEE IF THE STATEMENT ENDED
	JP	Z,NOTTHR	;IF NO THIRD ARGUMENT SKIP THIS
	CALL	SYNCHR
	DB	','		;MAKE SURE THERE IS A ","
	CALL	GETBYT
NOTTHR	POP	AF		;REGET THE "AND" MASK
	LD	D,A		;KEEP AND MASK IN [D]
				;GET READY TO READ PORT
LOPINP	IN	A,(00H)		;READ BYTE INTO [A]
WAITP	EQU	$-1
	XOR	E		;XOR WITH MASK2
	AND	D		;AND WITH MASK
	JP	Z,LOPINP	;LOOP UNTIL RESULT IS NON-ZERO
				;NOTE: THIS LOOP CANNOT BE CONTROL-C'ED
				;UNLESS THE WAIT IS BEING DONE ON CHANNEL
				;ZERO. HOWEVER A RESTART AT 0 IS OK.
	RET

;	Leftover garbage, not referenced
CONSOL	JP	SNERR		;Syntax Error


;-----------------------------------------------------------------------------
;	[CLOSE,] WIDTH Statements
; ## GIO86.ASM 229 ##
;
;	WIDTH [LPRINT] size   Statement
;
;	THIS IS THE WIDTH (TERMINAL WIDTH) COMMAND
;	ARG MUST BE .GT. 15 AND .LT. 255
;
;	WIDTH [LPRINT] <integer expression>
;	To set the printed line width in
;	number of characters for the terminal or line printer.
;
;	Invalid in TRSDOS 6:
;	 WIDTH Y[,X]/[#fnum,]/[device,]   Statement
;
;	Entry - (BX) = text pointer
;
WIDTHS	CP	LPRINTTK	;WIDTH LPRINT?
	JP	NZ,NOTLPR	;NO
	CALL	CHRGTR		;FETCH NEXT CHAR
	CALL	GETBYT		;GET WIDTH
	LD	(LPTSIZ),A	;SAVE IT
	LD	E,A		; (probably not needed)
	CALL	LSTCOM		;COMPUTE LAST COMMA COLUMN
	LD	(NLPPOS),A	;SAVE IT
	RET

;	Not WIDTH LPRINT => TTY
NOTLPR	CALL	GETBYT		;GET THE CHANNEL #
	LD	(TTYSIZ),A	;SETUP THE LINE LENGTH
	LD	E,A		; (probably not needed)
	CALL	LSTCOM		;COMPUTE LAST COMMA COLUMN
	LD	(NTTPOS),A	;SET LAST COMMA POSITION
	RET			;DONE

;	Compute last comma position
LSTCOM	SUB	CLMWID		;BACK TO NEWSTT
	JP	NC,LSTCOM
	ADD	A,2*CLMWID
	CPL
	INC	A
	ADD	A,E
	RET


;-----------------------------------------------------------------------------
; ## GWEVAL.ASM:1599 ##
;	Get subscript
GETINT	CALL	CHRGTR
;	GETIN2 replaces ADRGET in ## GIO86.ASM:759 ##
;	Comment for ADRGET:
;		ADRGET - parse 16 bit expression
;		 Entry - [BX]=text pointer
;		 Exit  - [DX]=result (0..65535)
;		         [BX]=updated text pointer
;		         AX used, other registers preserved.
;
GETIN2	CALL	FRMEVL		;EVALUATE A FORMULA
;	Get integer variable to DE, error if negative
;
;	CONVERT THE FAC TO AN INTEGER IN [D,E]
;	AND SET THE CONDITION CODES BASED ON THE HIGH ORDER
INTFR2	PUSH	HL		;SAVE THE TEXT POINTER
	CALL	CINT		;CONVERT THE FORMULA TO AN INTEGER IN [H,L]
	EX	DE,HL		;PUT THE INTEGER INTO [D,E]
	POP	HL		;RESTORE THE TEXT POINTER
	LD	A,D		;SET THE CONDITION CODES ON THE HIGH ORDER
	OR	A
	RET

	;Set I/O port for OUT ...,... and WAIT ...,...
SETIO	CALL	GETBYT		;READ A 8-BIT PORT FOR 8080+
	LD	(WAITP),A	;SAVE FOR USE BY WAIT AND FNOUT
	LD	(OUTP),A
	CALL	SYNCHR		;MAKE SURE THERE IS A COMMA
	DB	','
	JP	GETBYT		;READ THE DATA BYTE TO [A] AND [E]

;	Fetch and get a byte in E and A
GTBYTC	CALL	CHRGTR
;	Get byte value in E and A
GETBYT	CALL	FRMEVL		;EVALUATE A FORMULA
;	Get integer in range
CONINT	CALL	INTFR2		;CONVERT THE FAC TO AN INTEGER IN [D,E]
				;AND SET THE CONDITION CODES BASED
				;ON THE HIGH ORDER
	JP	NZ,FCERR	;WASN'T ERROR
	DEC	HL		;ACTUALLY FUNCTIONS CAN GET HERE
				;WITH BAD [H,L] BUT NOT SERIOUS
				;SET CONDITION CODES ON TERMINATOR
	CALL	CHRGTR
	LD	A,E		;RETURN THE RESULT IN [A] AND [E]
	RET

;
;	No BLOAD, BSAVE, BPARMS
;
;=============================================================================
;	GWLIST Copied from BINTRP.MAC
;-----------------------------------------------------------------------------
;	EXTENDED LIST, DELETE, LLIST
; ## GWLIST.ASM:270 ##
;
LLIST	LD	A,01H
	LD	(PRTFLG),A	;FDB pointer = LPT Pseudo FDB
;	=LIST (ZASM reserved word)
LISTS	POP	BC		;GET RID OF NEWSTT RETURN ADDR
	CALL	SCNLIN		;SCAN LINE RANGE
	PUSH	BC		;SAVE POINTER TO 1ST LINE
	CALL	PROCHK		;DONT EVEN LIST LINE #
LIST4	LD	HL,0FFFFH	;DONT ALLOW ^C TO CHANGE
	LD	(CURLIN),HL	;CONTINUE PARAMETERS
	POP	HL		;GET POINTER TO LINE
	POP	DE		;GET MAX LINE # OFF STACK
	LD	C,(HL)		;[B,C]=THE LINK POINTING TO THE NEXT LINE
	INC	HL
	LD	B,(HL)
	INC	HL
	LD	A,B		;SEE IF END OF CHAIN
	OR	C
	JP	Z,READY		;LAST LINE, STOP.
	CALL	ISFLIO
	CALL	Z,ISCNTC	;check CTL C, queue typeahead
	PUSH	BC		;SAVE LINK
	LD	C,(HL)		;PUSH THE LINE #
	INC	HL
	LD	B,(HL)
	INC	HL
	PUSH	BC
	EX	(SP),HL		;GET LINE # INTO [H,L]
	EX	DE,HL		;GET MAX LINE IN [H,L]
	CALL	COMPAR		;PAST LAST LINE IN RANGE?
	POP	BC		;TEXT POINTER TO [B,C]
	JP	C,STPRDY	;IF PAST, THEN DONE LISTING.
	EX	(SP),HL		;SAVE MAX ON BOTTOM OF STACK
	PUSH	HL		;SAVE LINK ON TOP
	PUSH	BC		;SAVE TEXT POINTER BACK
	EX	DE,HL		;GET LINE # IN [H,L]
	LD	(DOT),HL	;SAVE FOR LATER EDIT OR LIST
				;AND WE WANT [H,L] ON THE STACK
	CALL	LINPRT		;PRINT AS INTEGER WITHOUT LEADING SPACE
	POP	HL
	LD	A,(HL)		;GET BYTE FROM LINE
	CP	09H		;IS IT A TAB?
	JP	Z,LIST41	;THEN DONT PRINT SPACE
	LD	A,' '
	CALL	OUTDO		;PRINT A SPACE AFTER THE LINE #
LIST41	CALL	BUFLIN		;UNPACK THE LINE INTO BUF
	LD	HL,BUF		;POINT AT THE START OF THE UNPACKED CHARACTERS
	CALL	OUTSTRZ		;PRINT THE LINE
	CALL	CRDO		;PRINT CRLF
	JP	LIST4		;GO BACK FOR NEXT LINE

;	Output null-terminated string
OUTSTRZ	LD	A,(HL)
	OR	A
	RET	Z		;IF =0 THEN END OF LINE
	CALL	OUTCHR		;OUTPUT CHAR AND CHECK FOR LF
	INC	HL		;INCR POINTER
	JP	OUTSTRZ		;PRINT NEXT CHAR

;	Unpack the line into buf
BUFLIN	LD	BC,BUF		;GET START OF TEXT BUFFER
	LD	D,0FFH		;GET ITS LENGTH INTO [D]
	XOR	A		;SET MODE OF DECRUNCH
				;BIT0 IS QUOTE, BIT1 IS DATA, BIT2 IS REM
				;SET ON SPECIAL CHAR FOR SPACE INSERTION
	LD	(TEMPA),A
	CALL	PROCHK		;ONLY PROCEED IF OK
	JP	PLOOP2		;START HERE

PLOOP	INC	BC		;INCREMENT DEPOSIT PTR.
	INC	HL		;ADVANCE TEXT PTR
	DEC	D		;BUMP DOWN COUNT
	RET	Z		;IF BUFFER FULL, RETURN
PLOOP2	LD	A,(HL)		;GET CHAR FROM BUF
	OR	A		;SET CC'S
	LD	(BC),A		;SAVE THIS CHAR
	RET	Z		;IF END OF SOURCE BUFFER, ALL DONE.
	CP	OCTCON		;IS IT SMALLER THAN SMALLEST EMBEDDED CONSTANT?
	JP	C,NTEMBL	;YES, DONT TREAT AS ONE
	CP	DBLCON+1	;IS IT EMBEDDED CONSTANT?
	LD	E,A		;SAVE CHAR IN [E]
	JP	C,PRTVAR	;PRINT LEADING SPACE IF NESC.
NTEMBL	OR	A		;SET CC'S
	JP	M,PLOOPR	;RESERVED WORD OF SOME KIND
	LD	E,A		;SAVE CHAR IN [E]
	CP	'.'		;DOT IS PART OF VAR NAME
	JP	Z,PRTVAR
	CALL	TSTANM		;IS CHAR ALPHANUMERIC
	JP	NC,PRTVAR	;ALPHANUMERIC
	XOR	A		;MAKE SPECIAL
	JP	PLOOPH

PRTVAR	LD	A,(TEMPA)	;WHAT DID WE DO LAST?
	OR	A		;SET CONDITION CODES
	JP	Z,PLOOPG	;SPECIAL, NEVER INSERT SPACE
	INC	A		;IN RESERVED WORD?
	JP	NZ,PLOOPG	;NO
	LD	A,' '		;PUT OUT SPACE BEFORE RESWORD
	LD	(BC),A		;STORE IN BUFFER
	INC	BC		;INCREMENT POINTER INTO BUFFER
	DEC	D		;SPACE LEFT?
	RET	Z		;NO, DONE
PLOOPG	LD	A,01H		;STORE FLAG SAYING IN VAR
PLOOPH	LD	(TEMPA),A
	LD	A,E		;GET BACK CHAR WE HAD
	CP	OCTCON		;IS IT SMALLER THAN SMALLEST EMBEDDED CONSTANT?
	JP	C,PLOOPZ	;YES, DONT TREAT AS ONE
	CP	DBLCON+1	;IS IT EMBEDED CONSTANT?
	JP	C,NUMLIN	;YES, UNPACK IT
PLOOPZ	LD	(BC),A		;MAKE SURE BYTE STORED AFTER SPACE
	JP	PLOOP		;STORE IN BUFFER

PLOOPR	INC	A		;SET ZERO IF FN TOKEN
	LD	A,(HL)		;GET CHAR BACK
	JP	NZ,NTFNTK	;NOT FUNCTION JUST TREAT NORMALLY
	INC	HL		;BUMP POINTER
	LD	A,(HL)		;GET CHAR
	AND	7FH		;TURN OFF HIGH BIT
NTFNTK	INC	HL		;MUST SEE IF ITS SNGQTK
	CP	SNGQTK		;AND PRECEDED BY ":REM" (in GW-BASIC only)
	JP	NZ,NOSNGQ
	DEC	BC
	DEC	BC
	DEC	BC
	DEC	BC
	INC	D
	INC	D
	INC	D
	INC	D
NOSNGQ	CP	ELSETK		;ELSE?
	CALL	Z,DEXBRT	;DEC BC : RET
;	No ELSE (skip deleting preceding ':')
	CP	WHILETK		;MIGHT HAVE AN EXTRA "+" IN WHILE FORMULA
	JP	NZ,BFNTWH	;SO SKIP OVER IT IF IT'S THERE
	LD	A,(HL)		;GET CHARACTER TO SEE IF ITS PLUSTK
	INC	HL		;ASSUME IS PLUSTK
	CP	PLUSTK		;MIGHT NOT BE PLUS IF BINARY SAVED IN
	LD	A,WHILETK	;RESTORE TOKEN VALUE
	JP	Z,BFNTWH	;VERSION OF BASIC BEFORE CRUNCH CHANGED
	DEC	HL		;MOVE POINTER BACK
;	No WHILE (skip ungetting following char except '+')
BFNTWH	PUSH	HL		;SAVE TEXT PTR.
	PUSH	BC		;SAVE DEPOSIT PTR.
	PUSH	DE		;SAVE CHAR COUNT.
	LD	HL,RESLST1	;GET PTR TO START OF RESERVED WORD LIST
	LD	B,A		;SAVE THIS CHAR IN [B]
	LD	C,'A'-1		;INIT LEADING CHAR VALUE
;	Next initial letter
RESSR3	INC	C		;BUMP LEADING CHAR VALUE.
;	Next word with same initial
RESSR1	INC	HL		;BUMP POINTER INTO RESLST
	LD	D,H		;SAVE PTR TO START OF THIS RESWRD
	LD	E,L
;	Next letter
RESSR2	LD	A,(HL)		;GET CHAR FROM RESLST
	OR	A		;SET CC'S
	JP	Z,RESSR3	;IF END OF THIS CHARS TABLE, GO BACK & BUMP C
	INC	HL		;BUMP SOURCE PTR
	JP	P,RESSR2	;IF NOT END OF THIS RESWRD, THEN KEEP LOOKING
	LD	A,(HL)		;GET PTR TO RESERVED WORD VALUE
	CP	B		;SAME AS THE ONE WE SEARCH FOR?
	JP	NZ,RESSR1	;NO, KEEP LOOKING.
	EX	DE,HL		;SAVE FOUND PTR IN [H,L]
	CP	USRTK		;USR FUNCTION TOKEN?
	JP	Z,NOISPA	;DONT INSERT SPACE
	CP	FNTK		;IS IT FUNCTION TOKEN?
NOISPA	LD	A,C		;GET LEADING CHAR
	POP	DE		;RESTORE LINE CHAR COUNT
	POP	BC		;RESTORE DEPOSIT PTR
	LD	E,A		;SAVE LEADING CHAR
	JP	NZ,NTFNEX	;NOT "FN" EXPANSION
	LD	A,(TEMPA)	;SET CC'S ON TEMPA
	OR	A
	LD	A,00H		;CLEAR RESWRD FLAG - MARK AS SPECIAL
	LD	(TEMPA),A	;SET FLAG
	JP	MORLNZ		;DO EXPANSION

;	No USR or FN, don't strip following ' '
NTFNEX	CP	'['		;WAS IT A SPECIAL CHAR?
	JP	NZ,NTSPCH	;NON-SPECIAL CHAR
	XOR	A		;SET NON-SPECIAL
	LD	(TEMPA),A
	JP	MORPUR		;PRINT IT

;	No '[', don't strip following ' '
NTSPCH	LD	A,(TEMPA)	;WHAT DID WE DO LAST?
	OR	A		;SPECIAL?
	LD	A,0FFH		;FLAG IN RESERVED WORD
	LD	(TEMPA),A	;CLEAR FLAG
;	Check to append ' '
MORLNZ	JP	Z,MORLN0	;GET CHAR AND PROCEED
	LD	A,' '		;PUT SPACE IN BUFFER
	LD	(BC),A
	INC	BC
	DEC	D		;ANY SPACE LEFT IN BUFFER
	JP	Z,PPSWRT	;NO, RETURN
MORLN0	LD	A,E
	JP	MORLN1		;CONTINUE
				;FETCH
;	Loop to write res word to buffer
MORPUR	LD	A,(HL)		;GET BYTE FROM RESWRD
	INC	HL		;BUMP POINTER
	LD	E,A		;SAVE CHAR
;	Write res word to buffer
MORLN1	AND	7FH		;AND OFF HIGH ORDER BIT FOR DISK & EDIT
	LD	(BC),A		;STORE THIS CHAR
	INC	BC		;BUMP PTR
	DEC	D		;BUMP DOWN REMAINING CHAR COUNT
	JP	Z,PPSWRT	;IF END OF LINE, JUST RETURN
	OR	E		;SET CC'S
	JP	P,MORPUR	;END OF RESWRD?
	CP	0A8H		;SPC( OR TAB( ?
	JP	NZ,NTSPCT	;NO
	XOR	A		;CLEAR FLAG
	LD	(TEMPA),A	;TO INSERT SPACE AFTERWARDS
;	No ERROR
NTSPCT	POP	HL		;RESTORE SOURCE PTR.
	JP	PLOOP2		;GET NEXT CHAR FROM LINE

;	NC if alpha or digit
TSTANM	CALL	ISLET2		;LETTER?
	RET	NC		;YES
	CP	'0'		;DIGIT?
	RET	C		;TOO SMALL
	CP	'9'+1		;LAST DIGIT
	CCF			;MAKE CARRY RIGHT
	RET			;NO CARRY=DIGIT

;	Write numeric constant
NUMLIN	DEC	HL		;MOVE POINTER BACK AS CHRGET INX'S
	CALL	CHRGTR		;SCAN THE CONSTANT
	PUSH	DE		;SAVE CHAR COUNT
	PUSH	BC		;SAVE DEPOSIT PTR
	PUSH	AF		;SAVE CONSTANT TYPE.
	CALL	CONFC1		;MOVE CONSTANT INTO FAC
	POP	AF		;RESTORE CONSTANT TYPE
	LD	BC,CONLIN	;PUT RETURN ADDR ON STACK
	PUSH	BC		;SAVE IT
	CP	OCTCON		;OCTAL CONSTANT?
	JP	Z,$FOUTO	;PRINT IT
	CP	HEXCON		;HEX CONSTANT?
	JP	Z,FOUTH1	;PRINT IN HEX
	LD	HL,(CONLO)	;GET LINE # VALUE IF ONE.
	JP	FOUT		;PRINT REMAINING POSSIBILITIES.

CONLIN	POP	BC		;RESTORE DEPOSIT PTR.
	POP	DE		;RESTORE CHAR COUNT
	LD	A,(CONSAV)	;GET SAVED CONSTANT TOKEN
	LD	E,'O'		;ASSUME OCTAL CONSTANT
	CP	OCTCON		;OCTAL CONSTANT?
	JP	Z,SAVBAS	;YES, PRINT IT
	CP	HEXCON		;HEX CONSTANT?
	LD	E,'H'		;ASSUME SO.
	JP	NZ,NUMSLN	;NOT BASE CONSTANT
SAVBAS	LD	A,'&'		;PRINT LEADING BASE INDICATOR
	LD	(BC),A		;SAVE IT
	INC	BC		;BUMP PTR
	DEC	D		;BUMP DOWN CHAR COUNT
	RET	Z		;RETURN IF END OF BUFFER
	LD	A,E		;GET BASE CHAR
	LD	(BC),A		;SAVE IT
	INC	BC		;BUMP PTR
	DEC	D		;BUMP DOWN BASE COUNT
	RET	Z		;END OF BUFFER, DONE
NUMSLN	LD	A,(CONTYP)	;GET TYPE OF CONSTANT WE ARE
	CP	04H		;IS IT SINGLE OR DOUBLE PREC?
	LD	E,00H		;NO, NEVER PRINT TRAILING TYPE INDICATOR
	JP	C,TYPSET
	LD	E,'!'		;ASSUME SINGLE PREC.
	JP	Z,TYPSET	;IS CONTYP=4, WAS SINGLE
	LD	E,'#'		;DOUBLE PREC INDICATOR
TYPSET	LD	A,(HL)		;GET LEADING CHAR
	CP	' '		;LEADING SPACE
	CALL	Z,INXHRT	;GO BY IT
NUMSL2	LD	A,(HL)		;GET CHAR FROM NUMBER BUFFER
	INC	HL		;BUMP POINTER
	OR	A		;SET CC'S
	JP	Z,NUMDN		;IF ZERO, ALL DONE.
	LD	(BC),A		;SAVE CHAR IN BUF.
	INC	BC		;BUMP PTR
	DEC	D		;SEE IF END OF BUFFER
	RET	Z		;IF END OF BUFFER, RETURN
	LD	A,(CONTYP)	;GET TYPE OF CONSTANT TO BE PRINTED
	CP	04H		;TEST FOR SINGLE OR DOUBLE PRECISION
	JP	C,NUMSL2	;NO, WAS INTEGER
	DEC	BC		;PICK UP SAVED CHAR
	LD	A,(BC)		;EASIER THAN PUSHING ON STACK
	INC	BC		;RESTORE TO POINT WHERE IT SHOULD
	JP	NZ,DBLSCN	;IF DOUBLE, DONT TEST FOR EMBEDED "."
	CP	'.'		;TEST FOR FRACTION
	JP	Z,ZERE		;IF SINGLE & EMBEDED ., THEN DONT PRINT !
;	Double Precision specifier (exponential syntax, e.g. -1.09432D-06)
DBLSCN	CP	'D'		;DOUBLE PREC. EXPONENT?
	JP	Z,ZERE		;YES, MARK NO VALUE TYPE INDICATOR NESC.
;	Exponential format specifier (e.g. -1.09E-06)
	CP	'E'		;SINGLE PREC. EXPONENT?
	JP	NZ,NUMSL2	;NO, PROCEED
ZERE	LD	E,00H		;MARK NO PRINTING OF TYPE INDICATOR
	JP	NUMSL2		;KEEP MOVING NUMBER CHARS INTO BUF

NUMDN	LD	A,E		;GET FLAG TO INDICATE WHETHER TO INSERT
	OR	A		;A "D" AFTER DOUBLE PREC. #
	JP	Z,NOD		;NO, DONT INSERT IT
	LD	(BC),A		;SAVE IN BUFFER
	INC	BC		;BUMP POINTER
	DEC	D		;DECREMENT COUNT OF CHARS LEFT IN BUFFER
	RET	Z		;=0, MUST TRUNCATE LIST OF THIS LINE.
NOD	LD	HL,(CONTXT)	;GET BACK TEXT POINTER AFTER CONSTANT
	JP	PLOOP2		;GET NEXT CHAR

;	'DELETE' BASIC command
;
;	The following code is for the DELETE Range
;	command. Before the lines are deleted, "READY"
;	is typed.
DELETE	CALL	SCNLIN		;SCAN LINE RANGE
	PUSH	BC
	CALL	DEPTR		;CHANGE POINTERS BACK TO NUMBERS
	POP	BC
	POP	DE		;POP MAX LINE OFF STACK
	PUSH	BC		;SAVE POINTER TO START OF DELETION
				;FOR USE BY CHEAD AFTER FINI
	PUSH	BC		;SAVE POINTER TO START OF 1ST LINE
	CALL	FNDLIN		;FIND THE LAST LINE
	JP	NC,FCERRG1	;MUST HAVE A MATCH ON THE UPPER BOUND
	LD	D,H		;[D,E]=POINTER AT THE START OF THE LINE
	LD	E,L		;BEYOND THE LAST LINE IN THE RANGE
	EX	(SP),HL		;SAVE THE POINTER TO THE NEXT LINE
	PUSH	HL		;SAVE THE POINTER TO THE START OF
				;THE FIRST LINE IN THE RANGE
	CALL	COMPAR		;MAKE SURE THE START COMES BEFORE THE END
;	TODO: Dup FCERRG ...
FCERRG1	JP	NC,FCERR	;IF NOT, "ILLEGAL FUNCTION CALL"
				;DELETE must now be at EOL
				;Get text pointer
				;Check for end of statement
	LD	HL,REDDY	;PRINT "OK" PREMATURELY
	CALL	STROUT
	POP	BC		;GET POINTER TO FIRST IN [B,C]
	LD	HL,FINI		;GO BACK TO FINI WHEN DONE
	EX	(SP),HL		;[H,L]=POINTER TO THE NEXT LINE

;	Erase a line from memory
;	[B,C]=Start of line being deleted
;	[H,L]=Start of next line
;
DEL	EX	DE,HL		;[D,E] NOW HAVE THE POINTER TO THE LINE
				;BEYOND THIS ONE
	LD	HL,(VARTAB)	;COMPACTIFYING TO VARTAB
MLOOP	LD	A,(DE)
	LD	(BC),A		;SHOVING DOWN TO ELIMINATE A LINE
	INC	BC
	INC	DE
	CALL	COMPAR		;DONE COMPACTIFYING?
	JP	NZ,MLOOP	;NO
	LD	H,B
	LD	L,C
	LD	(VARTAB),HL
	RET


;-----------------------------------------------------------------------------
;	PEEK AND POKE
; ## GWMAIN.ASM:3032 ##
;
;
;	Note: In the 8K PEEK only accepts positive numbers up to 32767
;	POKE will only take an address up to 32767 , no
;	fudging allowed. The value is unsigned.
;	In the extended version negative numbers can be
;	used to refer to locations higher than 32767.
;	The correspondence is given by subtracting 65536 from locations
;	higher than 32767 or by specifying a positive number up to 65535.
;
PEEK	CALL	FRQINT		;GET AN INTEGER IN [H,L]
	CALL	PRODIR		;DONT ALLOW DIRECT IF PROTECTED FILE
	LD	A,(HL)		;Fetch the PEEK value
	JP	SNGFLT		;AND FLOAT IT

;	'POKE' BASIC command
POKE	CALL	FRMQNT		;READ INTEGER FORMULA INTO [D,E]
	PUSH	DE		;SAVE VALUE FOR LATER
	CALL	PRODIR		;DONT ALLOW DIRECT IF PROTECTED FILE
	CALL	SYNCHR
	DB	','		;CHECK FOR A COMMA
	CALL	GETBYT
	POP	DE		;GET THE ADDRESS BACK
	LD	(DE),A		;STORE IT AWAY
	RET			;SCANNED EVERYTHING

;	Get a number to DE
FRMQNT	CALL	FRMEVL		;READ FORMULA
	PUSH	HL		;SAVE TEXT POINTER
	CALL	FRQINT		;GET VALUE IN RANGE -32769-65535
	EX	DE,HL		;VALUE TO [D,E]
	POP	HL		;RESTORE TEXT POINTER
	RET

FRQINT	LD	BC,CINT		;RETURN HERE
	PUSH	BC		;SAVE ADDR
	CALL	GETYPR		;SET THE CC'S ON VALTYPE
	RET	M		;RETURN IF ALREADY INTEGER.
	LD	A,(FAC)		;GET EXPONENT
	CP	90H		;IS MAGNITUDE .GT. 32767
	RET	NZ		;NO, FORCE INTEGER
	CALL	SIGN		;GET SIGN OF NUMBER
				;IS IT NEGATIVE, ONLY ALLOWABLE # IS -32768
	RET	M		;ASSUME THATS WHAT IT IS, ELSE GIVE OVERFLOW
	CALL	CSNG		;MAKE NUMBER A SINGLE
	LD	BC,9180H	;GET -65536.
	LD	DE,0000H
	JP	FADD		;SUBTRACT IT, AND THEN FORCE INTEGER


;-----------------------------------------------------------------------------
;	RENUMBER
; ## GWMAIN.ASM:3102 ##
;
;	'RENUM' BASIC command
;
;	The RESEQ(UENCE) command take up to three arguments
;	RESEQ [NN[,MM[,INC]]]
;	where NN is the first destination line of the
;	lines being resequenced, lines less than MM are
;	not resequenced, and INC is the increment.
RESEQ	LD	BC,000AH	;ASSUME INC=10
	PUSH	BC		;SAVE ON STACK
	LD	D,B		;RESEQ ALL LINES BY SETTING [D,E]=0
	LD	E,B
	JP	Z,RESSN		;IF JUST 'RESEQ' RESEQ 10 BY 10
	CP	','		;COMMA
	JP	Z,EATCOM	;DONT USE STARTING # OF ZERO
	PUSH	DE		;SAVE [D,E]
	CALL	LINSPC		;GET NEW NN
	LD	B,D		;GET IN IN [B,C] WHERE IT BELONGS
	LD	C,E
	POP	DE		;GET BACK [D,E]
	JP	Z,RESSN		;IF EOS, DONE
EATCOM	CALL	SYNCHR
	DB	','		;EXPECT COMMA
	CALL	LINSPC		;GET NEW MM
	JP	Z,RESSN		;IF EOS, DONE
	POP	AF		;GET RID OF OLD INC
	CALL	SYNCHR
	DB	','		;EXPECT COMMA
	PUSH	DE		;SAVE MM
	CALL	LINGET		;GET NEW INC
	JP	NZ,SNERR	;SHOULD HAVE TERMINATED.
	LD	A,D
	OR	E		;SEE IF INC=0 (ILLEGAL)
	JP	Z,FCERR		;YES, BLOW HIM UP NOW
	EX	DE,HL		;FLIP NEW INC & [H,L]
	EX	(SP),HL		;NEW INC ONTO STACK
	EX	DE,HL		;GET [H,L] BACK, ORIG [D,E] BACK
RESSN	PUSH	BC		;SAVE NN ON STACK
	CALL	FNDLIN		;FIND MM LINE
	POP	DE		;GET NN OFF STACK
	PUSH	DE		;SAVE NN BACK
	PUSH	BC		;SAVE POINTER TO MM LINE
	CALL	FNDLIN		;FIND FIRST LINE TO RESEQ.
	LD	H,B		;GET PTR TO THIS LINE IN [H,L]
	LD	L,C
	POP	DE		;GET LINE PTD TO BY MM
	CALL	COMPAR		;COMPARE TO FIRST LINE RESEQED
	EX	DE,HL		;GET PTR TO MM LINE IN [H,L]
	JP	C,FCERR		;CANT ALLOW PROGRAM TO BE RESEQUED
				;ON TOP OF ITSELF
	POP	DE		;GET NN BACK
	POP	BC		;GET INC IN [B,C]
	POP	AF		;GET RID OF NEWSTT
	PUSH	HL		;SAVE PTR TO FIRST LINE TO RESEQ.
	PUSH	DE		;SAVE NN ON STACK
	JP	NXTRSL

NXTRSC	ADD	HL,BC		;ADD INCREMENT INTO
	JP	C,FCERR		;UH OH, HIS INC WAS TOO LARGE.
	EX	DE,HL		;FLIP LINK FIELD, ACCUM.
	PUSH	HL		;SAVE LINK FIELD
	LD	HL,0FFF9H	;TEST FOR TOO LARGE LINE
	CALL	COMPAR		;COMPARE TO CURRENT #
	POP	HL		;RESTORE LINK FIELD
	JP	C,FCERR		;UH OH, HIS INC WAS TOO LARGE.
NXTRSL	PUSH	DE		;SAVE CURRENT LINE ACCUM
	LD	E,(HL)		;GET LINK FIELD INTO [D,E]
	INC	HL
	LD	D,(HL)
	LD	A,D		;SET CC'S ON LINK FIELD
	OR	E
	EX	DE,HL		;SEE IF NEXT LINK ZERO
	POP	DE		;GET BACK ACCUM LINE #
	JP	Z,RESSD1	;ZERO, DONE
	LD	A,(HL)		;GET FIRST BYTE OF LINK
	INC	HL		;INC POINTER
	OR	(HL)		;SET CC'S
	DEC	HL		;MOVE POINTER BACK
	EX	DE,HL		;BACK IN [D,E]
	JP	NZ,NXTRSC	;INC COUNT

RESSD1	PUSH	BC		;SAVE INC
	CALL	SCCLIN		;SCAN PROGRAM CONVERTING LINES TO PTRS.
	POP	BC		;GET BACK INC
	POP	DE		;GET NN
	POP	HL		;GET PTR TO FIRST LINE TO RESEQ

RESNX1	PUSH	DE		;SAVE CURRENT LINE
	LD	E,(HL)		;PREPARE FOR ZERO LINK FIELD TEST
	INC	HL
	LD	D,(HL)
	LD	A,D		;END?
	OR	E
	JP	Z,SCCALL	;STOP RESEQING WHEN SEE END OF PGM
	EX	DE,HL		;FLIP LINE PTR, LINK FIELD
	EX	(SP),HL		;PUT LINK ON STACK, GET NEW LINE # OFF
	EX	DE,HL		;PUT NEW LINE # IN [D,E], THIS LINE
				;PTR IN [H,L]
	INC	HL		;POINT TO LINE # FIELD.
	LD	(HL),E		;CHANGE TO NEW LINE #
	INC	HL
	LD	(HL),D
	EX	DE,HL		;GET THIS LINE # IN [H,L]
	ADD	HL,BC		;ADD INC
	EX	DE,HL		;GET NEW LINE # BACK IN [D,E]
	POP	HL		;GET PTR TO NEXT LINE
	JP	RESNX1		;KEEP RESEQING

SCCALL	LD	BC,STPRDY	;WHERE TO GO WHEN DONE
	PUSH	BC		;SAVE ON STACK
	DB	0FEH	; SKIP	;"CPI AL," CALL SCCPTR

;	The subroutines SCCLIN and SCCPTR convert all
;	line #'s to pointers and vice-versa.
;	The only special case is "ON ERROR GOTO 0" where the "0"
;	is left as a line number token so it wont be changed by resequence.
SCCLIN	DB	0F6H		;"ORI AX," OVER NEXT BYTE
SCCPTR	XOR	A		;SET A=0
	LD	(PTRFLG),A	;SET TO SAY WHETER LINES OR PTRS EXTANT
	LD	HL,(TXTTAB)	;GET PTR TO START OF PGM
	DEC	HL		;NOP NEXT INX.
SCNPLN	INC	HL		;POINT TO BYTE AFTER ZERO AT END OF LINE
	LD	A,(HL)		;GET LINK FIELD INTO [D,E]
	INC	HL		;BUMP PTR
	OR	(HL)		;SET CC'S
	RET	Z		;RETURN IF ALL DONE.
	INC	HL		;POINT PAST LINE #
	LD	E,(HL)		;GET LOW BYTE OF LINE #
	INC	HL
	LD	D,(HL)
SCNEXT	CALL	CHRGTR		;GET NEXT CHAR FROM LINE
;	Line number to pointer
SCNEX2	OR	A		;END OF LINE
	JP	Z,SCNPLN	;SCAN NEXT LINE
	LD	C,A		;SAVE [A]
	LD	A,(PTRFLG)	;CHANGE LINE TOKENS WHICH WAY?
	OR	A		;SET CC'S
	LD	A,C		;GET BACK CURRENT CHAR
	JP	Z,SCNPT2	;CHANGING POINTERS TO #'S
	CP	ERRORTK		;IS IT ERROR TOKEN?
	JP	NZ,NTERRG	;NO.
	CALL	CHRGTR		;SCAN NEXT CHAR
	CP	GOTOTK		;ERROR GOTO?
	JP	NZ,SCNEX2	;GET NEXT ONE
	CALL	CHRGTR		;GET NEXT CHAR
	CP	LINCON		;LINE # CONSTANT?
	JP	NZ,SCNEX2	;NO, IGNORE.
	PUSH	DE		;SAVE [D,E]
	CALL	LINGT3		;GET IT
	LD	A,D		;IS IT LINE # ZERO?
	OR	E
	JP	NZ,CHGPTR	;CHANGE IT TO A POINTER
	JP	SCNEX3		;YES, DONT CHANGE IT

NTERRG	CP	LINCON		;LINE # CONSTANT?
	JP	NZ,SCNEXT	;NOT, KEEP SCANNING
	PUSH	DE		;SAVE CURRENT LINE # FOR POSSIBLE ERROR MSG
	CALL	LINGT3		;GET LINE # OF LINE CONSTANT INTO [D,E]
CHGPTR	PUSH	HL		;SAVE TEXT POINTER JUST AT END OF LINCON 3 BYTES
	CALL	FNDLIN		;TRY TO FIND LINE IN PGM.
	DEC	BC		;POINT TO ZERO AT END OF PREVIOUS LINE
	LD	A,PTRCON	;CHANGE LINE # TO PTR
	JP	C,MAKPTR	;IF LINE FOUND CHANGE # TO PTR
	CALL	CRDONZ		;PRINT CRLF IF REQUIRED
	LD	HL,LINM		;PRINT "UNDEFINED LINE" MESSAGE
	PUSH	DE		;SAVE LINE #
	CALL	STROUT		;PRINT IT
	POP	HL		;GET LINE # IN [H,L]
	CALL	LINPRT		;PRINT IT
	POP	BC		;GET TEXT PTR OFF STACK
	POP	HL		;GET CURRENT LINE #
	PUSH	HL		;SAVE BACK
	PUSH	BC		;SAVE BACK TEXT PTR
	CALL	INPRT		;PRINT IT
SCNPOP	POP	HL		;POP OFF CURRENT TEXT POINTER
SCNEX3	POP	DE		;GET BACK CURRENT LINE #
	DEC	HL		;BACKUP POINTER
JSCNXT	JP	SCNEXT		;KEEP SCANNING

LINM	DB	'Undefined line ',00H

SCNPT2	CP	PTRCON		;POINTER
	JP	NZ,JSCNXT	;NO, KEEP SCANNING
	PUSH	DE		;SAVE CURRENT LINE #
	CALL	LINGT3		;GET #
	PUSH	HL		;SAVE TEXT POINTER
	EX	DE,HL		;FLIP CURRENT TEXT PTR & PTR
	INC	HL		;BUMP POINTER
	INC	HL		;POINT TO LINE # FIELD
	INC	HL
	LD	C,(HL)		;PICK UP LINE #
	INC	HL		;POINT TO HIGH PART
	LD	B,(HL)
	LD	A,LINCON	;CHANGE TO LINE CONSTANT
MAKPTR	LD	HL,SCNPOP	;PLACE TO RETURN TO AFTER CHANGING CONSTANT
	PUSH	HL		;SAVE ON STACK
	LD	HL,(CONTXT)	;GET TXT PTR AFTER CONSTANT IN [H,L]
CONCH2	PUSH	HL		;SAVE PTR TO END OF CONSTANT
	DEC	HL
	LD	(HL),B
	DEC	HL
	LD	(HL),C		;CHANGE TO VALUE IN [B,C]
	DEC	HL		;POINT TO CONSTANT TOKEN
	LD	(HL),A		;CHANGE TO VALUE IN [A]
	POP	HL		;RESTORE POINTER TO AFTER CONSTANT
	RET

DEPTR	LD	A,(PTRFLG)	;DO LINE POINTERS EXIST IN PGM?
	OR	A		;SET CC'S
	RET	Z		;NO, JUST RETURN
	JP	SCCPTR		;CONVERT THEN TO LINE #'S


;-----------------------------------------------------------------------------
;	ANSI - THE ROUTINES TO HANDLE ANSI FEATURES
; ## GWMAIN.ASM:3325 ##
;
;	'OPTION' BASIC command
OPTION	CALL	SYNCHR
	DB	'B'
	CALL	SYNCHR
	DB	'A'
	CALL	SYNCHR
	DB	'S'
	CALL	SYNCHR
	DB	'E'
	LD	A,(OPTFLG)	;HAVE WE SEEN OPTION BASE BEFORE
	OR	A
	JP	NZ,DDERR	;IF SO "DOUBLE DIMENSION ERROR"
	PUSH	HL		;SAVE THE TEXT POINTER
	LD	HL,(ARYTAB)	;SEE IF WE HAVE ANY ARRAYS YET
	EX	DE,HL
	LD	HL,(STREND)
	CALL	COMPAR		;IF THESE ARE EQUAL WE HAVE NOT
	JP	NZ,DDERR
	POP	HL
	LD	A,(HL)
	SUB	'0'
	JP	C,SNERR
	CP	'2'-'0'		;ONLY 0 AND 1 ARE LEGAL
	JP	NC,SNERR
	LD	(OPTVAL),A	;SAVE IF FOR DIM AND PTRGET
	INC	A		;MAKE SURE [A] IS NON ZERO
	LD	(OPTFLG),A	;FLAG THAT WE HAVE SEEN "OPTION BASE"
	CALL	CHRGTR		;FETCH THE TERMINATOR
	RET

;	This routine is called by the Math Package
;	to print error messages without disturbing PTRFIL, etc.
STRPRN	LD	A,(HL)		;GET BYTE FROM MESSAGE
	OR	A		;END OF MESSAGE
	RET	Z		;YES, DONE
	CALL	CALTTY		;PRINT CHAR
	INC	HL		;INCREMENT POINTER
	JP	STRPRN		;PRINT NEXT CHAR


;-----------------------------------------------------------------------------
; ## GIOSCN.ASM:143 ##
;	CALTTY is a special routine to output error message to TTY, regardless
;	   of current file I/O.
;	 Entry - [A] = byte to be output
;	 Exit  - All registers preserved
CALTTY	PUSH	AF
	JP	OUTCON


;-----------------------------------------------------------------------------
;	'RANDOMIZE' BASIC command
RANDOM	JP	Z,RAND0		;IF NO ARGUMENT ASK FROM TERMINAL
	CALL	FRMEVL		;FETCH THE FORMULA ARGUMENT
	PUSH	HL
	CALL	CINT		;ALLOW NORMAL INTEGERS
	JP	RAND2		;STORE THE NEW RANDOM SEED
RAND0	PUSH	HL
RAND1	LD	HL,RANMES	;ASK FOR SOME RANDOM INPUT
	CALL	STROUT
	CALL	QINLIN
	POP	DE		;get back text pointer
	JP	C,STPEND1	;go away if control c
	PUSH	DE		;resave text pointer
	INC	HL		;MOVE PAST BUFMIN TO BUF
	LD	A,(HL)		;GET FIRST CHAR OF TYPEIN (FIN EXPECTS IT)
	CALL	FIN		;READ A NUMBER
	LD	A,(HL)		;GET THE TERMINATOR
	OR	A
	JP	NZ,RAND1	;DON'T ALLOW BAD FORMAT
	CALL	CINT		;ALLOW NORMAL INTEGERS
RAND2	LD	(RNDX+1),HL
	CALL	RNDMN2
	POP	HL		;GET BACK THE TEXT POINTER
	RET

;	Interactive message to initialize RND
RANMES	DB	'Random number seed (-32768 to 32767)',00H


;-----------------------------------------------------------------------------
; ## GWMAIN.ASM:3414 ##
;	This code scans ahead to find the "NEXT" that matches a "FOR"
;	in order to 1) handle empty loops and
;	            2) make sure loops match up properly.
;
WNDSCN	LD	C,ERRWH		;SCAN FOR MATCHING WEND THIS IS ERROR IF FAIL
	JP	SCNCNT

;	NEXT scan
NXTSCN	LD	C,ERRFN
SCNCNT	LD	B,00H		;SET UP THE COUNT OF "FOR"S SEEN
	EX	DE,HL		;INITIALIZE NXTLIN FOR NEXT ON SAME LINE
	LD	HL,(CURLIN)
	LD	(NXTLIN),HL
	EX	DE,HL		;RESTORE THE TEXT POINTER TO [H,L]
FORINC	INC	B		;INCREMENT THE COUNT WHENEVER "FOR" IS SEEN
FNLOP	DEC	HL		;** FIX HERE FOR 5.03 CAN'T CALL DATA
SCANWF	CALL	CHRGTR		;TO SKIP TO STATEMENT BECAUSE COULD
	JP	Z,FORTRM	;HAVE STATEMENT AFTER "THEN"
	CP	ELSETK		;ELSE STATEMENT
	JP	Z,FNNWST	;THEN ALLOW NEXT OR WEND AFTER IT
	CP	THENTK		;SO SCAN USING CHRGET WAITING FOR END
	JP	NZ,SCANWF	;OF STATEMENT OR $THEN
FORTRM	OR	A		;SEE HOW IT ENDED
	JP	NZ,FNNWST	;JUST NEW STATEMENT -- EXAMINE IT
				;OR COULD BE COLON IN STRING BUT NO HARM
				;IN NON KANABS (HGHBIT) VERSION SINCE NO RESERVED
				;WORDS WILL MATCH THE NEXT CHARACTER
	INC	HL
	LD	A,(HL)		;SCAN THE LINK AT THE START OF THE NEXT LINE
	INC	HL
	OR	(HL)		;TO SEE IF ITS ZERO (END OF PROGRAM)
	LD	E,C		;SET UP ERROR NUMBER
	JP	Z,ERROR
	INC	HL		;PICK UP THE NEW LINE NUMBER
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	EX	DE,HL
	LD	(NXTLIN),HL	;SAVE AS "NEXT" LINE NUMBER
	EX	DE,HL
FNNWST	CALL	CHRGTR		;GET THE TYPE OF THE NEXT STATEMENT
	LD	A,C		;GET THE ERROR NUMBER TO SEE WHAT WE ARE
	CP	ERRFN		;SCANNING FOR
	LD	A,(HL)		;GET BACK THE CHARACTER
	JP	Z,NXTLOK	;FOR/NEXT SEARCHING
	CP	WHILETK		;ANOTHER WHILE/WEND NEST?
	JP	Z,FORINC
	CP	WENDTK
	JP	NZ,FNLOP
	DEC	B
	JP	NZ,FNLOP
	RET

NXTLOK	CP	FORTK		;ANOTHER "FOR"?
	JP	Z,FORINC	;INCREMENT THE FOR COUNT
	CP	NEXTTK		;END WITH NEXT?
	JP	NZ,FNLOP	;SKIP OVER THIS STATEMENT
DECNXT	DEC	B		;DECREMENT THE LOOP COUNT
	RET	Z		;RETURN WITH [H,L] ABOUT TO GET
				; FIRST CHARACTER OF "NEXT" VARIABLE
;
;	Scan  the variables listed in a "NEXT" statement
;
	CALL	CHRGTR		;SEE IF THERE IS A NAME
	JP	Z,FORTRM	;ONLY ONE SO SCAN MORE STATEMENTS
	EX	DE,HL		;SAVE TEXT POINTER IN [D,E]
	LD	HL,(CURLIN)	;SAVE THE CURRENT LINE NUMBER
	PUSH	HL
	LD	HL,(NXTLIN)	;GET THE LINE NUMBER OF NEXT
	LD	(CURLIN),HL	;MAKE IT THE CURRENT LINE
	EX	DE,HL		;[H,L]= TEXT POINTER
	PUSH	BC		;SAVE THE "FOR" COUNT
	CALL	PTRGET		;SKIP OVER THE VARIABLE NAME
	POP	BC		;GET BACK THE "FOR" COUNT
	DEC	HL		;CHECK TERMINATOR
	CALL	CHRGTR
	LD	DE,FORTRM	;PLACE TO GO TO
	JP	Z,TRMNXT	;END OF "NEXT"
	CALL	SYNCHR
	DB	','		;SHOULD HAVE COMMAS IN BETWEEN
	DEC	HL		;RESCAN FIRST CHARACTER
	LD	DE,DECNXT	;PLACE TO GO BACK TO
TRMNXT	EX	(SP),HL		;SAVE THE TEXT POINTER ON THE STACK
	LD	(CURLIN),HL
	POP	HL
	PUSH	DE		;GO OFF TO ADDRESS IN [B,C]
	RET

;	This routine clears FLGOVC to reset to normal overflow mode.
;	In normal mode, OVERR always prints overflow because FLGOVC=0
;	Function dispatch, FIN (&FINDBL), and exponentiation set up an overflow
;	mode where FLGOVC=1 and after one overflow FLGOVC=2 and no more
;	overflow messages are printed. FIN (&FINDBL) also store FLGOVC in OVCSTR
;	before resetting FLGOVC so a caller can detect overflow occurrence.
FINOVC	PUSH	AF
	LD	A,(FLGOVC)	;STORE OVERFLOW FLAG TO INDICATE
	LD	(OVCSTR),A	;WHETHER AN OVERFLOW OCCURED
	POP	AF
;	BACK TO NORMAL OVERFLOW PRINT MODE
CLROVC	PUSH	AF		;SAVE EVERYTHING
	XOR	A		;NORMAL OVERFLOW MODE
	LD	(FLGOVC),A
	POP	AF
	RET

;
;*****************************************************************************
;	MATHPK FOR BASIC MCS 8080  GATES/ALLEN/DAVIDOFF
;*****************************************************************************
;
;=============================================================================
; ## MATH1.ASM and MATH2.ASM ##
;	FLOATING POINT MATH PACKAGE CONFIGURATION
;
;
;	The floating point format is as follows:
;
;	The sign is the first bit of the mantissa
;	The mantissa is 24 bits long
;	The binary point is to the left of the msb
;	Number = Mantissa * 2 ^ Exponent
;	The mantissa is positive, with a one assumed to be where the sign bit is
;	The sign of the exponent is the first bit of the exponent
;	The exponent is stored in excess 200 i.e. with a bias of 200 (octal)
;	So, the exponent is a signed 8-bit number with 200 added to it
;	An exponent of zerd means the number is zero, the other bytes are ignored
;	To keep the same number in the FAC while shifting:
;		to shift right, EXP:=EXP+1
;		to shift left,  EXP:=EXP-1
;
;	So, in memory the number looks like this:
;		[bits 17-24 of the mantissa]
;		[bits 9-16 of the mantissa]
;		[the sign in bit 7, bits 2-8 of the mantissa are in bits 6-0]
;		[the exponent as a signed number + 200]
;	(remember that bit 1 of the mantissa is always a one)
;
;	Arithmetic routine calling conventions:
;
;	For one argument functions:
;		The argument is in the FAC, the result is left in the FAC
;	For two argument operations:
;		The first argument is in B,C,D,E i.e. the "registers"
;		The second argument is in the FAC
;		The result is left in the FAC
;
;	The "S" entry points to the two argument operations have (HL) pointing to
;	the first argument instead of the first argument being in the registers.
;	MOVRM is called to get the argument in the registers.
;	The "T" entry points assume the first argument is on the stack,
;	POPR is used to get the argument in the registers.
;	Note: the "T" entry points should always be jumped to and never called
;	because the return address on the stack will be confused with the number.
;
;	On the stack, the two LO's are pushed on first and then the HO and sign.
;	This is done so if a number is stored in memory, it can be pushed on the
;	stack with two PUSHM's. The lower byte of each part is in the lower
;	memory address so when the number is popped into the registers, the higher
;	order byte will be in the higher order register of the register pair, i.e.
;	the higher order byte will be popped into B, D or H.
;

;-----------------------------------------------------------------------------
;	FLOATING POINT ADDITION AND SUBTRACTION
;
$FADDH	LD	HL,$SHALF	;ADD .5 TO FAC
;	Entry to FADD with pointer to arg in (HL)
;	($FAC)=(BXDX)+($FAC)
$FADDS	CALL	$MOVRM		;GET ARGUMENT INTO THE REGISTERS
	JP	FADD		;DO THE ADDITION

;	SUBTRACTION	FAC:=ARG-FAC
;	ENTRY IF POINTER TO ARG IS IN (HL)
$FSUBS	CALL	$MOVRM		; FPREG = -FPREG + number at HL
;	Subtract the single precision numbers in FAC1 and BCDE
;	Subtract BCDE from FP reg
FSUB	CALL	NEG		;NEGATE SECOND ARGUMENT
				;FALL INTO FADD
;	ADDITION	FAC:=ARG+FAC
;	Add BCDE to FP reg
;	ALTERS A,B,C,D,E,H,L
;
FADD	LD	A,B		;CHECK IF FIRST ARGUMENT IS ZERO
	OR	A		;GET EXPONENT
	RET	Z		;IT IS, RESULT IS NUMBER IN FAC
	LD	A,(FAC)		;GET EXPONENT
	OR	A		;SEE IF THE NUMBER IS ZERO
	JP	Z,$MOVFR	;IT IS, ANSWER IS IN REGISTERS

;***********************************************************
;	Know at this point that neither (BXDX) nor the
;	$FAC are zero. The sum will be performed by examination
;	of the exponents, placing the number with the larger
;	exponent in the $FAC, and shifting the smaller number right
;	until binary points align, then adding the mantissas
;	if the signs are the same or subtracting the mantissas
;	if the signs are different. The exponent of the answer
;	is the exponent of the larger number. The format of
;	floating point numbers is as follows:
;
;	Bit    33222222 22221111 11111100 00000000
;	       10987654 32109876 54321098 76543210
;	       AAAAAAAA BCCCCCCC CCCCCCCC CCCCCCCC
;	Byte   [ $FAC ] [$FAC-1] [$FAC-2] [$FAC-3]
;	                                  [$FACLO]
;
;	Where  A=bits of exponent biased by 128
;	       B=0 if number is positive, 1 if negative
;	       C=bits 2-24 of mantissa (bit 1 is understood 1)
;	Note: The binary point is to the left of the understood 1
;
;*************************************************************

;	We want to get the smaller number in the registers so we can shift it right
;	and align the binary points of the two numbers, then we can just add or
;	subtract them (depending on their signs) bytewise.
	SUB	B		;CHECK RELATIVE SIZES
	JP	NC,FADD1	;IS FAC SMALLER?
	CPL			;YES, NEGATE SHIFT COUNT
	INC	A
	EX	DE,HL		;SWITCH FAC AND REGISTERS, SAVE (DE)
	CALL	PUSHF		;PUT FAC ON STACK
	EX	DE,HL		;GET (DE) BACK WHERE IT BELONGS
	CALL	$MOVFR		;PUT REGISTERS IN THE FAC
	POP	BC		;GET THE OLD FAC IN THE REGISTERS
	POP	DE
FADD1	CP	19H		;ARE WE WITHIN 24 BITS?
	RET	NC
	PUSH	AF		;SAVE SHIFT COUNT
	CALL	UNPACK		;UNPACK THE NUMBERS
	LD	H,A		;SAVE SUBTRACTION FLAG
	POP	AF		;GET SHIFT COUNT BACK
	CALL	SHIFTR		;SHIFT REGISTERS RIGHT THE RIGHT AMOUNT
;	If the numbers have the same sign, then we add them. If the signs are
;	different, then we have to subtract them. We have to do this because the
;	mantissas are positive. Judging by the exponents, the larger number is in
;	the FAC, so if we subtract, the sign of the result should be the sign of the
;	FAC; however, if the exponents are the same, the number in the registers
;	could be bigger, so after we subtract them, we have to check if the result
;	was negative. If it was, we negate the number in the registers and
;	complement the sign of the FAC. (Here the FAC is unpacked)
;	If we have to add the numbers, the sign of the result is the sign of the
;	FAC. So, in either case, when we are all done, the sign of the result
;	will be the sign of the FAC.
	LD	A,H		;GET SUBTRACTION FLAG
	OR	A
	LD	HL,FACLO	;SET POINTER TO LO'S
	JP	P,FADD3		;SUBTRACT IF THE SIGNS WERE DIFFERENT
	CALL	FADDA		;ADD THE NUMBERS
	JP	NC,ROUND	;ROUND RESULT IF THERE WAS NO OVERFLOW
				;THE MOST IT CAN OVERFLOW IS ONE BIT
	INC	HL		;THERE WAS OVERFLOW
	INC	(HL)		;INCREMENT EXPONENT
	JP	Z,$OVFLS
	LD	L,01H		;SHIFT RESULT RIGHT ONE, SHIFT CARRY IN
	CALL	SHFTR4
	JP	ROUND		;ROUND RESULT AND WE ARE DONE

;	Here to subtract C,D,E,B from ((HL)+0,1,2),0
FADD3	XOR	A		;SUBTRACT NUMBERS, NEGATE UNDERFLOW BYTE
	SUB	B
	LD	B,A		;SAVE IT
	LD	A,(HL)		;SUBTRACT LOW ORDERS
	SBC	A,E
	LD	E,A
	INC	HL		;UPDATE POINTER TO NEXT BYTE
	LD	A,(HL)		;SUBTRACT MIDDLE ORDERS
	SBC	A,D
	LD	D,A
	INC	HL		;UPDATE POINTER TO HIGH ORDERS
	LD	A,(HL)		;SUBTRACT HIGH ORDERS
	SBC	A,C
	LD	C,A
;	Because we want a positive mantissa, check if we have to negate the
;	number
FADFLT	CALL	C,NEGR		;ENTRY FROM FLOATR, INT: NEGATE NUMBER IF IT
				; WAS NEGATIVE, FALL INTO NORMALIZE
;	Normalize C,D,E,B
;	Alters A,B,C,D,E,H,L
;	Here we shift the mantissa left until the MSB is a one.
;	Except in 4K, the idea is to shift left by 8 as many times as
;	possible.
NORMAL	LD	L,B		;PUT LOWEST 2 BYTES IN (HL)
	LD	H,E
	XOR	A		;ZERO SHIFT COUNT
NORM1	LD	B,A		;SAVE SHIFT COUNT
	LD	A,C		;DO WE HAVE 1 BYTE OF ZEROS
	OR	A
	JP	NZ,NORM3	;NO, SHIFT ONE PLACE AT A TIME
;	THIS LOOP SPEEDS THINGS UP BY SHIFTING 8 PLACES AT ONE TIME
	LD	C,D		;YES, SHIFT OVER 1 BYTE
	LD	D,H
	LD	H,L
	LD	L,A		;SHIFT IN 8 ZEROS FOR THE LOW ORDER
	LD	A,B		;UPDATE SHIFT COUNT
	SUB	08H
	CP	0E0H		;DID WE SHIFT IN 4 BYTES OF ZEROS?
	JP	NZ,NORM1	;NO, TRY TO SHIFT OVER 8 MORE
				;YES, NUMBER WAS ZERO.  FALL INTO ZERO
;	Zero FAC
;	Alters A only
;	Exit with A=0
;	By our floating point format, the number is zero if the exponent is
;	 zero
$ZERO	XOR	A		;ZERO A
ZERO0	LD	(FAC),A		;ZERO THE FAC'S EXPONENT, ENTRY IF A=0
	RET			;ALL DONE

ZERO1	LD	A,H		;CHECK FOR CASE OF NORMALIZING A SMALL INT
	OR	L
	OR	D
	JP	NZ,ZERO3	;DO USUAL THING
	LD	A,C		;GET BYTE TO SHIFT
ZERO2	DEC	B		;DECREMENT SHIFT COUNT
	RLA			;SHIFT LEFT
	JP	NC,ZERO2	;NORMALIZE LIKE SOB
	INC	B		;CORRECT SHIFT COUNT
	RRA			;WE DID IT ONE TOO MANY TIMES
	LD	C,A		;RESULT TO [C]
	JP	NORM4		;ALL DONE

ZERO3	DEC	B		;DECREMENT SHIFT COUNT
	ADD	HL,HL		;ROTATE (HL) LEFT ONE, SHIFT IN A ZERO
	LD	A,D		;ROTATE NEXT HIGHER ORDER LEFT ONE
	RLA
	LD	D,A
	LD	A,C		;ROTATE HIGH ORDER LEFT ONE
	ADC	A,A		;SET CONDITION CODES
	LD	C,A
NORM3	JP	P,ZERO1		; Not done - Keep going
NORM4	LD	A,B		;ALL NORMALIZED, GET SHIFT COUNT
	LD	E,H		;PUT LO'S BACK IN E,B
	LD	B,L
	OR	A		;CHECK IF WE DID NO SHIFTING
	JP	Z,ROUND
	LD	HL,FAC		;LOOK AT FAC'S EXPONENT
	ADD	A,(HL)		;UPDATE EXPONENT
	LD	(HL),A
	JP	NC,$ZERO	;CHECK FOR UNDERFLOW
	JP	Z,$ZERO		;NUMBER IS ZERO, ALL DONE
				;FALL INTO ROUND AND WE ARE DONE

;	Round result in C,D,E,B and put number in the FAC
;	Alters A,B,C,D,E,H,L
;	We round C,D,E up or down depending upon the MSB of B
ROUND	LD	A,B		;SEE IF WE SHOULD ROUND UP

ROUNDB	LD	HL,FAC		;ENTRY FROM FDIV, GET POINTER TO EXPONENT
	OR	A		;(  ?? INTEL FLOATING SOFTWARE FLAG ?? )
	CALL	M,ROUNDA	;DO IT IF NECESSARY
	LD	B,(HL)		;PUT EXPONENT IN B
				;HERE WE PACK THE HO AND SIGN
	INC	HL		;POINT TO SIGN
	LD	A,(HL)		;GET SIGN
	AND	80H		;GET RID OF UNWANTED BITS
	XOR	C		;PACK SIGN AND HO
	LD	C,A		;SAVE IT IN C
	JP	$MOVFR		;SAVE NUMBER IN FAC

;	Subroutine for ROUND:  add one to C,D,E
ROUNDA	INC	E		;ADD ONE TO THE LOW ORDER, ENTRY FROM QINT
	RET	NZ		;ALL DONE IF IT IS NOT ZERO
	INC	D		;ADD ONE TO NEXT HIGHER ORDER
	RET	NZ		;ALL DONE IF NO OVERFLOW
	INC	C		;ADD ONE TO THE HIGHEST ORDER
	RET	NZ		;RETURN IF NO OVEFLOW
	LD	C,80H		;THE NUMBER OVERFLOWED, SET NEW HIGH ORDER
	INC	(HL)		;UPDATE EXPONENT
	RET	NZ		;RETURN IF IT DID NOT OVERFLOW
	JP	$OVFLS1		;OVERFLOW AND CONTINUE

;	Add (HL)+2,1,0 to C,D,E
;	This code is used by FADD, FOUT
;
;	Add number pointed by HL to CDE
;
FADDA	LD	A,(HL)		;GET LOWEST ORDER
	ADD	A,E		;ADD IN OTHER LOWEST ORDER
	LD	E,A		;SAVE IT
	INC	HL		;UPDATE POINTER TO NEXT BYTE
	LD	A,(HL)		;ADD MIDDLE ORDERS
	ADC	A,D
	LD	D,A
	INC	HL		;UPDATE POINTER TO HIGH ORDER
	LD	A,(HL)		;ADD HIGH ORDERS
	ADC	A,C
	LD	C,A
	RET			;ALL DONE

;	Negate number in C,D,E,B
;	This code is used by FADD, QINT
;	Alters A,B,C,D,E,L
NEGR	LD	HL,FAC_1	;NEGATE FAC
	LD	A,(HL)		;GET SIGN
	CPL			;COMPLEMENT IT
	LD	(HL),A		;SAVE IT AGAIN
	XOR	A		;ZERO A
	LD	L,A		;SAVE ZERO IN L
	SUB	B		;NEGATE LOWEST ORDER
	LD	B,A		;SAVE IT
	LD	A,L		;GET A ZERO
	SBC	A,E		;NEGATE NEXT HIGHEST ORDER
	LD	E,A		;SAVE IT
	LD	A,L		;GET A ZERO
	SBC	A,D		;NEGATE NEXT HIGHEST ORDER
	LD	D,A		;SAVE IT
	LD	A,L		;GET ZERO BACK
	SBC	A,C		;NEGATE HIGHEST ORDER
	LD	C,A		;SAVE IT
	RET			;ALL DONE

;	Shift C,D,E right
;	A = shift count
;	Alters A,B,C,D,E,L
;	The idea is to shift right 8 places as many times as possible
SHIFTR	LD	B,00H		;ZERO OVERFLOW BYTE
SHFTR1	SUB	08H		;CAN WE SHIFT IT 8 RIGHT?
	JP	C,SHFTR2	;NO, SHIFT IT ONE PLACE AT A TIME
				;THIS LOOP SPEEDS THINGS UP BY
				; SHIFTING 8 PLACES AT ONE TIME
	LD	B,E		;SHIFT NUMBER 1 BYTE RIGHT
	LD	E,D
	LD	D,C
	LD	C,00H		;PUT 0 IN HO
	JP	SHFTR1		;TRY TO SHIFT 8 RIGHT AGAIN

; 	Shift right number in BCDE
;
SHFTR2	ADD	A,09H		;CORRECT SHIFT COUNT
	LD	L,A		;SAVE SHIFT COUNT
;	TEST FOR CASE (VERY COMMON) WHERE SHIFTING SMALL INTEGER RIGHT.
;	THIS HAPPENS IN FOR LOOPS, ETC.
	LD	A,D		;SEE IF THREE LOWS ARE ZERO.
	OR	E
	OR	B
	JP	NZ,SHFTR22	;IF SO, DO USUAL.
	LD	A,C		;GET HIGH BYTE TO SHIFT
SHFTR21	DEC	L		;DONE SHIFTING?
	RET	Z		;YES, DONE
	RRA			;ROTATE ONE RIGHT
	LD	C,A		;SAVE RESULT
	JP	NC,SHFTR21	;ZAP BACK AND DO NEXT ONE IF NONE
	JP	SHRADD		;CONTINUE SHIFTING

SHFTR22	XOR	A		;CLEAR CARRY
	DEC	L		;ARE WE DONE SHIFTING?
	RET	Z		;RETURN IF WE ARE
	LD	A,C		;GET HO
SHFTR4	RRA			;ENTRY FROM FADD, SHIFT IT RIGHT
	LD	C,A		;SAVE IT
SHRADD	LD	A,D		;SHIFT NEXT BYTE RIGHT
	RRA
	LD	D,A
	LD	A,E		;SHIFT LOW ORDER RIGHT
	RRA
	LD	E,A
	LD	A,B		;SHIFT OVERFLOW BYTE RIGHT
	RRA
	LD	B,A
	JP	SHFTR22		;SEE IF WE ARE DONE


;-----------------------------------------------------------------------------
;	NATURAL LOG FUNCTION
;
;	Calculation is by:
;	ln(F*2^N)=(N+log2(F))*ln(2)
;	An approximation polynomial is used to calculate log2(F)
;
;	Constants used by LOG
$SONE	DB	00H,00H,00H,81H		; 1
	;Constants for P(x) to calculate LOG2(x)=P(x)/Q(x) Hart #252
$LOGP	DB	04H			;HART 2524 COEFFICIENTS
	DB	9AH,0F7H,19H,83H	;4.8114746
	DB	24H,63H,43H,83H		;6.105852
	DB	75H,0CDH,8DH,84H	;-8.86266
	DB	0A9H,7FH,83H,82H	;-2.054667
$LOGQ	DB	04H
	DB	00H,00H,00H,81H		;1.0
	DB	0E2H,0B0H,4DH,83H	;6.427842
	DB	0AH,72H,11H,83H		;4.545171
	DB	0F4H,04H,35H,7FH	;.3535534

; 	'LOG' BASIC function
;
LOG	CALL	SIGN		;CHECK FOR A NEGATIVE OR ZERO ARGUMENT
	OR	A		;SET CC'S PROPERLY
	JP	PE,FCERR	;FAC .LE. 0, BLOW HIM OUT OF THE WATER
				;FSIGN ONLY RETURNS 0,1 OR 377 IN A
				;THE PARITY WILL BE EVEN IF A HAS 0 OR 377
	CALL	LOG10
	LD	BC,8031H	; BCDE = Ln(2)
	LD	DE,7218H
	JP	FMULT		;COMPLETE LOG CALCULATION

LOG10	CALL	$MOVRF		;MOVE FAC TO REGISTERS TOO
	LD	A,80H		;
	LD	(FAC),A		;ZERO THE EXPONENT
	XOR	B		;REMOVE 200 EXCESS FROM X
	PUSH	AF		;SAVE EXPONENT
	CALL	PUSHF		;SAVE THE FAC (X)
	LD	HL,$LOGP	;POINT TO P CONSTANTS
	CALL	$POLY		;CALCULATE P(X)
	POP	BC		;FETCH X
	POP	HL		;PUSHF WOULD ALTER DE
	CALL	PUSHF		;PUSH P(X) ON THE STACK
	EX	DE,HL		;GET LOW BYTES OF X TO (DE)
	CALL	$MOVFR		;AND MOVE TO FAC
	LD	HL,$LOGQ	;POINT TO Q COEFFICIENTS
	CALL	$POLY		;COMPUTE Q(X)
	POP	BC		;FETCH P(X) TO REGISTERS
	POP	DE
	CALL	FDIV		;CALCULATE P(X)/Q(X)
	POP	AF		;RE-FETCH EXPONENT
	CALL	PUSHF		;SAVE EVALUATION
	CALL	$FLT		;FLOAT THE EXPONENT
	POP	BC
	POP	DE
	JP	FADD		;GET EVAL. BACK


;-----------------------------------------------------------------------------
;	FLOATING POINT MULTIPLICATION AND DIVISION
;
;	Multiplication	FAC:=ARG*FAC
;	Alters A,B,C,D,E,H,L
FMULT	CALL	SIGN		;CHECK IF FAC IS ZERO
	RET	Z		;IF IT IS, RESULT IS ZERO
	LD	L,00H		;ADD THE TWO EXPONENTS, L IS A FLAG
	CALL	MULDIV		;FIX UP THE EXPONENTS
                		;SAVE THE NUMBER IN THE REGISTERS
                		; SO WE CAN ADD IT FAST
	LD	A,C		;GET HO
	LD	(FMULTA),A	;STORE HO OF REGISTERS
	EX	DE,HL		;STORE THE TWO LO'S OF THE REGISTERS
	LD	(FMULTB),HL
	LD	BC,0000H	;ZERO THE PRODUCT REGISTERS
	LD	D,B
	LD	E,B
	LD	HL,NORMAL	;PUT ADDRESS OF NORMAL, WHERE WE FINISH UP,
	PUSH	HL		; ON THE STACK
	LD	HL,FMULT2	;PUT FMULT2 ON THE STACK TWICE, SO AFTER
	PUSH	HL		; WE MULTIPLY BY THE LO BYTE, WE WILL
	PUSH	HL		; MULTIPLY BY THE MO AND HO
	LD	HL,FACLO	;GET ADDRESS OF LO OF FAC

;	8 bit multiply
FMULT2	LD	A,(HL)		;GET BYTE TO MULTIPLY BY
	INC	HL		;MOVE POINTER TO NEXT BYTE
	OR	A
	JP	Z,FMULT3	;ARE WE MULTIPLYING BY ZERO?
	PUSH	HL		;SAVE POINTER
	EX	DE,HL		;GET LO'S IN (HL)
	LD	E,08H		;SET UP A COUNT
;	The product will be formed in C,D,E,B. This will be in C,H,L,B part of the
;	time in order to use the "DAD" instruction. At FMULT2, we get the next
;	byte of the mantissa in the FAC to multiply by. ((HL) points to it)
;	(The FMULT2 subroutine preserves (HL))  In 8K, if the byte is zero, we just
;	shift the product 8 right. This byte is then shifted right and saved in D
;	(H in 4K). The carry determines if we should add in the second factor
;	If we do, we add it to C,H,L. B is only used to determine which way we
;	round. We then shift C,H,L,B (C,D,E,B in 4K) right one to get ready for the
;	next time through the loop. Note that the carry is shifted into the MSB of
;	C. E has a count (L in 4K) to determine when we have looked at all the bits
;	of D (H in 4K).
FMULT3?	RRA			;ROTATE BYTE RIGHT
	LD	D,A		;SAVE IT
	LD	A,C		;GET HO
	JP	NC,FMULT4	;DON'T ADD IN NUMBER IF BIT WAS ZERO
	PUSH	DE		;SAVE COUNTERS
	LD	DE,0000H	;GET LO'S OF NUMBER TO ADD, THIS IS SET ABOVE
FMULTB	EQU	$-2
	ADD	HL,DE		;ADD THEM IN
	POP	DE		;GET COUNTERS BACK
	ADC	A,00H		;ADD IN HO, THIS IS SET UP ABOVE
FMULTA	EQU	$-1
FMULT4	RRA			;ROTATE RESULT RIGHT ONE
	LD	C,A
	LD	A,H		;ROTATE NEXT BYTE
	RRA
	LD	H,A
	LD	A,L		;ROTATE NEXT LOWER ORDER
	RRA
	LD	L,A
	LD	A,B		;ROTATE LO
	RRA
	LD	B,A
	AND	10H		;SEE IF WE ROTATED THRU ST
	JP	Z,FMULT6	;IF NOT DON'T WORRY
	LD	A,B		;RE FETCH LO
	OR	20H		;"OR" IN STICKY
	LD	B,A		;BACK TO LO
FMULT6	DEC	E		;ARE WE DONE?
	LD	A,D		;GET NUMBER WE ARE MULTIPLYING BY
	JP	NZ,FMULT3?	;MULTIPLY AGAIN IF WE ARE NOT DONE
	EX	DE,HL		;GET LO'S IN (DE)
POPHRT	POP	HL		;GET POINTER TO NUMBER TO MULTIPLY BY
	RET			;ALL DONE
; 	Shift partial product left
;
FMULT3	LD	B,E		;MULTIPLY BY ZERO: SHIFT EVERYTHING 8 RIGHT
	LD	E,D
	LD	D,C
	LD	C,A		;SHIFT IN 8 ZEROS ON THE LEFT
	RET			;ALL DONE

;	Divide FAC by 10
;	Alters A,B,C,D,E,H,L
DIV10	CALL	PUSHF		;SAVE NUMBER
	LD	HL,FTEN		;GET POINTER TO THE CONSTANT '10'
	CALL	$MOVFM		;MOVE TEN INTO THE FAC

; 	Divide FP by number on stack
;
$FDIVS?	POP	BC		;GET NUMBER BACK IN REGISTERS
	POP	DE		;FALL INTO DIVIDE AND WE ARE DONE

; 	divide BCDE by FP reg
;
;	Division	FAC := ARG/FAC
;	Alters A,B,C,D,E,H,L
FDIV	CALL	SIGN		;CHECK FOR DIVISION BY ZERO
	JP	Z,$DIV0S1	;DON'T ALLOW DIVIDE BY ZERO
	LD	L,0FFH		;SUBTRACT THE TWO EXPONENTS, L IS A FLAG
	CALL	MULDIV		;FIX UP THE EXPONENTS AND THINGS
	INC	(HL)		;Add 2 to exponent to adjust..
	JP	Z,FINEDG1
	INC	(HL)
	JP	Z,FINEDG1	; .. (jp on errors)

;	HERE WE SAVE THE FAC IN MEMORY SO WE CAN SUBTRACT IT FROM THE NUMBER
;	IN THE REGISTERS QUICKLY.
	DEC	HL		;POINT TO HO
	LD	A,(HL)		;GET HO
	LD	(FDIV13),A	;SAVE IT
	DEC	HL		;SAVE MIDDLE ORDER
	LD	A,(HL)
	LD	(FDIV12),A	;PUT IT WHERE NOTHING WILL HURT IT
	DEC	HL		;SAVE LO
	LD	A,(HL)
	LD	(FDIV11),A

;	The numerator will be kept in B,H,L. The quotient will be formed in C,D,E.
;	To get a bit of the quotient, we first save B,H,L on the stack, then
;	subtract the denominator that we saved in memory. The carry indicates
;	whether or not B,H,L was bigger than the denominator. If B,H,L was bigger,
;	the next bit of the quotient is a one. To get the old B,H,L off the stack,
;	we pop them into the PSW. If the denominator was bigger, the next bit of
;	the quotient is zero, and we get the old B,H,L back by popping it off the
;	stack. We have to keep an extra bit of the quotient in FDIVG+1 in case the
;	denominator was bigger, then B,H,L will get shifted left. If the MSB of
;	B was one, it has to be stored somewhere, so we store it in FDIVG+1. then
;	the next time through the loop B,H,L will look bigger because it has an
;	extra HO bit in FOIVG+1. We are done dividing when the MSB of C is a one.
;	This occurs when we have calculated 24 bits of the quotient. When we jump
;	to ROUND, the 25th bit of the quotient determines whether we round or not.
;	It is in the MSB of A. If initially the denominator is bigger than the
;	numerator, the first bit of the quotient will be zero. This means we
;	will go through the divide loop 26 times, since it stops on the 25th bit
;	after the first non-zero bit of the exponent. So, this quotient will look
;	shifted left one from the quotient of two numbers in which the numerator is
;	bigger. This can only occur on the first time through the loop, so C,D,E
;	are all zero. So, if we finish the loop and C,D,E are all zero, then we
;	must decrement the exponent to correct for this.
	LD	B,C		;GET NUMBER IN B,H,L
	EX	DE,HL
	XOR	A		;ZERO C,D,E AND HIGHEST ORDER
	LD	C,A		;Clear MSB of quotient
	LD	D,A		;Clear NMSB of quotient
	LD	E,A		;Clear LSB of quotient
	LD	(FDIV14),A	;Clear overflow count
FDIV1	PUSH	HL		;SAVE LO'S OF NUMBER
	PUSH	BC		;SAVE HO OF NUMBER
	LD	A,L		;SUBTRACT NUMBER THAT WAS IN FAC
	SUB	00H		;SUB n,   ;SUBTRACT LO
FDIV11	EQU	$-1
	LD	L,A		;SAVE IT
	LD	A,H		;SUBTRACT MIDDLE ORDER
	SBC	A,00H		;SBC A,n
FDIV12	EQU	$-1
	LD	H,A
	LD	A,B		;SUBTRACT HO
	SBC	A,00H		;SBC A,n
FDIV13	EQU	$-1
	LD	B,A
				;GET HIGHEST ORDER
				;WE COULD DO THIS WITH NO CODE IN RAM, BUT
				; IT WOULD BE MUCH SLOWER.
	LD	A,00H		; LD A,n
FDIV14	EQU	$-1
	SBC	A,00H		; Count for overflows
	CCF
	JP	NC,FDIV2	; Restore divisor if borrow
	LD	(FDIV14),A	; Re-save overflow count
	POP	AF		; Scrap divisor
	POP	AF
	SCF			; Set carry to Skip "POP BC" and "POP HL"
	DB	0D2H		; "JP NC,nn"
FDIV2	POP	BC		;WE SUBTRACTED TOO MUCH
	POP	HL		;GET OLD NUMBER BACK
	LD	A,C		;ARE WE DONE?
	INC	A		;SET SIGN FLAG WITHOUT AFFECTING CARRY
	DEC	A
	RRA			;PUT CARRY IN MSB
	JP	P,FDIV22	;NOT READY TO ROUND YET
	RLA			;BIT BACK TO CARRY
	LD	A,(FDIV14)	;FETCH EXTRA BIT
	RRA			;BOTH NOW IN A
	AND	0C0H		;CLEAR SUPERFLUOUS BITS
	PUSH	AF		;SAVE FOR LATER
	LD	A,B		;FETCH HO OF REMAINDER
	OR	H		;FETCH HO
	OR	L		;SEE IF OTHER REMAINDER BITS AND IF SO SET ST
	JP	Z,FDIV21	;IF NOT IGNORE
	LD	A,20H		;SET BIT
FDIV21	POP	HL		;AND THE REST OF REMAINDER
	OR	H		;"OR" IN REST
	JP	ROUNDB		;USE REMAINDER

FDIV22	RLA			;WE AREN'T, GET OLD CARRY BACK
	LD	A,E		;ROTATE EVERYTHING LEFT ONE
	RLA			;ROTATE NEXT BIT OF QUOTIENT IN
	LD	E,A
	LD	A,D
	RLA
	LD	D,A
	LD	A,C
	RLA
	LD	C,A
	ADD	HL,HL		;ROTATE A ZERO INTO RIGHT END OF NUMBER
	LD	A,B		;THE HO BYTE, FINALLY!
	RLA
	LD	B,A
	LD	A,(FDIV14)	;ROTATE THE HIGHEST ORDER
	RLA
	LD	(FDIV14),A
	LD	A,C		;ADD ONE TO EXPONENT IF THE FIRST SUBTRACTION
	OR	D		; DID NOT WORK
	OR	E
	JP	NZ,FDIV1	;THIS ISN'T THE CASE
	PUSH	HL		;SAVE PART OF NUMBER
	LD	HL,FAC		;GET POINTER TO FAC
	DEC	(HL)		;DECREMENT EXPONENT
	POP	HL		;GET NUMBER BACK
	JP	NZ,FDIV1	;DIVIDE MORE IF NO OVERFLOW OCCURED
	JP	$ZERO		;UNDERFLOW!!

;	Check special cases and add exponents for FMULT, FDIV
;	Alters A,B,H,L
MULDVS	LD	A,0FFH		;ENTRY FROM DDIV, SUBTRACT EXPONENTS
	DB	2EH		; "LD L,n" to Mask 'XOR A'
MULDVA	XOR	A		;ENTRY FROM DMULT, ADD EXPONENTS
	LD	HL,ARGHI	;GET POINTER TO SIGN AND HO OF ARG
	LD	C,(HL)		;GET HO AND SIGN FOR UNPACKING
	INC	HL		;INCREMENT POINTER TO EXPONENT
	XOR	(HL)		;GET EXPONENT
	LD	B,A		;SAVE IT IN B FOR BELOW
	LD	L,00H		;SET FLAG TO ADD THE EXPONENTS BELOW

;	Add (or subtract) exponent
;
MULDIV	LD	A,B		;IS NUMBER IN REGISTERS ZERO?
	OR	A
	JP	Z,MULDV2	;IT IS, ZERO FAC AND WE ARE DONE
	LD	A,L		;GET ADD OR SUBTRACT FLAG
	LD	HL,FAC		;GET POINTER TO EXPONENT
	XOR	(HL)		;GET EXPONENT
	ADD	A,B		;ADD IN REGISTER EXPONENT
	LD	B,A		;SAVE IT
	RRA			;CHECK FOR OVERFLOW
	XOR	B		;OVERFLOW IF SIGN IS THE SAME AS CARRY
	LD	A,B		;GET SUM
	JP	P,MULDV1	;WE HAVE OVERFLOW!!
	ADD	A,80H		;PUT EXPONENT IN EXCESS 200
	LD	(HL),A		;SAVE IT IN THE FAC
	JP	Z,POPHRT	;WE HAVE UNDERFLOW!! RETURN.
	CALL	UNPACK		;UNPACK THE ARGUMENTS
	LD	(HL),A		;SAVE THE NEW SIGN
;	DECHRT ->  DEC HL, RET
DECHRT	DEC	HL		;POINT TO EXPONENT
	RET			;ALL DONE, LEAVE HO IN A

;	Unused?
MLDVEX	CALL	SIGN		;ENTRY FROM EXP, PICK UNDERFLOW IF NEGATIVE
	CPL			;PICK OVERFLOW IF POSITIVE
	POP	HL		;DON'T SCREW UP STACK
MULDV1	OR	A		;IS ERROR OVERFLOW OR UNDERFLOW?
MULDV2	POP	HL		;GET OLD RETURN ADDRESS OFF STACK
	JP	P,$ZERO		;Result zero
	JP	FINEDG1		;Overflow error

;	Multiply number in FPREG by 10
;
;	MULTIPLY FAC BY 10
;	ALTERS A,B,C,D,E,H,L
MUL10	CALL	$MOVRF		;GET NUMBER IN REGISTERS
	LD	A,B		;GET EXPONENT
	OR	A		;RESULT IS ZERO IF ARG IS ZERO
	RET	Z		;IT IS
	ADD	A,02H		;MULTIPLY BY 4 BY ADDING 2 TO EXPONENT
	JP	C,$OVFLS2
	LD	B,A		;RESTORE EXPONENT
	CALL	FADD		;ADD IN ORIGINAL NUMBER TO GET 5 TIMES IT
	LD	HL,FAC		;ADD 1 TO EXPONENT TO MULTIPLY NUMBER BY
	INC	(HL)		; 2 TO GET 10 TIMES ORIGINAL NUMBER
	RET	NZ		;ALL DONE IF NO OVERFLOW
	JP	$OVFLS2		;Overflow error


;-----------------------------------------------------------------------------
;	SIGN, SGN, FLOAT, NEG AND ABS
;
;	Put sign of FAC in A
;	Alters A only
;	Leaves FAC alone
;	Note: To take advantage of the RST instructions to save bytes,
;	FSIGN is defined to be an RST. "FSIGN" is equivalent to "CALL SIGN"
;	The first few instructions of SIGN (the ones before SIGNC) are done
;	in the 8 bytes at the RST location.
;
;	(from ALTAIR BASIC'S RST:)
;	The FSIGN RST returns A=-1 if FAC is less than 0
;	A=0 if FAC=Ø
;	A=1 if FAC greater than zero
;	The condition codes reflect the value of [A]
;	and no other registers are modified.
;	This works only when the FAC is a single or double precision number
;	The 'VSIGN' routine is more general since
;	it will take the sign of integers as well
;	and gives "TMERR" on strings.
SIGN	LD	A,(FAC)		;Get sign of FAC
	OR	A		;CHECK IF THE NUMBER IS ZERO
	RET	Z		;IT IS, A IS ZERO
	LD	A,(FACHI)	;GET SIGN OF FAC, IT IS NON-ZERO
	DB	0FEH		;"CPI" AROUND NEXT BYTE
FCOMPS	CPL			;ENTRY FROM FCOMP, COMPLEMENT SIGN
ICOMPS	RLA			;ENTRY FROM ICOMP, PUT SIGN BIT IN CARRY
SIGNS	SBC	A,A		;A=0 IF CARRY WAS 0, A=377 IF CARRY WAS 1
	RET	NZ		;RETURN IF NUMBER WAS NEGATIVE
	INC	A		;PUT ONE IN A IF NUMBER WAS POSITIVE
	RET			;ALL DONE

;	Float the signed integer in A
;	Alters A,B,C,D,E,H,L
;
;	USE MICROSOFT FORMAT IF NOT INTEL
$FLT	LD	B,88H		;SET EXPONENT CORRECTLY
	LD	DE,0000H	;ZERO D,E
				;FALL INTO FLOATR

;	Float the signed number IN B,A,D,E
;	Alters A,B,C,D,E,H,L
FLOATR	LD	HL,FAC		;GET POINTER TO FAC
	LD	C,A		;PUT HO IN C
	LD	(HL),B		;PUT EXPONENT IN THE FAC
	LD	B,00H		;ZERO OVERFLOW BYTE
	INC	HL		;POINT TO SIGN
	LD	(HL),80H	;ASSUME A POSITIVE NUMBER
	RLA			;PUT SIGN IN CARRY
	JP	FADFLT		;GO AND FLOAT THE NUMBER

;	Absolute value of FAC
;	Alters A,H,L
ABS	CALL	VSIGN		;GET THE SIGN OF THE FAC IN A
	RET	P		;IF IT IS POSITIVE, WE ARE DONE
				;FALL INTO VNEG

;	Negate any type value in the FAC
;	Alters A,B,C,D,E,H,L
VNEG	CALL	GETYPR		;SEE WHAT KIND OF NUMBER WE HAVE
	JP	M,INEG		;WE HAVE AN INTEGER, NEGATE IT THAT WAY
	JP	Z,TMERR		;BLOW UP ON STRINGS
				;FALL INTO NEG TO NEGATE A SNG OR DBL

;	Negate number in the FAC
;	Alters A,H,L
;	Note: The number must be packed
NEG	LD	HL,FACHI	;GET POINTER TO SIGN
	LD	A,(HL)		;GET SIGN
	XOR	80H		;COMPLEMENT SIGN BIT
	LD	(HL),A		;SAVE IT
	RET			;ALL DONE

;	'SGN' BASIC function
;
;	SGN function
;	Alters A,H,L
SGN	CALL	VSIGN		;GET THE SIGN OF THE FAC IN A

;	ENTRY TO CONVERT A SIGNED NUMBER IN A TO AN INTEGER
;
;	Signed char to signed int conversion, then return
;	Get back from function, result in A (signed)
CONIA	LD	L,A		;PUT IT IN THE LO POSITION
	RLA			;EXTEND THE SIGN TO THE HO
	SBC	A,A
	LD	H,A
	JP	MAKINT		;RETURN THE RESULT AND SET VALTYP

;	Get the sign of the value in the FAC in A
;	Assumes A has the VALTYP when called
;	Alters A,H,L
VSIGN	CALL	GETYPR		;SEE WHAT KIND OF A NUMBER WE HAVE
	JP	Z,TMERR		;BLOW UP ON STRINGS
	JP	P,SIGN		;SINGLE AND DOUBLE PREC. WORK THE SAME
	LD	HL,(FACLO)	;GET THE INTEGER ARGUMENT

;	ENTRY TO FIND THE SIGN OF (HL)
;	ALTERS A ONLY
;
;	Get the sign of an integer
;	A=1 	if (H,L) .GT. 0
;	A=Ø 	if (H,L) = 0
;	A=-1	if (H,L) .LT. 0
ISIGN	LD	A,H		;GET ITS SIGN
	OR	L		;CHECK IF THE NUMBER IS ZERO
	RET	Z		;IT IS, WE ARE DONE
	LD	A,H		;IT ISN'T, SIGN IS THE SIGN OF H
	JP	ICOMPS		;GO SET A CORRECTLY


;-----------------------------------------------------------------------------
;	FLOATING POINT MOVEMENT ROUTINES
;
;	Put FAC on stack
;	Alters D,E
PUSHF	EX	DE,HL		;SAVE (HL)
	LD	HL,(FACLO)	;GET LO'S
	EX	(SP),HL		;SWITCH LO'S AND RET ADDR
	PUSH	HL		;PUT RET ADDR BACK ON STACK
	LD	HL,(FACHI)	;GET HO'S
	EX	(SP),HL		;SWITCH HO'S AND RET ADDR
	PUSH	HL		;PUT RET ADDR BACK ON STACK
	EX	DE,HL		;GET OLD (HL) BACK
	RET			;ALL DONE

;	Move number from memory [(HL)] to FAC
;	Alters B,C,D,E,H,L
;	At exit number is in B,C,D,E
;	At exit (HL):=(HL)+4
$MOVFM	CALL	$MOVRM		;GET NUMBER IN REGISTERS
				;FALL INTO MOVFR AND PUT IT IN FAC

;	Move registers (B,C,D,E) to FAC
;	Alters D,E
$MOVFR	EX	DE,HL		;GET LO'S IN (HL)
	LD	(FACLO),HL	;PUT THEM WHERE THEY BELONG
	LD	H,B		;GET HO'S IN (HL)
	LD	L,C
	LD	(FACHI),HL	;PUT HO'S WHERE THEY BELONG
	EX	DE,HL		;GET OLD (HL) BACK
	RET			;ALL DONE

;	Move FAC to registers (B,C,D,E)
;	Alters B,C,D,E,H,L
$MOVRF	LD	HL,FACLO	;GET POINTER TO FAC
				;FALL INTO MOVRM

;	Get number in registers (B,C,D,E) from memory [(HL)]
;	Alters B,C,D,E,H,L
;	At exit (HL):=(HL)+4
$MOVRM	LD	E,(HL)		;GET LO
	INC	HL		;POINT TO MO
GETBCD	LD	D,(HL)		;GET MO, ENTRY FOR BILL
	INC	HL		;POINT TO HO
	LD	C,(HL)		;GET HO
	INC	HL		;POINT TO EXPONENT
	LD	B,(HL)		;GET EXPONENT
INXHRT	INC	HL		;INC POINTER TO BEGINNING OF NEXT NUMBER
	RET			;ALL DONE

;	Move number from FAC to memory [(HL)]
;	Alters A,B,D,E,H,L
$MOVMF	LD	DE,FACLO	;GET POINTER TO FAC
				;FALL INTO MOVE

;	Move number from (DE) to (HL)
;	Alters A,B,D,E,H,L
;	Exits with (DE):=(DE)+4, (HL):=(HL)+4
MOVMM	LD	B,04H		;SET COUNTER
	JP	MOVE1		;CONTINUE WITH THE MOVE

;	Copy number value from HL to DE
;
;	MOVE ANY TYPE VALUE (AS INDICATED BY VALTYP) FROM (HL) TO (DE)
;	ALTERS A,B,D,E,H,L
MOVVFM	EX	DE,HL		;ENTRY TO SWITCH (DE) AND (HL)

;	(HL) := (DE), (VALTYP) bytes
VMOVE	LD	A,(VALTYP)	;GET THE LENGTH OF THE NUMBER
	LD	B,A		;SAVE IT AWAY
MOVE1	LD	A,(DE)		;GET WORD, ENTRY FROM VMOVMF
	LD	(HL),A		;PUT IT WHERE IT BELONGS
	INC	DE		;INCREMENT POINTERS TO NEXT WORD
	INC	HL
	DEC	B
	JP	NZ,MOVE1
	RET

;	Unpack the FAC and the registers
;	Alters A,C,H,L
;	When the number in the fac is unpacked, the assumed one in the
;	mantissa is restored, and the complement of the sign is placed
;	in FAC+1
UNPACK	LD	HL,FACHI	;POINT TO HO AND SIGN
	LD	A,(HL)		;GET HO AND SIGN
	RLCA			;DUPLICATE THE SIGN IN CARRY AND THE LSB
	SCF			;RESTORE THE HIDDEN ONE
	RRA			;RESTORE THE NUMBER IN A
	LD	(HL),A		;SAVE HO
	CCF			;GET THE COMPLEMENT OF THE SIGN
	RRA			;GET IT IN THE SIGN BIT
	INC	HL		;POINT TO TEMPORARY SIGN BYTE
	INC	HL
	LD	(HL),A		;SAVE COMPLEMENT OF SIGN
	LD	A,C		;GET HO AND SIGN OF THE REGISTERS
	RLCA			;DUPLICATE THE SIGN IN CARRY AND THE LSB
	SCF			;RESTORE THE HIDDEN ONE
	RRA			;RESTORE THE HO IN A
	LD	C,A		;SAVE THE HO
	RRA			;GET THE SIGN BACK
	XOR	(HL)		;COMPARE SIGN OF FAC AND SIGN OF REGISTERS
	RET			;ALL DONE

;	Move any type value from memory [(HL)] to FAC
;	Alters A,B,D,E,H,L
VMOVFA	LD	HL,ARGLO	;ENTRY FROM DADD, MOVE ARG TO FAC
VMOVFM	LD	DE,MOVVFM	;GET ADDRESS OF LOCATION THAT DOES
	JP	VMVVFM		; AN "XCHG" AND FALLS INTO MOVE1

;	Move any type value from FAC to memory [(HL)]
;	Alters A,B,D,E,H,L
VMOVAF	LD	HL,ARGLO	;ENTRY FROM FIN, DMUL10, DDIV10

;	MOVE FAC TO ARG
VMOVMF	LD	DE,VMOVE	;GET ADDRESS OF MOVE SUBROUTINE

VMVVFM	PUSH	DE		;SHOVE IT ON THE STACK
	LD	DE,FACLO	;GET FIRST ADDRESS FOR INT, STR, SNG
	CALL	GETYPR		;GET THE VALUE TYPE
	RET	C		;GO MOVE IT IF WE DO NOT HAVE A DBL
	LD	DE,DFACLO	;WE DO, GET LO ADDR OF THE DBL NUMBER
	RET			;GO DO THE MOVE


;-----------------------------------------------------------------------------
;	COMPARE TWO NUMBERS
;
;	Compare two single precision numbers
;	A=1	if ARG .LT. FAC
;	A=0	if ARG  =   FAC
;	A=-1	if ARG .GT. FAC
;	DOREL depends upon the fact that FCOMP returns with Carry on
;	 iff A has 377
FCOMP	LD	A,B
	OR	A		;CHECK IF ARG IS ZERO
	JP	Z,SIGN
	LD	HL,FCOMPS	;WE JUMP TO FCOMPS WHEN WE ARE DONE
	PUSH	HL		;PUT THE ADDRESS ON THE STACK
	CALL	SIGN		;CHECK IF FAC IS ZERO
	LD	A,C		;IF IT IS, RESULT IS MINUS THE SIGN OF ARG
	RET	Z		;IT IS
	LD	HL,FACHI	;POINT TO SIGN OF FAC
	XOR	(HL)		;SEE IF THE SIGNS ARE THE SAME
	LD	A,C		;IF THEY ARE DIFFERENT, RESULT IS SIGN OF ARG
	RET	M		;THEY ARE DIFFERENT
	CALL	FCOMP2		;CHECK THE REST OF THE NUMBER
FCOMPD	RRA			;NUMBERS ARE DIFFERENT, CHANGE SIGN IF
	XOR	C		; BOTH NUMBERS ARE NEGATIVE
	RET			;GO SET UP A

;	Compare FP numbers
FCOMP2	INC	HL		;POINT TO EXPONENT
	LD	A,B		;GET EXPONENT OF ARG
	CP	(HL)		;COMPARE THE TWO
	RET	NZ		;NUMBERS ARE DIFFERENT
	DEC	HL		;POINT TO HO
	LD	A,C		;GET HO OF ARG
	CP	(HL)		;COMPARE WITH HO OF FAC
	RET	NZ		;THEY ARE DIFFERENT
	DEC	HL		;POINT TO MO OF FAC
	LD	A,D		;GET MO OF ARG
	CP	(HL)		;COMPARE WITH MO OF FAC
	RET	NZ		;THE NUMBERS ARE DIFFERENT
	DEC	HL		;POINT TO LO OF FAC
	LD	A,E		;GET LO OF ARG
	SUB	(HL)		;SUBTRACT LO OF FAC
	RET	NZ		;NUMBERS ARE DIFFERENT
	POP	HL		;NUMBERS ARE THE SAME, DON'T SCREW UP STACK
	POP	HL
	RET			;ALL DONE

;	Compare two integers
;	A=1	if (D,E) .LT. (H,L)
;	A=Ø	if (D,E)  =   (H,L)
;	A=-1	if (D,E) .GT. (H,L)
;	Alters A only
ICOMP	LD	A,D		;ARE THE SIGNS THE SAME?
	XOR	H
	LD	A,H		;IF NOT, ANSWER IS THE SIGN OF (HL)
	JP	M,ICOMPS	;THEY ARE DIFFERENT
	CP	D		;THEY ARE THE SAME, COMPARE THE HO'S
	JP	NZ,ICOMP1	;GO SET UP A
	LD	A,L		;COMPARE THE LO'S
	SUB	E
	RET	Z		;ALL DONE, THEY ARE THE SAME
ICOMP1	JP	SIGNS		;GO SET UP A

;	Compare two double precision numbers
;	A=1	if ARG .LT. FAC
;	A=0	if ARG  =   FAC
;	A=-1	if ARG ,GT. FAC
;	Alters A,B,C,D,E,H,L
DCOMPD	LD	HL,ARGLO	;ENTRY WITH POINTER TO ARG IN (DE)
	CALL	VMOVE		;MOVE THE ARGUMENT INTO ARG

;	Compare two double precision numbers (ARG with FAC)
DCOMP	LD	DE,ARG		;GET POINTER TO ARG
	LD	A,(DE)		;SEE IF ARG=0
	OR	A
	JP	Z,SIGN		;ARG=0, GO SET UP A
	LD	HL,FCOMPS	;PUSH FCOMPS ON STACK SO WE WILL RETURN TO
	PUSH	HL		; TO IT AND SET UP A
	CALL	SIGN		;SEE IF FAC=0
	DEC	DE		;POINT TO SIGN OF ARGUMENT
	LD	A,(DE)		;GET SIGN OF ARG
	LD	C,A		;SAVE IT FOR LATER
	RET	Z		;FAC=0, SIGN OF RESULT IS SIGN OF ARG
	LD	HL,FACHI	;POINT TO SIGN OF FAC
	XOR	(HL)		;SEE IF THE SIGNS ARE THE SAME
	LD	A,C		;IF THEY ARE, GET THE SIGN OF THE NUMBERS
	RET	M		;THE SIGNS ARE DIFFERENT, GO SET A
	INC	DE		;POINT BACK TO EXPONENT OF ARG
	INC	HL		;POINT TO EXPONENT OF FAC
	LD	B,08H		;SET UP A COUNT
DCOMP0	LD	A,(DE)		;GET A BYTE FROM ARG
	SUB	(HL)		;COMPARE IT WITH THE FAC
	JP	NZ,FCOMPD	;THEY ARE DIFFERENT, GO SET UP A
	DEC	DE		;THEY ARE THE SAME, EXAMINE THE NEXT LOWER
	DEC	HL		; ORDER BYTES
	DEC	B		;ARE WE DONE?
	JP	NZ,DCOMP0	;NO, COMPARE THE NEXT BYTES
	POP	BC		;THEY ARE THE SAME, GET FCOMPS OFF STACK
	RET			;ALL DONE

;	Compare the double precision numbers in FAC1 and ARG
;
;	COMPARE TWO DOUBLE PRECISION NUMBERS
;	A=1	IF ARG .GT. FAC
;	A=0	IF ARG  =   FAC
;	A=-1	IF ARG .LT. FAC
;	NOTE:	THIS IS THE REVERSE OF ICOMP, FCOMP AND XDCOMP
;	ALTERS A,B,C,D,E,H,L
;
;	Double precision COMPARE
DCOMPX	CALL	DCOMP		;COMPARE THE TWO NUMBERS
	JP	NZ,FCOMPS	;NEGATE THE ANSWER, MAKE SURE THE CARRY COMES
	RET			; OUT CORRECT FOR DOCMP


;-----------------------------------------------------------------------------
;	CONVERSION ROUTINES BETWEEN INTEGERS, SINGLE AND DOUBLE PRECISION
;
;	'CINT' BASIC function
;	aka FRCINT
;
;	Force the FAC to be an integer
;	Alters A,B,C,D,E,H,L
CINT	CALL	GETYPR		;SEE WHAT WE HAVE
	LD	HL,(FACLO)	;GET FACLO+0,1 IN CASE WE HAVE AN INTEGER
	RET	M		;WE HAVE AN INTEGER, ALL DONE
	JP	Z,TMERR		;WE HAVE A STRING, THAT IS A "NO-NO"
	JP	PO,CINT11	;GO DO S.P.
	CALL	VMOVAF		;ADD D.P. .5
	LD	HL,$DHALF
	CALL	VMOVFM
	CALL	DADD
	CALL	CONSD
	JP	CINT12

CINT11	CALL	$FADDH
CINT12	LD	A,(FACHI)	;GET SIGN BYTE
	OR	A		;SET CONDITION CODES CORRECTLY
	PUSH	AF
	AND	7FH		;CLEAR SIGN
	LD	(FACHI),A	;MAKE FAC POSITIVE
	LD	A,(FAC)		;GET EXPONENT
	CP	90H		;SEE IF TOO LARGE
	JP	NC,OVERR	;
	CALL	QINT		;CONVERT TO INTEGER
	LD	A,(FAC)
	OR	A
	JP	NZ,CINT13
	POP	AF
	EX	DE,HL
	JP	CINT14

CINT13	POP	AF
	EX	DE,HL		;MOVE INTEGER TO (HL)
	JP	P,CINT15
CINT14	LD	A,H
	CPL
	LD	H,A		;COMPLEMENT (HL)
	LD	A,L
	CPL
	LD	L,A
CINT15	JP	MAKINT

;	Unused...
FRCIN4	LD	HL,OVERR	;PUT OVERR ON THE STACK SO WE WILL GET ERROR
	PUSH	HL		; IF NUMBER IS TOO BIG
				;FALL INTO CONIS

;	Convert single precision number to integer
;	Alters A,B,C,D,E,H,L
CONIS	LD	A,(FAC)		;GET THE EXPONENT
	CP	90H		;SEE IF IT IS TOO BIG
	JP	NC,CONIS2	;IT IS, BUT IT MIGHT BE -32768
	CALL	QINT		;IT ISN'T, CONVERT IT TO AN INTEGER
	EX	DE,HL		;PUT IT IN (HL)

				;ENTRY FROM IADD
CONIS1	POP	DE		;GET ERROR ADDRESS OFF STACK

;	Get back from function, result in HL
;	aka CONISS
;
;	PUT (HL) IN FACLO, SET VALTYP TO INT
;	ALTERS A ONLY
MAKINT	LD	(FACLO),HL	;STORE THE NUMBER IN FACLO

; Set type to "integer"
;
SETINT	LD	A,02H		;SET VALTYP TO "INTEGER"

; Set variable/value type
;
CONISD	LD	(VALTYP),A	;ENTRY FROM CONDS
	RET			;ALL DONE

CONIS2	LD	BC,9080H	; BCDE = -32768  (float)
	LD	DE,0000H
	CALL	FCOMP		;CHECK IF NUMBER IS -32768, ENTRY FROM FIN
	RET	NZ		;ERROR:  IT CAN'T BE CONVERTED TO AN INTEGER
	LD	H,C		;IT IS -32768, PUT IT IN (HL)
	LD	L,D
	JP	CONIS1		;STORE IT IN THE FAC AND SET VALTYP

;	'CSNG' BASIC function
;
;	Force the FAC to be a single precision number
;	Alters A,B,C,D,E,H,L
CSNG	CALL	GETYPR		;SEE WHAT KIND OF NUMBER WE HAVE
	RET	PO		;WE ALREADY HAVE A SNG, ALL DONE
	JP	M,CONSI		;WE HAVE AN INTEGER, CONVERT IT
	JP	Z,TMERR		;STRINGS!! -- ERROR!!
				;DBL PREC -- FALL INTO CONSD

;	Convert double precision number to a single precison one
;	Alters A,B,C,D,E,H,L
CONSD	CALL	$MOVRF		;GET THE HO'S IN THE REGISTERS
	CALL	SETSNG		;SET VALTYP TO "SINGLE PRECISION"
	LD	A,B		;CHECK IF THE NUMBER IS ZERO
	OR	A
	RET	Z		;IF IT IS, WE ARE DONE
	CALL	UNPACK		;UNPACK THE NUMBER
	LD	HL,DFACLO3	;GET FIRST BYTE BELOW A SNG NUMBER
	LD	B,(HL)		;PUT IT IN B FOR ROUND
	JP	ROUND		;ROUND THE DBL NUMBER UP AND WE ARE DONE

;	Convert the signed integer in FAC1 to single precision.
;
;	Convert an integer to a single precision number
;	Alters A,B,C,D,E,H,L
CONSI	LD	HL,(FACLO)	;GET THE INTEGER
;	Convert HL to SNG
CONSIH	CALL	SETSNG		;SET VALTYP TO "SINGLE PRECISION"
;	Get back from function passing an INT value HL
	LD	A,H		;SET UP REGISTERS FOR FLOATR
	LD	D,L
	LD	E,00H
	LD	B,90H
	JP	FLOATR		;GO FLOAT THE NUMBER

;	'CDBL' BASIC function
;
;	Force the FAC to be a double precision number
;	Alters A,B,C,D,E,H,L
CDBL	CALL	GETYPR		;SEE WHAT KIND OF NUMBER WE HAVE
	RET	NC		;WE ALREADY HAVE A DBL, WE ARE DONE
	JP	Z,TMERR		;GIVE AN ERROR IF WE HAVE A STRING
				;SEE IF WE HAVE A SNG OR INT
	CALL	M,CONSI		;CONVERT TO SNG IF WE HAVE AN INT
				;FALL INTO CONDS AND CONVERT TO DBL

;	Convert a single precision number to a double precision one
;	Alters A,H,L
CONDS	LD	HL,0000H	;ZERO H,L
	LD	(DFACLO),HL	;CLEAR THE FOUR LOWER BYTES IN THE DOUBLE
	LD	(DFACLO2),HL	; PRECISION NUMBER

;	Set VALTYP=8 for DBL (* not in original section of code *)
;	Set type to "double precision"
;
SETDBL	LD	A,08H		;SET VALTYP TO "DOUBLE PRECISION"
	DB	01H		; "LD BC,nn" to jump over the next word without executing it

;	Set type to "single precision"
;
SETSNG	LD	A,04H		;SET VALTYP TO "SINGLE PRECISION"
	JP	CONISD

;	Test for string type, 'Type Error' if it is not
;	a.k.a. FRCSTR
;
;	FORCE THE FAC TO BE A STRING
;	ALTERS A ONLY
;
CHKSTR	CALL	GETYPR		 ;SEE WHAT KIND OF VALUE WE HAVE
	RET	Z		 ;WE HAVE A STRING, EVERYTHING IS OK
	JP	TMERR		 ;WE DON'T HAVE A STRING, JUMP TO TMERR


;-----------------------------------------------------------------------------
;	GREATEST INTEGER FUNCTION
;
;	Quick greatest integer function
;	Leaves INT(FAC) in C,D,E (signed)
;	Assumes FAC .LT. 2^23 = 8388608 (dec)
;	Assumes the exponent of FAC is in A
;	Alters A,B,C,D,E
;
QINT	LD	B,A		;ZERO B,C,D,E IN CASE THE NUMBER IS ZERO
	LD	C,A
	LD	D,A
	LD	E,A
	OR	A		;SET CONDITION CODES
	RET	Z		;IT IS ZERO, WE ARE DONE
;	The hard case in QINT is negative non-integers. To handle this, if the
;	number is negative, we regard the 3-byte mantissa as a 3-byte integer and
;	subtarct one. Then all the fractional bits are shifted out by shifting the
;	mantissa right. Then, if the number was negative, we add one. So, if we
;	had a negative integer, all the bits to the right of the binary point were
;	zero. So the net effect is we have the original number in C,D,E. If the
;	number was a negative non-integer, there is at least one non-zero bit to the
;	right of the binary point. So the net effect is that we get the absolute
;	value of INT(FAC) in C,D,E. C,D,E is then negated if the original number was
;	negative so the result will be signed.
	PUSH	HL		;SAVE (HL)
	CALL	$MOVRF		;GET NUMBER IN THE REGISTERS
	CALL	UNPACK		;UNPACK THE NUMBER
	XOR	(HL)		;GET SIGN OF NUMBER
	LD	H,A		;DON'T LOSE IT
	CALL	M,QINTA		;SUBTRACT 1 FROM LO IF NUMBER IS NEGATIVE
	LD	A,98H		;SEE HOW MANY WE HAVE TO SHIFT TO CHANGE
	SUB	B		; NUMBER TO AN INTEGER
	CALL	SHIFTR		;SHIFT NUMBER TO GET RID OF FRACTIONAL BITS
	LD	A,H		;GET SIGN
	RLA			;PUT SIGN IN CARRY SO IT WILL NOT BE CHANGED
	CALL	C,ROUNDA	;IF NUMBER WAS NEGATIVE, ADD ONE
	LD	B,00H		;FORGET THE BITS WE SHIFTED OUT
	CALL	C,NEGR		;NEGATE NUMBER IF IT WAS NEGATIVE BECAUSE
				; WE WANT A SIGNED MANTISSA
	POP	HL		;GET OLD (HL) BACK
	RET			;ALL DONE

;	Decrement FP value in BCDE
;
QINTA	DEC	DE		 ;SUBTRACT ONE FROM C,D,E
	LD	A,D		 ;WE HAVE TO SUBTRACT ONE FROM C IF
	AND	E		 ; D AND E ARE BOTH ALL ONES
	INC	A		 ;SEE IF BOTH WERE -1
	RET	NZ		 ;THEY WERE NOT, WE ARE DONE
DEXBRT	DEC	BC		 ;THIS IS FOR BILL.  C WILL NEVER BE ZERO
	RET			 ; (THE MSB WILL ALWAYS BE ONE) SO "DCX	B"
				 ; AND "DCR	C" ARE FUNCTIONALLY EQUIVALENT

;	'FIX' BASIC function
;
;	THIS IS THE FIX (X) FUNCTION. IT RETURNS
;	  FIX(X)=SGN(X)*INT(ABS(X))
;	Double Precision to Integer conversion
FIX	CALL	GETYPR		;GET VALTYPE OF ARG
	RET	M		;INT, DONE
	CALL	SIGN		;GET SIGN
	JP	P,INT		;IF POSITIVE, JUST CALL REGULAR INT CODE
	CALL	NEG		;NEGATE IT
	CALL	INT		;GET THE INTEGER OF IT
	JP	VNEG		;NOW RE-NEGATE IT

;	'INT' BASIC function
;
;	Greatest integer function
;	Alters A,B,C,D,E,H,L
INT	CALL	GETYPR		;SEE WHAT TYPE OF A NUMBER WE HAVE
	RET	M		;IT IS AN INTEGER, ALL DONE
	JP	NC,DINT		;CONVERT THE DOUBLE PRECISION NUMBER
	JP	Z,TMERR		;BLOW UP ON STRINGS
	CALL	CONIS		;TRY TO CONVERT THE NUMBER TO AN INTEGER
				;IF WE CAN'T, WE WILL RETURN HERE TO GIVE A
				; SINGLE PRECISION RESULT

FINT	LD	HL,FAC		;GET EXPONENT
	LD	A,(HL)
	CP	98H		;SEE IF NUMBER HAS ANY FRACTIONAL BITS
	LD	A,(FACLO)	;THE ONLY GUY WHO NEEDS THIS DOESN'T CARE
	RET	NC		; ABOUT THE SIGN
	LD	A,(HL)		;GET EXPONENT BACK
	CALL	QINT		;IT DOES, SHIFT THEM OUT
	LD	(HL),98H	;CHANGE EXPONENT SO IT WILL BE CORRECT
				; AFTER NORMALIZATION
				;NOTE:QINT UNPACKED THE NUMBER!!!!
	LD	A,E		;GET LO
	PUSH	AF		;SAVE IT
	LD	A,C		;NEGATE NUMBER IF IT IS NEGATIVE
	RLA			;PUT SIGN IN CARRY
	CALL	FADFLT		;REFLOAT NUMBER
	POP	AF		;GET LO BACK
	RET			;ALL DONE

;	Greatest integer function for double precision numbers
;	Alters A,B,C,D,E,H,L
DINT	LD	HL,FAC		;GET POINTER TO FAC
	LD	A,(HL)		;GET EXPONENT
	CP	90H		;CAN WE CONVERT IT TO AN INTEGER?
	JP	NZ,DINT2	;CHECK FOR -32768
	LD	C,A		;SAVE EXPONENT IN C
	DEC	HL		;GET POINTER TO SIGN AND HO
	LD	A,(HL)		;GET SIGN AND HO
	XOR	80H		;CHECK IF IT IS 200
	LD	B,06H		;SET UP A COUNT TO CHECK IF THE REST OF
DINT1	DEC	HL		; THE NUMBER IS ZERO, POINT TO NEXT BYTE
	OR	(HL)		;IF ANY BITS ARE NON-ZERO, A WILL BE NON-ZERO
	DEC	B		;ARE WE DONE?
	JP	NZ,DINT1	;NO, CHECK THE NEXT LOWER ORDER BYTE
	OR	A		;IS A NOW ZERO?
	LD	HL,8000H	;GET -32768 JUST IN CASE
	JP	NZ,DINT11
	CALL	MAKINT		;A IS ZERO SO WE HAVE -32768
	JP	CDBL		;FORCE BACK TO DOUBLE

DINT11	LD	A,C		;GET EXPONENT
DINT2	OR	A		;CHECK FOR ZERO VALUE
	RET	Z		;***FIX 5.11***^1 -- ALA LOW 0 IN DINT
	CP	0B8H		;ARE THERE ANY FRACTIONAL BITS?
	RET	NC		;NO, THE NUMBER IS ALREADY AN INTEGER
DINTFO	PUSH	AF		;ENTRY FROM FOUT, CARRY IS ZERO
				; IF WE COME HERE FROM FOUT
	CALL	$MOVRF		;GET HO'S OF NUMBER IN REGISTERS FOR UNPACKING
	CALL	UNPACK		;UNPACK IT
	XOR	(HL)		;GET ITS SIGN BACK
	DEC	HL		;SET THE EXPONENT TO NORMALIZE CORRECTLY
	LD	(HL),0B8H
	PUSH	AF		;SAVE THE SIGN
	DEC	HL
	LD	(HL),C		;GET UNPACKED HIGH BYTE
	CALL	M,DINTA		;SUBTRACT 1 FROM LO IF NUMBER IS NEGATIVE
	LD	A,(FACHI)	;FETCH NEW HIGH MANTISSA BYTE
	LD	C,A		;AND PUT IN C
	LD	HL,FACHI	;POINT TO THE HO OF THE FAC
	LD	A,0B8H		;GET HOW MANY BITS WE HAVE TO SHIFT OUT
	SUB	B
	CALL	DSHFTR		;SHIFT THEM OUT!!
	POP	AF		;GET THE SIGN BACK
	CALL	M,DROUNA	;IF NUMBER WAS NEGATIVE, ADD ONE
	XOR	A		;PUT A ZERO IN THE EXTRA LO BYTE SO WHEN
	LD	(DFACLO1),A	; WE NORMALIZE, WE WILL SHIFT IN ZEROS
	POP	AF		;IF WE WERE CALLED FROM FOUT, DON'T NORMALIZE,
	RET	NC		; JUST RETURN
	JP	DNORML		;RE-FLOAT THE INTEGER

DINTA	LD	HL,DFACLO	;SUBTRACT ONE FROM FAC, GET POINTER TO LO
DINTA1	LD	A,(HL)		;GET A BYTE OF FAC
	DEC	(HL)		;SUBTRACT ONE FROM IT
	OR	A		;CONTINUE ONLY IF THE BYTE USED TO BE ZERO
	INC	HL		;INCREMENT POINTER TO NEXT BYTE
	JP	Z,DINTA1	;CONTINUE IF NECESSARY
	RET			;ALL DONE


;-----------------------------------------------------------------------------
;	INTEGER ARITHMETIC ROUTINES
;
;	Two byte unsigned integer multiply
;	for multiple dimensioned arrays
;
;	(DE):=(BC)*(DE)
;
;	A,D,E,H,L are changed
;	(DMULT in ALTAIR BASIC source)
UMULT	PUSH	HL		;SAVE [H,L]
	LD	HL,0000H	;ZERO PRODUCT REGISTERS
	LD	A,B		;CHECK IF (BC) IS ZERO
	OR	C		;IF SO, JUST RETURN, (HL) IS ALREADY ZERO
	JP	Z,UMULTX	;THIS IS DONE FOR SPEED
	LD	A,10H		;SET UP A COUNT
UMULT1	ADD	HL,HL		;ROTATE (HL) LEFT ONE
	JP	C,BSERR		;CHECK FOR OVERFLOW, IF SO,
	EX	DE,HL		; BAD SUBSCRIPT (BS) ERROR
	ADD	HL,HL		;ROTATE (DE) LEFT ONE
	EX	DE,HL
	JP	NC,UMULT2	;ADD IN (BC) IF HO WAS 1
	ADD	HL,BC
	JP	C,BSERR		;CHECK FOR OVERFLOW
UMULT2	DEC	A		;SEE IF DONE
	JP	NZ,UMULT1
UMULTX	EX	DE,HL		;RETURN THE RESULT IN [D,E]
	POP	HL		;GET BACK THE SAVED [H,L]
	RET

;	INTEGER ARITHMETIC CONVENTIONS
;
;	Integer variables are 2 byte, signed numbers
;		The LO byte comes first in memory
;
;	Calling conventions:
;	For one argument functions:
;		The argument is in (HL), the result is left in (HL)
;	For two argument operations:
;		The first argument is in (DE)
;		The second argument is in (HL)
;		The result is left in (HL)
;	If overflow occurs, the arguments are converted to single precision
;	when integers are stored in the FAC, they are stored at FACLO+0,1
;	VALTYP(INTEGER)=2

;	Integer SUB
;
;	Integer subtraction	(HL):=(DE)-(HL)
;	Alters A,B,C,D,E,H,L
ISUB	LD	A,H		;EXTEND THE SIGN OF (HL) TO B
	RLA			;GET SIGN IN CARRY
	SBC	A,A
	LD	B,A
	CALL	INEGHL		;NEGATE (HL)
	LD	A,C		;GET A ZERO
	SBC	A,B		;NEGATE SIGN
	JP	IADDS		;GO ADD THE NUMBERS

;	Integer ADD
;
;	Integer addition	(HL):=(DE)+(HL)
;	Alters A,B,C,D,E,H,L
IADD	LD	A,H		;EXTEND THE SIGN OF (HL) TO B
	RLA			;GET SIGN IN CARRY
	SBC	A,A
IADDS	LD	B,A		;SAVE THE SIGN
	PUSH	HL		;SAVE THE SECOND ARGUMENT IN CASE OF OVERFLOW
	LD	A,D		;EXTEND THE SIGN OF (DE) TO A
	RLA			;GET SIGN IN CARRY
	SBC	A,A
	ADD	HL,DE		;ADD THE TWO LO'S
	ADC	A,B		;ADD THE EXTRA HO
	RRCA			;IF THE LSB OF A IS DIFFERENT FROM THE MSB OF
	XOR	H		; H, THEN OVERFLOW OCCURED
	JP	P,CONIS1	;NO OVERFLOW, GET OLD (HL) OFF STACK
				; AND WE ARE DONE, SAVE (HL) IN THE FAC ALSO
	PUSH	BC		;OVERFLOW -- SAVE EXTENDED SIGN OF (HL)
	EX	DE,HL		;GET (DE) IN (HL)
	CALL	CONSIH		;FLOAT IT
	POP	AF		;GET SIGN OF (HL) IN A
	POP	HL		;GET OLD (HL) BACK
	CALL	PUSHF		;PUT FIRST ARGUMENT ON STACK
	EX	DE,HL		;PUT SECOND ARGUMENT IN (DE) FOR FLOATR
	CALL	INEGAD		;FLOAT IT
	JP	FADDT		;ADD THE TWO NUMBERS USING SINGLE PRECISION

;	Integer MULTIPLY
;
;	Integer multiplication	(HL):=(DE)*(HL)
;	Alters A,B,C,D,E,H,L
IMULT	LD	A,H		;CHECK (HL) IF IS ZERO, IF SO
	OR	L		; JUST RETURN.  THIS IS FOR SPEED.
	JP	Z,MAKINT	;UPDATE FACLO TO BE ZERO AND RETURN
	PUSH	HL		;SAVE SECOND ARGUMENT IN CASE OF OVERFLOW
	PUSH	DE		;SAVE FIRST ARGUMENT
	CALL	IMULDV		;FIX UP THE SIGNS
	PUSH	BC		;SAVE THE SIGN OF THE RESULT
	LD	B,H		;COPY SECOND ARGUMENT INTO (BC)
	LD	C,L
	LD	HL,0000H	;ZERO (HL), THAT IS WHERE THE PRODUCT GOES
	LD	A,10H		;SET UP A COUNT
IMULT1	ADD	HL,HL		;ROTATE PRODUCT LEFT ONE
	JP	C,IMULTS	;CHECK FOR OVERLFOW
	EX	DE,HL		;ROTATE FIRST ARGUMENT LEFT ONE TO SEE IF
	ADD	HL,HL		; WE ADD IN (BC) OR NOT
	EX	DE,HL
	JP	NC,IMULT2	;DON'T ADD IN ANYTHING
	ADD	HL,BC		;ADD IN (BC)
	JP	C,IMULTS	;CHECK FOR OVERFLOW
IMULT2	DEC	A		;ARE WE DONE?
	JP	NZ,IMULT1	;NO, DO IT AGAIN
	POP	BC		;WE ARE DONE, GET SIGN OF RESULT
	POP	DE		;GET ORIGINAL FIRST ARGUMENT
IMULT21	LD	A,H		;ENTRY FROM IDIV, IS RESULT .GE. 32768?
	OR	A
	JP	M,IMULT3	;IT IS, CHECK FOR SPECIAL CASE OF -32768
	POP	DE		;RESULT IS OK, GET SECOND ARGUMENT OFF STACK
	LD	A,B		;GET THE SIGN OF RESULT IN A
	JP	INEGA		;NEGATE THE RESULT IF NECESSARY

IMULT3	XOR	80H		;IS RESULT 32768?
	OR	L		;NOTE: IF WE GET HERE FROM IDIV, THE RESULT
	JP	Z,IMULT4	; MUST BE 32768, IT CANNOT BE GREATER
	EX	DE,HL		;IT IS .GT. 32768, WE HAVE OVERFLOW
	DB	01H		; "LD BC,nn" OVER NEXT 2 BYTES
IMULTS	POP	BC		;GET SIGN OF RESULT OFF STACK
	POP	HL		;GET THE ORIGINAL FIRST ARGUMENT
	CALL	CONSIH		;FLOAT IT
	POP	HL		;GET THE ORIGINAL SECOND ARGUMENT
	CALL	PUSHF		;SAVE FLOATED FIRST ARGUMENT
	CALL	CONSIH		;FLOAT SECOND ARGUMENT
FMULTT	POP	BC
	POP	DE		;GET FIRST ARGUMENT OFF STACK, ENTRY FROM POLYX
	JP	FMULT		;MULTIPLY THE ARGUMENTS USING SINGLE PRECISION

IMULT4	LD	A,B		;IS RESULT +32768 OR -32768?
	OR	A		;GET ITS SIGN
	POP	BC		;DISCARD ORIGINAL SECOND ARGUMENT
	JP	M,MAKINT	;THE RESULT SHOULD BE NEGATIVE, IT IS OK
	PUSH	DE		;IT IS POSITIVE, SAVE REMAINDER FOR MOD
	CALL	CONSIH		;FLOAT -32768
	POP	DE		;GET MOD'S REMAINDER BACK
	JP	NEG		;NEGATE -32768 TO GET 32768, WE ARE DONE

;	Divide the signed integer in DE by the signed integer in HL.
;
;	INTEGER DIVISION	(HL):=(DE)/(HL)
;	REMAINDER IS IN (DE), QUOTIENT IN (HL)
;	ALTERS A,B,C,D,E,H,L
IDIV	LD	A,H		;CHECK FOR DIVISION BY ZERO
	OR	L
	JP	Z,DV0ERR	;WE HAVE DIVISION BY ZERO!!
	CALL	IMULDV		;FIX UP THE SIGNS
	PUSH	BC		;SAVE THE SIGN OF THE RESULT
	EX	DE,HL		;GET DENOMINATOR IN (HL)
	CALL	INEGHL		;NEGATE IT
	LD	B,H		;SAVE NEGATED DENOMINATOR IN (BC)
	LD	C,L
	LD	HL,0000H	;ZERO WHERE WE DO THE SUBTRACTION
	LD	A,11H		;SET UP A COUNT
	PUSH	AF		;SAVE IT
	OR	A		;CLEAR CARRY
	JP	IDIV14		;GO DIVIDE

IDIV12	PUSH	AF		;SAVE COUNT
	PUSH	HL		;SAVE (HL) I.E. CURRENT NUMERATOR
	ADD	HL,BC		;SUBTRACT DENOMINATOR
	JP	NC,IDIV13	;WE SUBTRACTED TOO MUCH, GET OLD (HL) BACK
	POP	AF		;THE SUBTRACTION WAS GOOD, DISCARD OLD (HL)
	SCF			;NEXT BIT IN QUOTIENT IS A ONE
	DB	3EH		; "LD A,n" OVER NEXT BYTE
IDIV13	POP	HL		;IGNORE THE SUBTRACTION, WE COULDN'T DO IT
IDIV14	LD	A,E		;SHIFT IN THE NEXT QUOTIENT BIT
	RLA
	LD	E,A
	LD	A,D		;SHIFT THE HO
	RLA
	LD	D,A
	LD	A,L		;SHIFT IN THE NEXT BIT OF THE NUMERATOR
	RLA
	LD	L,A
	LD	A,H		;DO THE HO; Double MSB of quotient (MSW)
	RLA
	LD	H,A		;SAVE THE HO
	POP	AF		;GET COUNT BACK
	DEC	A		;ARE WE DONE?
	JP	NZ,IDIV12	;NO, DIVIDE AGAIN
	EX	DE,HL		;GET QUOTIENT IN (HL), REMAINDER IN (DE)
	POP	BC		;GET SIGN OF RESULT
	PUSH	DE		;SAVE REMAINDER SO STACK WILL BE ALRIGHT
	JP	IMULT21		;CHECK FOR SPECIAL CASE OF 32768

;	GET READY TO MULTIPLY OR DIVIDE
;	ALTERS A,B,C,D,E,H,L
IMULDV	LD	A,H		;GET SIGN OF RESULT
	XOR	D
	LD	B,A		;SAVE IT IN B
	CALL	INEGH		;NEGATE SECOND ARGUMENT IF NECESSARY
	EX	DE,HL		;PUT (DE) IN (HL), FALL IN
				; AND NEGATE FIRST ARGUMENT IF NECESSARY

;	Negate H,L
;	Alters A,C,H,L
INEGH	LD	A,H		;GET SIGN OF (HL)
INEGA	OR	A		;SET CONDITION CODES
	JP	P,MAKINT	;WE DON'T HAVE TO NEGATE, IT IS POSITIVE
				;SAVE THE RESULT IN THE FAC FOR WHEN
				; OPERATORS RETURN THROUGH HERE
INEGHL	XOR	A
	LD	C,A		;CLEAR A
	SUB	L		;STORE A ZERO (WE USE THIS METHOD FOR ISUB)
	LD	L,A		;NEGATE LO
	LD	A,C		;SAVE IT
	SBC	A,H		;GET A ZERO BACK
	LD	H,A		;NEGATE HO
	JP	MAKINT		;SAVE IT
				;ALL DONE, SAVE THE RESULT IN THE FAC
				; FOR WHEN OPERATORS RETURN THROUGH HERE

;	Integer negation
;	Alters A,B,C,D,E,H,L
INEG	LD	HL,(FACLO)	;GET THE INTEGER
	CALL	INEGHL		;NEGATE IT
	LD	A,H		;GET THE HIGH ORDER
	XOR	80H		;CHECK FOR SPECIAL CASE OF 32768
	OR	L
	RET	NZ		;IT DID NOT OCCUR, EVERYTHING IS FINE
MAKSNG	EX	DE,HL		;WE HAVE IT, FLOAT 32768
	CALL	SETSNG		;CHANGE VALTYP TO "SINGLE PRECISION"
	XOR	A		;GET A ZERO FOR THE HIGH ORDER
INEGAD	LD	B,98H		;ENTRY FROM IADD, SET EXPONENT
	JP	FLOATR		;GO FLOAT THE NUMBER

;	MOD OPERATOR
;	(HL):=(DE)-(DE)/(HL)*(HL),  (DE)=QUOTIENT
;	ALTERS A,B,C,D,E,H,L
IMOD	PUSH	DE		;SAVE (DE) FOR ITS SIGN
	CALL	IDIV		;DIVIDE AND GET THE REMAINDER
	XOR	A		;TURNOFF THE CARRY AND TRANFER
	ADD	A,D		;THE REMAINDER*2 WHICH IS IN [D,E]
	RRA			;TO [H,L] DIVIDING BY TWO
	LD	H,A
	LD	A,E
	RRA
	LD	L,A		; ***WHG01*** FIX TO MOD OPERATOR
	CALL	SETINT		;SET VALTYP TO "INTEGER" IN CASE RESULT OF
	POP	AF
	JP	INEGA


;-----------------------------------------------------------------------------
;	DOUBLE PRECISION ARITHMETIC ROUTINES
;
;	DOUBLE PRECISION ARITHMETIC CONVENTIONS
;
;Double precision numbers are 8 byte quantities
;The last 4 bytes in memory are in the same format as single precision numbers
;The first 4 bytes are 32 more low order bits of precision
;The lowest order byte comes first in memory
;
;Calling conventions:
;For one argument functions:
;	The argument is in the FAC, the result is left in the FAC
;For two argument operations:
;	The first argument is in ARG-7,6,5,4,3,2,1,0 (Note: ARGLO=ARG-7)
;	The second argument is in the FAC
;	The result is left in the FAC
;VALTYP (double precision)=10 OCTAL

;	Double precision subtraction	FAC:=FAC-ARG
;	Alters all registers
DSUB	LD	HL,ARGHI	;NEGATE THE SECOND ARGUMENT
	LD	A,(HL)		;GET THE HO AND SIGN
	XOR	80H		;COMPLEMNT THE SIGN
	LD	(HL),A		;PUT IT BACK
				;FALL INTO DADD

;	Double precision addition	FAC:=FAC+ARG
;	Alters all registers
DADD	LD	HL,ARG		;GET  POINTER TO EXPONENT OF FIRST ARGUMENT
	LD	A,(HL)		;CHECK IF IT IS ZERO
	OR	A
	RET	Z		;IT IS, RESULT IS ALREADY IN FAC
	LD	B,A		;SAVE EXPONENT FOR UNPACKING
	DEC	HL		;POINT TO HO AND SIGN
	LD	C,(HL)		;GET HO AND SIGN FOR UNPACKING
	LD	DE,FAC		;GET POINTER TO EXPONENT OF SECOND ARGUMENT
	LD	A,(DE)		;GET EXPONENT
	OR	A		;SEE IF IT IS ZERO
	JP	Z,VMOVFA	;IT IS, MOVE ARG TO FAC AND WE ARE DONE
	SUB	B		;SUBTRACT EXPONENTS TO GET SHIFT COUNT
	JP	NC,DADD2	;PUT THE SMALLER NUMBER IN FAC
	CPL			;NEGATE SHIFT COUNT
	INC	A
	PUSH	AF		;SAVE SHIFT COUNT
	LD	C,08H		;SWITCH FAC AND ARG, SET UP A COUNT
	INC	HL		;POINT TO ARG
	PUSH	HL		;SAVE POINTER TO ARG
DADD1	LD	A,(DE)		;GET A BYTE OF THE FAC
	LD	B,(HL)		;GET A BYTE OF ARG
	LD	(HL),A		;PUT THE FAC BYTE IN ARG
	LD	A,B		;PUT THE ARG BYTE IN A
	LD	(DE),A		;PUT THE ARG BYTE IN FAC
	DEC	DE		;POINT TO THE NEXT LO BYTE OF FAC
	DEC	HL		;POINT TO THE NEXT LO BYTE OF ARG
	DEC	C		;ARE WE DONE?
	JP	NZ,DADD1	;NO, DO THE NEXT LO BYTE
	POP	HL		;GET THE HO BACK
	LD	B,(HL)		;GET THE EXPONENT
	DEC	HL		;POINT TO THE HO AND SIGN
	LD	C,(HL)		;GET HO AND SIGN FOR UNPACKING
	POP	AF		;GET THE SHIFT COUNT BACK

;	WE NOW HAVE THE SUSPECTED LARGER NO IN THE FAC, WE NEED
;	TO KNOW IF WE ARE TO SUBTRACT (SIGNS ARE DIFFERENT) AND
;	WE NEED TO RESTORE THE HIDDEN MANTISSA BIT
;	FURTHER, IF THERE IS TO BE MORE THAN 56 BITS SHIFTED
;	TO ALIGN THE BINARY POINTS THEN THE LESSOR NO. IS
;	INSIGNIFICANT IN COMPARISON TO THE LARGER NO. SO WE
;	CAN JUST RETURN AND CALL THE LARGER NO. THE ANSWER.
DADD2	CP	56+1		;ARE WE WITHIN 56 BITS?
	RET	NC		;NO, ALL DONE
	PUSH	AF		;SAVE SHIFT COUNT
	CALL	UNPACK		;UNPACK THE NUMBERS
	LD	HL,ARGLO11	;POINT TO ARGLO-1
	LD	B,A		;SAVE SUBTRACTION FLAG
	LD	A,00H		;
	LD	(HL),A		;CLEAR TEMPORARY LEAST SIG BYTE
	LD	(DFACLO1),A	;CLEAR EXTRA BYTE
	POP	AF		;GET SHIFT COUNT
	LD	HL,ARGHI	;POINT TO THE HO OF ARG
	CALL	DSHFTR		;SHIFT ARG RIGHT THE RIGHT NUMBER OF
	LD	A,B		;TRANSFER OVERFLOW BYTE
	OR	A		;FROM ARG TO FAC
	JP	P,DADD3
	LD	A,(ARGLO11)	;GET SUBTRACTION FLAG
	LD	(DFACLO1),A	;SUBTRACT NUMBERS IF THEIR SIGNS ARE
	CALL	DADDAR		;SIGNS ARE THE SAME, ADD THE NUMBERS
	JP	NC,DROUND	;ROUND THE RESULT IF NO CARRY
	EX	DE,HL		;GET POINTER TO FAC IN (HL)
	INC	(HL)		;ADD 1 TO EXPONENT
	JP	Z,$OVFLS
;**************************************************************
; WE ARE NOW SET TO SHIFT THE FAC RIGHT 1 BIT. RECALL WE GOT HERE WITH CF=1.
; THE INSTRUCTIONS SINCE WE GOT HERE HAVEN'T AFFECTED
; CF SO WHEN WE SHIFT RIGHT WE WILL SHIFT CF INTO THE HIGH MANTISSA BIT.
;*************************************************************
	CALL	DSHRRA1		;SHIFT NUMBER RIGHT ONE, SHIFT IN CARRY
	JP	DROUND		;ROUND THE RESULT

DADD3	LD	A,9EH		;"SBC A,(HL)", SUBTRACT THE NUMBERS
	CALL	DADSBA		;GET THE SUBTRACT INSTRUCTION IN A
	LD	HL,FAC_1	;SUBTRACT THE NUMBERS
	CALL	C,DNEGR		;FIX [H,L] TO POINT TO SIGN FOR DNEGR
				;NEGATE THE RESULT IF IT WAS NEGATIVE
				;FALL INTO DNORML
;	Normalize FAC
;	Alters A,B,C,D,H,L
DNORML	XOR	A		;CLEAR SHIFT COUNT
DNORM1	LD	B,A		;SAVE SHIFT COUNT
	LD	A,(FACHI)	;GET HO
	OR	A		;SEE IF WE CAN SHIFT 8 LEFT
	JP	NZ,DNORM5	;WE CAN'T, SEE IF NUMBER IS NORMALIZED
	LD	HL,DFACLO1	;WE CAN, GET POINTER TO LO
	LD	C,08H		;SET UP A COUNT
DNORM2	LD	D,(HL)		;GET A BYTE OF FAC
	LD	(HL),A		;PUT IN BYTE FROM LAST LOCATION,
				; THE FIRST TIME THROUGH A IS ZERO
	LD	A,D		;PUT THE CURRENT BYTE IN A FOR NEXT TIME
	INC	HL		;INCREMENT POINTER TO NEXT HIGHER ORDER
	DEC	C		;ARE WE DONE?
	JP	NZ,DNORM2	;NO, DO THE NEXT BYTE
	LD	A,B		;SUBTRACT 8 FROM SHIFT COUNT
	SUB	08H
	CP	0C0H		;HAVE WE SHIFTED ALL BYTES TO ZERO?
	JP	NZ,DNORM1	;NO, TRY TO SHIFT 8 MORE
	JP	$ZERO		;YES, THE NUMBER IS ZERO

DNORM3	DEC	B		;DECREMENT SHIFT COUNT
	LD	HL,DFACLO1	;GET POINTER TO LO
	CALL	DSHFLC		;SHIFT THE FAC LEFT
	OR	A		;SEE IF NUMBER IS NORMALIZED
DNORM5	JP	P,DNORM3	;SHIFT FAC LEFT ONE IF IT IS NOT NORMALIZED
	LD	A,B		;GET THE SHIFT COUNT
	OR	A		;SEE IF NO SHIFTING WAS DONE
	JP	Z,DROUND	;NONE WAS, PROCEED TO ROUND THE NUMBER
	LD	HL,FAC		;GET POINTER TO EXPONENT
	ADD	A,(HL)		;UPDATE IT
	LD	(HL),A		;SAVE UPDATED EXPONENT
	JP	NC,$ZERO	;UNDERFLOW, THE RESULT IS ZERO
	RET	Z		;RESULT IS ALREADY ZERO, WE ARE DONE
				;FALL INTO DROUND AND ROUND THE RESULT

;	Round FAC
;	Alters A,B,H,L
DROUND	LD	A,(DFACLO1)	;GET EXTRA BYTE TO SEE IF WE HAVE TO ROUND
DROUNB	OR	A		;ENTRY FROM DDIV
	CALL	M,DROUNA	;ROUND UP IF NECESSARY
	LD	HL,FAC_1	;GET POINTER TO UNPACKED SIGN
	LD	A,(HL)		;GET SIGN
	AND	80H		;ISOLATE SIGN BIT
	DEC	HL		;POINT TO HO
	DEC	HL
	XOR	(HL)		;PACK SIGN AND HO
	LD	(HL),A		;PUT PACKED SIGN AND HO IN FAC
	RET			;WE ARE DONE

;	Subroutine for ROUND: add one to FAC
DROUNA	LD	HL,DFACLO	;GET POINTER TO LO, ENTRY FROM DINT
	LD	B,07H		;SET UP A COUNT
DRONA1	INC	(HL)		;INCREMENT A BYTE
	RET	NZ		;RETURN IF THERE WAS NO CARRY
	INC	HL		;INCREMENT POINTER TO NEXT HIGHER ORDER
	DEC	B		;HAVE WE INCREMENTED ALL BYTES
	JP	NZ,DRONA1	;NO, TRY THE NEXT ONE
	INC	(HL)		;YES, INCREMENT THE EXPONENT
	JP	Z,$OVFLS	;THE NUMBER OVERFLOWED ITS EXPONENT
	DEC	HL
	LD	(HL),80H	;PUT 200 (128D) IN HO
	RET			;ALL DONE

;	Add or subtract 2 DBL quantities
;	Alters A,C,D,E,H,L
DADDDA	LD	DE,FMLTT1	;ENTRY FROM DDIV
	LD	HL,ARGLO
	JP	DADDS

DADDAR	LD	A,8EH		; ADC A,(HL)
DADSBA	LD	HL,ARGLO	;GET POINTER TO ARG, ENTRY FROM DADD
DADDFO	LD	DE,DFACLO	;GET POINTER TO FAC, ENTRY FROM FOUT
DADDS	LD	C,07H		;SET UP A COUNT
	LD	(ADCSBC),A	;STORE THE ADD OR SUBTRACT INSTRUCTION
	XOR	A		;CLEAR CARRY
DADDL	LD	A,(DE)		;GET A BYTE FROM RESULT NUMBER
ADCSBC	ADC	A,(HL)		;THIS IS EITHER "ADC" OR "SBC"
	LD	(DE),A		;SAVE THE CHANGED BYTE
	INC	DE		;INCREMENT POINTERS TO NEXT HIGHER ORDER BYTE
	INC	HL
	DEC	C		;ARE WE DONE?
	JP	NZ,DADDL	;NO, DO THE NEXT HIGHER ORDER BYTE
	RET			;ALL DONE

;	Negate signed number in FAC
;	This is used by DADD, DINT
;	Alters A,B,C,H,L
DNEGR	LD	A,(HL)		;COMPLEMENT SIGN OF FAC
	CPL			;USE THE UNPACKED SIGN BYTE
	LD	(HL),A		;SAVE THE NEW SIGN
	LD	HL,DFACLO1	;GET POINTER TO LO
	LD	B,08H		;SET UP A COUNT
	XOR	A		;CLEAR CARRY AND GET A ZERO
	LD	C,A		;SAVE ZERO IN C
DNEGR1	LD	A,C		;GET A ZERO
	SBC	A,(HL)		;NEGATE THE BYTE OF FAC
	LD	(HL),A		;UPDATE FAC
	INC	HL		;INCREMENT POINTER TO NEXT HIGHER ORDER BYTE
	DEC	B		;ARE WE DONE?
	JP	NZ,DNEGR1	;NO, NEGATE THE NEXT BYTE
	RET			;ALL DONE

;	Shift DBL FAC right one
;	A = shift count
;	Alters A,C,D,E,H,L
DSHFTR	LD	(HL),C		;PUT THE UNPACKED HO BACK
	PUSH	HL		;SAVE POINTER TO WHAT TO SHIFT
DSHFR1	SUB	08H		;SEE IF WE CAN SHIFT 8 RIGHT
	JP	C,DSHFR3	;WE CAN'T, CHECK IF WE ARE DONE
	POP	HL		;GET POINTER BACK
DSHFTRM	PUSH	HL		;ENTRY FROM DMULT, SAVE POINTER TO HO
	LD	DE,0800H	;SHIFT A ZERO INTO THE HO, SET UP A COUNT
DSHFR2	LD	C,(HL)		;SAVE A BYTE OF FAC
	LD	(HL),E		;PUT THE LAST BYTE IN ITS PLACE
	LD	E,C		;SET UP E FOR NEXT TIME THROUGH THE LOOP
	DEC	HL		;POINT TO NEXT LOWER ORDER BYTE
	DEC	D		;ARE WE DONE?
	JP	NZ,DSHFR2	;NO, DO THE NEXT BYTE
	JP	DSHFR1		;YES, SEE IF WE CAN SHIFT OVER 8 MORE

DSHFR3	ADD	A,09H		;CORRECT SHIFT COUNT
	LD	D,A		;SAVE SHIFT COUNT IN D
DSHFR4	XOR	A		;CLEAR CARRY
	POP	HL		;GET POINTER TO HO
	DEC	D		;ARE WE DONE?
	RET	Z		;YES
DSHFRA	PUSH	HL		;NO, SAVE POINTER TO LO, ENTRY FROM DADD, DMULT
	LD	E,08H		;SET UP A COUNT, ROTATE FAC ONE LEFT
DSHFR5	LD	A,(HL)		;GET A BYTE OF THE FAC
	RRA			;ROTATE IT LEFT
	LD	(HL),A		;PUT THE UPDATED BYTE BACK
	DEC	HL		;DECREMENT POINTER TO NEXT LOWER ORDER BYTE
	DEC	E		;ARE WE DONE?
	JP	NZ,DSHFR5	;NO, ROTATE THE NEXT LOWER ORDER BYTE
	JP	DSHFR4		;YES, SEE IF WE ARE DONE SHIFTING

;	ENTRY TO DSHFTR FROM DADD, DMULT
;
DSHRRA1	LD	HL,FACHI	;GET POINTER TO HO OF FAC
	LD	D,01H		;SHIFT RIGHT ONCE
	JP	DSHFRA		;GO DO IT

;	Rotate FAC left one
;	Alters A,C,H,L
DSHFLC	LD	C,08H		;SET UP A COUNT
DSHFTL	LD	A,(HL)		;GET A BYTE OF FAC
	RLA			;ROTATE IT LEFT ONE
	LD	(HL),A		;UPDATE BYTE IN FAC
	INC	HL		;INCREMENT POINTER TO NEXT HIGHER ORDER BYTE
	DEC	C		;ARE WE DONE?
	JP	NZ,DSHFTL	;NO, ROTATE THE NEXT BYTE
	RET			;ALL DONE

;	Double precision multiplication		FAC:=FAC*ARG
;	Alters all registers
DMULT	CALL	SIGN		;CHECK IF WE ARE MULTIPLYING BY ZERO
	RET	Z		;YES, ALL DONE, THE FAC IS ZERO
	LD	A,(ARG)		;MUST SEE IF ARG IS ZERO
	OR	A
	JP	Z,$ZERO		;RETURN ZERO
	CALL	MULDVA		;ADD EXPONENTS AND TAKE CARE OF SIGNS
	CALL	DMULDV		;ZERO FAC AND PUT FAC IN FBUFFR
	LD	(HL),C		;PUT UNPACKED HO IN ARG
	INC	DE		;GET POINTER TO LO OF ARG
	LD	B,07H		;SET UP A COUNT
DMULT2	LD	A,(DE)		;GET THE BYTE OF ARG TO MULTIPLY BY
	INC	DE		;INCREMENT POINTER TO NEXT HIGHER BYTE
	OR	A		;CHECK IF WE ARE MULTIPLYING BY ZERO
	PUSH	DE		;SAVE POINTER TO ARG
	JP	Z,DMULT5	;WE ARE
	LD	C,08H		;SET UP A COUNT
DMULT3	PUSH	BC		;SAVE COUNTERS
	RRA			;ROTATE MULTIPLIER RIGHT
	LD	B,A		;SAVE IT
	CALL	C,DADDAR	;ADD IN OLD FAC IF BIT OF MULTIPLIER WAS ONE
	CALL	DSHRRA1		;ROTATE PRODUCT RIGHT ONE
	LD	A,B		;GET MULTIPLIER IN A
	POP	BC		;GET COUNTERS BACK
	DEC	C		;ARE WE DONE WITH THIS BYTE OF ARG?
	JP	NZ,DMULT3	;NO, MULTIPLY BY THE NEXT BIT OF THE MULTIPLIER
DMULT4	POP	DE		;YES, GET POINTER INTO ARG BACK
	DEC	B		;ARE WE DONE?
	JP	NZ,DMULT2	;NO, MULTIPLY BY NEXT HIGHER ORDER BY OF ARG
				;POINT IS TO RIGHT OF UNDERSTOOD ONE
	JP	DNORML		;ALL DONE, NORMALIZE AND ROUND RESULT

DMULT5	LD	HL,FACHI	;GET POINTER TO HO OF FAC
	CALL	DSHFTRM		;SHIFT PRODUCT RIGHT ONE BYTE, WE ARE
	JP	DMULT4		; MULTIPLYING BY ZERO

;	Constant for DIV10, DDIV10
;	= 0.1#	one tenth in DP
$DMP01	DB	0CDH,0CCH,0CCH,0CCH,0CCH,0CCH,4CH,7DH
; 	= 10.#	ten in DP
DTEN	DB	00H,00H,00H,00H
; 	= 10.!	ten in SP
FTEN	DB	00H,00H,20H,84H

;	Double precision divide FAC by 10
;	Alters all registers
;
;	(FAC)=(FAC)*3/4*16/15*1/8
$DIV10	LD	A,(FAC)		;MUST ASSURE OURSELVES WE CAN DO
	CP	41H		;65 EXPONENT DECREMENTS W/O   ($41 = 65)
	JP	NC,DIV11	;REACHING ZERO
	LD	DE,$DMP01	;POINT TO .1D0
	LD	HL,ARGLO	;POINT TO ARG
	CALL	VMOVE		;(DE) := (HL), (VALTYP) bytes
	JP	DMULT		;FAC := FAC * ARG (DP)

DIV11	LD	A,(FACHI)	;NEGATIVE NO?
	OR	A
	JP	P,DIV12
	AND	7FH		;WANT ONLY POS. NOS.
	LD	(FACHI),A
	LD	HL,NEG
	PUSH	HL		;WILL NEGATE WHEN FINISHED
;	FAC := FAC * 3/4
;	    := FAC/2 + FAC/4
DIV12	CALL	DFAC2		;FAC := FAC / 2
	LD	DE,DFACLO	;ARG := FAC
	LD	HL,ARGLO
	CALL	VMOVE
	CALL	DFAC2		;FAC := FAC / 2
	CALL	DADD		;FAC := FAC + ARG
;	FAC := FAC * 16 / 15
;	    := FAC + FAC/16^1 + FAC/16^2 + ... + FAC/16^15
	LD	DE,DFACLO	;ARG := FAC
	LD	HL,ARGLO
	CALL	VMOVE		;
	LD	A,0FH		; Loop 15 times
DIV13	PUSH	AF		;SAVE LOOP COUNTER
	CALL	DARG16		;ARG := ARG / 16
	CALL	PSHARG		;
	CALL	DADD		;FAC := FAC + ARG
	LD	HL,ARGHI
	CALL	POPARG		;POP ARG OFF THE STACK
	POP	AF		;FETCH LOOP COUNTER
	DEC	A
	JP	NZ,DIV13
;	FAC := FAC / 8
	CALL	DFAC2
	CALL	DFAC2

;	Divide FAC by 2 by decrementing the exponent
DFAC2	LD	HL,FAC
	DEC	(HL)		;(FAC)=(FAC)/2
	RET	NZ
	JP	$ZERO		;UNDERFLOW

;	ARG := ARG / 16
DARG16	LD	HL,ARG
	LD	A,04H
DARG161	DEC	(HL)
	RET	Z
	DEC	A
	JP	NZ,DARG161	;Loop if no underflow
	RET

;	PUSH DOUBLE PRECISION ARG ON THE STACK
;	 STACK <- ARG
PSHARG	POP	DE		;GET OUR RETURN ADDRESS OFF THE STACK
	LD	A,04H
	LD	HL,ARGLO
PSHARG1	LD	C,(HL)
	INC	HL
	LD	B,(HL)
	INC	HL
	PUSH	BC
	DEC	A
	JP	NZ,PSHARG1
	PUSH	DE
	RET

	; ARG <- STACK
POPARG	POP	DE
	LD	A,04H
	LD	HL,ARG
POPARG1	POP	BC
	LD	(HL),B
	DEC	HL
	LD	(HL),C
	DEC	HL
	DEC	A
	JP	NZ,POPARG1
	PUSH	DE
	RET

;	Double precision DIVIDE
;
;	Double precision division	FAC:=FAC/ARG
;	Alters all registers
DDIV	LD	A,(ARG)		;CHECK FOR DIVISION BY ZERO
	OR	A		;GET THE EXPONENT OF ARG
	JP	Z,$DIV0S2
	LD	A,(FAC)		;IF FAC=0 THEN ANS IS ZERO
	OR	A
	JP	Z,$ZERO		;DON'T LET HIM DO IT
	CALL	MULDVS		;SUBTRACT EXPONENTS AND CHECK SIGNS
	INC	(HL)		;MULDIV DIFFERENT FOR TRUANS=0
	INC	(HL)		;MUST CORRECT FOR INCORRECT EXP CALC
	JP	Z,$OVFLS
	CALL	DMULDV		;ZERO FAC AND PUT FAC IN FBUFFR
	LD	HL,FMLTT17	;GET POINTER TO THE EXTRA HO BYTE WE WILL USE
	LD	(HL),C		;ZERO IT
	LD	B,C		;ZERO FLAG TO SEE WHEN WE START DIVIDING
DDIV1	LD	A,9EH		; SBC A,(HL) -> SUBTRACT ARG FROM FBUFFR
	CALL	DADDDA		;DO THE SUBTRACTION
	LD	A,(DE)		;SUBTRACT FROM EXTRA HO BYTE
	SBC	A,C		;HERE C=0
	CCF			;CARRY=1 IF SUBTRACTION WAS GOOD
	JP	C,DDIV2		;WAS IT OK?
	LD	A,8EH		; ADC A,(HL) -> NO, ADD FBUFFR BACK IN
	CALL	DADDDA		;DO THE ADDITION
	XOR	A		;CLEAR CARRY
	DB	0DAH		; "JP C,n"  OVER NEXT TWO BYTES
DDIV2	LD	(DE),A		;STORE THE NEW HIGHEST ORDER BYTE
	INC	B		;INCREMENT FLAG TO SHOW WE COULD DIVIDE
	LD	A,(FACHI)	;CHECK IF WE ARE DONE DIVIDING
	INC	A		;SET SIGN FLAG WITHOUT AFFECTING CARRY
	DEC	A
	RRA			;PUT CARRY IN MSB FOR DROUND
	JP	M,DROUNB	;WE ARE DONE, WE HAVE 57 BITS OF ACCURACY
	RLA			;GET OLD CARRY BACK WHERE IT BELONGS
	LD	HL,DFACLO	;GET POINTER TO LO OF FAC
	LD	C,07H		;SET UP A COUNT, SHIFT FAC LEFT ONE
	CALL	DSHFTL		;SHIFT IN THE NEXT BIT IN THE QUOTIENT
	LD	HL,FMLTT1	;GET POINTER TO LO IN FBUFFR
	CALL	DSHFLC		;SHIFT DIVIDEND ONE LEFT
	LD	A,B		;IS THIS THE FIRST TIME AND WAS THE
	OR	A		; SUBTRACTION NOT GOOD? (B WILL GET
	JP	NZ,DDIV1	; CHANGED ON THE FIRST OR SECOND SUBTRACTION)
	LD	HL,FAC		;YES, SUBTRACT ONE FROM EXPONENT TO CORRECT
	DEC	(HL)		; SCALING
	JP	NZ,DDIV1	;CONTINUE DIVIDING IF NO UNDERFLOW
	JP	$ZERO		;UNDERFLOW

;	Transfer FAC to FBUFFR for DMULT and DDIV
;	Alters A,B,C,D,E,H,L
DMULDV	LD	A,C		;PUT UNPACKED HO BACK IN ARG
	LD	(ARGHI),A
	DEC	HL		;POINT TO HO OF FAC
	LD	DE,FMLTT16	;POINT TO END OF FBUFFR
	LD	BC,0700H	;SET UP A COUNT TO FBUFFR:
				; B=7, set C to 0
DMULDV1	LD	A,(HL)		;GET A BYTE FROM FAC
	LD	(DE),A		;PUT IT IN FBUFFR
	LD	(HL),C		;PUT A ZERO IN FAC
	DEC	DE		;POINT TO NEXT BYTE IN FBUFFR
	DEC	HL		;POINT TO NEXT LOWER ORDER BYTE IN FAC
	DEC	B		;ARE WE DONE?
	JP	NZ,DMULDV1	;NO, TRANSFER THE NEXT BYTE
	RET			;ALL DONE

;	Double precision multiply the FAC by 10
;	Alters all registers
DMUL10	CALL	VMOVAF		;SAVE THE FAC IN ARG,
				; VMOVAF EXITS WITH (DE)=FAC+1
	EX	DE,HL		;GET THE POINTER INTO THE FAC IN (HL)
	DEC	HL		;POINT TO THE EXPONENT
	LD	A,(HL)		;GET THE EXPONENT
	OR	A		;IS THE NUMBER ZERO?
	RET	Z		;YES, ALL DONE
	ADD	A,02H		;MULTIPLY FAC BY 4 BY ADDING 2
				; TO THE EXPONENT
	JP	C,$OVFLS
	LD	(HL),A		;SAVE THE NEW EXPONENT
	PUSH	HL		;SAVE POINTER TO FAC
	CALL	DADD		;ADD IN THE ORIGINAL FAC TO GET 5 TIMES FAC
	POP	HL		;GET THE POINTER TO FAC BACK
	INC	(HL)		;ADD ONE TO EXPONENT TO GET 10 TIMES FAC
	RET	NZ		;ALL DONE IF OVERFLOW DID NOT OCCUR
	JP	$OVFLS


;-----------------------------------------------------------------------------
;	FLOATING POINT INPUT ROUTINE
;
;	Alters all registers
;	The number is left in FAC
;	At entry, (HL) points to the first character in a text buffer.
;	The first character is also in A. We pack the digits into the FAC
;	as an integer and keep track of where the decimal point is.
;	C is 377 if we have not seen a decimal point, 0 if we have.
;	B is the number of digits after the decimal point.
;	At the end, B and the exponent (in E) are used to determine how many
;	times we multiply or divide by TEN to get the correct number.
FINDP	CALL	$ZERO		;ZERO THE FAC
	CALL	SETDBL		;FORCE TO DOUBLE PRECISION
	DB	0F6H		;SO FRCINT IS NOT CALLED
FIN	XOR	A		;FORCE CALL TO FRCINT
	LD	BC,FINOVC
	PUSH	BC		;WHEN DONE STORE OVERFLOW FLAG
	PUSH	AF		;INTO STROVC AND GO TO NORMAL OVERFLOW MODE
	LD	A,01H		;SET UP ONCE ONLY OVERFLOW MODE
	LD	(FLGOVC),A
	POP	AF
	EX	DE,HL		;SAVE THE TEXT POINTER IN (DE)
	LD	BC,00FFH	;CLEAR FLAGS:  B=DECIMAL PLACE COUNT,
				; C="." FLAG
	LD	H,B		;ZERO (HL)
	LD	L,B
	CALL	Z,MAKINT	;ZERO FAC, SET VALTYP TO "INTEGER"
	EX	DE,HL		;GET THE TEXT POINTER BACK IN (HL)
				; AND ZEROS IN (DE)
	LD	A,(HL)		;RESTORE CHAR FROM MEMORY
;	ASCII to FP number (also '&' prefixes)
	CP	'&'
	JP	Z,OCTCNS
;	ASCII to FP number
				;IF WE ARE CALLED BY VAL OR INPUT OR
				; READ, THE SIGNS MAY NOT BE CRUNCHED
	CP	'-'		;SEE IF NUMBER IS NEGATIVE
	PUSH	AF		;SAVE SIGN
	JP	Z,FINC		;IGNORE MINUS SIGN
	CP	'+'		;IGNORE A LEADING SIGN
	JP	Z,FINC
	DEC	HL		;SET CHARACTER POINTER BACK ONE
;	Here to check for a digit, a decimal point, "E" or "D"
FINC	CALL	CHRGTR		;GET THE NEXT CHARACTER OF THE NUMBER
	JP	C,FINDIG	;WE HAVE A DIGIT
	CP	'.'		;CHECK FOR A DECIMAL POINT
	JP	Z,FINDP1	;WE HAVE ONE, I GUESS
	CP	'e'		;LOWER CASE "E"
	JP	Z,FINC1
	CP	'E'		;CHECK FOR A SINGLE PRECISION EXPONENT
FINC1	JP	NZ,FINC4	;NO
	PUSH	HL		;SAVE TEXT PTR
	CALL	CHRGTR		;GET NEXT CHAR
	CP	'l'		;SEE IF LOWER CASE "L"
	JP	Z,FINC2		;IF SO POSSIBLE ELSE
	CP	'L'		;IS THIS REALLY AN "ELSE"?
	JP	Z,FINC2		;WAS ELSE
	CP	'q'		;SEE IF LOWER CASE "Q"
	JP	Z,FINC2		;IF SO POSSIBLE "EQV"
	CP	'Q'		;POSSIBLE "EQV"
FINC2	POP	HL		;RESTORE [H,L]
	JP	Z,FINC3		;IT WAS JUMP!
	LD	A,(VALTYP)	;IF DOUBLE DON'T DOWNGRADE TO SINGLE
	CP	08H		;SET CONDITION CODES
	JP	Z,FINC5
	LD	A,00H		;MAKE A=0 SO NUMBER IS A SINGLE
	JP	FINC5

FINC3	LD	A,(HL)		;RESTORE ORIGINAL CHAR
FINC4	CP	'%'		;TRAILING % (RSTS-11 COMPATIBILITY)
	JP	Z,FINDP11	;MUST BE INTEGER.
	CP	'#'		;FORCE DOUBLE PRECISION?
	JP	Z,FINDP12	;YES, FORCE IT & FINISH UP.
	CP	'!'		;FORCE SINGLE PREC.
	JP	Z,FINDP13
	CP	'd'		;LOWER CASE "D"
	JP	Z,FINC5
	CP	'D'		;CHECK FOR A DOUBLE PRECISION EXPONENT
	JP	NZ,FINE		;WE DON'T HAVE ONE, THE NUMBER IS FINISHED
FINC5	OR	A		;DOUBLE PRECISION NUMBER -- TURN OFF ZERO FLAG
	CALL	FINFRC		;FORCE THE FAC TO BE SNG OR DBL
	CALL	CHRGTR		;GET THE FIRST CHARACTER OF THE EXPONENT
	CALL	MINPLS		;EAT SIGN OF EXPONENT  ( test '+', '-'..)
;	Here to get the next digit of the exponent
FINEC	CALL	CHRGTR		;GET THE NEXT CHARATER
	JP	C,FINEDG	;PACK THE NEXT DIGIT INTO THE EXPONENT
	INC	D		;IT WAS NOT A DIGIT, PUT THE CORRECT SIGN ON
	JP	NZ,FINE		; THE EXPONENT, IT IS POSITIVE
	XOR	A		;THE EXPONENT IS NEGATIVE
	SUB	E		;NEGATE IT
	LD	E,A		;SAVE IT AGAIN
;	Here to finish up the number
FINE	PUSH	HL		;SAVE THE TEXT POINTER
	LD	A,E		;FIND OUT HOW MANY TIMES WE HAVE TO MULTIPLY
	SUB	B		; OR DIVIDE BY TEN
	LD	E,A		;SAVE NEW EXPONENT IN E
;	Here to multiply or divide by ten the correct number of times
;	If the number is an INT, A is 0 here.
FINEF2	CALL	P,FINMUL	;MULTIPLY IF WE HAVE TO
	CALL	M,FINDIV	;DIVIDE IF WE HAVE TO
	JP	NZ,FINEF2	;MULTIPLY OR DIVIDE AGAIN IF WE ARE NOT DONE
;	Here to put the correct sign on the number
	POP	HL		;GET THE TEXT POINTER
	POP	AF		;GET THE SIGN
	PUSH	HL		;SAVE THE TEXT POINTER AGAIN
	CALL	Z,VNEG		;NEGATE IF NECESSARY
	POP	HL		;GET THE TEXT POINTER IN (HL)
	CALL	GETYPR		;WE WANT -32768 TO BE AN INT,
				; BUT UNTIL NOW IT WOULD BE A SNG
	RET	PE		;IT IS NOT SNG, SO IT IS NOT -32768
	PUSH	HL		;WE HAVE A SNG, SAVE TEXT POINTER
	LD	HL,POPHRT	;GET ADDRESS THAT POP'S H OFF STACK BECAUSE
	PUSH	HL		; CONIS2 DOES FUNNY THINGS WITH THE STACK
	CALL	CONIS2		;CHECK IF WE HAVE -32768
	RET			;WE DON'T, POPHRT IS STILL ON THE STACK
				; SO WE CAN JUST RETURN

;	Here to check if we have seen 2 decimal points and set the decimal
;	point flag
FINDP1	CALL	GETYPR		;SET CARRY IF WE DON'T HAVE A DOUBLE
	INC	C		;SET THE FLAG
	JP	NZ,FINE		;WE HAD 2 DECIMAL POINTS, NOW WE ARE DONE
	CALL	C,FINFRC	;THIS IS THE FIRST ONE, CONVERT FAC TO SNG
				; IF WE DON'T ALREADY HAVE A DOUBLE
	JP	FINC		;CONTINUE LOOKING FOR DIGITS

FINDP11	CALL	CHRGTR		; Gets next character (or token) from BASIC text.
	POP	AF		;GET SIGN OFF THE STACK
	PUSH	HL		;SAVE TEXT POINTER
	LD	HL,POPHRT	;ADDRESS POP (HL) AND RETURN
	PUSH	HL		; * (added in the late March edition)
	LD	HL,CINT		; *
	PUSH	HL		;WILL WANT TO FORCE ONCE D.P. DONE
	PUSH	AF		;PUT SIGN BACK ON THE STACK
	JP	FINE		;ALL DONE

FINDP12	OR	A		;SET NON-ZERO TO FORCE DOUBLE PREC
FINDP13	CALL	FINFRC		;FORCE THE TYPE
	CALL	CHRGTR		;READ AFTER TERMINATOR
	JP	FINE		;ALL DONE

;	Force the FAC to be SNG or DBL
;	If the Zero flag is ON, then force the FAC to be SNG
;	If the Zero flag is OFF, force the FAC to be DBL
FINFRC	PUSH	HL		;SAVE TEXT POINTER
	PUSH	DE		;SAVE EXPONENT INFORMATION
	PUSH	BC		;SAVE DECIMAL POINT INFORMATION
	PUSH	AF		;SAVE WHAT WE WANT THE FAC TO BE
	CALL	Z,CSNG		;CONVERT TO SNG IF WE HAVE TO
	POP	AF		;GET TYPE FLAG BACK
	CALL	NZ,CDBL		;CONVERT TO DBL IF WE HAVE TO
	POP	BC		;GET DECIMAL POINT INFORMATION BACK
	POP	DE		;GET EXPONENT INFORMATION BACK
	POP	HL		;GET TEXT POINTER BACK
	RET			;ALL DONE

;	This subroutine muliplies by ten once.
;	It is a subroutine because it saves bytes when we check if A is zero
FINMUL	RET	Z		;RETURN IF EXPONENT IS ZERO,
				; ENTRY FROM FOUT
;	Multiply FP value by ten
FINMLT	PUSH	AF		;SAVE EXPONENT, ENTRY FROM FOUT
	CALL	GETYPR		;SEE WHAT KIND OF NUMBER WE HAVE
	PUSH	AF		;SAVE THE TYPE
	CALL	PO,MUL10	;WE HAVE A SNG, MULTIPLY BY 10.0
	POP	AF		;GET THE TYPE BACK
	CALL	PE,DMUL10	;WE HAVE A DBL, MULTIPLY BY 10D0
	POP	AF		;GET EXPONENT
DCRART	DEC	A		;DECREASE IT
	RET			;ALL DONE

;	THIS SUBROUTINE DIVIDES BY TEN ONCE.
;	IT IS USED BY FIN, FOUT
;	ALTERS A,B,C
FINDIV	PUSH	DE		;SAVE D,E
	PUSH	HL		;SAVE H,L
	PUSH	AF		;WE HAVE TO DIVIDE -- SAVE COUNT
	CALL	GETYPR		;SEE WHAT KIND OF NUMBER WE HAVE
	PUSH	AF		;SAVE THE TYPE
	CALL	PO,DIV10	;WE HAVE A SNG NUMBER
	POP	AF		;GET THE TYPE BACK
	CALL	PE,$DIV10	;WE HAVE A DBL NUMBER
	POP	AF		;GET COUNT BACK
	POP	HL		;GET H,L BACK
	POP	DE		;GET D,E BACK
	INC	A		;UPDATE IT
	RET

;	Here to pack the next digit of the nomber into the FAC
;	We multiply the FAC by ten and add in the next digit
FINDIG	PUSH	DE		;SAVE EXPONENT INFORMATION
	LD	A,B		;INCREMENT DECIMAL PLACE COUNT IF WE ARE
	ADC	A,C		; PAST THE DECIMAL POINT
	LD	B,A
	PUSH	BC		;SAVE DECIMAL POINT INFORMATION
	PUSH	HL		;SAVE TEXT POINTER
	LD	A,(HL)		;GET THE DIGIT
	SUB	30H		;CONVERT IT TO ASCII
	PUSH	AF		;SAVE THE DIGIT
	CALL	GETYPR		;SEE WHAT KIND OF A NUMBER WE HAVE
	JP	P,FINDGV	;WE DO NOT HAVE AN INTEGER
;	Here to pack the next digit of an integer
	LD	HL,(FACLO)	;WE HAVE AN INTEGER, GET IT IN (HL)
	LD	DE,0CCDH	;SEE IF WE WILL OVERFLOW
	CALL	COMPAR		;COMPAR RETURNS WITH CARRY ON IF
	JP	NC,FINDG2	; (HL) .LT. (DE), SO THE NUMBER IS TOO BIG
	LD	D,H		;COPY (HL) INTO (DE)
	LD	E,L
	ADD	HL,HL		;MULTIPLY (HL) BY 2
	ADD	HL,HL		;MULTIPLY (HL) BY 2, (HL) NOW IS 4*(DE)
	ADD	HL,DE		;ADD IN OLD (HL) TO GET 5*(DE)
	ADD	HL,HL		;MULTIPLY BY 2 TO GET TEN TIMES THE OLD (HL)
	POP	AF		;GET THE DIGIT
	LD	C,A		;SAVE IT SO WE CAN USE DAD, B IS ALREADY ZERO
	ADD	HL,BC		;ADD IN THE NEXT DIGIT
	LD	A,H		;CHECK FOR OVERFLOW
	OR	A		;OVERFLOW OCCURED IF THE MSB IS ON
	JP	M,FINDG1	;WE HAVE OVERFLOW!!
	LD	(FACLO),HL	;EVERYTHING IS FINE, STORE THE NEW NUMBER
FINDGE	POP	HL		;ALL DONE, GET TEXT POINTER BACK
	POP	BC		;GET DECIMAL POINT INFORMATION BACK
	POP	DE		;GET EXPONENT INFORMATION BACK
	JP	FINC		;GET THE NEXT CHARACTER

;	Here to handle 32768, 32769
FINDG1	LD	A,C		;GET THE DIGIT
	PUSH	AF		;PUT IT BACK ON THE STACK
;	Here to convert the integer digits to single precision digits
FINDG2	CALL	CONSI		;CONVERT THE INTEGER TO SINGLE PRECISION
	SCF			;DO NOT TAKE THE FOLLOWING JUMP

;	Here to decide if we have a single or double precision number
FINDGV	JP	NC,FINDGD	;FALL THROUGH IF VALTYP WAS 4 I.E. SNG PREC
	LD	BC,9474H	;GET 1'000'000, DO WE HAVE 7 DIGITS ALREADY?
	LD	DE,2400H
	CALL	FCOMP		;IF SO, FAC .GE. 1'000'000
	JP	P,FINDG3	;WE DO, CONVERT TO DOUBLE PRECISION
	CALL	MUL10		;MULTIPLY THE OLD NUMBER BY TEN
	POP	AF		;GET THE NEXT DIGIT
	CALL	FINLOG		;PACK IT INTO THE FAC
	JP	FINDGE		;GET FLAGS OFF STACK AND WE ARE DONE

;	Here to convert a 7 digit single precision number to double precision
FINDG3	CALL	CONDS		;CONVERT SINGLE TO DOUBLE PRECISION
;	Here to pack in the next digit of a double precision number
FINDGD	CALL	DMUL10		;MULTIPLY THE FAC BY 10
	CALL	VMOVAF		;SAVE THE FAC IN ARG
	POP	AF		;GET THE NEXT DIGIT
	CALL	$FLT		;CONVERT THE DIGIT TO SINGLE PRECISION
	CALL	CONDS		;NOW, CONVERT THE DIGIT TO DOUBLE PRECISION
	CALL	DADD		;ADD IN THE DIGIT
	JP	FINDGE		;GET THE FLAGS OFF THE STACK AND WE ARE DONE

;	Subroutine for FIN, LOG
FINLOG	CALL	PUSHF		;SAVE FAC ON STACK
	CALL	$FLT		;CONVERT TO A FLOATING POINT NUMBER
FADDT	POP	BC		;GET PREVIOUS NUMBER OFF STACK
	POP	DE
	JP	FADD		;ADD THE TWO NUMBERS

;	Here we pack in the next digit of the exponent
;	We mutiply the old exponent by ten and add in the next digit
;	Note: Exponent overflow is not checked for
FINEDG	LD	A,E		;EXPONENT DIGIT -- MULTIPLY EXPONENT BY 10
	CP	0AH		;CHECK THAT THE EXPONENT DOES NOT OVERFLOW
				;IF IT DID, E COULD GET GARBAGE IN IT.
	JP	NC,FINEDG0	;WE ALREADY HAVE TWO DIGITS
	RLCA			;FIRST BY 4
	RLCA
	ADD	A,E		;ADD 1 TO MAKE 5
	RLCA			;NOW DOUBLE TO GET 10
	ADD	A,(HL)		;ADD IT IN
	SUB	30H		;SUBTRACT OFF ASCII CODE, THE RESULT IS
				; POSITIVE ON LENGTH=2 BECAUSE OF THE ABOVE CHECK
	LD	E,A		;STORE EXPONENT
	DB	0FAH		; JP M,nn  to mask the next 2 bytes
FINEDG0	LD	E,7FH		;AN EXPONENT LIKE THIS WILL SAFELY
				; CAUSE OVERFLOW OR UNDERFLOW
	JP	FINEC		;CONTINUE

FINEDG1	PUSH	HL
	LD	HL,FACHI	;POINT (HL) TO SIGN BYTE
	CALL	GETYPR
	JP	PO,FINEDG2	;SP PROCEED AS NORMAL
	LD	A,(ARGHI)
	JP	FINEDG3

FINEDG2	LD	A,C
FINEDG3	XOR	(HL)		;SIGN IN HIGH BIT OF (A)
	RLA			;SIGN IN CARRY
	POP	HL
	JP	DV010		;GO PRINT OVERFLOW

FINEDG4	POP	AF		; This entry is used by __EXP
	POP	AF		; (RESZER exits here)
;	Deal with various overflow conditions
$OVFLS2	LD	A,(FACHI)
	RLA
	JP	DV010		;GO PRINT OVERFLOW

$OVFLS1	POP	AF		;DO A POP THEN FALL INTO OVERR_1
$OVFLS	LD	A,(FAC_1)	;GET SIGN BYTE
	CPL			;SIGN WAS STORED COMPLEMENTED
	RLA			;SIGN TO CARRY
	JP	DV010		;GO PRINT OVERFLOW

$DIV0S1	LD	A,C
	JP	$DIV0S

; Division (exponent is 0)
;
$DIV0S2	LD	A,(ARGHI)
$DIV0S	RLA			;TO CARRY
	LD	HL,$DIV0M	;GET MESSAGE ADDRESS
	LD	(OVERRI),HL	;STORE SO OVFINT WILL PICK UP

;	ANSI OVERFLOW ROUTINE
;
DV010	PUSH	HL
	PUSH	BC
	PUSH	DE
	PUSH	AF		;SAVE MACHINE STATUS
	PUSH	AF		;AGAIN
	LD	HL,(ONELIN)	;TRAPPING ERRORS?
	LD	A,H
	OR	L
	JP	NZ,INF00	;JUMP PRINT IF TRAPPING OTHERWISE +INFINITY
	LD	HL,FLGOVC	;PRINT INDICATOR FLAG
	LD	A,(HL)
	OR	A		;PRINT IF 0,1;SET TO 2 IF 1
	JP	Z,DV011
	DEC	A
	JP	NZ,INF00
	INC	(HL)
DV011	LD	HL,(OVERRI)	;ADDRESS OF OVERFLOW MESSAGE
	CALL	STRPRN		;PRINT
	LD	(TTYPOS),A	;SET TTY POSITION TO CHAR 0
	LD	A,0DH
	CALL	CALTTY
	LD	A,0AH
	CALL	CALTTY		;CARRIAGE RETURN,LINE FEED
	;Put correct infinity in FAC
INF00	POP	AF		;GET PLUS,MINUS INDICATION BACK
	LD	HL,FACLO	;MUST NOW PUT RIGHT INFINITY INTO THE FAC
	LD	DE,$INFPS
	JP	NC,INF10
	LD	DE,$INFMS	;MINUS INFINITY
INF10	CALL	MOVMM		;MOVE INTO FAC
	CALL	GETYPR
	JP	C,INF20		;SP ALL OK
	LD	HL,DFACLO
	LD	DE,$INFMS	;ALL ONES
	CALL	MOVMM
INF20	LD	HL,(ONELIN)	;TRAPPING ERRORS?
	LD	A,H
	OR	L
	LD	HL,(OVERRI)
	LD	DE,$OVMSG
	EX	DE,HL		;PUT "OVRMSG" ADDRESS IN OVERRI
	LD	(OVERRI),HL	;IN CASE THIS WAS A DIV BY 0
	JP	Z,INF30		;JUMP IF NOT TRAPPING
	CALL	COMPAR
	JP	Z,OVERR		;ALL RESTORED
	JP	DV0ERR		;CONTINUE PROCESSING

INF30	POP	AF
	POP	DE
	POP	BC
	POP	HL		;ALL RESTORED
	RET			;CONTINUE PROCESSING


$INFPS	DB	0FFH,0FFH,7FH,0FFH	;INFINITY
$INFMS	DB	0FFH,0FFH,0FFH,0FFH	;MINUS INFINITY


;-----------------------------------------------------------------------------
;	FLOATING POINT OUTPUT ROUTINE
;
;	Entry to LINPRT
;
;	'in' <line number> message
;
INPRT	PUSH	HL		;SAVE LINE NUMBER
	LD	HL,INTXT	;PRINT MESSAGE    (.." in "..)
	CALL	STROUT
	POP	HL		;FALL INTO LINPRT
;	Print HL in ASCII form at the current cursor position
;
;	Print the 2 byte number in H,L
;	Alters all registers
LINPRT	LD	BC,STROUI
	PUSH	BC
	CALL	MAKINT		;PUT THE LINE NUMBER IN THE FAC AS AN INTEGER
	XOR	A		;SET FORMAT TO FREE FORMAT
	CALL	FOUINI		;SET UP THE SIGN
	OR	(HL)		;TURN OFF THE ZERO FLAG
	JP	FOUT2		;CONVERT THE NUMBER INTO DIGITS

;	Output the value in the FAC according to the format specifications
;	 in A,B,C
;	All registers are altered
;	The original contents of the FAC is lost

;	The format is specified in A, B and C as follows:
;	The bits of A mean the following:
;BIT 7	0 means free format output, i.e. the other bits of A must be zero,
;	trailing zeros are suppressed, A number is printed in fixed or floating
;	point notation according to its magnitude, the number is left
;	justified in its field, B and C are ignored.
;	1 means fixed format output, i.e. the other bits of A are checked for
;	formatting information, the number is right justified in its field,
;	trailing zeros are not suppressed. This is used for PRINT USING.
;BIT 6	1 means group the digits in the integer part of the number into groups
;	of three and separate the groups by commas
;	0 means don't print the number with commas
;BIT 5	1 means fill the leading spaces in the field with asterisks ("*")
;BIT 4	1 means output the number with a floating dollar sign ("$")
;BIT 3	1 means print the sign of a positive number as a plus sign ("+")
;	instead of a space
;BIT 2	1 means print the sign of the number after the number
;BIT 1	unused
;BIT 0	1 means print the number in floating point notation i.e. "E notation"
;	If this bit is on, the comma specification (bit 6) is ignored.
;	0 means print the number in fixed point notation. Number .GE. 1E16
;	cannot be printed in fixed point notation.

;	B and C tell how big the field is:
;B  =	The number of places in the field to the left of the decimal point
;	(B does not include the decimal point)
;C  =	The number of places in the field to the right of the decimal point
;	(C includes the decimal point)
;	B and C donot include the 4 positions for the exponent if bit o is on
;	FOUT assumes B+C .LE. 24 (decimal)


;	Convert number/expression to string (format not specified)
;
;	FLOATING OUTPUT OF FAC
;	ALTERS ALL REGISTERS
;	THE ORIGINAL CONTENTS OF THE FAC IS LOST
;
;	Entry to print the FAC in free format
FOUT	XOR	A		;SET FORMAT FLAGS TO FREE FORMATTED OUTPUT

;	Entry to print the FAC using the format specifications in A, B and C
;
;	Convert the binary number in FAC1 to ASCII.
;	A - Bit configuration for PRINT USING options
PUFOUT	CALL	FOUINI		;SAVE THE FORMAT SPECIFICATION IN A AND PUT
				; A SPACE FOR POSITIVE NUMBERS IN THE BUFFER
	AND	08H		;CHECK IF POSITIVE NUMBERS GET A PLUS SIGN
	JP	Z,FOUT1		;THEY DON'T
	LD	(HL),'+'	;THEY DO, PUT IN A PLUS SIGN
FOUT1	EX	DE,HL		;SAVE BUFFER POINTER
	CALL	VSIGN		;GET THE SIGN OF THE FAC
	EX	DE,HL		;PUT THE BUFFER POINTER BACK IN (HL)
	JP	P,FOUT2		;IF WE HAVE A NEGATIVE NUMBER, NEGATE IT
	LD	(HL),'-'	; AND PUT A MINUS SIGN IN THE BUFFER
	PUSH	BC		;SAVE THE FIELD LENGTH SPECIFICATION
	PUSH	HL		;SAVE THE BUFFER POINTER
	CALL	VNEG		;NEGATE THE NUMBER
	POP	HL		;GET THE BUFFER POINTER BACK
	POP	BC		;GET THE FIELD LENGTH SPECIFICATIONS BACK
	OR	H		;TURN OFF THE ZERO FLAG, THIS DEPENDS ON
				; THE FACT THAT FBUFFR IS NEVER ON PAGE 0.
FOUT2	INC	HL		;POINT TO WHERE THE NEXT CHARACTER GOES
	LD	(HL),'0'	;PUT A ZERO IN THE BUFFER IN CASE
				; THE NUMBER IS ZERO (IN FREE FORMAT)
				; OR TO RESERVE SPACE FOR A FLOATING
				; DOLLAR SIGN (FIXED FORMAT)
	LD	A,(TEMP3)	;GET THE FORMAT SPECIFICATION
	LD	D,A		;SAVE IT FOR LATER
	RLA			;PUT THE FREE FORMAT OR NOT BIT IN THE CARRY
	LD	A,(VALTYP)	;GET THE VALTYP, VNEG COULD HAVE CHANGED
				; THIS SINCE -32768 IS INT AND 32768 IS SNG.
	JP	C,FOUTFX	;THE MAN WANTS FIXED FORMATED OUTPUT
;	Here to print numbers in free format
	JP	Z,FOUTZR	;IF THE NUMBER IS ZERO, FINISH IT UP
	CP	04H		;DECIDE WHAT KIND OF A VALUE WE HAVE
	JP	NC,FOUFRV	;WE HAVE A SNG OR DBL
;	Here to print an integer in free format
	LD	BC,0000H	;SET THE DECIMAL POINT COUNT AND COMMA
				; COUNT TO ZERO
	CALL	$FOTCI		;CONVERT THE INTEGER TO DECIMAL
				;FALL INTO FOUTZS AND ZERO SUPPRESS
				; THE THING
;	Zero suppress the digits in FBUFFR
;	Asterisk fill and zero suppress if necessary
;	Set up B and condition codes if we have a trailing sign
FOUTSZ	LD	HL,FBUFFR1	;GET POINTER TO THE SIGN
	LD	B,(HL)		;SAVE THE SIGN IN B
	LD	C,' '		;DEFAULT FILL CHARACTER TO A SPACE
	LD	A,(TEMP3)	;GET FORMAT SPECS TO SEE IF WE HAVE TO
	LD	E,A		; ASTERISK FILL.  SAVE IT
	AND	20H
	JP	Z,FOTSZ1	;WE DON'T
	LD	A,B		;WE DO, SEE IF THE SIGN WAS A SPACE
	CP	C		;ZERO FLAG IS SET IF IT WAS
	LD	C,'*'		;SET FILL CHARACTER TO AN ASTERISK
	JP	NZ,FOTSZ1	;SET THE SIGN TO AN ASTERISK
				; IF IT WAS A SPACE
	LD	A,E		;GET FORMAT SPECS AGAIN
	AND	04H		;SEE IF SIGN IS TRAILING
	JP	NZ,FOTSZ1	;IF SO DON'T ASTERISK FILL
	LD	B,C		;B HAS THE SIGN, C THE FILL CHARACTER
FOTSZ1	LD	(HL),C		;FILL IN THE ZERO OR THE SIGN
	CALL	CHRGTR		;GET THE NEXT CHARACTER IN THE BUFFER
				; SINCE THERE ARE NO SPACES, "CHRGET"
				; IS EQUIVALENT TO "INX	H"/"MOV	A,M"
	JP	Z,FOTSZ11	;IF WE SEE A REAL ZERO, IT IS THE END OF THE NUMBER,
				; AND WE MUST BACK UP AND PUT IN A ZERO.
				;CHRGET SETS THE ZERO FLAG ON REAL ZEROS OR COLONS,
				; BUT WE WON'T SEE ANY COLONS IN THIS BUFFER.
	CP	'E'		;BACK UP AND PUT IN A ZERO IF WE SEE
	JP	Z,FOTSZ11	; AN "E" OR A "D" SO WE CAN PRINT 0 IN
	CP	'D'		; FLOATING POINT NOTATION WITH THE C FORMAT ZERO
	JP	Z,FOTSZ11
	CP	'0'		;DO WE HAVE A ZERO?
	JP	Z,FOTSZ1	;YES, SUPPRESS IT
	CP	','		;DO WE HAVE A COMMA?
	JP	Z,FOTSZ1	;YES, SUPPRESS IT
	CP	'.'		;ARE WE AT THE DECIMAL POINT?
	JP	NZ,FOTSZ2	;NO, I GUESS NOT
FOTSZ11	DEC	HL		;YES, BACK UP AND PUT A ZERO BEFORE IT
	LD	(HL),'0'
FOTSZ2	LD	A,E		;GET THE FORMAT SPECS TO CHECK FOR A FLOATING
	AND	10H		; DOLLAR SIGN
	JP	Z,FOTSZ3	;WE DON'T HAVE ONE
	DEC	HL		;WE HAVE ONE, BACK UP AND PUT IN THE DOLLAR
	LD	(HL),'$'	; SIGN
FOTSZ3	LD	A,E		;DO WE HAVE A TRAILING SIGN?
	AND	04H
	RET	NZ		;YES, RETURN; NOTE THE NON-ZERO FLAG IS SET
	DEC	HL		;NO, BACK UP ONE AND PUT THE SIGN BACK IN
	LD	(HL),B		;PUT IN THE SIGN
	RET			;ALL DONE

;	Here to initially set up the format specs and put in a space for
;	sign of a positive number
FOUINI	LD	(TEMP3),A	;SAVE THE FORMAT SPECIFICATION
	LD	HL,FBUFFR1	;GET A POINTER INTO FBUFFR
				;WE START AT FBUFFR+1 IN CASE THE NUMBER
				; WILL OVERFLOW ITS FIELD, THEN THERE IS
				; ROOM IN FBUFFR FOR THE PERCENT SIGN.
	LD	(HL),' '	;PUT IN A SPACE
	RET			;ALL DONE

;	The following code down to FOUFRF: is added to address the
;	ANSI standard of printing numbers in fixed format rather than
;	scientific notation if they can be as accurately represented
;	in fixed format

;	Here to print a SNG or DBL in free format
FOUFRV	CALL	PUSHF		;SAVE IN CASE NEEDED FOR 2ED PASS
	EX	DE,HL		;SAVE BUFFER POINTER IN (HL)
	LD	HL,(DFACLO)
	PUSH	HL		;SAVE FOR D.P.
	LD	HL,(DFACLO2)
	PUSH	HL
	EX	DE,HL		;BUFFER POINTER BACK TO (HL)
	PUSH	AF		;SAVE IN CASE NEEDED FOR SECOND PASS
	XOR	A		;(A)=0
	LD	(FLGSCN),A	;INITIALIZE FANSII FLAG
	POP	AF		;GET PSW RIGHT
	PUSH	AF		;SAVE PSW
	CALL	FOUFRVC		;FORMAT NUMBER
	LD	B,'E'		;WILL SEARCH FOR SCIENTIFIC NOTN.
	LD	C,00H		;DIGIT COUNTER
FOUFRV1:			;GET ORIGINAL FBUFFER POINTER
	PUSH	HL		;SAVE IN CASE WE NEED TO LOOK FOR "D"
	LD	A,(HL)		;FETCH UP FIRST CHARACTER
FOUFRV2	CP	B		;SCIENTIFIC NOTATION?
	JP	Z,FOUFRV5	;IF SO, JUMP
	CP	':'		;IF CARRY NOT SET NOT A DIGIT
	JP	NC,FOUFRV3
	CP	'0'		;IF CARRY SET NOT A DIGIT
	JP	C,FOUFRV3
	INC	C		;INCREMENTED DIGITS TO PRINT
FOUFRV3	INC	HL		;POINT TO NEXT BUFFER CHARACTER
	LD	A,(HL)		;FETCH NEXT CHARACTER
	OR	A		;0(BINARY) AT THE END OF CHARACTERS
	JP	NZ,FOUFRV2	;CONTINUE SEARCH IF NOT AT END
	LD	A,'D'		;NOW TO CHECK TO SEE IF SEARCHED FOR D
	CP	B
	LD	B,A		;IN CASE NOT YET SEARCHED FOR
	POP	HL		;NOW TO CHECK FOR "D"
	LD	C,00H		;ZERO DIGIT COUNT
	JP	NZ,FOUFRV1	;GO SEARCH FOR "D" IF NOT DONE SO
FOUFRV4	POP	AF		;POP	ORIGINAL PSW
	POP	BC
	POP	DE		;GET DFACLO-DFACLO+3
	EX	DE,HL		;(DE)=BUF PTR,(HL)=DFACLO
	LD	(DFACLO),HL
	LD	H,B
	LD	L,C
	LD	(DFACLO2),HL
	EX	DE,HL
	POP	BC
	POP	DE		;GET ORIG FAC OFF STACK
	RET			;COMPLETE

FOUFRV5:			;PRINT IS IN SCIENTIFIC NOTATION , IS THIS BEST?
	PUSH	BC		;SAVE TYPE,DIGIT COUNT
	LD	B,00H		;EXPONENT VALUE (IN BINARY)
	INC	HL		;POINT TO NEXT CHARACTER OF EXP.
	LD	A,(HL)		;FETCH NEXT CHARACTER OF EXPONENT
FOUFRV6	CP	'+'		;IS EXPONENT POSITIVE?
	JP	Z,FOUFRVA	;IF SO NO BETTER PRINTOUT
	CP	'-'		;MUST BE NEGATIVE!
	JP	Z,FOUFRV7	;MUST PROCESS THE DIGITS
	SUB	30H		;SUBTRACT OUT ASCII BIAS
	LD	C,A		;DIGIT TO C
	LD	A,B		;FETCH OLD DIGIT
	ADD	A,A		;*2
	ADD	A,A		;*4
	ADD	A,B		;*5
	ADD	A,A		;*10
	ADD	A,C		;ADD IN NEW DIGIT
	LD	B,A		;BACK OUT TO EXPONENT ACCUMULATOR
	CP	10H		;16 D.P. DIGITS FOR MICROSOFT FORMAT
	JP	NC,FOUFRVA	;IF SO STOP TRYING
FOUFRV7	INC	HL		;POINT TO NEXT CHARACTER
	LD	A,(HL)		;FETCH UP
	OR	A		;BINARY ZERO AT END
	JP	NZ,FOUFRV6	;CONTINUE IF NOT AT END
	LD	H,B		;SAVE EXPONENT
	POP	BC		;FETCH TYPE, DIGIT COUNT
	LD	A,B		;DETERMINE TYPE
	CP	'E'		;SINGLE PRECISION?
	JP	NZ,FOUFRV9	;NO - GO PROCESS AS DOUBLE PRECISION
	LD	A,C		;DIGIT COUNT
	ADD	A,H		;ADD EXPONENT VALUE
	CP	09H
	POP	HL		;POP OLD BUFFER POINTER
	JP	NC,FOUFRV4	;CAN'T DO BETTER
FOUFRV8	LD	A,80H
	LD	(FLGSCN),A
	JP	FOUFRVB		;DO FIXED POINT PRINTOUT

FOUFRV9	LD	A,H		;SAVE EXPONENT
	ADD	A,C		;TOTAL DIGITS NECESSARY
	CP	12H		;MUST PRODUCE CARRY TO USE FIXED POINT
	POP	HL		;GET STACK RIGHT
	JP	NC,FOUFRV4
	JP	FOUFRV8		;GO PRINT IN FIXED POINT

FOUFRVA	POP	BC
	POP	HL		;GET ORIGINAL BUFFER PTR BACK
	JP	FOUFRV4

FOUFRVB	POP	AF		;GET ORIGINAL PSW OFF STACK
	POP	BC
	POP	DE		;GET DFACLO-DFACLO+3
	EX	DE,HL		;(DE)=BUFFER PTR,(HL)=DFACLO
	LD	(DFACLO),HL
	LD	H,B
	LD	L,C
	LD	(DFACLO2),HL
	EX	DE,HL
	POP	BC
	POP	DE		;GET ORIGINAL FAC BACK
	CALL	$MOVFR		;MOVE TO FAC
	INC	HL		;BECAUSE WHEN WE ORIGINALLY ENTERED
				; FOUFRV THE (HL) POINTED TO A CHAR.
				;PAST THE SIGN AND THE PASS THROUGH THIS
				; CODE LEAVES (HL) POINTING TO THE SIGN.
				;(HL) MUST POINT PAST SIGN!
FOUFRVC	CP	05H		;SET CC'S FOR Z80
	PUSH	HL		;SAVE THE BUFFER POINTER
	SBC	A,00H		;MAP 4 TO 6 AND 10 TO 20
	RLA			;THIS CALCULATES HOW MANY DIGITS
	LD	D,A		;WE WILL PRINT
	INC	D
	CALL	FOUTNV		;NORMALIZE THE FAC SO ALL SIGNIFICANT
				; DIGITS ARE IN THE INTEGER PART
	LD	BC,0300H	;B = DECIMAL POINT COUNT
				;C = COMMA COUNT
				;SET COMMA COUNT TO ZERO AND DECIMAL
				; POINT COUNT FOR E NOTATION
	PUSH	AF		;SAVE FOR NORMAL CASE
	LD	A,(FLGSCN)	;SEE IF FORCED FIXED OUTPUT
	OR	A		;SET CONDITION CODES CORRECTLY
	JP	P,FOUFRVD	;DO NORMAL THING
	POP	AF
	ADD	A,D
	JP	FOUFRVE		;FIXED OUTPUT

FOUFRVD	POP	AF		;NORMAL ROUTE
	ADD	A,D		;SEE IF NUMBER SHOULD BE PRINTED IN E NOTATION
	JP	M,FOFRS1	;IT SHOULD, IT IS .LT. .01
	INC	D		;CHECK IF IT IS TOO BIG
	CP	D
	JP	NC,FOFRS1	;IT IS TOO BIG, IT IS .GT. 10^D-1
FOUFRVE	INC	A		;IT IS OK FOR FIXED POINT NOTATION
	LD	B,A		;SET DECIMAL POINT COUNT
	LD	A,02H		;SET FIXED POINT FLAG, THE EXPONENT IS ZERO
				; IF WE ARE USING FIXED POINT NOTATION
FOFRS1	SUB	02H		;E NOTATION: ADD D-2 TO ORIGINAL EXPONENT
				;RESTORE EXP IF NOT D.P.
	POP	HL		;GET THE BUFFER POINTER BACK
	PUSH	AF		;SAVE THE EXPONENT FOR LATER
	CALL	FOUNSC8		;.01 .LE. NUMBER .LT. .1?
	LD	(HL),'0'	;YES, PUT ".0" IN BUFFER
	CALL	Z,INXHRT
	CALL	FOUNSCG		;CONVERT THE NUMBER TO DECIMAL DIGITS
;	Here to suppress the trailing zeros
FOFRS2	DEC	HL		;MOVE BACK TO THE LAST CHARACTER
	LD	A,(HL)		;GET IT AND SEE IF IT WAS ZERO
	CP	'0'
	JP	Z,FOFRS2	;IT WAS, CONTINUE SUPPRESSING
	CP	'.'		;HAVE WE SUPPRESSED ALL THE FRACTIONAL DIGITS?
	CALL	NZ,INXHRT	;YES, IGNORE THE DECIMAL POINT ALSO
	POP	AF		;GET THE EXPONENT BACK
	JP	Z,FOUTZR1	;WE ARE DONE IF WE ARE IN FIXED POINT NOTATION
				;FALL IN AND PUT THE EXPONENT IN THE BUFFER
;	Here to put the exponent and "E" or "D" in the buffer
;	The exponent is in A, the condition codes are assumed to be set
;	correctly.
;
FOFLDN	PUSH	AF		;SAVE THE EXPONENT
	CALL	GETYPR		;SET CARRY FOR SINGLE PRECISION
	LD	A,'"'		;[A]="D"/2
	ADC	A,A		;MULTIPLY BY 2 AND ADD CARRY
	LD	(HL),A		;SAVE IT IN THE BUFFER
	INC	HL		;INCREMENT THE BUFFER POINTER
	POP	AF		;GET THE EXPONENT BACK
;	PUT IN THE SIGN OF THE EXPONENT
	LD	(HL),'+'	;A PLUS IF POSITIVE
	JP	P,FOUCE1
	LD	(HL),'-'	;A MINUS IF NEGATIVE
	CPL			;NEGATE EXPONENT
	INC	A
;	Calculate the two digit exponent
FOUCE1	LD	B,'0'-1		;INITIALIZE TEN'S DIGIT COUNT
FOUCE2	INC	B		;INCREMENT DIGIT
	SUB	0AH		;SUBTRACT TEN
	JP	NC,FOUCE2	;DO IT AGAIN IF RESULT WAS POSITIVE
	ADD	A,3AH		;ADD BACK IN TEN AND CONVERT TO ASCII
;	Put the exponent in the buffer
	INC	HL
	LD	(HL),B		;PUT TEN'S DIGIT OF EXPONENT IN BUFFER
	INC	HL		;WHEN WE JUMP TO HERE, A IS ZERO
	LD	(HL),A		;PUT ONE'S DIGIT IN BUFFER
FOUTZR	INC	HL		;INCREMENT POINTER, HERE TO FINISH UP

;	Printing a free format zero
FOUTZR1	LD	(HL),00H	;PUT A ZERO AT THE END OF THE NUMBER
	EX	DE,HL		;SAVE THE POINTER TO THE END OF THE
				; NUMBER IN (DE) FOR FFXFLV
	LD	HL,FBUFFR1	;GET A POINTER TO THE BEGINNING
	RET			;ALL DONE

;	Here to print a number in fixed format
FOUTFX	INC	HL		;MOVE PAST THE ZERO FOR THE DOLLAR SIGN
	PUSH	BC		;SAVE THE FIELD LENGTH SPECIFICATIONS
	CP	04H		;CHECK WHAT KIND OF VALUE WE HAVE
	LD	A,D		;GET THE FORMAT SPECS
	JP	NC,FOUFXV	;WE HAVE A SNG OR A DBL

;	Here to print an integer in fixed format
	RRA			;CHECK IF WE HAVE TO PRINT IT IN FLOATING
	JP	C,FFXIFL	; POINT NOTATION

;	Here to print an integer in fixed format-fixed point notation
	LD	BC,0603H	;SET DECIMAL POINT COUNT TO 6 AND COMMA COUNT TO 3
	CALL	FOUNSC7		;CHECK IF WE DON'T HAVE TO USE THE COMMAS
	POP	DE		;GET THE FIELD LENGTHS
	LD	A,D		;SEE IF WE HAVE TO PRINT EXTRA SPACES BECAUSE
	SUB	05H		; THE FIELD IS TOO BIG
	CALL	P,FOUNSC0	;WE DO, PUT IN ZEROS, THEY WILL LATER BE
				; CONVERTED TO SPACES OR ASTERISKS BY FOUTZS
	CALL	$FOTCI		;CONVERT THE NUMBER TO DECIMAL DIGITS
FOUTFX1	LD	A,E		;DO WE NEED A DECIMAL POINT?
	OR	A
	CALL	Z,DECHRT	;WE DON'T, BACKSPACE OVER IT.
	DEC	A		;GET HOW MANY TRAILING ZEROS TO PRINT
	CALL	P,FOUNSC0	;PRINT THEM
				;IF WE DO HAVE DECIMAL PLACES, FILL THEM UP WITH ZEROS
				;FALL IN AND FINISH UP THE NUMBER


;	Here to finish up a fixed format number
FOUTTS	PUSH	HL		;SAVE BUFFER POINTER
	CALL	FOUTSZ		;ZERO SUPPRESS THE NUMBER
	POP	HL		;GET THE BUFFER POINTER BACK
	JP	Z,FFXIX1	;CHECK IF WE HAVE A TRAILING SIGN
	LD	(HL),B		;WE DO, PUT THE SIGN IN THE BUFFER
	INC	HL		;INCREMENT THE BUFFER POINTER
FFXIX1	LD	(HL),00H	;PUT A ZERO AT THE END OF THE NUMBER
;	Here to check if a fixed format-fixed point number overflowed its
;	field length
;	D = the B in the format specification
;	This assumes the location of the decimal point is in TEMP2
	LD	HL,FBUFFR	;GET A POINTER TO THE BEGINNING
FOUBE1	INC	HL		;INCREMENT POINTER TO THE NEXT CHARACTER
FOUBE5	LD	A,(TEMP2)	;GET THE LOCATION OF THE DECIMAL POINT
;	SINCE FBUFFR IS ONLY 35 (DECIMAL) LONG, WE
;	ONLY HAVE TO LOOK AT THE LOW ORDER TO SEE
;	IF THE FIELD IS BIG ENOUGH
	SUB	L		;FIGURE OUT HOW MUCH SPACE WE ARE TAKING
	SUB	D		;IS THIS THE RIGHT AMOUNT OF SPACE TO TAKE?
	RET	Z		;YES, WE ARE DONE, RETURN FROM FOUT
	LD	A,(HL)		;NO, WE MUST HAVE TOO MUCH SINCE WE STARTED
;	CHECKING FROM THE BEGINNING OF THE BUFFER
;	AND THE FIELD MUST BE SMALL ENOUGH TO FIT IN THE BUFFER.
;	GET THE NEXT CHARACTER IN THE BUFFER.
	CP	' '		;IF IT IS A SPACE OR AN ASTERISK, WE CAN
	JP	Z,FOUBE1	; IGNORE IT AND MAKE THE FIELD SHORTER WITH
	CP	'*'		; NO ILL EFFECTS
	JP	Z,FOUBE1
	DEC	HL		;MOVE THE POINTER BACK ONE TO READ
				; THE CHARACTER WITH CHRGET
	PUSH	HL		;SAVE THE POINTER
;	Here we see if we can ignore the leading zero before a decimal point.
;	This occurs if we see the following: (in order)
;		A sign (either "=" or "+")	[optional]
;		A dollar sign			[optional]
;		A zero				[mandatory]
;		A decimal point			[mandatory]
;		Another digit			[mandatory]
;	If you see a leading zero, it must be the one before a decimal point
;	or else FOUTZS would have suppressed it, so we can just "INX H"
;	over the character following the zero, and not check for the
;	decimal point explicitly.
FOUBE2	PUSH	AF		;PUT THE LAST CHARACTER ON THE STACK.
				;THE ZERO FLAG IS SET.
				;THE FIRST TIME THE ZERO ZERO FLAG IS NOT SET.
	LD	BC,FOUBE2	;GET ADDRESS WE GO TO IF WE SEE A CHARACTER
	PUSH	BC		; WE ARE LOOKING FOR
	CALL	CHRGTR		;GET THE NEXT CHARACTER
	CP	'-'		;SAVE IT AND GET THE NEXT CHARACTER IF IT IS
	RET	Z		; A MINUS SIGN, A PLUS SIGN OR A DOLLAR SIGN
	CP	'+'
	RET	Z
	CP	'$'
	RET	Z
	POP	BC		;IT ISN'T, GET THE ADDRESS OFF THE STACK
	CP	'0'		;IS IT A ZERO?
	JP	NZ,FOUBE4	;NO, WE CAN NOT GET RID OF ANOTHER CHARACTER
	INC	HL		;SKIP OVER THE DECIMAL POINT
	CALL	CHRGTR		;GET THE NEXT CHARACTER
	JP	NC,FOUBE4	;IT IS NOT A DIGIT, WE CAN'T SHORTEN THE FIELD
	DEC	HL		;WE CAN!!!  POINT TO THE DECIMAL POINT
	DB	01H		; "LD BC,nn" OVER THE NEXT 2 BYTES
FOUBE3	DEC	HL		;POINT BACK ONE CHARACTER
	LD	(HL),A		;PUT THE CHARACTER BACK
;	If we can get rid of the zero, we put the characters on the stack
;	back into the buffer one position in front of where they originally
;	were. Note that the maximum number of stack levels this uses is
;	three -- one for the last entry flag, one for a possible sign,
;	and one for a possible dollar sign. We don't have to worry about
;	the first character being in the buffer twice because the pointer
;	when FOUT exits will be pointing to the second occurrence.
	POP	AF		;GET THE CHARACTER OFF THE STACK
	JP	Z,FOUBE3	;PUT IT BACK IN THE BUFFER
				; IF IT IS NOT THE LAST ONE
	POP	BC		;GET THE BUFFER POINTER OFF THE STACK
	JP	FOUBE5		;SEE IF THE FIELD IS NOW SMALL ENOUGH

;	Here if the number is too big for the field
FOUBE4	POP	AF		;GET THE CHARACTERS OFF THE STACK
	JP	Z,FOUBE4	;LEAVE THE NUMBER IN THE BUFFER ALONE
	POP	HL		;GET THE POINTER TO THE BEGINNING
				; OF THE NUMBER MINUS 1
	LD	(HL),'%'	;PUT IN A PERCENT SIGN TO INDICATE
				; THE NUMBER WAS TOO LARGE FOR THE FIELD
	RET			;ALL DONE -- RETURN FROM FOUT

;	Here to print a SNG or DBL in fixed format
FOUFXV	PUSH	HL		;SAVE THE BUFFER POINTER
	RRA			;GET FIXED OR FLOATING NOTATION FLAG IN CARRY
	JP	C,FFXFLV	;PRINT THE NUMBER IN E-NOTATION
	JP	Z,FFXSFX	;WE HAVE A SNG HERE TO PRINT A DBL IN
				; FIXED FORMAT--FIXED POINT NOTATION
	LD	DE,$1D16$	;GET POINTER TO 1D16
	CALL	DCOMPD		;WE CAN'T PRINT A NUMBER .GE. 10^16 IN
				; FIXED POINT NOTATION
	LD	D,10H		;SET D = NUMBER OF DIGITS TO PRINT FOR A DBL
				;C = 0 FOR DBL (THIS IS FOR COMMAS)
	JP	M,FFXSDC	;IF THE FAC WAS SMALL ENOUGH, GO PRINT IT
;	Here to print in free format with a percent sign a number .GE. 10^16
FFXSD0	POP	HL		;GET THE BUFFER POINTER OFF THE STACK
	POP	BC		;GET THE FIELD SPECIFICATION OFF THE STACK
	CALL	FOUT		;PRINT THE NUMBER IN FREE FORMAT
	DEC	HL		;POINT TO IN FRONT OF THE NUMBER
	LD	(HL),'%'	;PUT IN THE PERCENT SIGN
	RET			;ALL DONE--RETURN FROM FOUT

;	Here to print a SNG in fixed format--fixed point notation
FFXSFX	LD	BC,0B60EH	;(10000000000000000) GET 1E16,
				; CHECK IF THE NUMBER IS TOO BIG
	LD	DE,1BCAH
	CALL	FCOMP
	JP	P,FFXSD0	;IT IS, PRINT IT IN FREE FORMAT WITH A % SIGN
	LD	D,06H		;D = NUMBER OF DIGITS TO PRINT IN A SNG
;	Here to actually print a SNG or DBL in E notation
FFXSDC	CALL	SIGN		;SEE IF WE HAVE ZERO
	CALL	NZ,FOUTNV	;IF NOT, NORMALIZE THE NUMBER SO ALL DIGITS
				; TO BE PRINTED ARE IN THE INTEGER PART
	POP	HL		;GET THE BUFFER POINTER
	POP	BC		;GET THE FIELD LENGTH SPECS
	JP	M,FFXXVS	;DO DIFFERENT STUFF IF EXPONENT IS NEGATIVE
;	Here to print a number with no fractional digits
	PUSH	BC		;SAVE THE FIELD LENGTH SPECS AGAIN
	LD	E,A		;SAVE THE EXPONENT IN E
	LD	A,B		;WE HAVE TO PRINT LEADING ZEROS IF THE FIELD
	SUB	D		; HAS MORE CHARACTERS THAN THERE ARE DIGITS
	SUB	E		; IN THE NUMBER.

;	If we are using commas, a may be too big.
;	This doesn't matter because FOUTTS will find the correct beginning.
;	There is room in FBUFFR because the maximum value B can be
;	24 (Decimal) so D+C .LE. 16 (Decimal)  since FAC .LT. 10^16.
;	So we need 8 more bytes for zeros.
;	4 come since we will not need to print an exponent.
;	FBUFFR also contains an extra 4 bytes for this case.
;	(It would take more than 4 bytes to check for this.)
	CALL	P,FOUNSC0	;FOUTZS WILL LATER SUPPRESS THEM
	CALL	FOUNSC5		;SETUP DECIMAL POINT AND COMMA COUNT
	CALL	FOUNSCG		;CONVERT THE NUMBER TO DECIMAL DIGITS
	OR	E		;PUT IN DIGITS AFTER THE NUMBER IF
				; IT IS BIG ENOUGH, HERE A=0
	CALL	NZ,FOUNSC4	;THERE CAN BE COMMAS IN THESE ZEROS
	OR	E		;MAKE SURE WE GET A DECIMAL POINT
				; FOR FOUTTS
	CALL	NZ,FOUNSCA
	POP	DE		;GET THE FIELD LENGTH SPECS
	JP	FOUTFX1		;GO CHECK THE SIZE, ZERO SUPPRESS,
				; ETC. AND FINISH THE NUMBER

;	Here to print a SNG or DBL that has fractional digits
FFXXVS	LD	E,A		;SAVE THE EXPONENT, WE DON'T NEED WHAT IS IN E
	LD	A,C		;DIVIDE BY TEN THE RIGHT NUMBER OF TIMES SO
	OR	A		; THE RESULT WILL BE ROUNDED CORRECTLY AND
	CALL	NZ,DCRART	; HAVE THE CORRECT NUMBER OF SIGNIFICANT
	ADD	A,E		; DIGITS
	JP	M,FFXXVS1	;FOR LATER CALCULATIONS, WE WANT A ZERO IF THE
	XOR	A		; RESULT WAS NOT NEGATIVE
FFXXVS1	PUSH	BC		;SAVE THE FIELD SPECS
	PUSH	AF		;SAVE THIS NUMBER FOR LATER
FFXXV2	CALL	M,FINDIV	;THIS IS THE DIVIDE LOOP
	JP	M,FFXXV2
	POP	BC		;GET THE NUMBER WE SAVED BACK IN B
	LD	A,E		;WE HAVE TWO CASES DEPENDING ON WHETHER THE
	SUB	B		; THE NUMBER HAS INTEGER DIGITS OR NOT
	POP	BC		;GET THE FILED SPECS BACK
	LD	E,A		;SAVE HOW MANY DECIMAL PLACES BEFORE THE
	ADD	A,D		; THE NUMBER ENDS
	LD	A,B		;GET THE "B" FIELD SPEC
	JP	M,FFXXV3
;	Here to print numbers with integer digits
	SUB	D		;PRINT SOME LEADING ZEROS IF THE FIELD IS
	SUB	E		; BIGGER THAN THE NUMBER OF DIGITS WE WILL
	CALL	P,FOUNSC0	; PRINT
	PUSH	BC		;SAVE FIELD SPEC
	CALL	FOUNSC5		;SET UP DECIMAL POINT AND COMMA COUNT
	JP	FFXXV31		;CONVERT THE DIGITS AND DO THE TRIMMING UP

;	Here to print a number without integer digits
FFXXV3	CALL	FOUNSC0		;PUT ALL ZEROS BEFORE THE DECIMAL POINT
	LD	A,C		;SAVE C
	CALL	FOUNSCE		;PUT IN A DECIMAL POINT
	LD	C,A		;RESTORE C
	XOR	A		;DECIDE HOW MANY ZEROS TO PRINT BETWEEN THE
	SUB	D		; DECIMAL POINT AND THE FIRST DIGIT WE WILL
	SUB	E		; PRINT.
	CALL	FOUNSC0		;PRINT THE ZEROS
	PUSH	BC		;SAVE EXPONENT AND THE "C" IN THE FIELD SPEC
	LD	B,A		;ZERO THE DECIMAL PLACE COUNT
	LD	C,A		;ZERO THE COMMA COUNT
;	Here to print an integer in fixed format--floating poing notation
FFXXV31	CALL	FOUNSCG		;CONVERT THE NUMBER TO DECIMAL DIGITS
	POP	BC		;GET THE FIELD SPECS BACK
	OR	C		;CHECK IF WE HAVE TO PRINT ANY ZEROS
				; AFTER THE LAST DIGIT
	JP	NZ,FFXXV32	;CHECK IF THERE WERE ANY DECIMAL PLACES AT ALL
				;E CAN NEVER BE 200, (IT IS NEGATIVE) SO IF
				; A=0 HERE, THERE IS NO WAY WE WILL CALL FOTZER
	LD	HL,(TEMP2)	;THE END OF THE NUMBER IS WHERE THE DP IS
FFXXV32	ADD	A,E		;PRINT SOME MORE TRAILING ZEROS
	DEC	A
	CALL	P,FOUNSC0
	LD	D,B		;GET THE "B" FIELD SPEC IN D FOR FOUTTS
	JP	FOUTTS		;FINISH UP THE NUMBER


;	Here to print an integer in fixed format--floating point notation
FFXIFL	PUSH	HL		;SAVE THE BUFFER POINTER
	PUSH	DE		;SAVE THE FORMAT SPECS
	CALL	CONSI		;CONVERT THE INTEGER TO A SNG
	POP	DE		;GET THE FORMAT SPECS BACK
	XOR	A		;SET FLAGS TO PRINT THE NUMBER AS A SNG
				;FALL INTO FFXFLV

;	Here to print a SNG or DBL in fixed format-floating point notation
FFXFLV	JP	Z,FFXSFL	;IF WE HAVE A SNG, SET THE RIGHT FLAGS
	LD	E,10H		;WE HAVE A DBL, GET HOW MANY DIGITS WE HAVE
	DB	01H		; "LD BC,nn" OVER THE NEXT TWO BYTES
FFXSFL	LD	E,06H		;WE HAVE A SNG, GET HOW MANY DIGITS WE PRINT
	CALL	SIGN		;SEE IF WE HAVE ZERO
	SCF			;SET CARRY TO DETERMINE IF WE ARE PRINTING ZERO.
				; NOTE: THIS DEPENDS ON THE FACT THAT FOUTNV
				; EXITS WITH CARRY OFF
	CALL	NZ,FOUTNV	;IF NOT, NORMALIZE THE NUMBER SO ALL DIGITS
				; TO BE PRINTED ARE IN THE INTEGER PART
	POP	HL		;GET THE BUFFER POINTER BACK
	POP	BC		;GET THE FIELD LENGTH SPECS
	PUSH	AF		;SAVE THE EXPONENT
	LD	A,C		;CALCULATE HOW MANY SIGNIFICANT DIGITS WE MUST
	OR	A		; PRINT
	PUSH	AF		;SAVE THE "C" FIELD SPEC FOR LATER
	CALL	NZ,DCRART
	ADD	A,B
	LD	C,A
	LD	A,D		;GET THE "A" FIELD SPEC
	AND	04H		;SEE IF THE SIGN IS A TRAILING SIGN
	CP	01H		;SET CARRY IF A IS ZERO
	SBC	A,A		;SET D=0 IF WE HAVE A TRAILING SIGN,
	LD	D,A		; D=377 IF WE DO NOT
	ADD	A,C
	LD	C,A		;SET C=NUMBER OF SIGNIFICANT DIGITS TO PRINT
	SUB	E		;IF WE HAVE LESS THAN E, THEN WE MUST GET RID
	PUSH	AF		;SAVE COMPARISON # OF SIG DIGITS
				; AND THE # OF DIGITS WE WILL PRINT
	PUSH	BC		;SAVE THE "B" FIELD SPEC AND # OF SIG DIGITS
FFXLV1	CALL	M,FINDIV	; OF SOME BY DIVIDING BY TEN AND ROUNDING
	JP	M,FFXLV1
	POP	BC		;GET "B" FIELD SPEC AND # OF SIG DIGITS BACK
	POP	AF		;GET # OF TRAILING ZEROS TO PRINT
	PUSH	BC		;SAVE THE "B" FIELD SPEC AND # OF SIG DIGITS
	PUSH	AF		;SAVE # OF TRAILING ZEROS TO PRINT
	JP	M,FFXLV15	;TAKE INTO ACCOUNT DIGITS THAT WERE
	XOR	A		;DIVIDED OFF AT FFXLV1
;	Normalize the number in the FAC so all the digits are in the integer
;	part. Return the base 10 exponent in A
;	D,E are left unaltered
FFXLV15	CPL
	INC	A
	ADD	A,B		;SET THE DECIMAL PLACE COUNT
	INC	A
	ADD	A,D		;TAKE INTO ACCOUNT IF THE SIGN IS TRAILING
	LD	B,A		; OR NOT
	LD	C,00H		;SET COMMA COUNT TO ZERO, THE COMMA SPEC IS IGNORED.
	CALL	FOUNSCG		;CONVERT THE NUMBER TO DECIMAL DIGITS
	POP	AF		;GET NUMBER TRAILING ZEROS TO PRINT
;	If the field length is longer than the # of digits
;	we can print
	CALL	P,FOUNSC2	;THE DECIMAL POINT COULD COME OUT IN HERE
	CALL	FOUNSCA		;IN CASE D.P. IS LAST ON LIST
	POP	BC		;GET # OF SIG DIGITS AND "B" FIELD SPAC BACK
	POP	AF		;GET THE "C" FIELD SPEC BACK
	JP	NZ,FFXLV16	;IF NON-ZERO PROCEED
	CALL	DECHRT		;SEE IF D.P. THERE
	LD	A,(HL)		;FETCH TO MAKE SURE D.P.
	CP	'.'		;IF NOT MUST BE ZERO
	CALL	NZ,INXHRT	;IF NOT MUST LEAVE AS IS
	LD	(TEMP2),HL	;NEED D.P. LOCATION IN TEMP2
				; SO IGNORE IT.
FFXLV16	POP	AF		;GET THE EXPONENT BACK
	JP	C,FFXLV17	;EXPONENT=0 IF THE NUMBER IS ZERO
	ADD	A,E		;SCALE IT CORRECTLY
	SUB	B
	SUB	D
FFXLV17	PUSH	BC		;SAVE THE "B" FIELD SPEC
	CALL	FOFLDN		;PUT THE EXPONENT IN THE BUFFER
	EX	DE,HL		;GET THE POINTER TO THE END IN (HL)
				; IN CASE WE HAVE A TRAILING SIGN
	POP	DE		;GET THE "B" FIELD SPEC IN D, PUT ON A
	JP	FOUTTS		; POSSIBLE TRAILING SIGN AND WE ARE DONE


;	Normalize the number in the FAC so all the digits are in the integer
;	part. Return the base 10 exponent in A
;	D,E are left unaltered
FOUTNV	PUSH	DE		;SAVE (DE)
	XOR	A		;ZERO THE EXPONENT
	PUSH	AF		;SAVE IT
	CALL	GETYPR		;GET TYPE OF NUMBER TO BE PRINTED
	JP	PO,FOUNS10	;NOT DOUBLE, DO NORMAL THING
FOUTNV1	LD	A,(FAC)		;GET EXPONENT
	CP	91H		;IS IT .LT.1D5?
	JP	NC,FOUNS10	;NO, DONT MULTPLY
	LD	DE,$D1E10$	;MULTIPLY BY 1D10
	LD	HL,ARGLO	;MOVE INTO ARG
	CALL	VMOVE		;PUT IN ARG
	CALL	DMULT		;MULTIPLY BY IT
	POP	AF		;GET ORIG EXPONENT OFF STACK
	SUB	0AH		;GET PROPER OFFSET FOR EXPONENT
	PUSH	AF		;SAVE EXPONENT BACK
	JP	FOUTNV1		;FORCE IT BIGGER IF POSSIBLE

FOUNS10	CALL	FOUNSC		;IS THE FAC TOO BIG OR TOO SMALL?
FOUNS1	CALL	GETYPR		;SEE WHAT KIND OF VALUE WE HAVE SO
				; WE CAN SEE IF THE FAC IS BIG ENOUGH
	JP	PE,FOUNS1D	;WE HAVE A DBL
	LD	BC,9143H	;GET 99999.95 TO SEE IF THE FAC IS BIG
	LD	DE,4FF9H	; ENOUGH YET
	CALL	FCOMP
	JP	FOUNS1C		;GO DO THE CHECK

FOUNS1D	LD	DE,$D1E15$	;GET POINTER TO 999,999,999,999,999.5
	CALL	DCOMPD		;SEE IF THE NUMBER IS STILL TOO SMALL
FOUNS1C	JP	P,FOUNS3	;IT ISN'T ANY MORE, WE ARE DONE
	POP	AF		;IT IS, MULTIPLY BY TEN
	CALL	FINMLT
	PUSH	AF		;SAVE THE EXPONENT AGAIN
	JP	FOUNS1		;NOW SEE IF IT IS BIG ENOUGH

FOUNS2	POP	AF		;THE FAC IS TOO BIG, GET THE EXPONENT
	CALL	FINDIV		;DIVIDE IT BY TEN
	PUSH	AF		;SAVE THE EXPONENT AGAIN
	CALL	FOUNSC		;SEE IF THE FAC IS SMALL ENOUGH
FOUNS3	POP	AF		;WE ARE DONE, GET THE EXPONENT BACK
	OR	A		;CLEAR CARRY
	POP	DE		;GET (DE) BACK
	RET			;ALL DONE

;	Here to see if the FAC is small enough yet
FOUNSC	CALL	GETYPR		;SEE WHAT TYPE NUMBER WE HAVE
	JP	PE,FOUNSCD	;WE HAVE A DBL
	LD	BC,9474H	;GET 999999.5 TO SEE IF THE FAC IS TOO BIG
	LD	DE,23F8H
	CALL	FCOMP
	JP	FOUNSCC		;GO DO THE CHECK

FOUNSCD	LD	DE,$HIDBL	;GET POINTER TO 9,999,999,999,999,999.5
	CALL	DCOMPD		;SEE IF THE NUMBER IS TOO BIG
FOUNSCC	POP	HL		;GET THE RETURN ADDRESS OFF THE STACK
	JP	P,FOUNS2	;THE NUMBER IS TOO BIG, DIVIDE IT BY TEN
	JP	(HL)		;IT ISN'T TOO BIG, JUST RETURN

;	Here to put some zeros in the buffer
;	The count is in A, it can be zero, but the Zero flag must be set
;	Only (HL) and A are altered
;	We exit with A=0
FOUNSC0	OR	A		;THIS IS BECAUSE FFXXV3 CALL US
				; WITH THE CONDITION CODES NOT SET UP
FOUNSC1	RET	Z		;RETURN IF WE ARE DONE
	DEC	A		;WE ARE NOT DONE, SO DECREMENT THE COUNT
	LD	(HL),'0'	;PUT A ZERO IN THE BUFFER
	INC	HL		;UPDATE THE BUFFER POINTER
	JP	FOUNSC1		;GO SEE IF WE ARE NOW DONE

;	Here to put zeros in the buffer with commas or a decimal point in the middle.
;	The count is in A, it can be zero, but the Zero flag must be set.
;	B the decimal point count and C the comma count are updated
;	A,B,C,H,L are altered
FOUNSC2	JP	NZ,FOUNSC4	;ENTRY AFTER A "CALL FOUTCV"
FOUNSC3	RET	Z		;RETURN IF WE ARE DONE
	CALL	FOUNSCA		;SEE IF WE HAVE TO PUT A COMMA
				; OR A DECIMAL POINT BEFORE THIS ZERO
FOUNSC4	LD	(HL),'0'	;PUT A ZERO IN THE BUFFER
	INC	HL		;UPDATE THE BUFFER POINTER
	DEC	A		;DECREMENT THE ZERO COUNT
	JP	FOUNSC3		;GO BACK AND SEE IF WE ARE DONE

;	Here to put a possible comma count in C, and zero C if we are not
;	using the comma specification
FOUNSC5	LD	A,E		;SETUP DECIMAL POINT COUNT
	ADD	A,D
	INC	A
	LD	B,A
	INC	A		;SETUP COMMA COUNT
FOUNSC6	SUB	03H		;REDUCE [A] MOD 3
	JP	NC,FOUNSC6
	ADD	A,05H		;ADD 3 BACK IN AND ADD 2 MORE FOR SCALING
	LD	C,A		;SAVE A POSSIBLE COMMA COUNT
FOUNSC7	LD	A,(TEMP3)	;GET THE FORMAT SPECS
	AND	40H		;LOOK AT THE COMMA BIT
	RET	NZ		;WE ARE USING COMMAS, JUST RETURN
	LD	C,A		;WE AREN'T, ZERO THE COMMA COUNT
	RET			;ALL DONE


;*****************************************************************
;
;       $FOUTAN  This routine is called by the free format output
;               code to output decimal point and leading zeros.
;       $FOUTED  This routine is called by both the free format
;               output routine and the PRINT USING code to output
;               the decimal point when necessary and to put in
;               commas "," after each three digits if this option
;               is invoked.
;       Calling sequence:       CALL    $FOUTAN
;                               CALL    $FOUTED
;               with $FMTCX containing number places prior to
;               decimal point (negatively) in upper byte and
;               no places before next comma in low byte
;
;*******************************************************************

;	Here to put decimal points and commas in their correct places
;	This subroutine should be called before the next digit is put in the
;	buffer.  B=the decimal point count, C=the comma count
;	The counts tell how many more digits have to go in before the comma
;	or decimal point go in.  The comma or decimal point then goes before
;	the last digit in the count.  For example, if the decimal point should
;	come after the first digit, the decimal point count should be 2.
FOUNSC8	DEC	B		;IF NEGATIVE THEN LEADING ZEROS
	JP	P,FOUNSCB	;PROCESS AS NORMAL
	LD	(TEMP2),HL	;SAVE DECIMAL POINT COUNT
	LD	(HL),'.'	;MOVE IN DECIMAL POINT
FOUNSC9	INC	HL		;POINT TO NEXT OUTPUT POSITION
	LD	(HL),'0'	;PUT IN LEADING ZERO
	INC	B		;WILL INCREMENT B UNTIL ZERO
	JP	NZ,FOUNSC9
	INC	HL		;PUT IN LEADING ZEROS UNTIL B ZERO
	LD	C,B		;POINT TO NEXT AVAILABLE BUFFER LOCATION
	RET

FOUNSCA	DEC	B		;TIME FOR D.P.?
FOUNSCB	JP	NZ,FOUNSCF	;IF NOT D.P. TIME, SEE IF COMMA TIME

;	Entry to put a decimal point in the buffer
FOUNSCE	LD	(HL),'.'	;YES, PUT THE DECIMAL POINT IN
	LD	(TEMP2),HL	;SAVE THE LOCATION OF THE DECIMAL POINT
	INC	HL		;INCREMENT THE BUFFER POINTER PAST D.P.
	LD	C,B		;PUT ZERO IN C SO WE WON"T PRINT ANY
				; COMMAS AFTER THE DECIMAL POINT.
	RET			;ALL DONE

;	Here to see if it is time to print a comma
FOUNSCF	DEC	C		;IF ZERO TIME FOR COMMA
	RET	NZ		;NOPE, WE CAN RETURN
	LD	(HL),','	;YES, PUT A COMMA IN THE BUFFER
	INC	HL		;POINT TO NEXT BUFFER POSITION
	LD	C,03H		;RESET THE COMMA COUNT SO WE WILL PRINT
				; A COMMA AFTER THREE MORE DIGITS.
	RET			;ALL DONE

;	Here to convert a SNG or DBL number that has been normalized to decimal digits.
;	The decimal point count and comma count are in B and C respectively.
;	(HL) points to where the first digit will go.
;	This exits with A=0.  (DE) is left unaltered.
FOUNSCG	PUSH	DE		;SAVE (DE)
	CALL	GETYPR		;SEE WHAT KIND OF A NUMBER WE HAVE
	JP	PO,FOUNSCJ	;WE HAVE A SNG
;	Here to convert a double precision number to decimal digits
	PUSH	BC		;SAVE THE DECIMAL POINT AND COMMA COUNTS
	PUSH	HL		;SAVE THE BUFFER POINTER
	CALL	VMOVAF		;MOVE THE FAC INTO ARG
	LD	HL,$DHALF	;GET POINTER TO .5D0
	CALL	VMOVFM		;MOVE THE CONSTANT INTO THE FAC
	CALL	DADD		;ADD .5 TO THE ORIGINAL NUMBER TO ROUND IT
	XOR	A		;CLEAR THE CARRY
	CALL	DINTFO		;TAKE THE INTEGER PART OF THE NUMBER
				;THE NUMBER IS NOT NORMALIZED AFTERWARDS
	POP	HL		;GET THE BUFFER POINTER BACK
	POP	BC		;GET THE COMMA AND DECIMAL POINT COUNTS BACK
	LD	DE,$FODTB	;GET A POINTER TO THE DBL POWER OF TEN TABLE
	LD	A,0AH		;CONVERT TEN DIGITS, THE OTHERS WILL BE
				; CONVERTED AS SNG'S AND INT'S
				; BECAUSE WE BRACKETED THE NUMBER A POWER OF
				; TEN LESS IN MAGNITUDE AND
				; SINGLE PRECISION CONVERSION CAN HANDLE
				; A MAGNITUDE OF TEN LARGER
;	Here to convert the next digit
FOUNSCH	CALL	FOUNSCA		;SEE IF WE HAVE TO PUT IN A DP OR COMMA
	PUSH	BC		;SAVE DP AND COMMA INFORMATION
	PUSH	AF		;SAVE DIGIT COUNT
	PUSH	HL		;SAVE BUFFER POINTER
	PUSH	DE		;SAVE POWER OF TEN POINTER
	LD	B,'0'-1		;SET UP THE COUNT FOR THE DIGIT
FOUNSCI	INC	B		;INCREMENT THE DIGIT COUNT
	POP	HL		;GET THE POINTER TO THE POWER OF TEN
	PUSH	HL		;SAVE IT AGAIN
	LD	A,9EH		; SBC A,(HL)
				; GET THE INSTRUCTION TO SUBTRACT THE POWER OF TEN
	CALL	DADDFO		;GO SUBTRACT THEM
	JP	NC,FOUNSCI	;IF THE NUMBER WAS NOT LESS THAN THE POWER OF TEN,
				; SUBTRACT AGAIN
	POP	HL		;WE ARE DONE SUBTRACTING, BUT WE DID IT ONCE
	LD	A,8EH		; ADC A,(HL)
				; GET THE INSTRUCTION TO ADD THE POWER OF TEN
				; AND THE NUMBER
	CALL	DADDFO		;ADD THE TWO NUMBERS
	EX	DE,HL		;PUT THE POWER OF TEN POINTER IN (DE).
				; IT IS UPDATED FOR THE NEXT POWER OF TEN
	POP	HL		;GET THE BUFFER POINTER BACK
	LD	(HL),B		;PUT THE DIGIT INTO THE BUFFER
	INC	HL		;INCREMENT THE BUFFER POINTER
	POP	AF		;GET THE DIGIT COUNT BACK
	POP	BC		;GET THE DECIMAL POINT AND COMMA COUNTS
	DEC	A		;HAVE WE PRINTED THE LAST DIGIT?
	JP	NZ,FOUNSCH	;NO, GO DO THE NEXT ONE
	PUSH	BC		;YES, CONVERT REMAINING DIGITS USING SINGLE
	PUSH	HL		; PRECISION, THIS IS FASTER, MOVE THE NUMBER
	LD	HL,DFACLO	; THAT IS LEFT INTO THE SNG FAC
	CALL	$MOVFM
	JP	FOUNSCK

;	Here to convert a single precision number to decimal digits
FOUNSCJ	PUSH	BC		;SAVE THE DECIMAL POINT AND COMMA COUNTS
	PUSH	HL		;SAVE THE BUFFER POINTER
	CALL	$FADDH		;ROUND NUMBER TO NEAREST INTEGER
	LD	A,01H		;MAKE A NON-ZERO, SINCE NUMBER IS POSITIVE AND
				; NON-ZERO, ROUND WILL EXIT WITH THE HO
				; IN A, SO THE MSB WILL ALWAYS BE ZERO AND ADDING
				; ONE WILL NEVER CAUSE A TO BE ZERO
	CALL	QINT		;GET INTEGER PART IN C,D,E
	CALL	$MOVFR		;SAVE NUMBER IN FAC
FOUNSCK	POP	HL		;GET THE BUFFER POINTER BACK
	POP	BC		;GET THE DECIMAL POINT AND COMMA COUNTS BACK
	XOR	A		;CLEAR CARRY, THE CARRY IS OUR FLAG
				; TO CALCULATE TWO DIGITS
	LD	DE,$FOSTB	;GET POINTER TO POWER OF TEN TABLE
;	Here to calculate the next digit of the number
FOUNSCL	CCF			;COMPLEMENT FLAG THAT TELLS WHEN WE ARE DONE
	CALL	FOUNSCA		;SEE IF A COMMA OR DP GOES BEFORE THIS DIGIT
	PUSH	BC		;SAVE COMMA AND DECIMAL POINT INFORMATION
	PUSH	AF		;SAVE CARRY I.E. DIGIT COUNT
	PUSH	HL		;SAVE CHARACTER POINTER
	PUSH	DE		;SAVE POWER OF TEN POINTER
	CALL	$MOVRF		;GET NUMBER IN C,D,E
	POP	HL		;GET POWER OF TEN POINTER
	LD	B,'0'-1		;B = NEXT DIGIT TO BE PRINTED
FOUNSCM	INC	B		;ADD ONE TO DIGIT
	LD	A,E		;SUBTRACT LO
	SUB	(HL)
	LD	E,A
	INC	HL		;POINT TO NEXT BYTE OF POWER OF TEN
	LD	A,D		;SUBTRACT MO
	SBC	A,(HL)
	LD	D,A
	INC	HL
	LD	A,C		;SUBTRACT HO
	SBC	A,(HL)
	LD	C,A
	DEC	HL		;POINT TO BEGINNING OF POWER OF TEN
	DEC	HL
	JP	NC,FOUNSCM	;SUBTRACT AGAIN IF RESULT WAS POSITIVE
	CALL	FADDA		;IT WASN'T, ADD POWER OF TEN BACK IN
	INC	HL		;INCREMENT POINTER TO NEXT POWER OF TEN
	CALL	$MOVFR		;SAVE C,D,E IN FAC
	EX	DE,HL		;GET POWER OF TEN POINTER IN (DE)
	POP	HL		;GET BUFFER POINTER
	LD	(HL),B		;PUT CHARACTER IN BUFFER
	INC	HL		;INCREMENT BUFFER POINTER
	POP	AF		;GET DIGIT COUNT (THE CARRY) BACK
	POP	BC		;GET COMMA AND DP INFORMATION BACK
	JP	C,FOUNSCL	;CALCULATE NEXT DIGIT IF WE HAVE NOT DONE 2
	INC	DE		;WE HAVE, INCREMENT POINTER TO CORRECT PLACE
	INC	DE		; IN THE INTEGER POWER OF TEN TABLE
	LD	A,04H		;GET THE DIGIT COUNT
	JP	FCI20		;COMPUTE THE REST OF THE DIGITS LIKE INTEGERS
				;NOTE THAT THE CARRY IS OFF

;*************************************************************
;
;       $FOUTCI  Convert the integer in (FACLO)-two bytes to
;               ASCII digits.
;       Calling sequence:       CALL    $FOUTCI
;               with decimal point and comma counts in (BC)
;       $FOUTO  Convert integer in $FACLO:FACLO+1 to octal
;       $FOUTH  Convert integer in $FACLO:FACLO+1 to hexadecimal
;       Calling sequence:       CALL    $FOUTO/$FOUTH
;               with $FACLO:FACLO+1 containing integer to be
;               printed. Returns with (HL) pointing to $FBUFF
;
;**************************************************************

;	Here to convert an integer into decimal digits
;	This exits with A=0.  (DE) is left unaltered.
$FOTCI	PUSH	DE		;SAVE (DE)
	LD	DE,$FOITB	;GET POINTER TO THE INTEGER POWER OF TEN TABLE
	LD	A,05H		;SET UP A DIGIT COUNT, WE HAVE TO CALCULATE
				; 5 DIGITS BECAUSE THE MAX POS INTEGER IS 32768
;	Here to calculate each digit
FCI20	CALL	FOUNSCA		;SEE IF A COMMA OR DP GOES BEFORE THE DIGIT
	PUSH	BC		;SAVE COMMA AND DECIMAL POINT INFORMATION
	PUSH	AF		;SAVE DIGIT COUNT
	PUSH	HL		;SAVE BUFFER POINTER
	EX	DE,HL		;GET THE POWER OF TEN POINTER IN (HL)
	LD	C,(HL)		;PUT THE POWER OF TEN ON THE STACK
	INC	HL
	LD	B,(HL)
	PUSH	BC
	INC	HL		;INCREMENT THE PWR OF TEN PTR TO NEXT POWER
	EX	(SP),HL		;GET THE POWER OF TEN IN (HL) AND
				; PUT THE POINTER ON THE STACK
	EX	DE,HL		;PUT THE POWER OF TEN IN (DE)
	LD	HL,(FACLO)	;GET THE INTEGER IN (HL)
	LD	B,'0'-1		;SET UP THE DIGIT COUNT, B=DIGIT TO BE PRINTED
FCI30	INC	B		;INCREMENT THE DIGIT COUNT
				;HL=HL-DE: SUBTRACT OUT POWER OF TEN
	LD	A,L		;SUBTRACT (DE) FROM (HL)
	SUB	E		;SUBTRACT THE LOW ORDERS
	LD	L,A		;SAVE THE NEW RESULT
	LD	A,H		;SUBTRACT THE HIGH ORDERS
	SBC	A,D
	LD	H,A		;SAVE THE NEW HIGH ORDER
	JP	NC,FCI30	;IF (HL) WAS .GE. (DE) THEN SUBTRACT AGAIN
	ADD	HL,DE		;WE ARE DONE, BUT WE SUBTRACTED (DE)
				; ONCE TOO OFTEN, SO ADD IT BACK IN
	LD	(FACLO),HL	;SAVE IN THE FAC WHAT IS LEFT
	POP	DE		;GET THE POWER OF TEN POINTER BACK
	POP	HL		;GET THE BUFFER POINTER BACK
	LD	(HL),B		;PUT THE NEW DIGIT IN THE BUFFER
	INC	HL		;INCREMENT THE BUFFER POINTER TO NEXT DIGIT
	POP	AF		;GET THE DIGIT COUNT BACK
	POP	BC		;GET THE COMMA AND DP INFORMATION BACK
	DEC	A		;WAS THAT THE LAST DIGIT?
	JP	NZ,FCI20	;NO, GO DO THE NEXT ONE
	CALL	FOUNSCA		;YES, SEE IF A DP GOES AFTER THE LAST DIGIT
	LD	(HL),A		;PUT A ZERO AT THE END OF THE NUMBER,
				; BUT DON'T INCREMENT (HL)
				; SINCE AN EXPONENT OR A TRAILING SIGN MAY BE COMMING
	POP	DE		;GET (DE) BACK
	RET			;ALL DONE, RETURN WITH A=0

$D1E10$	DB	00H,00H,00H,00H,0F9H,02H,15H,0A2H	;1D10  (10000000000)
$D1E15$	DB	0E1H,0FFH,9FH,31H,0A9H,5FH,63H,0B2H	;999,999,999,999,999.5
$HIDBL	DB	0FEH,0FFH,03H,0BFH,0C9H,1BH,0EH,0B6H	;9,999,999,999,999,999.5
$DHALF	DB	00H,00H,00H,00H
$SHALF	DB	00H,00H,00H,80H
$1D16$	DB	00H,00H,04H,0BFH,0C9H,1BH,0EH,0B6H	; 1D16: 10000000000000000
	;1D15..1D6 (7 bytes each)
$FODTB	DB	00H,80H,0C6H,0A4H,7EH,8DH,03H		; 1D15: 1000000000000000
	DB	00H,40H,7AH,10H,0F3H,5AH,00H		; 1D14: 100000000000000
	DB	00H,0A0H,72H,4EH,18H,09H,00H		; 1D13: 10000000000000
	DB	00H,10H,0A5H,0D4H,0E8H,00H,00H		; 1D12: 1000000000000
	DB	00H,0E8H,76H,48H,17H,00H,00H		; 1D11: 100000000000
	DB	00H,0E4H,0BH,54H,02H,00H,00H		; 1D10: 10000000000
	DB	00H,0CAH,9AH,3BH,00H,00H,00H		;  1D9: 1000000000
	DB	00H,0E1H,0F5H,05H,00H,00H,00H		;  1D8: 100000000
	DB	80H,96H,98H,00H,00H,00H,00H		;  1D7: 10000000
	DB	40H,42H,0FH,00H,00H,00H,00H		;  1D6: 1000000
	;1E5..1E4 (3 bytes each)
$FOSTB	DB	0A0H,86H,01H				;  1E5: 100000
	DB	10H,27H,00H				;  1E4: 10000
	;10000..1 (2 bytes each)
$FOITB	DB	10H,27H					;	10000
	DB	0E8H,03H				;	1000
	DB	64H,00H					;	100
	DB	0AH,00H					;	10
	DB	01H,00H					;	1



;
;	OUTPUT ROUTINES FOR OCTAL AND HEX NUMBERS
;

;	Octal string conversion
;
$FOUTO	XOR	A		;MAKE A=0, SET ZERO
	LD	B,A		;SAVE IN [B]
	DB	0C2H		; "JP NZ,nn" AROUND NEXT TWO BYTES

;	Hex string conversion
;
FOUTH1	LD	B,01H		;SET HEX FLAG
	PUSH	BC		;SAVE HEX/OCTAL FLAG
	CALL	FRQINT		;GET DOUBLE BYTE INT IN [H,L]
	POP	BC		;GET BACK HEX/OCTAL FLAG
	LD	DE,FBUFFR	;POINTER TO OUTPUT BUFFER IN [D,E]
	PUSH	DE		;SAVE SO WE CAN RETURN IT LATER
	XOR	A		;GET SET TO HAVE FIRST DIGIT FOR OCTAL
	LD	(DE),A		;CLEAR DIGIT SEEN FLAG
	DEC	B		;SEE IF OCTAL
	INC	B		;IF SO, ZERO SET
	LD	C,06H		;SIX DIGITS FOR OCTAL
	JP	Z,FOUTH4	;DO FIRST OCTAL DIGIT
	LD	C,04H		;FOUR DIGIT FOR HEX
FOUTH2	ADD	HL,HL		;SHIFT LEFT ONE BIT
	ADC	A,A		;ADD IN THE SHIFTED BIT
FOUTH3	ADD	HL,HL		;SHIFT LEFT ONE BIT
	ADC	A,A
	ADD	HL,HL
	ADC	A,A
FOUTH4	ADD	HL,HL		;ENTER HERE FOR FIRST OCTAL DIGIT
	ADC	A,A
	OR	A		;SEE IF WE GOT A ZERO DIGIT
	JP	NZ,FOUTH5	;NO, MAKE A DIGIT
	LD	A,C		;GET DIGIT COUNTER
	DEC	A		;WAS IT GOING TO GO TO ZERO (LAST DIG?)
	JP	Z,FOUTH5	;IF SO, FORCE ONE ZERO DIGIT
	LD	A,(DE)		;HAVE WE PRINTED A NON-ZERO DIGIT?
	OR	A		;SET CC'S
	JP	Z,FOUTH7	;NO, DONT PRINT THIS LEADING ZERO
	XOR	A		;GET ZERO
FOUTH5	ADD	A,30H		;MAKE NUMERIC DIGIT
	CP	':'		;IS IT A BIG HEX DIGIT? (A-F)
	JP	C,FOUTH6	;NO, DONT ADD OFFSET
	ADD	A,07H		;(A..F) ADD OFFSET
FOUTH6	LD	(DE),A		;SAVE DIGIT IN FBUFFR
	INC	DE		;BUMP POINTER
	LD	(DE),A		;SAVE HERE TO FLAG PRINTED SIG. DIG.
FOUTH7	XOR	A		;MAKE A ZERO
	DEC	C		;ALL DONE PRINTING?
	JP	Z,FOUTH8	;YES, RETURN
	DEC	B		;SEE IF HEX OR OCTAL
	INC	B		;TEST
	JP	Z,FOUTH3	;WAS OCTAL
	JP	FOUTH2		;WAS HEX

;	aka FINOHO
FOUTH8	LD	(DE),A		;STORE FINAL ZERO
	POP	HL		;GET POINTER TO FBUFFR
	RET			;ALL DONE.

;	Negate number, a.k.a. NEGAFT
PSHNEG	LD	HL,NEG		;GET THE ADDRESS OF NEG
	EX	(SP),HL		;SWITCH RET ADDR AND ADDR OF NEG
	JP	(HL)		;RETURN, THE ADDRESS OF NEG IS ON THE STACK

;	(END No equivalent code in ALTAIR BASIC)
;

;-----------------------------------------------------------------------------
;	EXPONENTIATION AND THE SQUARE ROOT FUNCTION
;
;	Square root function  ---  X=SQR(A)
;	We first scale the argument to between .5 and 2 by looking at the
;	exponent and using SQR(M*2^(2*N))=2^N*SQR(M). Then newton's method
;	is used to compute SQR(M). The exponent is saved to scale the
;	result at the end.
;	Newton's method for square root:
;	  X(0)=A
;	  X(N+1)=(X(N)+A/X(N))/2
;	(not implemented as such in the TRS-80 LEVEL II BASIC)
;
;	Square root function
;	We use SQR(X)=X^.5
SQR	CALL	PUSHF		;SAVE ARG X
	LD	HL,$SHALF	;GET 1/2
	CALL	$MOVFM		;SQR(X)=X^.5
	JP	SQRC		;SKIP OVER THE NEXT 3 BYTES

;	Exponentiation  ---  X^Y
;	N.B. 0^0=1
;	First we check if Y=0, if so, the result is 1.
;	Next, we check if X=0, if so, the result is 0.
;	Then we check if X is positive, if not, we check that Y is a
;	negative integer, and whether it is even or odd. If Y is a negative
;	integer, we negate X. If not, log will give an FC Error when we call
;	it. If X is negative and Y is odd, we push the address of NEG on the
;	stack so we will return to it and get a negative result. To compute
;	the result we use X^Y=EXP(Y*LOG(X))
FPWRQ	CALL	CSNG		;MAKE SURE THE FAC IS A SNG
SQRC	POP	BC		;GET ARG IN REGISTERS, ENTRY TO FPWR
	POP	DE		; IF ARGUMENT IS ON STACK.  FALL INTO FPWR
	LD	HL,CLROVC	;BACK TO NORMAL OVERFLOW PRINT MODE
	PUSH	HL		;
	LD	A,01H		;SET UP ONCE ONLY OVERFLOW MODE
	LD	(FLGOVC),A	;SEE IF Y IS ZERO
	CALL	SIGN		;IT IS, RESULT IS ONE
	LD	A,B		;POSITIVE EXPONENT
	JP	Z,EXP		;IS IT ZERO TO MINUS POWER?
	JP	P,FP20		;GIVE DIV BY ZERO AND CONTINUE
	OR	A		;Zero to negative power?
	JP	Z,$DIV0S	;Yes - ?/0 Error
FP20	OR	A		;Base zero?
	JP	Z,ZERO0		;IT IS, RESULT IS ZERO
	PUSH	DE		;Save base
	PUSH	BC		;SAVE X ON STACK
	LD	A,C		;CHECK THE SIGN OF X
	OR	7FH		;TURN THE ZERO FLAG OFF
	CALL	$MOVRF		;GET Y IN THE REGISTERS
	JP	P,FP303		;NO PROBLEMS IF X IS POSITIVE
	PUSH	AF		;Save exponent
	LD	A,(FAC)		;Check the sign of power
	CP	99H
	JP	C,FP302		;Negative
	POP	AF		;Restore exponent
	JP	FP303		;Positive

FP302	POP	AF		;Restore exponent
	PUSH	DE
	PUSH	BC
	CALL	FINT		;SEE IF Y IS AN INTEGER
	POP	BC
	POP	DE		;GET Y BACK
	PUSH	AF		;SAVE LO OF INT FOR EVEN AND ODD INFORMATION
	CALL	FCOMP		;SEE IF WE HAVE AN INTEGER
	POP	HL		;GET EVEN-ODD INFORMATION
	LD	A,H		;PUT EVEN-ODD FLAG IN CARRY
	RRA
FP303	POP	HL		;GET X BACK IN FAC
	LD	(FACHI),HL	;STORE HO'S
	POP	HL		;GET LO'S OFF STACK
	LD	(FACLO),HL	;STORE THEM IN FAC
	CALL	C,PSHNEG	;NEGATE NUMBER AT END IF Y WAS ODD
	CALL	Z,NEG		;NEGATE THE NEGATIVE NUMBER
	PUSH	DE
	PUSH	BC		;SAVE Y AGAIN
	CALL	LOG		;COMPUTE  EXP(Y*LOG(X))
	POP	BC
	POP	DE
				;IF X WAS NEGATIVE AND Y NOT AN INTEGER THEN
				; LOG WILL BLOW HIM OUT OF THE WATER
	CALL	FMULT		; Multiply LOG by power

;-----------------------------------------------------------------------------
;	EXPONENTIAL FUNCTION
;
;	THE FUNCTION EXP(X) CALCULATES e^X WHERE e=2.718282
;
;	THE TECHNIQUE USED IS TO EMPLOY A COUPLE
;	OF FUNDAMENTAL IDENTITIES THAT ALLOWS US TO
;	USE THE BASE 2 THROUGH THE DIFFICULT PORTIONS OF
;	THE CALCULATION:
;
;		(1)e^X=2^y  WHERE y=X*LOG2(e)
;			[LOG2(e) IS LOG BASE 2 OF e ]
;
;		(2) 2^y=2^[ INT(y)+(y-INT(y)]
;		(3) IF Ny=INT(y) THEN
;		    2^(Ny+y-Ny)=[2^Ny]*[2^(y-Ny)]
;
;	NOW, SINCE 2^Ny IS EASY TO COMPUTE (AN EXPONENT
;	CALCULATION WITH MANTISSA BITS OF ZERO) THE DIFFICULT
;	PORTION IS TO COMPUTE 2^(Y-Ny) WHERE 0.LE.(Y-Ny).LT.1
;	THIS IS ACCOMPLISHED WITH A POLYNOMIAL APPROXIMATION
;	TO 2^Z WHERE 0.LE.Z.LT.1  . ONCE THIS IS COMPUTED WE
;	HAVE TO EFFECT THE MULTIPLY BY 2^Ny .
EXP	LD	BC,8138H	;GET LOG2(e)
	LD	DE,0AA3BH
	CALL	FMULT		;y=FAC*LOG2(e)
	LD	A,(FAC)		;MUST SEE IF TOO LARGE
	CP	88H		;ABS .GT. 128?
	JP	NC,EXP110	;IF SO OVERFLOW
	CP	68H		;IF TOO SMALL ANSWER IS 1
	JP	C,EXP200
	CALL	PUSHF		;SAVE y
	CALL	FINT		;DETERMINE INTEGER POWER OF 2
	ADD	A,81H		;INTEGER WAS RETURNED IN A
				;BIAS IS $81 BECAUSE BINARY POINT
				; IS TO LEFT OF UNDERSTOOD 1
	POP	BC
	POP	DE		;RECALL y
	JP	Z,EXP100	;OVERFLOW
	PUSH	AF		;SAVE EXPONENT
	CALL	FSUB		;FAC=y-INT(y)
	LD	HL,$EXPCN	;WILL USE HART 1302 POLY.
	CALL	$POLY		;COMPUTE 2^[y-INT(y)]
	POP	BC		;INTEGER POWER OF 2 EXPONENT
	LD	DE,0000H	;NOW HAVE FLOATING REPRESENTATION
				; OF INT(y) IN (BCDE)
	LD	C,D
	JP	FMULT		;MULTIPLY BY 2^[y-INT(y)] AND RETURN

EXP110	CALL	PUSHF
EXP100	LD	A,(FACHI)	;IF NEG. THEN JUMP TO ZERO
	OR	A
	JP	P,$OVFLS?	;OVERFLOW IF PLUS
	POP	AF		;NEED STACK RIGHT
	POP	AF
	JP	$ZERO		;GO ZERO THE FAC

$OVFLS?	JP	FINEDG4		;OVERFLOW

;	Load '1' to FP accumulator
EXP200	LD	BC,8100H
	LD	DE,0000H
	JP	$MOVFR

;*************************************************************
;	Hart 1302 polynomial coefficients
;	Coefficients for polynomial evaluation of 2^x
;	where .5 .LE. x .LE. 1
;*************************************************************
$EXPCN	DB	07H
;	.0002074557
$EXPC5	DB	7CH,88H,59H,74H
;	.0012710057
$EXPC4	DB	0E0H,97H,26H,77H
;	.00965065
$EXPC3	DB	0C4H,1DH,1EH,7AH
;	.055496565
$EXPC2	DB	5EH,50H,63H,7CH
;	.24022713
$EXPC1	DB	1AH,0FEH,75H,7EH
;	.6931471
$EXPC0	DB	18H,72H,31H,80H
;	1.000000 (additional constant)
$EXPCA	DB	00H,00H,00H,81H


;-----------------------------------------------------------------------------
;	POLYOMIAL EVALUATOR AND THE RANDOM NUMBER GENERATOR
;
;	Evaluate P(X^2)*X
;	Pointer to Degree+1 is in (HL)
;	The constants follow the degree
;	Constants should be stored in reverse order, FAC has X
;	We compute:
;	 CO*X+C1*X^3+C2*X^5+C3*X^7+...+C(N)*X^(2*N+1)
$POLYX	CALL	PUSHF		;SAVE X
	LD	DE,FMULTT	;PUT ADDRESS OF FMULTT ON STACK SO WHEN WE
	PUSH	DE		; RETURN WE WILL MULTIPLY BY X
	PUSH	HL		;SAVE CONSTANT POINTER
	CALL	$MOVRF		;SQUARE X
	CALL	FMULT
	POP	HL		;GET CONSTANT POINTER
				;FALL INTO POLY

;	Polynomial evaluator
;	Pointer to Degree+1 is in (HL), it is updated
;	The constants follow the degree
;	Constants should be stored in reverse order, FAC has X
;	We compute:
;	 CO+C1*X+C2*X^2+C3^X3+...+C(N-1)*X^(N-1)+C(N)*X^N
$POLY	CALL	PUSHF		;SAVE X
	LD	A,(HL)		;GET DEGREE
	INC	HL		;INCREMENT POINTER TO FIRST CONSTANT
	CALL	$MOVFM		;MOVE FIRST CONSTANT TO FAC
	DB	06H		;"MVI	B" OVER NEXT BYTE
POLY1	POP	AF		;GET DEGREE
	POP	BC
	POP	DE		;GET X
	DEC	A		;ARE WE DONE?
	RET	Z		;YES, RETURN
	PUSH	DE
	PUSH	BC		;NO, SAVE X
	PUSH	AF		;SAVE DEGREE
	PUSH	HL		;SAVE CONSTANT POINTER
	CALL	FMULT		;EVALUATE THE POLY, MULTIPLY BY X
	POP	HL		;GET LOCATION OF CONSTANTS
	CALL	$MOVRM		;GET CONSTANT
	PUSH	HL		;STORE LOCATION OF CONSTANTS SO FADD AND FMULT
	CALL	FADD		; WILL NOT SCREW THEM UP, ADD IN CONSTANT
	POP	HL		;MOVE CONSTANT POINTER TO NEXT CONSTANT
	JP	POLY1		;SEE IF DONE

;	Pseudo-random number generator
;	If ARG=0, the last random number generated is returned
;	If arg .LT. 0, a new sequence of random numbers is started
;	using the argument
;	To form the next random number in the sequence, we multiply the
;	previous random number by a random constant, and add in another
;	random constant.  Then the HO and LO bytes are switched, the
;	exponent is put where it will be shifted in by NORMAL, and the
;	exponent in the FAC set to 200 so the result will be less than 1.
;	This is then normalized and saved for the next time.
;	The HO and LO bytes were switched so we have a random chance of
;	getting a number less than or greater than .5

;	A COPY OF RNDX TO COPY AT RUN TIME
RNDINI	DB	52H,0C7H,4FH,80H

;	Used by the routine at ISFUN.
RNDMON	CALL	CHRGTR
;	Used by the routine at __RANDOMIZE.
RNDMN2	PUSH	HL		;SAVE TEXT POINTER FOR MONADIC RND
	LD	HL,$SONE	;PRETEND ARG IS 1.0
	CALL	$MOVFM
	CALL	RND		;PICK UP A RANDOM VALUE
	POP	HL		;GET BACK THE TEXT POINTER
	JP	SETSNG

RND	CALL	SIGN		;GET SIGN OF ARG
	LD	HL,SEED+2
	JP	M,RESEED	;START NEW SEQUENCE IF NEGATIVE
	LD	HL,RNDX		;GET LAST NUMBER GENERATED
	CALL	$MOVFM
	LD	HL,SEED+2
	RET	Z		;RETURN LAST NUMBER GENERATED IF ZERO
	ADD	A,(HL)		;GET COUNTER INTO CONSTANTS
	AND	07H		;AND ADD ONE
	LD	B,00H
	LD	(HL),A		;Re-save seed
	INC	HL		;Move to coefficient table
	ADD	A,A		;4 bytes
	ADD	A,A		;per entry
	LD	C,A		;BC = Offset into table
	ADD	HL,BC		;Point to coefficient
	CALL	$MOVRM		;Coefficient to BCDE
	CALL	FMULT		;Multiply FPREG by coefficient
	LD	A,(SEED+1)	;Get (SEED+1)
	INC	A		;Add 1
	AND	03H		;0 to 3
	LD	B,00H
	CP	01H		;Is it zero?
	ADC	A,B		;Yes - Make it 1
	LD	(SEED+1),A	;Re-save seed
	LD	HL,RNDX		;Addition table
	ADD	A,A		;4 bytes
	ADD	A,A		;per entry
	LD	C,A		;BC = Offset into table
	ADD	HL,BC		;Point to value
	CALL	$FADDS		;Add value to FPREG
RND1	CALL	$MOVRF		;SWITCH HO AND LO BYTES,
	LD	A,E		;GET LO
	LD	E,C		;PUT HO IN LO BYTE
	XOR	4FH
	LD	C,A		;PUT LO IN HO BYTE
	LD	(HL),80H	;MAKE RESULT POSITIVE
	DEC	HL		;GET POINTER TO EXPONENT
	LD	B,(HL)		;PUT EXPONENT IN OVERFLOW POSITION
	LD	(HL),80H	;SET EXP SO RESULT WILL BE BETWEEN 0 AND 1
	LD	HL,SEED
	INC	(HL)		;INCREMENT THE PERTUBATION COUNT
	LD	A,(HL)		;SEE IF ITS TIME
	SUB	0ABH
	JP	NZ,RND2
	LD	(HL),A		;ZERO THE COUNTER
	INC	C
	DEC	D
	INC	E

RND2	CALL	NORMAL		;NORMALIZE THE RESULT
	LD	HL,RNDX		;SAVE RANDOM NUMBER GENERATED..
	JP	$MOVMF		;..FOR NEXT TIME

;	Re-seed random numbers
RESEED	LD	(HL),A
	DEC	HL
	LD	(HL),A
	DEC	HL
	LD	(HL),A
	JP	RND1

SEED	DB	00H
	DB	00H
	DB	00H
;	Table used by RND
RNDTAB:
	DB	35H,4AH,0CAH,99H	; -2.65145E+07
	DB	39H,1CH,76H,98H		; 1.61291E+07
	DB	22H,95H,0B3H,98H	; -1.17691E+07
	DB	0AH,0DDH,47H,98H	; 1.30983E+07
	DB	53H,0D1H,99H,99H	; -2-01612E+07
	DB	0AH,1AH,9FH,98H		; -1.04269E+07
	DB	65H,0BCH,0CDH,98H	; -1.34831E+07
	DB	0D6H,77H,3EH,98H	; 1.24825E+07

;	LAST RANDOM NUMBER GENERATED, BETWEEN 0 AND 1
RNDX	DB	52H,0C7H,4FH,80H	; Last random number
	DB	68H,0B1H,46H,68H	; Table used by RND
	DB	99H,0E9H,92H,69H
	DB	10H,0D1H,75H,68H


;-----------------------------------------------------------------------------
;	SINE, COSINE AND TANGENT FUNCTIONS
;
;	Cosine function
;	Idea: use COS(X)=SIN(X+PI/2)
COS	LD	HL,$HLFPI	;ADD PI/2 TO FAC
	CALL	$FADDS
				;END INTFSW
				;FALL INTO SIN

;	Sine function
;	Idea: use identities to get FAC in quadrants I or IV
;	The FAC is divided by 2*PI and the integer part is ignored because
;	SIN(X+2*PI) =SIN(X). Then the argument can be compared with PI/2 by
;	comparing the result of the division with PI/2/(2*PI)=1/4.
;	Identities are then used to get the result in quadrants I or IV.
;	An approximation polynomial is then used to compute SIN(X).
SIN	LD	A,(FAC)		;WILL SEE IF .LT.2^-10
	CP	77H		;AND IF SO SIN(X)=X
	RET	C
;	SIN BY HART #3341
	LD	A,(FACHI)
	OR	A
	JP	P,SIN1
	AND	7FH
	LD	(FACHI),A
	LD	DE,NEG
	PUSH	DE
				;WILL CALCULATE X=FAC/(2*PI)
SIN1	LD	BC,7E22H	; BCDE = FP_EPSILON: 1/(2*PI) =~ 0.159155
	LD	DE,0F983H
	CALL	FMULT
	CALL	PUSHF		;SAVE X
	CALL	FINT		;FAC=INT(X)
	POP	BC
	POP	DE		;FETCH X TO REGISTERS
	CALL	FSUB		;FAC=X-INT(X)
	LD	BC,7F00H	; 0.25
	LD	DE,0000H	;(GET 1/4)
	CALL	FCOMP		;FAC=FAC-1/4
	JP	M,SIN11
	LD	BC,7F80H	; -.025
	LD	DE,0000H	;(-1/4)
	CALL	FADD
	LD	BC,8080H	; -0.5
	LD	DE,0000H	;(-1/2)
	CALL	FADD		;X=X-1/2
	CALL	SIGN
	CALL	P,NEG
	LD	BC,7F00H	; 0.25
	LD	DE,0000H	;(1/4)
	CALL	FADD
	CALL	NEG
SIN11	LD	A,(FACHI)	;MUST REDUCE TO [0,1/4]
	OR	A		;SIGN IN PSW
	PUSH	AF		;SAVE FOR POSSIBLE NEG. AFTER CALC
	JP	P,SIN12
	XOR	80H
	LD	(FACHI),A	;NOW IN [0,1/4]
SIN12	LD	HL,$SINCN	;POINT TO HART COEFFICIENTS
	CALL	$POLYX		;DO POLY EVAL
	POP	AF		;NOW TO DO SIGN
	RET	P		;OK IF POS
	LD	A,(FACHI)	;FETCH SIGN BYTE
	XOR	80H		;MAKE NEG
	LD	(FACHI),A	;REPLACE SIGN
	RET
				;END OF INTFSW COND

	DB	00H,00H,00H,00H

$IN2PI	DB	83H,0F9H,22H,7EH	; 1/(2*PI) 0.159155
;	Constants for SIN, COS
$HLFPI	DB	0DBH,0FH,49H,81H	; 1.5708 (PI/2)
;	(unused)
$QRTER	DB	00H,00H,00H,7FH		; 0.25

;	Hart Algorithm 3341 Constants

;	Note that Hart constants have been scaled by a power of 2.
;	This is due to range reduction as a % of 2*PI rather than PI/2.
;	Would need to multiply argument by 4 but instead we factor this
;	thru the constants.
$SINCN	DB	05H			; Table used by SIN
	DB	0FBH,0D7H,1EH,86H	; 39.711   ->  .1514851E-3
	DB	65H,26H,99H,87H		; -76.575  -> -.4673767E-2
	DB	58H,34H,23H,87H		; 81.602   ->  .7968968E-1
	DB	0E1H,5DH,0A5H,86H	; -41.342  -> -.6459637
	DB	0DBH,0FH,49H,83H	; 6.2832   -> 1.570796

;	Tangent function
;	TAN(X)=SIN(X)/COS(X)
TAN	CALL	PUSHF		;SAVE ARG
	CALL	SIN		;   TAN(X)=SIN(X)/COS(X)
	POP	BC		;GET X OFF STACK
	POP	HL		;PUSHF SMASHES (DE)
	CALL	PUSHF
	EX	DE,HL		;GET LO'S WHERE THEY BELONG
	CALL	$MOVFR
	CALL	COS
	JP	$FDIVS?


;-----------------------------------------------------------------------------
;	ARCTANGENT FUNCTION
;
;	Idea: use identities to get ARG between ø and 1 and then use an
;	approximation polynomial to compute ARCTAN(X)
ATAN	CALL	SIGN		;SEE IF ARG IS NEGATIVE
	CALL	M,PSHNEG	;IF ARG IS NEGATIVE, USE:
	CALL	M,NEG		;   ARCTAN(X)=-ARCTAN(-X)
	LD	A,(FAC)		;SEE IF FAC .GT. 1
	CP	81H
	JP	C,ATAN30
	LD	BC,8100H	;GET THE CONSTANT 1
	LD	D,C
	LD	E,C		;COMPUTE RECIPROCAL TO USE THE IDENTITY:
	CALL	FDIV		;  ARCTAN(X)=PI/2-ARCTAN(1/X)
	LD	HL,$FSUBS	;PUT FSUBS ON THE STACK SO WE WILL RETURN
	PUSH	HL		; TO IT AND SUBTRACT THE REULT FROM PI/2
ATAN30	LD	HL,$ATNCN	;EVALUATE APPROXIMATION POLYNOMIAL
	CALL	$POLYX
	LD	HL,$HLFPI	;GET POINTER TO PI/2 IN CASE WE HAVE TO
	RET			; SUBTRACT THE RESULT FROM PI/2

;	Constants for ATN
$ATNCN	DB	09H
	DB	4AH,0D7H,3BH,78H	; 1/17        ; .002866226
	DB	02H,6EH,84H,7BH		; -1/15       ; -.01616574
	DB	0FEH,0C1H,2FH,7CH	; 1/13        ; .04290961
	DB	74H,31H,9AH,7DH		; -1/11       ; -.07528964
	DB	84H,3DH,5AH,7DH		; 1/9         ; .1065626
	DB	0C8H,7FH,91H,7EH	; -1/7        ; -.142089
	DB	0E4H,0BBH,4CH,7EH	; 1/5         ; .1999355
	DB	6CH,0AAH,0AAH,7FH	; -1/3        ; -.3333315
	DB	00H,00H,00H,81H		; 1/1         ; 1.0

;
;*****************************************************************************
;	END MATHPK
;*****************************************************************************
;
;=============================================================================
;	DIMENSION & VARIABLE SEARCHING - PTRGET
; ## BIPTRG.ASM:19 ##
;
DIMCON	DEC	HL		;SEE IF COMMA ENDED THIS VARIABLE
	CALL	CHRGTR
	RET	Z		;IF TERMINATOR, GOOD BYE
	CALL	SYNCHR
	DB	','		;MUST BE COMMA
;
;	The "DIM" code sets DIMFLG and then falls into the variable
;	search routine. The variable search routine looks at
;	DIMFLG at three different points:
;
;		1) if an entry is found, DIMFLG being on indicates
;			a "doubly dimensioned" variable
;		2) when a new entry is being built DIMFLG's being on
;			indicates the indices should be used for
;			the size of each indice. Otherwise the default
;			of ten is used.
;		3) When the build entry code finishes, only if DIMFLG is
;			off will indexing be done
;
DIM	LD	BC,DIMCON	;PLACE TO COME BACK TO
	PUSH	BC
	DB	0F6H
;
;	Routine to read the variable name at the current text position
;	and put a pointer to its value in [D,E). [H,L] is updated
;	to point to the character after the variable name.
;	VALTYP is setup. Note that evaluating subscripts in
;	a variable name can cause recursive calls to PTRGET so at
;	that point all values must be stored on the stack.
;	On return, [A] does not reflect the value of the terminating character.
;
PTRGET	XOR	A		;MAKE [A]=0
	LD	(DIMFLG),A	;FLAG IT AS SUCH
	LD	C,(HL)		;GET FIRST CHARACTER IN [C]
PTRGT2	CALL	ISLET		;CHECK FOR LETTER
	JP	C,SNERR		;MUST HAVE A LETTER
	XOR	A
	LD	B,A		;ASSUME NO SECOND CHARACTER
	LD	(NAMCNT),A	;ZERO NAMCNT
	INC	HL		;INCREMENT TEXT POINTER
	LD	A,(HL)		;GET CHAR
	CP	'.'		;IS IT A DOT?
	JP	C,NOSEC		;TOO SMALL FOR ANYTHING REASONABLE
	JP	Z,ISSEC		;"." IS VALID VAR CHAR
	CP	':'		;TOO BIG FOR NUMERIC?
	JP	NC,PTRGT3	;YES
	CP	'0'		;IN RIGHT RANGE?
	JP	NC,ISSEC	;YES, WAS NUMERIC
PTRGT3	CALL	ISLET2		;SET CARRY IF NOT ALPHABETIC
	JP	C,NOSEC		;ALLOW ALPHABETICS
ISSEC	LD	B,A		;IT IS A NUMBER--SAVE IN B
	PUSH	BC		;SAVE [B,C]
	LD	B,0FFH		;[B] COUNTS THE CHARACTERS PAST #2
	LD	DE,NAMCNT	;THE PLACE TO PUT THE CHARACTERS
VMORCH	OR	80H		;EXTRA CHARACTERS MUST HAVE THE HIGH BIT ON
				;SO ERASE CAN SCAN BACKWARDS OVER THEM
	INC	B		;INCREASE THE CHACRACTER COUNT
	LD	(DE),A		;AND STORE INTO THE BUFFER
	INC	DE		;AND UPDATE THE BUFFER POINTER
	INC	HL		;INCREMENT TEXT POINTER
	LD	A,(HL)		;GET CHAR
	CP	':'		;TOO BIG?
	JP	NC,VMORC1	;YES
	CP	'0'		;IN RANGE FOR DIGIT
	JP	NC,VMORCH	;YES, VALID CHAR
VMORC1	CALL	ISLET2		;AS ARE ALPHABETICS
	JP	NC,VMORCH
	CP	'.'		;DOTS ALSO OK
	JP	Z,VMORCH	;SO EAT IT
	LD	A,B		;CHECK FOR MAXIMUM COUNT
	CP	NAMLEN-1	;LIMITED TO SIZE OF NAMBUF ONLY
	JP	NC,SNERR	;MUST BE BAD SYNTAX
	POP	BC		;GET BACK THE STORED [B,C]
	LD	(NAMCNT),A	;ALWAYS SET UP COUNT OF EXTRAS
	LD	A,(HL)		;RESTORE TERMINATING CHAR
NOSEC	CP	'%'+1		;NOT A TYPE INDICATOR
	JP	NC,TABTYP	;THEN DONT CHECK THEM
	LD	DE,HAVTYP	;SAVE JUMPS BY USING RETURN ADDRESS
	PUSH	DE
	LD	D,02H		;CHECK FOR INTEGER
	CP	'%'
	RET	Z
	INC	D		;CHECK FOR STRING
	CP	'$'
	RET	Z
	INC	D		;CHECK FOR SINGLE PRECISION
	CP	'!'
	RET	Z
	LD	D,08H		;ASSUME ITS DOUBLE PRECISION
	CP	'#'		;CHECK THE CHARACTER
	RET	Z		;WHEN WE MATCH, SETUP VALTYP
	POP	AF		;POP OFF NON-USED HAVTYP ADDRESS
TABTYP	LD	A,C		;GET THE STARTING CHARACTER
	AND	7FH		;GET RID OF THE USER-DEFINED
				;FUNCTION BIT IN [C]
	LD	E,A		;BUILD A TWO BYTE OFFSET
	LD	D,00H
	PUSH	HL		;SAVE THE TEXT POINTER
	LD	HL,DEFT41	;SEE WHAT THE DEFAULT IS
	ADD	HL,DE
	LD	D,(HL)		;GET THE TYPE OUT OF THE TABLE
	POP	HL		;GET BACK THE TEXT POINTER
	DEC	HL		;NO MARKING CHARACTER
HAVTYP	LD	A,D		;SETUP VALTYP
	LD	(VALTYP),A
	CALL	CHRGTR		;READ PAST TYPE MARKER
	LD	A,(SUBFLG)	;GET FLAG WHETHER TO ALLOW ARRAYS
	DEC	A		;IF SUBFLG=1, "ERASE" HAS CALLED
	JP	Z,ERSFIN	;PTRGET, AND SPECIAL HANDLING MUST BE DONE
	JP	P,NOARYS	;NO ARRAYS ALLOWED
	LD	A,(HL)		;GET CHAR BACK
	SUB	'('		;ARRAY PERHAPS (IF SUBFLG SET NEVER WILL MATCH)
	JP	Z,ISARY		;IT IS!
	SUB	33H		;SEE IF LEFT BRACKET
	JP	Z,ISARY		;IF SO, OK SUBSCRIPT
NOARYS	XOR	A		;ALLOW PARENS AGAIN
	LD	(SUBFLG),A	;SAVE IN FLAG LOCATION
	PUSH	HL		;SAVE THE TEXT POINTER
	LD	A,(NOFUNS)	;ARE FUNCTIONS ACTIVE?
	OR	A
	LD	(PRMFLG),A	;INDICATE IF PARM1 NEEDS SEARCHING
	JP	Z,SNFUNS	;NO FUNCTIONS SO NO SPECIAL SEARCH
	LD	HL,(PRMLEN)	;GET THE SIZE TO SEARCH
	LD	DE,PARM1	;GET THE BASE OF THE SEARCH
	ADD	HL,DE		;[H,L]= PLACE TO STOP SEARCHING
	LD	(ARYTA2),HL	;SET UP STOPPING POINT
	EX	DE,HL		;[H,L]=START [D,E]=END
	JP	LOPFND


;-----------------------------------------------------------------------------
; ## MATH1.ASM:2808 ##
;
;	This is the inner loop of symbol table searching for non-subscripted
;	variables.
;
;	Note 9-Aug-82/MLC - Entry is made at LOPFND, which does a CLD.
;	Want this code to be fast so don't only do this one CLD for the
;	entire piece of code.
LOPTOP	LD	A,(DE)		;GET THE VALTYP OF THIS SIMPLE VARIABLE
	LD	L,A		;SAVE SO WE KNOW HOW MUCH TO SKIP
	INC	DE
	LD	A,(DE)		;[A]=FIRST CHARACTER OF THIS VARIABLE
	INC	DE		;POINT TO 2ND CHAR OF VAR NAME
	CP	C		;SEE IF OUR VARIABLE MATCHES
	JP	NZ,NOTIT1
	LD	A,(VALTYP)	;GET TYPE WERE LOOKING FOR
	CP	L		;COMPARE WITH OUR VALTYP
	JP	NZ,NOTIT1	;NOT RIGHT KIND -- SKIP IT
	LD	A,(DE)		;SEE IF SECOND CHACRACTER MATCHES
	CP	B
	JP	Z,ISIT1		;THAT WAS IT, ALL DONE
				;NO, KEEP LOOKING
NOTIT1	INC	DE		;POINT AT LENGTH OF REST OF VAR CHARS
NOTIT11	LD	A,(DE)		;GET LENGTH OF VAR NAME IN [A]
NOTIT0	LD	H,00H		;[H,L]=NUMBER OF BYTES TO SKIP
	ADD	A,L		;ADD VALTYPE TO LENGTH OF VAR
	INC	A		;PLUS ONE
	LD	L,A		;SAVE IN [L] TO MAKE OFFSET
	ADD	HL,DE		;ADD ON THE POINTER
LOPFND	EX	DE,HL		;START SEARCHING HERE
	LD	A,(ARYTA2)	;LIMIT OF VARIABLE SEARCH
	CP	E		;DONE WITH SYMBOL TABLE SEARCH?
	JP	NZ,LOPTOP	;YES
	LD	A,(ARYTA21)	;ARE HIGH BYTES DIFFERENT
	CP	D		;THE SAME?
	JP	NZ,LOPTOP	;NO, KEEP LOOKING
;-----------------------------------------------------------------------------
; ## BIPTRG.ASM:198 ##
;
	LD	A,(PRMFLG)	;HAS PARM1 BEEN SEARCHED
	OR	A
	JP	Z,SMKVAR	;IF SO, CREATE VARIABLE
	XOR	A		;FLAG PARM1 AS SEARCHED
	LD	(PRMFLG),A
SNFUNS	LD	HL,(ARYTAB)	;STOPPING POINT IS [ARYTA2]
	LD	(ARYTA2),HL
	LD	HL,(VARTAB)	;SET UP STARTING POINT
	JP	LOPFND

;	This entry point is for those callers who want to return
;	from PTRGET without creating a symbol table entry if the
;	variable is not found in the symbol table. PTRGET then returns
;	through VARNOT and returns with [D,E]=0 and [A]=0
PTRGTN	CALL	PTRGET		;CALL PTRGET
PTRGTR	RET			;DONT CHANGE THIS SEQUENCE AS RETURN
				;ADDRESS IS CHECKED FOR

;	This is exit for VARPTR and others
VARNOT	LD	D,A		;ZERO [D,E]
	LD	E,A
	POP	BC		;GET RID OF PUSHED [D,E]
	EX	(SP),HL		;PUT RETURN ADDRESS BACK ON STACK
	RET			;RETURN FROM PTRGET

SMKVAR	POP	HL		;[H,L]= TEXT POINTER
	EX	(SP),HL		;[H,L]= RETURN ADDRESS
	PUSH	DE		;SAVE CURRENT VARIABLE TABLE POSITION
	LD	DE,PTRGTR	;ARE WE RETURNING TO PTRGTN?
	CALL	COMPAR		;COMPARE
	JP	Z,VARNOT	;YES.
	LD	DE,RETVAR	;DID EVAL CALL US?
	CALL	COMPAR		;IF SO, DON'T MAKE A NEW VARIABLE
	POP	DE		;RESTORE THE POSITION
	JP	Z,FINZER	;MAKE FAC ZERO (ALL TYPES) AND SKIP RETURN
				;XTHL
	EX	(SP),HL		;PUT RETURN ADDRESS BACK
	PUSH	HL		;PUT THE TEXT POINTER BACK
	PUSH	BC		;SAVE THE LOOKS
	LD	A,(VALTYP)	;GET LENGTH OF SYMBOL TABLE ENTRY
	LD	B,A		;[B]=VALTYP
	LD	A,(NAMCNT)	;INCLUDE EXTRA CHARACTERS IN SIZE
	ADD	A,B
	INC	A		;AS WELL AS THE EXTRA CHARACTER COUNT
	LD	C,A		;[B,C]=LENGTH OF THIS VARIABLE
	PUSH	BC		;SAVE THE VALTYP ON THE STACK
	LD	B,00H		;[B]=0
	INC	BC		;MAKE THE LENGTH INCLUDE
				;THE LOOKS TOO
	INC	BC
	INC	BC
	LD	HL,(STREND)	;THE CURRENT END OF STORAGE
	PUSH	HL		;SAVE THIS #
	ADD	HL,BC		;ADD ON THE AMOUNT OF SPACE
				;EXTRA NOW BEING USED
	POP	BC		;POP OFF HIGH ADDRESS TO MOVE
	PUSH	HL		;SAVE NEW CANDIDATE FOR STREND
	CALL	BLTU		;BLOCK TRANSFER AND MAKE SURE
				;WE ARE NOT OVERFLOWING THE
				;STACK SPACE
	POP	HL		;[H,L]=NEW STREND
	LD	(STREND),HL	;STORE SINCE WAS OK
				;THERE WAS ROOM, AND BLOCK TRANSFER
				;WAS DONE, SO UPDATE POINTERS
	LD	H,B		;GET BACK [H,L] POINTING AT THE END
	LD	L,C		;OF THE NEW VARIABLE
	LD	(ARYTAB),HL	;UPDATE THE ARRAY TABLE POINTER
ZEROER	DEC	HL		;[H,L] IS RETURNED POINTING TO THE
	LD	(HL),00H	;END OF THE VARIABLE SO WE
	CALL	COMPAR		;ZERO BACKWARDS TO [D,E] WHICH
	JP	NZ,ZEROER	;POINTS TO THE START OF THE VARIABLE
	POP	DE		;[E]=VALTYP
	LD	(HL),D		;VALTYP IS IN HIGH ORDER
	INC	HL
	POP	DE
	LD	(HL),E
	INC	HL
	LD	(HL),D		;PUT DESCRIPTION OF THIS VARIABLE
				;INTO MEMORY
	CALL	NPUTSB		;SAVE THE EXTRA CHARACTERS IN THE NAME
	EX	DE,HL		;POINTER AT VARIABLE INTO [D,E]
	INC	DE		;POINT AT THE VALUE
	POP	HL		;RESTORE THE TEXT POINTER
	RET

;	aka FINPTR
ISIT1	INC	DE		;POINT AT THE EXTRA CHARACTER COUNT
	LD	A,(NAMCNT)	;SEE IF THE EXTRA COUNTS MATCH
	LD	H,A		;SAVE LENGTH OF NEW VAR
	LD	A,(DE)		;GET LENGTH OF CURRENT VAR
	CP	H		;ARE THEY THE SAME?
	JP	NZ,NOTIT11	;SKIP EXTRAS AND CONTINUE SEARCH
	OR	A		;LENGTH ZERO?
	JP	NZ,ISIT11	;NO, MORE CHARS TO LOOK AT
	INC	DE		;POINT TO VALUE OF VAR
	POP	HL		;RESTORE TEXT POINTER
	RET			;ALL DONE WITH THIS VAR

;	aka NFTPRT
ISIT11	EX	DE,HL
	CALL	MATSUB		;SEE IF THE CHARACTERS MATCH
	EX	DE,HL		;TABLE POINTER BACK INTO [D,E]
	JP	NZ,NOTIT0	;IF NOT, CONTINUE SEARCH
	POP	HL		;GET BACK THE TEXT POINTER
	RET

;	Make all types zero and skip return
FINZER	LD	(FAC),A		;MAKE SINGLES AND DOUBLES ZERO
	LD	H,A		;MAKE INTEGERS ZERO
	LD	L,A
	LD	(FACLO),HL
	CALL	GETYPR		;SEE IF ITS A STRING
	JP	NZ,POPHR2	;IF NOT, DONE
	LD	HL,DSEGZ	;ZERO
	LD	(FACLO),HL	;POINTING AT A ZERO
POPHR2	POP	HL		;GET THE TEXT POINTER
	RET			;RETURN FROM EVAL


;-----------------------------------------------------------------------------
;	MULTIPLE DIMENSION CODE
; ## BIPTRG.ASM:308 ##
;
;	Format of arrays in core
;
;	Descriptor
;		Low byte = second charcter (200 bit is string flag)
;		High byte = first character
;	Length of array in core in bytes (does not include descriptor)
;	Number of dimensions 1 byte
;	For each dimension starting with the first a list
;	(2 bytes each) of the max indice+1
;	The values
;
ISARY	PUSH	HL		;SAVE DIMFLG AND VALTYP FOR RECURSION
	LD	HL,(DIMFLG)
	EX	(SP),HL		;TEXT POINTER BACK INTO [H,L]
	LD	D,A		;SET # DIMENSIONS =0
INDLOP	PUSH	DE		;SAVE NUMBER OF DIMENSIONS
	PUSH	BC		;SAVE LOOKS
	LD	DE,NAMCNT	;POINT AT THE AREA TO SAVE
	LD	A,(DE)		;GET LENGTH
	OR	A		;IS IT ZERO?
	JP	Z,SHTNAM	;YES, SHORT NAME
	EX	DE,HL		;SAVE THE TEXT POINTER IN [D,E]
	ADD	A,02H		;WE WANT SMALLEST INT .GE.(NAMCNT+1)/2
	RRA
	LD	C,A		;SEE IF THERE IS ROOM TO SAVE THIS STUFF
	CALL	GETSTK
	LD	A,C		;RESTORE COUNT OF PUSHES
LPPSNM	LD	C,(HL)		;GET VALUES TO PUSH
	INC	HL
	LD	B,(HL)
	INC	HL
	PUSH	BC		;AND DO THE SAVE
	DEC	A		;[A] TIMES
	JP	NZ,LPPSNM
	PUSH	HL		;SAVE THE ADDRESS TO STORE TO
	LD	A,(NAMCNT)	;SAVE THE NUMBER OF BYTES FOR A COUNT
	PUSH	AF
	EX	DE,HL		;RESTORE THE TEXT POINTER
	CALL	INTIDX		;EVALUATE INDICE INTO [D,E]
	POP	AF		;COUNT TELLING HOW MUCH TO RESTORE
	LD	(NAMTMP),HL	;SAVE THE TEXT POINTER
	POP	HL		;THE PLACE TO RESTORE TO
	ADD	A,02H		;CALCULATE BYTE POPS AGAIN
	RRA
LPLNAM	POP	BC
	DEC	HL
	LD	(HL),B
	DEC	HL
	LD	(HL),C
	DEC	A		;LOOP [A] TIMES POPING NAME BACK INTO NAMBUF
	JP	NZ,LPLNAM
	LD	HL,(NAMTMP)
	JP	LNGNAM		;WAS LONG ONE

SHTNAM	CALL	INTIDX		;EVALUATE IT
	XOR	A		;MAKE SURE NAMCNT=0
	LD	(NAMCNT),A
LNGNAM	LD	A,(OPTVAL)	;SEE WHAT THE OPTION BASE IS
	OR	A
	JP	Z,OPTB0		;IF BASE 0 DO NOTHING
	LD	A,D		;CHECK FOR 0 SUBSCRIPT
	OR	E		;WHICH IS ILLEGAL IN BASE 1
	JP	Z,BSERR
	DEC	DE		;ADJUST SUBSCRIPT
OPTB0	POP	BC		;POP OFF THE LOOKS
	POP	AF		; POP PSW
				;[A] = NUMBER OF DIMENSIONS SO FAR
	EX	DE,HL		;[D,E]=TEXT POINTER
				;[H,L]=INDICE
				;XTHL
	EX	(SP),HL		;PUT THE INDICE ON THE STACK
				;[H,L]=VALTYP & DIMFLG
	PUSH	HL		;RESAVE VALTYP AND DIMFLG
	EX	DE,HL		;[H,L]=TEXT POINTER
	INC	A		;INCREMENT # OF DIMENSIONS
	LD	D,A		;[D]=NUMBER OF DIMENSIONS
	LD	A,(HL)		;GET TERMINATING CHARACTER
	CP	','		;A COMMA SO MORE INDICES FOLLOW?
	JP	Z,INDLOP	;IF SO, READ MORE
	CP	')'		;EXPECTED TERMINATOR?
	JP	Z,DOCHRT	;DO CHRGET FOR NEXT ONE
	CP	']'		;BRACKET?
	JP	NZ,SNERR	;NO, GIVE ERROR
DOCHRT	CALL	CHRGTR
	LD	(TEMP2),HL	;SAVE THE TEXT POINTER
	POP	HL		;[H,L]= VALTYP & DIMFLG
	LD	(DIMFLG),HL	;SAVE VALTYP AND DIMFLG
	LD	E,00H		;WHEN [D,E] IS POPED INTO PSW, WE
				;DON'T WANT THE ZERO FLAG TO BE SET, SO
				;"ERASE" WILL HAVE A UNIQUE CONDITION
	PUSH	DE		;SAVE NUMBER OF DIMENSIONS
	DB	11H		;"LD DE,nn", OVER THE NEXT TWO BYTES
ERSFIN	PUSH	HL		;SAVE THE TEXT POINTER
	PUSH	AF		;SAVE A DUMMY NUMBER OF DIMENSIONS
				; WITH THE ZERO FLAG SET
;
;	At this point [B,C]=Looks. The text pointer is in TEMP2.
;	The indices are all on the stack, followed by the number of dimensions.
;
	LD	HL,(ARYTAB)	;[H,L]=PLACE TO START THE SEARCH
	DB	3EH


;-----------------------------------------------------------------------------
; ## MATH1.ASM:2863 ##
;
;	This is the corresponding code for array searches
;	Note 9-Aug-82/MLC - This code is supposed to be fast so only a
;	single CLD is done here for the entire piece of code.
;	aka FNDARY
LOPFD1	ADD	HL,DE		;SKIP OVER THIS ARRAY SINCE
				; IT'S NOT THE ONE
	EX	DE,HL
	LD	HL,(STREND)	;GET THE PLACE TO STOP INTO [D,E]
	EX	DE,HL
	CALL	COMPAR		;STOPPING TIME?
	JP	Z,NOTFDD	;YES, COULDN'T FIND THIS ARRAY
	LD	E,(HL)		;GET VALTYP IN [E]
	INC	HL
	LD	A,(HL)		;GET FIRST CHARACTER
	INC	HL
	CP	C		;SEE IF IT MATCHES
	JP	NZ,NMARY1	;NOT THIS ONE
	LD	A,(VALTYP)	;GET TYPE OF VAR WERE LOOKING FOR
	CP	E		;SAME AS THIS ONE?
	JP	NZ,NMARY1	;NO, SKIP THIS VAR
	LD	A,(HL)		;GET SECOND CHARACTER
	CP	B		;ANOTHER MATCH?
	JP	Z,ISARY1	;MATCH, CHECK OUT REST OF NAME
NMARY1	INC	HL		;POINT TO LENGTH OF VAR
NMARY2	LD	E,(HL)		;GET VAR NAME LENGTH IN [E]
	INC	E		;ADD ONE TO GET CORRECT LENGTH
	LD	D,00H		;HIGH BYTE OF ZERO
	ADD	HL,DE		;ADD OFFSET
CNOMAT	LD	E,(HL)		;[D,E]=LENGTH
	INC	HL		;OF THE ARRAY BEING LOOKED AT
	LD	D,(HL)
	INC	HL
	JP	NZ,LOPFD1	;IF NO MATCH, SKIP THIS ONE AND TRY AGAIN
	LD	A,(DIMFLG)	;SEE IF CALLED BY "DIM"
	OR	A		;ZERO MEANS NO
	JP	NZ,DDERR	;PRESERVE [D,E], AND DISPATCH TO
				;"REDIMENSIONED VARIABLE" ERROR
				;IF ITS "DIM" CALLING PTRGET
;-----------------------------------------------------------------------------
; ## BIPTRG.ASM:445 ##
;
;	TEMP2=THE TEXT POINTER
;	WE HAVE LOCATED THE VARIABLE WE WERE LOOKING FOR
;	AT THIS POINT [H,L] POINTS BEYOND THE SIZE TO THE NUMBER OF DIMENSIONS
;	THE INDICES ARE ON THE STACK FOLLOWED BY THE NUMBER OF DIMENSIONS
;
	POP	AF		;[A]=NUMBER OF DIMENSIONS
	LD	B,H		;SET [B,C] TO POINT AT NUMBER OF DIMENSIONS
	LD	C,L
	JP	Z,POPHRT	;"ERASE" IS DONE AT THIS POINT, SO RETURN
	SUB	(HL)		;MAKE SURE THE NUMBER GIVEN NOW AND
				;AND WHEN THE ARRAY WAS SET UP ARE THE
				;SAME
	JP	Z,GETDEF	;JUMP OFF AND READ
				;THE INDICES....
BSERR	LD	DE,ERRBS	;"SUBSCRIPT OUT OF RANGE"
	JP	ERROR

;	=local ISARY in MATH1.SRC
;	aka CMPNAM
ISARY1	INC	HL		;POINT TO LENGTH OF NAME
	LD	A,(NAMCNT)	;SEE IF COUNT MATCHES COUNT IN COMPLEX TABLE
	CP	(HL)
	JP	NZ,NMARY2	;BAD NAME SIZE JUST SKIP AND SET NZ CC
	INC	HL		;POINT ONE BYTE AFTER LENGTH FIELD
	OR	A		;LENGTH ZERO?
	JP	Z,CNOMAT	;THEN FOUND, EXIT
	DEC	HL		;MOVE BACK ONE
	CALL	MATSUB		;OTHERWISE TRY TO MATCH CHARACTERS
	JP	CNOMAT		;USING COMMON SUBROUTINE


;-----------------------------------------------------------------------------
; ## BIPTRG.ASM:470 ##
;
;	Here when variable is not found in the array table
;
;	Building an entry:
;
;		Put down the descriptor
;		Setup numer of dimensions
;		Make sure there is room for the new entry
;		Remember VARPTR
;		TALLY=4 (VALTYP for the extended)
;		Skip 2 locs for later fill in -- the size
;	LOOP: 	Get an indice
;		Put number +1 down at VARPTR and increment VARPTR
;		TALLY = TALLY * NUMBER+1
;		Decrement NUMBER-DIMS
;		JNZ	LOOP
;		CALL 	REASON with [H,L] reflecting last loc of variable
;		Update STREND
;		Zero backwards
;		Make TALLY include MAXDIMS
;		Put down TALLY
;		If called by dimension, return
;		Otherwise index into the variable as if it
;		were found on the initial search
NOTFDD	LD	A,(VALTYP)	;GET VALTYP OF NEW VAR
	LD	(HL),A		;PUT DOWN THE VARIABLE TYPE
	INC	HL
	LD	E,A
	LD	D,00H		;[D,E]=SIZE OF ONE VALUE (VALTYP)
	POP	AF		;[A]=NUMBER OF DIMENSIONS
	JP	Z,PTRRNZ	;CALLED BY CHAIN, JUST RETURN NON-ZERO
	LD	(HL),C		;PUT DOWN THE DESCRIPTOR
	INC	HL
	LD	(HL),B
	CALL	NPUTSB		;STORE THE EXTRA CHARACTERS IN THE TABLE
	INC	HL
	LD	C,A		;[C]=NUMBER OF TWO BYTE ENTRIES NEEDED
				; TO STORE THE SIZE OF EACH DIMENSION
	CALL	GETSTK		;GET SPACE FOR DIMENSION ENTRIES
	INC	HL		;SKIP OVER THE SIZE LOCATIONS
	INC	HL
	LD	(TEMP3),HL	;SAVE THE LOCATION TO PUT THE SIZE
				; IN -- POINTS AT THE NUMBER OF DIMENSIONS
	LD	(HL),C		;STORE THE NUMBER OF DIMENSIONS
	INC	HL
	LD	A,(DIMFLG)	;CALLED BY DIMENSION?
	RLA			;SET CARRY IF SO
	LD	A,C		;[A]=NUMBER OF DIMENSIONS
LOPPTA	JP	C,POPDIM
	PUSH	AF
	LD	A,(OPTVAL)	;GET THE OPTION BASE
	XOR	0BH		;MAP 0 TO 11 AND 1 TO 10
	LD	C,A		;[B,C]=DEFAULT DIMENSION
	LD	B,00H
	POP	AF
	JP	NC,NOTDIM	;DEFAULT DIMENSIONS TO TEN
POPDIM	POP	BC		;POP OFF AN INDICE INTO [B,C]
	INC	BC		;ADD ONE TO IT FOR THE ZERO ENTRY
NOTDIM	LD	(HL),C		;PUT THE MAXIMUM DOWN
	PUSH	AF		;SAVE THE NUMBER OF DIMENSIONS AND
				;DIMFLG (CARRY)
	INC	HL
	LD	(HL),B
	INC	HL
	CALL	UMULT		;MULTIPLY [B,C]=NEWMAX BY CURTOL=[D,E]
	POP	AF		;GET THE NUMBER OF DIMENSIONS AND
				;DIMFLG (CARRY) BACK
	DEC	A		;DECREMENT THE NUMBER OF DIMENSIONS LEFT
	JP	NZ,LOPPTA	;HANDLE THE OTHER INDICES
	PUSH	AF		;SAVE DIMFLG (CARRY)
	LD	B,D		;[B,C]=SIZE
	LD	C,E
	EX	DE,HL		;[D,E]=START OF VALUES
	ADD	HL,DE		;[H,L]=END OF VALUES
	JP	C,OMERR		;OUT OF MEMORY POINTER BEING GENERATED?
	CALL	REASON		;SEE IF THERE IS ROOM FOR THE VALUES
	LD	(STREND),HL	;UPDATE THE END OF STORAGE
ZERITA	DEC	HL		;ZERO THE NEW ARRAY
	LD	(HL),00H
	CALL	COMPAR		;BACK AT THE BEGINNING?
	JP	NZ,ZERITA	;NO, ZERO MORE
				;MAKE SURE [AL]=0
	INC	BC		;ADD ONE TO THE SIZE TO INCLUDE
				;THE BYTE FOR THE NUMBER OF DIMENSIONS
	LD	D,A		;[D]=ZERO
	LD	HL,(TEMP3)	;GET A POINTER AT THE NUMBER OF DIMENSIONS
	LD	E,(HL)		;[E]=NUMBER OF DIMENSIONS
	EX	DE,HL		;[H,L]=NUMBER OF DIMENSIONS
	ADD	HL,HL		;[H,L]=NUMBER OF DIMENSIONS TIMES TWO
	ADD	HL,BC		;ADD ON THE SIZE
				;TO GET THE TOTAL NUMBER OF BYTES USED
	EX	DE,HL		;[D,E]=TOTAL SIZE
	DEC	HL		;BACK UP TO POINT TO LOCATION TO PUT
	DEC	HL		;THE SIZE OF THE ARRAY IN BYTES IN.
	LD	(HL),E
	INC	HL
	LD	(HL),D
	INC	HL		;PUT DOWN THE SIZE
	POP	AF		;GET BACK DIMFLG (CARRY) AND SET [A]=0
	JP	C,FINNOW

;	At this point [H,L] points beyond the size to the number of dimensions
;
;	Strategy:
;		NUMDIM=number of dimensions
;		CURTOL=0
;	INLPNM:	Get a new INDICE
;		Pop new MAX into CURMAX
;		Make sure INDICE is not too big
;		Mutliply CURTOL by CURMAX
;		Add INDICE to CURTOL
;		NUMDIM=NUMDIM-1
;		JNZ	INLPNM
;		Use CURTOL*4 (VALTYP for extended) as offset
GETDEF	LD	B,A		;[B,C]=CURTOL=ZERO
	LD	C,A
	LD	A,(HL)		;[A]=NUMBER OF DIMENSIONS
	INC	HL		;POINT PAST THE NUMBER OF DIMENSIONS
	DB	16H		;"MVI D," AROUND THE NEXT BYTE
INLPNM	POP	HL		;[H,L]= POINTER INTO VARIABLE ENTRY
	LD	E,(HL)		;[D,E]=MAXIMUM FOR THE CURRENT INDICE
	INC	HL
	LD	D,(HL)
	INC	HL
	EX	(SP),HL		;[H,L]=CURRENT INDICE
				;POINTER INTO THE VARIABLE GOES ON THE STACK
	PUSH	AF		;SAVE THE NUMBER OF DIMENSIONS
	CALL	COMPAR		;SEE IF THE CURRENT INDICE IS TOO BIG
	JP	NC,BSERR	;IF SO "BAD SUBSCRIPT" ERROR
	CALL	UMULT		;CURTOL=CURTOL*CURRENT MAXIMUM
	ADD	HL,DE		;ADD THE INDICE TO CURTOL
	POP	AF		;GET THE NUMBER OF DIMENSIONS IN [A]
	DEC	A		;SEE IF ALL THE INDICES HAVE BEEN PROCESSED
	LD	B,H		;[B,C]=CURTOL IN CASE WE LOOP BACK
	LD	C,L
	JP	NZ,INLPNM	;PROCESS THE REST OF THE INDICES
	LD	A,(VALTYP)	;SEE HOW BIG THE VALUES ARE
				;AND MULTIPLY BY THAT SIZE
	LD	B,H		;SAVE THE ORIGINAL VALUE FOR MULTIPLYING
	LD	C,L		;BY THREE
	ADD	HL,HL		;MULTIPLY BY TWO AT LEAST
	SUB	04H		;FOR INTEGERS AND STRINGS
				;NO MORE MULTIPLYING BY TWO
	JP	C,SMLVAL
	ADD	HL,HL		;NOW MULTIPLIED BY FOUR
	JP	Z,DONMUL	;RE-GEN CONDITION CODES
	ADD	HL,HL		;IF SINGLE ALL DONE
				;BY EIGHT FOR DOUBLES
SMLVAL	OR	A		;FIX CC'S FOR Z-80
	JP	PO,DONMUL	;FOR STRINGS
	ADD	HL,BC		;ADD IN THE ORIGINAL
DONMUL	POP	BC		;POP OFF THE ADDRESS OF WHERE THE VALUES
				;BEGIN
	ADD	HL,BC		;ADD IT ONTO CURTOL TO GET THE
				;PLACE THE VALUE IS STORED
	EX	DE,HL		;RETURN THE POINTER IN [D,E]
FINNOW	LD	HL,(TEMP2)	;REGET THE TEXT POINTER
	RET

PTRRNZ	SCF			;RETURN WITH NON-ZERO IN [A]
	SBC	A,A		;AND CONDITION CODES SET
	POP	HL		;RESTORE TEST POINTER
	RET

;	Long variable name subroutines. After the normal 2 character name
;	the count of additional characters is stored. Following this
;	comes the characters in order with the high bit turned on so a backward
;	scan is possible
IADAHL	LD	A,(HL)		;GET THE CHARACTER COUNT
	INC	HL
ADDAHL	PUSH	BC		;ADD [A] TO [H,L]
	LD	B,00H
	LD	C,A
	ADD	HL,BC
	POP	BC		;RESTORE THE SAVED [B,C]
	RET

;	This routine stores the "long" name at [H,L]
NPUTSB	PUSH	BC		;THIS ROUTINE STORE THE "LONG" NAME AT [H,L]
	PUSH	DE
	PUSH	AF
	LD	DE,NAMCNT	;POINT AT DATA TO SAVE
	LD	A,(DE)		;GET THE COUNT
	LD	B,A
	INC	B		;[B]= NUMBER OF BYTES TO SAVE
SLPLNG	LD	A,(DE)		;FETCH STORE VALUE
	INC	DE
	INC	HL		;MOVE UP TO STORE NAME INTO TABLE
	LD	(HL),A		;DO THE STORE
	DEC	B		;AND REPEAT [B] TIMES
	JP	NZ,SLPLNG	;FOR THE COUNT AND DATA
	POP	AF
	POP	DE
	POP	BC
	RET

;-----------------------------------------------------------------------------
;	Compare two var names
;	This routine tries to perform a match
MATSUB	PUSH	DE
	PUSH	BC
	LD	DE,NAMBUF	;POINT AT COUNT AND DATA
	LD	B,A		;[B]=CHARACTER COUNT
	INC	HL		;POINT AT THE DATA
	INC	B		;START OFF LOOP
SLPMAT	DEC	B		;MATCHED ALL CHARACTERS YET?
	JP	Z,ISMAT2	;IF SO, ITS A MATCH
	LD	A,(DE)		;GET ANOTHER CHARACTER
	INC	DE		;SEE IF ITS THE SAME
	CP	(HL)		;MOVE FORWARD IN DEFINITION TABLE
	INC	HL		;MORE FORWARD IN STORED NAME
	JP	Z,SLPMAT	;IF MATCH KEEP GOING UNTIL END
	LD	A,B		;NEED TO ADVANCE BY [B]-1 TO SKIP BAD CHARS
	DEC	A
	CALL	NZ,ADDAHL	;USE THE COMMON SUBROUTINE. [H,L]=[H,L]+[A]
	XOR	A		;SET CC'S NON ZERO FOR NO MATCH
	DEC	A		;AND RETURN [A]=FF
ISMAT2	POP	BC		;RESTORE SAVED REGISTERS
	POP	DE
	RET


;=============================================================================
;	EDIT CODE
; ## SCNEDT.ASM: ##
;
;	AUTOMATIC EDIT FOR ERRORS
ERREDT	LD	(ERRFLG),A
	LD	HL,(ERRLIN)
	OR	H
	AND	L
	INC	A
	EX	DE,HL
	RET	Z
	JP	EREDIT

;	EDIT COMMAND
;	(CP/M implementation)
EDIT	CALL	LINSPC		;GET THE ARGUMENT LINE NUMBER
	RET	NZ		;ERROR IF NOT END OF LINE
;	Enter EDIT mode after an SN error
EREDIT	POP	HL		;GET RID OF NEWSTT ADDRESS
;	Edit again (reload the line)
EDTAGN	EX	DE,HL
	LD	(DOT),HL	;SAVE CURRENT LINE IN DOT FOR LATER EDIT OR LIST
	EX	DE,HL
	CALL	FNDLIN		;FIND THE LINE IN QUESTION
	JP	NC,USERR	;IF NOT FOUND, UNDEFINED STATEMENT ERROR.
	LD	H,B		;PONTER TO LINE IS IN [B,C]
	LD	L,C		;TRANSFER IT TO [H,L]
	INC	HL		;PASS OVER POINTER TO NEXT LINE
	INC	HL		;LIKE SO.
	LD	C,(HL)		;GET FIRST BYTE OF LINE #
	INC	HL		;MOVE TO 2ND BYTE
	LD	B,(HL)		;PICK IT UP INTO B
	INC	HL		;ADVANCE TO POINT TO FIRST BYTE OF LINE
	PUSH	BC		;SAVE LINE # ON STACK
	CALL	BUFLIN		;UNPACK LINE INTO BUF

;	Print line in buffer
EDTLIN	POP	HL		;GET BACK LINE #
;	To enter Edit Mode on the line you are currently
;	typing, type Control-A. MBASIC responds with CR,
;	'!', and a space. The cursor will be positioned at
;	the first character in the line.
EDTLI1	PUSH	HL		;SAVE IT BACK ON STACK
	LD	A,H		;TEST FOR DOUBLE BYTE ZERO
	AND	L
	INC	A
	LD	A,'!'		;GET PROMPT FOR DIRECT EDIT
	CALL	Z,OUTDO		;SEND IT
	CALL	NZ,LINPRT	;PRINT LINE # IF NOT INLIN EDIT
	LD	A,' '		;TYPE A SPACE
	CALL	OUTDO
	LD	HL,BUF		;GET START OF BUF IN [H,L]
	PUSH	HL		;SAVE [H,L] WHILE WE CALC LINE LENGTH
	LD	C,0FFH		;ASSUME 0 CHAR LINE
;	Count chars in buffer -> C
EDTCNT	INC	C		;BUMP COUNT OF CHARS
	LD	A,(HL)		;GET CHAR FROM LINE
	INC	HL		;BUMP POINTER
	OR	A
	JP	NZ,EDTCNT	;IF NOT ZERO (END OF LINE) KEEP COUNTING...
	POP	HL		;GET BACK POINTER TO LINE
	LD	B,A		;SET CURRENT LINE POSIT TO ZERO
;	Get command with repeat count (main loop)
;	Optional number of repetitions of the next subcommand.
EDTCMD	LD	D,00H		;ASSUME REPITION COUNT IS ZERO
;	Wait command keystroke
EDTWTC	CALL	SVCKBD		;GET A CHAR FROM USER
	OR	A		;IGNORE NULLS
	JP	Z,EDTWTC
	CALL	MAKUPS		;MAKE UPPER CASE COMMAND
	SUB	30H		; convert from ASCII
	JP	C,EDTDOC	;GET RID OF OFFSET
	CP	0AH		;...
	JP	NC,EDTDOC
	LD	E,A		;SAVE CHAR
	LD	A,D		;GET ACCUM REPITITION
	RLCA			;MULTIPLY BY 2
	RLCA			;BY 4
	ADD	A,D		;AND ADD TO GET 5*D
	RLCA			;*2 TO GET 10*D
	ADD	A,E		;ADD DIGIT
	LD	D,A		;SAVE BACK NEW ACCUM
	JP	EDTWTC		;GET NEXT CHAR

;	Proceed by typing an Edit Mode subcommand.
EDTDOC	PUSH	HL		;SAVE TEXT POINTER
	LD	HL,EDTCMD	;PUT RETURN ADDRESS TO DISPED
	EX	(SP),HL		;ON THE STACK
	DEC	D		;SEE IF D=0 (REP FACTOR)
	INC	D		;SET CONDITION CODES
	JP	NZ,EDTDO1	;BRANCH AROUND
	INC	D		;MAKE IT 1
;	D has the command repeat count
EDTDO1	CP	08H-'0'		; BKS:  $08 - '0' = $D8
	JP	Z,EDTBKS	;BACK: Do a backspace
	CP	7FH-'0'		; DEL:  $7F - '0' = $4F
	JP	Z,EDTBKS1	;DEL: Do a backspace
	CP	0DH-'0'		; CR:   $0D - '0' = $DD
	JP	Z,EDTENT	;ENTER: Display the line and validate the changes
	CP	' '-'0'
	JP	Z,EDTSPC	;SPACE: display next char and advance cursor
	CP	'a'-'0'		;COMMAND IN LOWER CASE?
	JP	C,EDTNLC	;Jump if not an LC letter
	SUB	20H		;TO UPPERCASE
;	Jump if not an LC letter
EDTNLC	CP	'Q'-'0'		;'Q': QUIT?
	JP	Z,EDTQQ		;IF SO, QUIT & PRINT "OK" OR RETURN TO INLIN
	CP	'L'-'0'
	JP	Z,EDTLL		;BRANCH
	CP	'S'-'0'
	JP	Z,EDTSS		;SEARCH
	CP	'I'-'0'
	JP	Z,EDTII		;INSERT
	CP	'D'-'0'
	JP	Z,EDTDD		;DELETE
	CP	'C'-'0'
	JP	Z,EDTCC		;CHANGE
	CP	'E'-'0'		;END?
	JP	Z,EDTEE		;(SAME AS <CR> BUT DOESNT PRINT REST)
	CP	'X'-'0'		;EXTEND?
	JP	Z,EDTXX		;GO TO END OF LINE & INSERT
	CP	'K'-'0'		;KILL??
	JP	Z,EDTKK		;(SAME AS "S" BUT DELETES CHARS)
	CP	'H'-'0'		;HACK??
	JP	Z,EDTHH		;HACK OFF THE REST OF THE LINE & INSERT
	CP	'A'-'0'		;AGAIN??
	LD	A,07H
	JP	NZ,OUTDO	; Ring a bell if unknown command

;	The A subcommand lets you begin editing a line over again.
;	It restores the original line and repositions the cursor at the beginning.
	POP	BC		;DISPI RETURN ADDRESS
	POP	DE		;LINE NUMBER INTO [D,E]
	CALL	CRDO
	JP	EDTAGN		;RESTART EDITING

;	SPACE: display next char and advance cursor
EDTSPC	LD	A,(HL)		;GET CHAR FROM CURENT POSIT
	OR	A		;ARE WE AT END OF LINE?
	RET	Z		;IF SO, RETURN
	INC	B		;BUMP CURRENT POSITION
	CALL	OUTCHR		;TYPE CHARACTER
	INC	HL		;MOVE POINTER TO NEXT CHAR
	DEC	D		;TEST IF DONE WITH REPITITIONS
	JP	NZ,EDTSPC	;REPEAT
	RET			;RETURN TO DISPATCHER

;	KILL: delete until char
;	The subcommand [i]K<ch> is similar to [i]S<ch>, except all the characters
;	passed over in the search are deleted. The cursor is positioned before <ch>,
;	and the deleted characters are enclosed in backslashes
EDTKK	PUSH	HL		;SAVE CURRENT CHAR POSIT
	LD	HL,EDTBSL	;TYPE SLASH WHEN DONE
	EX	(SP),HL		;PUT IT ON STACK & GET POSIT BACK
	SCF

;	SEARCH: seek char
EDTSS	PUSH	AF		;SAVE CONDITION CODES
	CALL	SVCKBD		;GET SEARCH CHAR
	LD	E,A		;SAVE IT
	POP	AF
	PUSH	AF
	CALL	C,EDTBSL	;TYPE BEGINNING SLASH FOR "K"
;	Search char in E
EDTSCH	LD	A,(HL)
	OR	A
	JP	Z,EDTPPW
	CALL	OUTCHR		;TYPE THE CHAR
	POP	AF		;GET KILL FLAG
	PUSH	AF		;SAVE BACK
	CALL	C,EDTIDL	;DELETE THE CHAR IF K COMMAND.
	JP	C,EDTSSK	;AND DONT MOVE POINTER AS DELCHR ALREADY DID
	INC	HL
	INC	B		;INCREMENT LINE POSIT

EDTSSK	LD	A,(HL)		;ARE WE AT END ?
	CP	E		;ARE CURRENT CHAR & SEARCH
	JP	NZ,EDTSCH	;CHAR THE SAME? IF NOT, LOOK MORE
	DEC	D		;LOOK FOR N MATCHES
	JP	NZ,EDTSCH	;IF NOT 0, KEEP LOOKING

EDTPPW	POP	AF		;GET RID OF KILL FLAG
	RET			;DONE SEARCHING

;	LIST: display remaining of line
;	"BRANCH" command in EDIT mode
EDTLL	CALL	OUTSTRZ		  ;TYPE REST OF LINE
	CALL	CRDO		;TYPE CARRIAGE RETURN
	POP	BC		;GET RID OF RETURN TO DISPED
	JP	EDTLIN		;GO TO MAIN CODE

;	DELETE: delete 1 or D chars
;	[i]D deletes i characters to the right of the cursor. The deleted characters
;	are echoed between backslashes, and the cursor is positioned to the right of
;	the last character deleted. If there are fewer than i characters to the right
;	of the cursor, [i]D deletes deletes the remainder of the line.
EDTDD	LD	A,(HL)		;GET CHAR WHICH WE ARE TRYING TO DELETE
	OR	A		;IS IT THE END OF LINE MARKER?
	RET	Z		;DONE IF SO
	LD	A,'\'		;TYPE BACKSLASH
	CALL	OUTCHR
EDTDD1	LD	A,(HL)		;GET CHAR FROM LINE
	OR	A		;ARE WE AT END?
	JP	Z,EDTBSL	;TYPE SLASH
	CALL	OUTCHR		;TYPE CHAR WE'RE GOING TO DELETE
	CALL	EDTIDL		;DELETE CURRENT CHAR
	DEC	D		;DECREMENT DELETE COUNT
	JP	NZ,EDTDD1	;KEEP DOING IT
;	Output a '\'
EDTBSL	LD	A,'\'		;TYPE ENDING SLASH
	CALL	OUTDO
	RET

;	CHANGE: change 1 or D chars
EDTCC	LD	A,(HL)		;ARE WE AT END OF LINE?
	OR	A		;SEE IF 0
	RET	Z		;RETURN

EDTCCL	CALL	SVCKBD
	CP	' '		;IS IT CONTROL CHAR?
	JP	NC,EDTCCC	;NO
	CP	0AH		;IS IT LF?
	JP	Z,EDTCCC	;YES
	CP	07H		;OR BELL?
	JP	Z,EDTCCC	;OK
	CP	09H		;OR TAB?
	JP	Z,EDTCCC	;OK
	LD	A,07H		;GET BELL
	CALL	OUTDO		;SEND IT
	JP	EDTCCL

EDTCCC	LD	(HL),A		;SAVE IN MEMORY
	CALL	OUTCHR		;ECHO THE CHAR WERE USING TO REPLACE
	INC	HL		;BUMP POINTER
	INC	B		;INCREMENT POSITION WITHIN LINE
	DEC	D		;ARE WE DONE CHANGING?
	JP	NZ,EDTCC	;IF NOT, CHANGE SOME MORE.
	RET			;DONE

;	HACK: hack the remaining of line and insert
;	H deletes all characters to the right of the cursor and then automatically
;	enters insert mode.  H is useful for replacing statements at the end of a line.
EDTHH	LD	(HL),00H	;MAKE LINE END AT CURRENT POSITION
	LD	C,B		;SET UP LINE LENGTH CORRECTLY

;	EXTEND: extend the line
;	Moves the cursor to the end of the line, goes into insert mode,
;	and allows insertion of text as if an Insert command had been given.
EDTXX	LD	D,0FFH		;FIND END OF LINE
	CALL	EDTSPC		;BY CALLING SPACER

;	INSERT: enter insert mode
;	I<text>$
;	Inserts <text> at the current cursor position. The inserted characters
;	are printed on the terminal. To terminate insertion, type Escape.
;	If Carriage Return is typed during an Insert command, the effect is the
;	same as typing Escape and then Carriage Return. During an Insert command,
;	the Rubout or Delete key on the terminal may be used to delete characters
;	to the left of the cursor.
EDTII	CALL	SVCKBD		;GET CHAR TO INSERT
	CP	7FH		;DELETE??
	JP	Z,EDTIBS	;YES, ACT LIKE "_"
	CP	08H		;Backspace?
	JP	Z,EDTIB1	;Do delete
	CP	0DH		;IS IT A CARRIAGE RETURN?
	JP	Z,EDTENT	;DONT INSERT, AND SIMULATE <CR>
	CP	1BH		;IS IT ESCAPE?
	RET	Z		;IF SO, DONE.
	CP	08H		;BACKSPACE? (twice??)
	JP	Z,EDTIB1	;TYPE BACKARROW AND DELETE
	CP	0AH		;LINE FEED?
	JP	Z,EDTINS	;ALLOW IT
	CP	07H		;BELL?
	JP	Z,EDTINS	;ALLOW IT
	CP	09H		;TAB?
	JP	Z,EDTINS	;ALLOW IT
	CP	' '		;IS IT ILLEGAL CHAR
	JP	C,EDTII		;TOO SMALL
	CP	'_'		;DELETE PREVIOUS CHAR INSERTED?
	JP	NZ,EDTINS	;IF NOT, JUMP AROUND NEXT CODE

;	Backspace in insert mode
EDTIBS	LD	A,'_'		;TYPE IT
EDTIB1	DEC	B		;ARE WE AT START OF LINE?
	INC	B		;LETS SEE
	JP	Z,DINGI		;IF SO, TYPE DING.
	CALL	OUTCHR		;TYPE THE BACK ARROW
	DEC	HL		;BACK UP THE POINTER
	DEC	B		;MOVE BACK POSIT IN LINE
	LD	DE,EDTII	;SET UP RETURN ADDRESS
	PUSH	DE		;SAVE IT  ON STACK & FALL THROUGH
;	Subroutine to delete char pointed to by [H,L]. Corrects C.
EDTIDL	PUSH	HL		;SAVE CURRENT POSIT POINTER
	DEC	C		;MAKE LENGTH OF LINE ONE LESS
EDTID1	LD	A,(HL)		;GET CHAR TO DELETE
	OR	A		;ARE WE AT END OF LINE
	SCF			;FLAG THAT DELCHR WAS CALLED (FOR K)
	JP	Z,POPHRT	;IF SO, DONE COMPRESSING
	INC	HL		;POINT TO NEXT BYTE
	LD	A,(HL)		;PICK IT UP
	DEC	HL		;NOW BACK AGAIN
	LD	(HL),A		;DEPOSIT IT
	INC	HL		;NOW TO NEXT BYTE
	JP	EDTID1		;KEEP CRUNCHING

;	Insert a char in buffer if line not too long
EDTINS	PUSH	AF		;SAVE THE CHAR TO BE INSERTED
	LD	A,C		;GET LENGTH OF LINE
;	If an attempt is made to insert a character that will make the line longer than
;	255 characters, a bell (Control-G) is typed and the character is not printed.
	CP	BUFLEN		;SEE IF WE ARENT TRYING TO MAKE LINE TOO LONG
	JP	C,EDTIN1	;IF LENGTH OK, GO INSERT
	POP	AF		;GET THE UNLAWFUL CHAR

DINGI	LD	A,07H		;TYPE A BELL TO LET USER KNOW
	CALL	OUTDO		;IT ALL OVER

EDINSLP	JP	EDTII		;HE HAS TO TYPE <ESC> TO GET OUT

;	Insert a char in buffer
EDTIN1	SUB	B		;CALC POINTER TO 0 AT END OF LINE
	INC	C		;WE ARE GOING TO HAVE LINE LONGER BY 1
	INC	B		;POSITION MOVES UP ONE ALSO
	PUSH	BC		;SAVE [B,C]
	EX	DE,HL		;SAVE [D,E] IN [H,L]
	LD	L,A		;SAVE # OF BYTES TO MOVE IN [L]
	LD	H,00H		;GET SET TO ADD [D,E] TO [H,L]
	ADD	HL,DE		;CALC HIGH POINTER
	LD	B,H		;GET HIGH BYTE TO MOVE POINTER
	LD	C,L		;IN [B,C]
	INC	HL		;ALWAYS MOVE AT LEAST ZERO AT END
	CALL	BLTUC		;MOVE LINE OUT 1 CHAR
	POP	BC		;RESTORE [B,C]
	POP	AF		;GET CHAR BACK
	LD	(HL),A		;SAVE IT IN LINE
	CALL	OUTCHR		;TYPE THE CHAR
	INC	HL		;POINT TO NEXT CHAR
	JP	EDINSLP		;AND GO GET MORE CHARS

;	BACK: Do a backspace
EDTBKS	LD	A,B		;ARE WE MOVING BACK PAST THE
	OR	A		;FIRST CHARACTER
	RET	Z		;DON'T ALLOW IT
	DEC	HL		;MOVE CHAR POINTER BACK
	LD	A,08H
	CALL	OUTCHR		;ECHO IT
	DEC	B		;CHANGE CURRENT POSITION
	DEC	D		;ARE WE DONE MOVING BACK?
	JP	NZ,EDTBKS1	;IF NOT, GO BACK MORE
	RET			;RETURN

;	In Edit Mode, [i]Rubout moves the cursor i spaces to the left (backspaces).
;	Characters are printed as you backspace over them.
EDTBKS1	LD	A,B		;ARE WE MOVING BACK PAST THE
	OR	A		;FIRST CHARACTER
	RET	Z		;DON'T ALLOW IT
	DEC	B		;CHANGE CURRENT POSITION
	DEC	HL		;MOVE CHAR POINTER BACK
	LD	A,(HL)		;GET CURRENT CHAR
	CALL	OUTCHR		;ECHO IT
	DEC	D		;ARE WE DONE MOVING BACK?
	JP	NZ,EDTBKS1	;IF NOT, GO BACK MORE
	RET			;RETURN

;	ENTER: Display the line and validate the changes
;	Typing Carriage Return prints the remainder of the line, saves the changes
;	you made and exits Edit Mode.
EDTENT	CALL	OUTSTRZ		;TYPE REST OF LINE

;	EDIT: Validate the changes
EDTEE	CALL	CRDO		;TYPE CARRIAGE RETURN
	POP	BC		;GET RID OF DISPED ADDRESS
	POP	DE		;GET LINE # OFF STACK
	LD	A,D		;DOUBLE BYTE ZERO.
	AND	E
	INC	A		;SET ZERO IF [D,E] = ALL ONES.

;	Used by AUTO code
;	Crunch and store line; exit
EDTSTL	LD	HL,BUFMIN	;START KRUNCHING AT BUF
	RET	Z		;RETURN TO INLIN IF CALLED FROM THERE
	SCF			;FLAG LINE # WAS SEEN TO FOOL INSERT CODE
	PUSH	AF		;PSW IS ON STACK
	INC	HL		;NOW POINT AT BUF.
	JP	EDENT		;GO TO ENTRY POINT IN MAIN CODE

;	QUIT: Exit without applying the changes
;	Returns to BASIC command level, without saving any of the
;	changes that were made to the line during Edit Mode.
EDTQQ	POP	BC		;GET RID OF DISPED ADDRESS
	POP	DE		;GET LINE # OFF STACK
	LD	A,D		;DOUBLE BYTE ZERO.
	AND	E
	INC	A		;SET ZERO IF [D,E] = ALL ONES.
	JP	Z,FININL	;TYPE CR AND STORE ZERO IN BUF.
	JP	READY		;OTHERWISE CALLED FROM MAIN


;=============================================================================
;	BIPRTU  BASIC Interpreter PRINT USING Driver/WHG
;-----------------------------------------------------------------------------
;	PRINT USING DRIVER
; ## BIPRTU.ASM:17 ##
;
;	Come here after the "USING" clause in a PRINT statement
;	is recognized. The idea is to scan the USING string until
;	the value list is exhausted, finding string and numeric
;	fields to print values out of the list in,
;	and just outputing any characters that aren't part of
;	a PRINT field.
;
PRINUS	CALL	FRMCHK		;EVALUATE THE "USING" STRING
	CALL	CHKSTR		;MAKE SURE IT IS A STRING
	CALL	SYNCHR
	DB	';'		;MUST BE DELIMITED BY A SEMI-COLON
	EX	DE,HL		;[D,E]=TEXT POINTER
	LD	HL,(FACLO)	;GET POINTER TO "USING" STRING DESCRIPTOR
	JP	INIUS		;DONT POP OFF OR LOOK AT USFLG

REUSST	LD	A,(SARYFL)	;DID WE PRINT OUT A VALUE LAST SCAN?
	OR	A		;SET CC'S
	JP	Z,FCERR3	;NO, GIVE ERROR
	POP	DE		;[D,E]=POINTER TO "USING" STRING DESCRIPTOR
	EX	DE,HL		;[D,E]=TEXT POINTER
INIUS	PUSH	HL		;SAVE THE POINTER TO "USING" STRING DESCRIPTOR
	XOR	A		;INITIALLY INDICATE THERE ARE MORE
				;VALUES IN THE VALUE LIST
	LD	(SARYFL),A	;RESET THE FLAG THAT SAYS VALUES PRINTED
	INC	A		;TURN THE ZERO FLAG OFF
				;TO INDICATE THE VALUE LIST HASN'T ENDED
	PUSH	AF		;SAVE FLAG INDICATING WHETHER THE VALUE
				;LIST HAS ENDED
	PUSH	DE		;SAVE THE TEXT POINTER INTO THE VALUE LIST
	LD	B,(HL)		;[B]=LENGTH OF THE "USING" STRING
	OR	B		;Is the using string null?
				;(Bug? Is [A] null?)
FCERR3	JP	Z,FCERR		;IF SO, "ILLEGAL FUNCTION CALL"
	INC	HL		;[H,L]=POINTER AT THE "USING" STRING'S
	LD	A,(HL)		;DATA
	INC	HL
	LD	H,(HL)
	LD	L,A
	JP	PRCCHR		;GO INTO THE LOOP TO SCAN
				;THE "USING" STRING
				;NECPPC does not have string formatter
				; because of ROM space
BGSTRF	LD	E,B		;SAVE THE "USING" STRING CHARACTER COUNT
	PUSH	HL		;SAVE THE POINTER INTO THE "USING" STRING
	LD	C,02H		;THE \\ STRING FIELD HAS 2 PLUS
				;NUMBER OF ENCLOSED SPACES WIDTH
LPSTRF	LD	A,(HL)		;GET THE NEXT CHARACTER
	INC	HL		;ADVANCE THE POINTER AT THE "USING" STRING
				;DATA
	CP	'\'		;THE FIELD TERMINATOR?
	JP	Z,ISSTRF	;GO EVALUATE A STRING AND PRINT
	CP	' '		;A FIELD EXTENDER?
	JP	NZ,NOSTRF	;IF NOT, ITS NOT A STRING FIELD
	INC	C		;INCREMENT THE FIELD WIDTH
				;SEE IF THERE ARE MORE CHARACTERS
	DEC	B
	JP	NZ,LPSTRF	;KEEP SCANNING FOR THE FIELD TERMINATOR
;
;	Since  string field wasn't found, the "USING" string
;	character count and the pointer into its data must
;	be restored and the "\" printed
;
NOSTRF	POP	HL		;RESTORE THE POINTER INTO "USING" STRING'S DATA
	LD	B,E		;RESTORE THE "USING" STRING CHARACTER COUNT
	LD	A,'\'		;RESTORE THE CHARACTER
;
;	Here to print the character in [A] since it wasn't part of any field
;
NEWUCH	CALL	PLSPRT		;IF A "+" CAME BEFORE THIS CHARACTER
				;MAKE SURE IT GETS PRINTED
	CALL	OUTDO		;PRINT THE CHARACTER THAT WASN'T
				;PART OF A FIELD
PRCCHR	XOR	A		;SET [D,E]=0 SO IF WE DISPATCH
	LD	E,A		;SOME FLAGS ARE ALREADY ZEROED
	LD	D,A		;DON'T PRINT "+" TWICE
PLSFIN	CALL	PLSPRT		;ALLOW FOR MULTIPLE PLUSES
				;IN A ROW
	LD	D,A		;SET "+" FLAG
	LD	A,(HL)		;GET A NEW CHARACTER
	INC	HL
	CP	'!'		;CHECK FOR A SINGLE CHARACTER
	JP	Z,SMSTRF	;STRING FIELD
	CP	'#'		;CHECK FOR THE START OF A NUMERIC FIELD
	JP	Z,NUMNUM	;GO SCAN IT
	CP	'&'		;SEE IF ITS A VARIABLE LENGTH STRING FIELD
	JP	Z,VARSTR	;GO PRINT ENTIRE STRING
	DEC	B		;ALL THE OTHER POSSIBILITIES
				;REQUIRE AT LEAST 2 CHARACTERS
	JP	Z,REUSIN	;IF THE VALUE LIST IS NOT EXHAUSTED
				;GO REUSE "USING" STRING
	CP	'+'		;A LEADING "+" ?
	LD	A,08H		;SETUP [D] WITH THE PLUS-FLAG ON IN
	JP	Z,PLSFIN	;CASE A NUMERIC FIELD STARTS
	DEC	HL		;POINTER HAS ALREADY BEEN INCREMENTED
	LD	A,(HL)		;GET BACK THE CURRENT CHARACTER
	INC	HL		;REINCREMENT THE POINTER
	CP	'.'		;NUMERIC FIELD WITH TRAILING DIGITS
	JP	Z,DOTNUM	;IF SO GO SCAN WITH [E]=
				;NUMBER OF DIGITS BEFORE THE "."=0
	CP	'_'		;CHECK FOR LITERAL CHARACTER DECLARATION
	JP	Z,LITCHR
	CP	'\'		;CHECK FOR A BIG STRING FIELD STARTER
	JP	Z,BGSTRF	;GO SEE IF IT REALLY IS A STRING FIELD
	CP	(HL)		;SEE IF THE NEXT CHARACTER MATCHES THE
				;CURRENT ONE
	JP	NZ,NEWUCH	;IF NOT, CAN'T HAVE $$ OR ** SO ALL THE
				;POSSIBILITIES ARE EXHAUSTED
	CP	'$'		;IS IT $$ ?
	JP	Z,DOLRNM	;GO SET UP THE FLAG BIT
	CP	'*'		;IS IT ** ?
	JP	NZ,NEWUCH	;IF NOT, ITS NOT PART
				;OF A FIELD SINCE ALL THE POSSIBILITIES
				;HAVE BEEN TRIED
	LD	A,B		;CHECK FOR $
	INC	HL		;SEE IF THE "USING" STRING IS LONG
	CP	02H		;ENOUGH FOR THE SPECIAL CASE OF
	JP	C,NOTSPC	; **$
	LD	A,(HL)
	CP	'$'		;IS THE NEXT CHARACTER $ ?
NOTSPC	LD	A,' '		;SET THE ASTERISK BIT
	JP	NZ,SPCNUM	;IF IT NOT THE SPECIAL CASE, DON'T
				;SET THE DOLLAR SIGN FLAG
	DEC	B		;DECREMENT THE "USING" STRING CHARACTER COUNT
				;TO TAKE THE $ INTO CONSIDERATION
	INC	E		;INCREMENT THE FIELD WIDTH FOR THE
				;FLOATING DOLLAR SIGN
	DB	0FEH		;"CPI" OVER THE NEXT BYTE
				;MVI SI,  IN 8086
DOLRNM	XOR	A		;CLEAR [A]
	ADD	A,10H		;SET BIT FOR FLOATING DOLLAR SIGN FLAG
	INC	HL		;POINT BEYOND THE SPECIAL CHARACTERS
SPCNUM	INC	E		;SINCE TWO CHARACTERS SPECIFY
				;THE FIELD SIZE, INITIALIZE [E]=1
	ADD	A,D		;PUT NEW FLAG BITS IN [A]
	LD	D,A		;INTO [D]. THE PLUS FLAG MAY HAVE
				;ALREADY BEEN SET
NUMNUM	INC	E		;INCREMENT THE NUMBER OF DIGITS BEFORE
				;THE DECIMAL POINT
	LD	C,00H		;SET THE NUMBER OF DIGITS AFTER
				;THE DECIMAL POINT = 0
	DEC	B		;SEE IF THERE ARE MORE CHARACTERS
	JP	Z,ENDNUS	;IF NOT, WE ARE DONE SCANNING THIS
				;NUMERIC FIELD
	LD	A,(HL)		;GET THE NEW CHARACTER
	INC	HL		;ADVANCE THE POINTER AT THE "USING" STRING DATA
	CP	'.'		;DO WE HAVE TRAILING DIGITS?
	JP	Z,AFTDOT	;IF SO, USE SPECIAL SCAN LOOP
	CP	'#'		;MORE LEADING DIGITS ?
	JP	Z,NUMNUM	;INCREMENT THE COUNT AND KEEP SCANNING
	CP	','		;DOES HE WANT A COMMA
				;EVERY THREE DIGITS?
	JP	NZ,FINNUM	;NO MORE LEADING DIGITS, CHECK FOR ^^^
	LD	A,D		;TURN ON THE COMMA BIT
	OR	40H
	LD	D,A
	JP	NUMNUM		;GO SCAN SOME MORE

;
;	Here when a "." is seen in the "USING" string
;	It starts a numeric field if and only if
;	it is followed by a "#"
;
DOTNUM	LD	A,(HL)		;GET THE CHARACTER THAT FOLLOWS
	CP	'#'		;IS THIS A NUMERIC FIELD?
	LD	A,'.'		;IF NOT, GO BACK AND PRINT "."
	JP	NZ,NEWUCH
	LD	C,01H		;INITIALIZE THE NUMBER OF
				;DIGITS AFTER THE DECIMAL POINT
	INC	HL
AFTDOT	INC	C		;INCREMENT THE NUMBER OF DIGITS
				;AFTER THE DECIMAL POINT
	DEC	B		;SEE IF THE "USING" STRING HAS MORE
	JP	Z,ENDNUS	;CHARACTERS, AND IF NOT, STOP SCANNING
	LD	A,(HL)		;GET THE NEXT CHARACTER
	INC	HL
	CP	'#'		;MORE DIGITS AFTER THE DECIMAL POINT?
	JP	Z,AFTDOT	;IF SO, INCREMENT THE COUNT AND KEEP
				;SCANNING
;
;	Check for the "^^^^" that indicates scientific notation
;
FINNUM	PUSH	DE		;SAVE [D]=FLAGS AND [E]=LEADING DIGITS
	LD	DE,NOTSCI	;PLACE TO GO IF ITS NOT SCIENTIFIC
	PUSH	DE		;NOTATION
	LD	D,H		;REMEMBER [H,L] IN CASE
	LD	E,L		;ITS NOT SCIENTIFIC NOTATION
	CP	'^'		;IS THE FIRST CHARACTER "^" ?
	RET	NZ
	CP	(HL)		;IS THE SECOND CHARACTER "^" ?
	RET	NZ
	INC	HL
	CP	(HL)		;IS THE THIRD CHARACTER "^" ?
	RET	NZ
	INC	HL
	CP	(HL)		;IS THE FOURTH CHARACTER "^" ?
	RET	NZ
	INC	HL
	LD	A,B		;WERE THERE ENOUGH CHARACTERS FOR "^^^^"
	SUB	04H		;IT TAKES FOUR
	RET	C
	POP	DE		;POP OFF THE NOTSCI RETURN ADDRESS
	POP	DE		;GET BACK [D]=FLAGS [E]=LEADING DIGITS
	LD	B,A		;MAKE [B]=NEW CHARACTER COUNT
	INC	D		;TURN ON THE SCIENTIFIC NOTATION FLAG
	INC	HL
	DB	0CAH
NOTSCI	EX	DE,HL		;RESTORE THE OLD [H,L]
	POP	DE		;GET BACK [D]=FLAGS [E]=LEADING DIGITS
ENDNUS	LD	A,D		;IF THE LEADING PLUS FLAG IS ON
	DEC	HL
	INC	E		;INCLUDE LEADING "+" IN NUMBER OF DIGITS
	AND	08H		;DON'T CHECK FOR A TRAILING SIGN
	JP	NZ,ENDNUM	;ALL DONE WITH THE FIELD IF SO
				;IF THERE IS A LEADING PLUS
	DEC	E		;NO LEADING PLUS SO DON'T INCREMENT THE
				;NUMBER OF DIGITS BEFORE THE DECIMAL POINT
	LD	A,B
	OR	A		;SEE IF THERE ARE MORE CHARACTERS
	JP	Z,ENDNUM	;IF NOT, STOP SCANNING
	LD	A,(HL)		;GET THE CURRENT CHARACTER
	SUB	'-'		;TRAIL MINUS?
	JP	Z,SGNTRL	;SET THE TRAILING SIGN FLAG
	CP	'+' - '-'	;A TRAILING PLUS?
	JP	NZ,ENDNUM	;IF NOT, WE ARE DONE SCANNING
	LD	A,08H		;TURN ON THE POSITIVE="+" FLAG
SGNTRL	ADD	A,04H		;TURN ON THE TRAILING SIGN FLAG
	ADD	A,D		;INCLUDE WITH OLD FLAGS
	LD	D,A
	DEC	B		;DECREMENT THE "USING" STRING CHARACTER
				;COUNT TO ACCOUNT FOR THE TRAILING SIGN
ENDNUM	POP	HL		;[H,L]=THE OLD TEXT POINTER
	POP	AF		;POP OFF FLAG THAT SAYS WHETHER THERE
				;ARE MORE VALUES IN THE VALUE LIST
	JP	Z,FLDFIN	;IF NOT, WE ARE DONE WITH THE "PRINT"
	PUSH	BC		;SAVE [B]=# OF CHARACTERS REMAINING IN
				;"USING" STRING AND [C]=TRAILING DIGITS
	PUSH	DE		;SAVE [D]=FLAGS AND [E]=LEADING DIGITS
	CALL	FRMEVL		;READ A VALUE FROM THE VALUE LIST
	POP	DE		;[D]=FLAGS & [E]=# OF LEADING DIGITS
	POP	BC		;[B]=# CHARACTER LEFT IN "USING" STRING
				;[C]=NUMBER OF TRAILING DIGITS
	PUSH	BC		;SAVE [B] FOR ENTERING SCAN AGAIN
	PUSH	HL		;SAVE THE TEXT POINTER
	LD	B,E		;[B]=# OF LEADING DIGITS
	LD	A,B		;MAKE SURE THE TOTAL NUMBER OF DIGITS
	ADD	A,C		;DOES NOT EXCEED TWENTY-FOUR
	CP	19H
	JP	NC,FCERR	;IF SO, "ILLEGAL FUNCTION CALL"
	LD	A,D		;[A]=FLAG BITS
	OR	80H		;TURN ON THE "USING" BIT
	CALL	PUFOUT		;PRINT THE VALUE
	CALL	STROUT		;PRINT FROM THE DATA SEGMENT
FNSTRF	POP	HL		;GET BACK THE TEXT POINTER
	DEC	HL		;SEE WHAT THE TERMINATOR WAS
	CALL	CHRGTR
	SCF			;SET FLAG THAT CRLF IS DESIRED
	JP	Z,CRDNUS	;IF IT WAS A END-OF-STATEMENT
				;FLAG THAT THE VALUE LIST ENDED
				;AND THAT  CRLF SHOULD BE PRINTED
	LD	(SARYFL),A	;FLAG THAT VALUE HAS BEEN PRINTED.
				;DOESNT MATTER IF ZERO SET, [A]
				;MUST BE NON-ZERO OTHERWISE
	CP	';'		;A SEMI-COLON?
	JP	Z,SEMUSN	;A LEGAL DELIMITER
	CP	','		;A COMMA ?
	JP	NZ,SNERR	;THE DELIMETER WAS ILLEGAL
SEMUSN	CALL	CHRGTR		;IS THERE ANOTHER VALUE?
CRDNUS	POP	BC		;[B]=CHARACTERS REMAINING IN "USING" STRING
	EX	DE,HL		;[D,E]=TEXT POINTER
	POP	HL		;[H,L]=POINT AT THE "USING" STRING
	PUSH	HL		;DESCRIPTOR. RESAVE IT.
	PUSH	AF		;SAVE THE FLAG THAT INDICATES
				;WHETHER OR NOT THE VALUE LIST TERMINATED
	PUSH	DE		;SAVE THE TEXT POINTER
;
;	Since FRMEVL may have forced garbage collection
;	we have to use the number of characters already scanned
;	as an offset to the pointer to the "USING" string's data
;	to get a new pointer to the rest of the characters to
;	be scanned
;
	LD	A,(HL)		;GET THE "USING" STRING'S LENGTH
	SUB	B		;SUBTRACT THE NUMBER OF CHARACTERS
				;ALREADY SCANNED
	INC	HL		;[H,L]=POINTER AT
	LD	D,00H		;SETUP [D,E] AS A DOUBLE BYTE OFFSET
	LD	E,A
	LD	A,(HL)		;THE "USING" STRING'S STRING DATA
	INC	HL		;SETUP [D,E] AS A DOUBLE BYTE OFFSET
	LD	H,(HL)
	LD	L,A		;THE "USING" STRING'S STRING DATA
	ADD	HL,DE		;ADD ON THE OFFSET TO GET
				;THE NEW POINTER
CHKUSI	LD	A,B		;[A]=THE NUMBER OF CHARACTERS LEFT TO SCAN
	OR	A		;SEE IF THERE ARE ANY LEFT
	JP	NZ,PRCCHR	;IF SO, KEEP SCANNING
	JP	FINUSI		;SEE IF THERE ARE MORE VALUES

REUSIN	CALL	PLSPRT		;PRINT A "+" IF NECESSARY
	CALL	OUTDO		;PRINT THE FINAL CHARACTER
FINUSI	POP	HL		;POP OFF THE TEXT POINTER
	POP	AF		;POP OFF THE INDICATOR OF WHETHER OR NOT
				;THE VALUE LIST HAS ENDED
	JP	NZ,REUSST	;IF NOT, REUSE THE "USING" STRING
FLDFIN	CALL	C,CRDO		;IF NOT COMMA OR SEMI-COLON
				;ENDED THE VALUE LIST
				;PRINT A CRLF
	EX	(SP),HL		;SAVE THE TEXT POINTER
				;[H,L]=POINT AT THE "USING" STRING'S
				;DESCRIPTOR
	CALL	FRETM2		;FINALLY FREE IT UP
	POP	HL		;GET BACK THE TEXT POINTER
	JP	FINPRT		;ZERO [PTRFIL]
;
;	HERE TO HANDLE A LITERAL CHARACTER IN THE USING STRING PRECEDED
;	BY "_"
;
LITCHR	CALL	PLSPRT		;PRINT PREVIOUS "+" IF ANY
	DEC	B		;DECREMENT COUNT FOR ACTUAL CHARACTER
	LD	A,(HL)		;FETCH LITERAL CHARACTER
	INC	HL
	CALL	OUTDO
	JP	CHKUSI		;GO SEE IF USING STRING ENDED

;
;	HERE TO HANDLE VARIABLE LENGTH STRING FIELD SPECIFIED WITH "&"
;
VARSTR	LD	C,00H		;SET LENGTH TO 0 TO FLAG VARIABLE LENGTH
	JP	ISSTR1

;
;	Here when the "!" indicating a single character
;	string field has been scanned
;
SMSTRF	LD	C,01H		;SET THE FIELD WIDTH TO 1
	DB	3EH
ISSTRF	POP	AF		;GET RID OF THE [H,L] THAT WAS BEING
				;SAVED IN CASE THIS WASN'T A STRING FIELD
ISSTR1	DEC	B		;DECREMENT THE "USING" STRING CHARACTER COUNT
	CALL	PLSPRT		;PRINT A "+" IF ONE CAME BEFORE THE FIELD
	POP	HL		;TAKE OFF THE TEXT POINTER
	POP	AF		;TAKE OF THE FLAG WHICH SAYS
				;WHETHER THERE ARE MORE VALUES IN THE
				;VALUE LIST
	JP	Z,FLDFIN	;IF THERE ARE NO MORE VALUES
				;THEN WE ARE DONE
	PUSH	BC		;SAVE [B]=NUMBER OF CHARACTERS YET TO
				;BE SCANNED IN "USING" STRING
	CALL	FRMEVL		;READ A VALUE
	CALL	CHKSTR		;MAKE SURE ITS A STRING
	POP	BC		;[C]=FIELD WIDTH
	PUSH	BC		;RESAVE [B]
	PUSH	HL		;SAVE THE TEXT POINTER
	LD	HL,(FACLO)	;GET A POINTER TO THE DESCRIPTOR
	LD	B,C		;[B]=FIELD WIDTH
	LD	C,00H		;SET UP FOR "LEFT$"
	LD	A,B		;GET FIELD WITH IN [A]
	PUSH	AF		;SAVE THE FIELD WIDTH FOR SPACE PADDING
	LD	A,B		;SEE IF VARIABLE LENGTH FIELD
	OR	A
	CALL	NZ,LEFTUS
	CALL	STRPRT		;PRINT THE STRING
	LD	HL,(FACLO)	;SEE IF IT NEEDS TO BE PADDED
	POP	AF		;[A]=FIELD WIDTH
	OR	A		;IF FIELD LENGTH IS 0 MUST BE "&" SO
	JP	Z,FNSTRF	;DONT PRINT ANY TRAILING SPACES
	SUB	(HL)		;[A]=AMOUNT OF PADDING NEEDED
	LD	B,A
	LD	A,' '		;SETUP THE PRINT CHARACTER
	INC	B		;DUMMY INCREMENT OF NUMBER OF SPACES
UPRTSP	DEC	B		;SEE IF MORE SPACES
	JP	Z,FNSTRF	;NO, GO SEE IF THE VALUE LIST ENDED AND
				;RESUME SCANNING
	CALL	OUTDO		;PRINT A SPACE
	JP	UPRTSP		;AND LOOP PRINTING THEM

;
;	When a "+" is detected in the "USING" string
;	If a numeric field follows a bit in [D] should
;	be set, otherwise "+" should be printed.
;	since deciding whether a numeric field follows is very
;	difficult, the bit is always set in [D].
;	At the point it is decided a character is not part
;	of a numeric field, this routine is called to see
;	if the bit in [D] is set, which means
;	a "+" preceded the character and should be
;	printed.
;
PLSPRT	PUSH	AF		;SAVE THE CURRENT CHARACTER
	LD	A,D		;CHECK THE PLUS BIT
	OR	A		;SINCE IT IS THE ONLY THING THAT COULD
				;BE TURNED ON
	LD	A,'+'		;SETUP TO PRINT THE PLUS
	CALL	NZ,OUTDO	;PRINT IT IF THE BIT WAS SET
	POP	AF		;GET BACK THE CURRENT CHARACTER
	RET


;=============================================================================
;	GIO86   - BASIC-86 Interpreter Device Independent I/O Module
;
;	--------- --- ---- -- ---------
;	COPYRIGHT (C) 1982 BY MICROSOFT
;	--------- --- ---- -- ---------
;
;		T. Corbett      Microsoft Inc.
;
;-----------------------------------------------------------------------------
; ## GIO86.ASM:1033 ##
;
;	Output char in 'A' to console
;
;	OUTDO (either CALL or RST) prints char in [A] no registers affected
;		to either terminal or disk file or printer depending
;		flags:
;			PRTFLG if non-zero print to printer
;			PTRFIL if non-zero print to disk file pointed to by PTRFIL
;
OUTDO	PUSH	AF
	PUSH	HL
	CALL	ISFLIO		;Tests if I/O to device is taking place
	JP	NZ,FILOUT
	POP	HL
	LD	A,(PRTFLG)	;SEE IF WE WANT TO TALK TO LPT
	OR	A		;TEST BITS
	JP	Z,OUTCON	;IF ZERO THEN NOT
	POP	AF		;GET BACK CHAR
	PUSH	AF
	CP	08H		;BACKSPACE?
	JP	NZ,OUTDO2	;NO
	LD	A,(LPTPOS)	;GET LPT POS
	SUB	01H		;SUBTRACT ONE FROM PRINTER POSIT
	JP	C,OUTDO1
	LD	(LPTPOS),A
OUTDO1	POP	AF		;GET BACK BACKSPACE
	JP	OUTDO9		;SEND CHAR
OUTDO2	CP	09H		;TAB
	JP	NZ,OUTDO5	;NO
OUTDO3	LD	A,' '
	CALL	OUTDO
	LD	A,(LPTPOS)	;GET CURRENT PRINT POSIT
	AND	07H		;AT TAB STOP?
	JP	NZ,OUTDO3	;GO BACK IF MORE TO PRINT
	POP	AF		;POP OFF CHAR
	RET			;RETURN

OUTDO5	POP	AF		;GET CHAR BACK
	PUSH	AF		;SAVE AGAIN
	SUB	0DH		;IF FUNNY CONTROL CHAR, (LF) DO NOTHING
	JP	Z,OUTDO7
	JP	C,OUTDO8	;JUST PRINT CHAR
	LD	A,(LPTSIZ)	;GET SIZE OF PRINTER
	INC	A		;IS IT INFINITE?
	LD	A,(LPTPOS)	;GET POSIT
	JP	Z,OUTDO6	;THEN DONT FOLD
				;If 'WIDTH' is 255, the line width
				; is "infinite" (no CRLF)
	PUSH	HL		;SAVE [H,L]
	LD	HL,LPTSIZ	;Value for 'WIDTH' on printer output.
	CP	(HL)		;MAX size reached ?
	POP	HL
	CALL	Z,OUTDOB	;THEN DO CRLF
OUTDO6	CP	0FFH		;MAX LENGTH?
	JP	Z,OUTDO8	;THEN JUST PRINT
	INC	A		;INCREMENT POSIT
OUTDO7	LD	(LPTPOS),A	;SAVE POSIT
;	Output character to printer
OUTDO8	POP	AF		;GET CHAR BACK
OUTDO9	PUSH	AF		;SAVE BACK AGAIN
	PUSH	BC		;SAVE [B,C]
	PUSH	DE		;SAVE [D,E]
	PUSH	HL
	LD	C,A		;CPM WANTS CHAR IN [C]
	CALL	0000H		;BIOS "LIST"
VLIST	EQU	$-2		;PRINTER ROUTINE ADDRESS STORED HERE
	POP	HL		;RESTORE REGS
	POP	DE
	POP	BC
	POP	AF		;RESTORE CHAR
	RET			;RETURN FROM OUTCHR

;	aka FINLPT
OUTDOA	XOR	A		;RESET PRINT FLAG SO
	LD	(PRTFLG),A	;OUTPUT GOES TO TERMINAL
	LD	A,(LPTPOS)	;GET CURRENT LPT POSIT
	OR	A		;ON LEFT HAND MARGIN ALREADY?
	RET	Z		;YES, RETURN
OUTDOB	LD	A,0DH		;PUT OUT CRLF
	CALL	OUTDO9
	LD	A,0AH
	CALL	OUTDO9
	XOR	A		;ZERO LPTPOS
	LD	(LPTPOS),A	;DONE
	RET


;-----------------------------------------------------------------------------
;	CP/M: output to console
OUTCON	LD	A,(CNTOFL)
	OR	A
	JP	NZ,PPSWRT	;NO, DO OUTPUT
	POP	AF		;GET THE CHARACTER
	PUSH	BC
	PUSH	AF		;AND SAVE IT AGAIN
	CP	08H		;BACKSPACE?
	JP	NZ,OCNBKS
	LD	A,(TTYPOS)	;GET TTY POS
	OR	A		;SET CC'S
	JP	Z,OCXTAB	;RETURN
	DEC	A		;DECREMENT POSIT BY ONE
	LD	(TTYPOS),A	;CORRECT TTYPOS
	LD	A,08H		;GET BACK BACKSPACE CHAR
	JP	OCEOL		;SEND IT

OCNBKS	CP	09H		;OUTPUTTING TAB?
	JP	NZ,OCNTAB	;NO.
;	Do TAB using spaces
OCTAB	LD	A,' '		;GET SPACE CHAR
	CALL	OUTDO		;CALL OUTCHR RECURSIVELY (!)
	LD	A,(TTYPOS)	;GET CURRENT PRINT POS.
	AND	07H		;AT TAB STOP YET??
	JP	NZ,OCTAB	;NO, KEEP SPACING
;	Done TAB
OCXTAB	POP	AF		;RESTORE CURRENT CHAR (TAB)
	POP	BC		;GET [B,C] BACK
	RET			;ALL DONE

OCNTAB	CP	' '		;IS THIS A MEANINGFUL CHARACTER?
	JP	C,OCEOL		;IF IT'S A NON-PRINTING CHARACTER
	LD	A,(TTYSIZ)	;[B]=LINE LENGTH  (DON'T INCLUDE IT IN TTYPOS)
	LD	B,A		;DON'T INCLUDE IT IN TTYPOS
	LD	A,(TTYPOS)	;SEE IF PRINT HEAD IS AT THE END OF THE LINE
	INC	B		;IS WIDTH 255?
	JP	Z,OCNEOL	;YES, JUST INC TTYPOS
	DEC	B		;CORRECT [B]
	CP	B
	CALL	Z,CRDO		;TYPE CRLF AND SET TTYPOS AND [A]=0 IF SO
;	Jump if no EOL
OCNEOL	CP	0FFH		;HAVE WE HIT MAX #?
	JP	Z,OCEOL		;THEN LEAVE IT THERE
	INC	A		;INCREMENT TTYPOS SINCE WE'RE GOING TO PRINT A CHARACTER.
	LD	(TTYPOS),A	;STORE NEW PRINT HEAD POSITION
;	Jump if got EOL
OCEOL	POP	AF		;GET CHAR OFF STACK
	POP	BC		;RESTORE [B,C]
	PUSH	AF		;SAVE PSW BACK
	POP	AF
	PUSH	AF
	PUSH	BC
	PUSH	DE
	PUSH	HL
	LD	C,A		;CPM WANTS CHAR IN [C]
	CALL	0000H		;
VCONOUT	EQU	$-2		;CONSOLE OUTPUT ROUTINE ADDRESS STORED HERE
	POP	HL		;RESTORE REGS
	POP	DE
	POP	BC
	POP	AF		;RESTORE CHAR
	RET			;RETURN FROM OUTCHR

; TODO: OCNDCB1 vs. INCHR
INCHR	CALL	ISFLIO
	JP	Z,SVCKBD	;GET CHARACTER FROM TERMINAL
	CALL	INDSKC		;READ A CHARACTER
	RET	NC		;RETURN WITH CHARACTER
	PUSH	BC		;SAVE ALL REGISTERS
	PUSH	DE
	PUSH	HL
	CALL	PRGFIN		;CLOSE THE FILE
	POP	HL
	POP	DE
	POP	BC
	LD	A,(CHNFLG)	;CHAIN IN PROGRESS?
	OR	A		;TEST..
	JP	NZ,CHNRET	;YES, PERFORM VARIABLE BLOCK TRANSFER, ETC.
	LD	A,(RUNFLG)	;RUN IT OR NOT?
	OR	A
	JP	Z,INCPRMT
	LD	HL,NEWSTT
	PUSH	HL
	JP	RUNC		;RUN IT

INCPRMT	PUSH	HL		;PRESERVE REGISTERS
	PUSH	BC
	PUSH	DE
	LD	HL,REDDY	;PRINT PROMPT "OK"
	CALL	STROUT
	POP	DE
	POP	BC
	XOR	A
	POP	HL
	RET

;-----------------------------------------------------------------------------
SVCKBD	PUSH	BC		;SAVE REGS
	PUSH	DE
	PUSH	HL
	CALL	0000H		; CALL nn
VCONIN	EQU	$-2		;CHANGED TO CALL CI
	POP	HL		;RESTORE REGS
	POP	DE
	POP	BC
	AND	7FH
	CP	0FH		;GET RID OF PARITY BIT
	RET	NZ		;IS IT SUPRESS OUTPUT?
	LD	A,(CNTOFL)
	OR	A		;ARE WE SUPRESSING OUTPUT?
	CALL	Z,CTROPT	;THEN PRINT CONTROL-O NOW.
	CPL			;COMPLEMENT ITS STATE
	LD	(CNTOFL),A	;SAVE BACK
	OR	A		;SEE IF WE ARE TURNING OUTPUT ON.
	JP	Z,CTROPT	;PRINT THE ^O
	XOR	A
	RET			;RETURN WITH NULL WHICH IS ALWAYS IGNORED


;-----------------------------------------------------------------------------
; ## GIO86.ASM:1012 ##
;	CRDONZ - output carriage return if file PTRFIL is not at left margin
CRDONZ	LD	A,(TTYPOS)	;GET CURRENT TTYPOS
	OR	A		;SET CC'S
	RET	Z		;IF ALREADY ZERO, RETURN
	JP	CRDO		;DO CR

FININL	LD	(HL),00H	;PUT A ZERO AT THE END OF BUF
	LD	HL,BUFMIN	;SETUP POINTER

;-----------------------------------------------------------------------------
;	CRDO - output ASCII carriage return to current file
;	and return A=0
CRDO	LD	A,0DH
	CALL	OUTDO		;output Carriage Return
	LD	A,0AH
	CALL	OUTDO		;output Line Feed

; ## GIO86.ASM:1007 ##
;	DON'T PUT CR/LF OUT TO LOAD FILE
CRFIN	CALL	ISFLIO		;SEE IF OUTPUTTING TO DISK
	JP	Z,CRCONT	;NOT DISK FILE, CONTINUE
	XOR	A		;CRFIN MUST ALWAYS RETURN WITH A=0
	RET			;AND CARRY=0.

;	Set BOL TTY or LPT; output NULs if not LPT
;TODO:	BEGLIN vs. CRCONT
CRCONT	LD	A,(PRTFLG)	;GOING TO PRINTER?
	OR	A		;TEST
	JP	Z,NTPRTR	;NO
	XOR	A		;DONE, RETURN
	LD	(LPTPOS),A	;ZERO POSITON
	RET

;-----------------------------------------------------------------------------
;	Set BOL TTY; output NULs
;TODO: BEGLDO vs. NTPRTR
NTPRTR	XOR	A		;SET TTYPOS=0
	LD	(TTYPOS),A
	LD	A,(NULLS)	;GET NUMBER OF NULLS
NULLP	DEC	A
	RET	Z		;ALL NULLS DONE [A]=0
				;SOME ROUTINES DEPEND ON CRDO
				;AND CRFIN RETURNING [A]=0 AND Z TRUE
	PUSH	AF		;SAVE THE COUNT
	XOR	A		;[A]= A NULL
	CALL	OUTDO		;SEND IT OUT
	POP	AF		;RESTORE THE COUNT
	JP	NULLP		;LOOP PRINTING NULLS

;-----------------------------------------------------------------------------
;	Check console for keystroke of BREAK or PAUSE (^O)
;	Pause if PAUSE was pressed.
;	Called during BASIC program execution
ISCNTC	PUSH	BC
	PUSH	DE
	PUSH	HL
	CALL	0000H		;"GET CONSOLE STATUS"
VCONST2	EQU	$-2
	POP	HL
	POP	DE
	POP	BC
	OR	A		;SET CC'S
	RET	Z		;0=FALSE - NO CHARACTER TYPED
				;IF NONE, RETURN

;-----------------------------------------------------------------------------
;	"STOP" pressed.  Now wait for ^O or ^C
STALL	CALL	SVCKBD		;READ THE CHARACTER THAT WAS PRESENT
	CP	13H		;PAUSE? (^S)
	CALL	Z,SVCKBD	;IF PAUSE, READ NEXT CHAR
	LD	(NXTKEY),A
	CP	03H
	CALL	Z,KILIN
	JP	STOP

;-----------------------------------------------------------------------------
;	INKEY$ special function
INKEY$	CALL	CHRGTR
	PUSH	HL		;SAVE THE TEXT POINTER
	CALL	GNXTKY		;GET CHARC AND CLEAR IF SET
	JP	NZ,BUFCIN
	CALL	0000H		;"GET CONSOLE STATUS"
VCONST1	EQU	$-2
	OR	A		;SET NON-ZERO IF CHAR THERE
	JP	Z,MKNLST	;NO, RETURN NULL STRING
	CALL	SVCKBD		;GET CHAR IF ONE
BUFCIN	PUSH	AF
	CALL	STRIN1		;MAKE ONE CHAR STRING
	POP	AF
	LD	E,A		;CHAR TO [D]
	CALL	PUT1CH		;STUFF IN DESCRIPTOR AND GOTO PUTNEW

MKNLST	LD	HL,DSEGZ	;ZERO
	LD	(FACLO),HL	;POINTING AT A ZERO
	LD	A,03H
	LD	(VALTYP),A
	POP	HL
	RET

;-----------------------------------------------------------------------------
	;Get next key
GNXTKY	LD	A,(NXTKEY)	;GET SAVED CHAR
	OR	A		;IS THERE ONE?
	RET	Z		;NO, DONE
	PUSH	AF		;SAVE CHAR
	XOR	A		;CLEAR IT
	LD	(NXTKEY),A	;BY STORING ZERO
	POP	AF		;RESTORE CHAR AND NON-ZERO CC'S
	RET

;-----------------------------------------------------------------------------
;	CP/M: output character to current file/device
;	Special handling of char LF (0AH): output a CR (0DH) after the LF
OUTCHR	CALL	OUTDO		;OUTPUT THE CHAR
	CP	0AH		;WAS IT A LF?
	RET	NZ		;NO, RETURN
	LD	A,0DH		;DO CR
	CALL	OUTDO
	CALL	CRFIN
	LD	A,0AH		;RESTORE CHAR (LF)
	RET


;-----------------------------------------------------------------------------
; ## MATH1.SRC ##
;
;	This is the block transfer routine
;	It makes space by shoving everything forward
;
;	[H,L] = Destination of high address
;	[D,E] = Low address to be transferred
;	[B,C] = High address to be transferred
;
;	A check is made to make sure a reasonable amount
;	of space remains between the top of the stack and
;	the highest location transferred into
;
;	On exit [H,L]=[D,E]=LOW[B,C]=LOCATION LOW was moved into
;
BLTU	CALL	REASON		;CHECK DESTINATION TO MAKE SURE
				;STRING SPACE WONT BE OVERWRITTEN
BLTUC	PUSH	BC		;SAVE END OF SOURCE
	EX	(SP),HL		;SWAP SOURCE AND DEST" END
	POP	BC		;GET END OF DESTINATION
BLTUC1	CALL	COMPAR		;SEE IF LIST MOVED
	LD	A,(HL)		;GET BYTE
	LD	(BC),A		;MOVE IT
	RET	Z		;EXIT IF ALL DONE
	DEC	BC		;NEXT BYTE TO MOVE TO
	DEC	HL		;NEXT BYTE TO MOVE
	JP	BLTUC1		;LOOP UNTIL ALL BYTES MOVED


;=============================================================================
;	BIMISC  BASIC Interpreter miscellaneous routines/WHG/PGA etc.
;-----------------------------------------------------------------------------
;	GENERAL STORAGE MANAGEMENT ROUTINES
; ## BIMISC.ASM:79 ##
;
;	This routine is used to make sure a certain number
;	of locations remain available for the
;	stack. The call is :
;       	LD      C,NUMBER OF 2 BYTE ENTRIES NECESSARY
;       	CALL    GETSTK
;
;	This routine must be called by any routine which puts
;	an arbitrary amount of stuff on the stack
;	(I.E. any recursive routine like FRMEVL)
;	It is also called by routines such as GOSUB and FOR
;	which make permanent entries on the stack
;
;	Routines which merely use and free up the guaranteed
;	NUMLEV stack locations need not call this
GETSTK	PUSH	HL		;SAVE [H,L]
	LD	HL,(MEMSIZ)	;LOWEST FREE MEMORY
	LD	B,00H		;BC = NUMBER OF LEVELS TO TEST
	ADD	HL,BC		;2 BYTES FOR EACH LEVEL
	ADD	HL,BC		;SEE IF WE CAN HAVE THIS MANY
;
;	[H,L]= SOME ADDRESS
;	[H,L] IS EXAMINED TO MAKE SURE AT LEAST NUMLEV
;	LOCATIONS REMAIN BETWEEN IT AND THE TOP OF THE STACK
;
CONS1	EQU	256-(2*NUMLEV)
	LD	A,CONS1		;SET [H,L]=-[H,L]-2*NUMLEV
	SUB	L
	LD	L,A
	LD	A,0FFH		;(-1 FOR MSB) -(2*NUMLEV) BYTES MINIMUM RAM
	SBC	A,H
	LD	H,A
	JP	C,OMERR		;IN CASE [H,L] WAS TOO BIG(MBM 3/18**)
				;NOW SEE IF SP IS LARGER
	ADD	HL,SP		;IF SO, CARRY WILL BE SET
	POP	HL		;GET BACK ORIGINAL [H,L]
	RET	C		;WAS OK?
;	OMERR resets SAVSTK to TOPMEM-2
;	 and issues an Out-of-Memory error message
OMERR	LD	HL,(FILTAB)	;ELIMINATE ALL STACK CONTEXT TO FREE
	DEC	HL		; UP SOME MEMORY SPACE
	DEC	HL		;MAKE SURE THE FNDFOR STOPPER IS SAVED
	LD	(SAVSTK),HL	;PLACE STACK IS RESTORED FROM
OMERRS	LD	DE,ERROM	;"OUT OF MEMORY"
	JP	ERROR

REASON	CALL	REASON1		;ENOUGH SPACE BETWEEN STRING & STACK
	RET	NC		;YES
;	LD	A,(CHNFLG)	;This extra check was not present
;	OR	A		;on CP/M 5.20, nor on MSX 5.22
;	JR	NZ,OMERR	;Can't garbage collect if CHAINing
	PUSH	BC		;SAVE ALL REGS
	PUSH	DE
	PUSH	HL
	CALL	GARBA2		;DO A GARBAGE COLLECTION
	POP	HL		;RESTORE ALL REGS
	POP	DE
	POP	BC
	CALL	REASON1		;ENOUGH SPACE THIS TIME?
	RET	NC		;YES
	JP	OMERRS		;NO, GIVE "OUT OF MEMORY" BUT DON'T TOUCH STACK

REASON1	PUSH	DE		;SAVE [D,E]
	EX	DE,HL		;SAVE [H,L] IN [D,E]
	LD	HL,(FRETOP)	;GET WHERE STRINGS ARE
	CALL	COMPAR		;IS TOP OF VARS LESS THAN STRINGS?
	EX	DE,HL		;BACK TO [D,E]
	POP	DE		;RESTORE [D,E]
	RET			;DONE

;-----------------------------------------------------------------------------
;	CP/M: Clear Files Table
;
;	THE CODE BELOW SETS THE FILE MODE TO 0 (CLOSED) FOR ALL FCB'S
NODSKS	LD	A,(NFILES)	;GET LARGEST FILE #
	LD	B,A		;INTO B FOR COUNTER
	LD	HL,FDBTAB	;POINT TO TABLE OF FILE DATA BLOCKS
	XOR	A		;MAKE A ZERO TO MARK FILES AS CLOSED
	INC	B
LOPNTO	LD	E,(HL)		;GET POINTER TO FILE DATA BLOCK IN [D,E]
	INC	HL
	LD	D,(HL)
	INC	HL
	LD	(DE),A		;MARK FILE AS CLOSED (MODE ZERO)
	DEC	B		;LOOP UNTIL DONE
	JP	NZ,LOPNTO
	CALL	CLSALL
	XOR	A
				;FALL INTO SCRATH
;-----------------------------------------------------------------------------
;	ERROR HANDLER, READY, COMPACTIFICATION, NEW, CLEAR, MAIN
; ## BIMISC.ASM ##
;
;
;	The "NEW" command clears the program text as well
;	as variable space
;
SCRATH	RET	NZ		;MAKE SURE THERE IS A TERMINATOR
SCRTCH	LD	HL,(TXTTAB)	;GET POINTER TO START OF TEXT
	CALL	TOFF		;TURN OFF TRACE. SET [A]=0.
	LD	(PROFLG),A	;NO LONGER A PROTECTED FILE
	LD	(AUTFLG),A	;CLEAR AUTO MODE
	LD	(PTRFLG),A	;SAY NO POINTERS EXIST
	LD	(HL),A		;SAVE AT END OFF TEXT
	INC	HL		;BUMP POINTER
	LD	(HL),A		;SAVE ZERO
	INC	HL		;BUMP POINTER
	LD	(VARTAB),HL	;NEW START OF VARIABLES
RUNC	LD	HL,(TXTTAB)	;POINT AT THE START OF TEXT
	DEC	HL
;
;	CLEARC is a subroutine which initializes the variable and
;	array space by reseting ARYTAB [the end of simple variable space]
;	and STREND [the end of array storage], it falls into STKINI
;	which resets the stack. [H,L] is preserved.
;
CLEARC	LD	(TEMP),HL	;SAVE [H,L] IN TEMP
	LD	A,(MRGFLG)	;DOING A CHAIN MERGE?
	OR	A		;TEST
	JP	NZ,CLEARC1	;LEAVE DEFAULT TABLE ALONE
	XOR	A
	LD	(OPTFLG),A	;INDICATE NO "OPTION" HAS BEEN SEEN
	LD	(OPTVAL),A	;DEFAULT TO "OPTION BASE 0"
	LD	B,1AH		;INITIALIZE THE DEFAULT VALTYPE TABLE
	LD	HL,DEFTBL	;POINT AT THE FIRST ENTRY
LDSNGAZ	LD	(HL),04H	;LOOP 26 TIMES STORING A DEFAULT VALTYP
	INC	HL		;FOR SINGLE PRECISION
	DEC	B		;COUNT OFF THE LETTERS
	JP	NZ,LDSNGAZ	;LOOP BACK, AND SETUP THE REST OF THE TABLE

CLEARC1	LD	DE,RNDINI	;RESET THE RANDOM NUMBER GENERATOR
	LD	HL,RNDX		;SEED IN RNDX
	CALL	MOVMM
	LD	HL,SEED		;AND ZERO COUNT REGISTERS
	XOR	A
	LD	(HL),A
	INC	HL
	LD	(HL),A
	INC	HL
	LD	(HL),A
	XOR	A
	LD	(ONEFLG),A	;RESET ON ERROR FLAG FOR RUNS
	LD	L,A		;RESET ERROR LINE NUMBER
	LD	H,A		;BY SETTING ONELIN=0.
	LD	(ONELIN),HL	;CLEAR ERROR LINE NUMBER
	LD	(OLDTXT),HL	;MAKE CONTINUING IMPOSSIBLE
	LD	HL,(MEMSIZ)
	LD	A,(CHNFLG)	;ARE WE CHAINING?
	OR	A		;TEST
	JP	NZ,CLEARC2	;FRETOP IS GOOD, LEAVE IT ALONE
	LD	(FRETOP),HL	;FREE UP STRING SPACE
CLEARC2	XOR	A		;MAKE SURE [A] IS ZERO, CC'S SET
	CALL	RESTOR		;RESTORE DATA
	LD	HL,(VARTAB)	;GET START OF VARIABLE SPACE
	LD	(ARYTAB),HL	;SAVE IN START OF ARRAY SPACE
	LD	(STREND),HL	;AND END OF VARIABLE STORAGE
	LD	A,(MRGFLG)
	OR	A		;DOING CHAIN MERGE?
	CALL	Z,CLSALL	;IF SO, DONT CLOSE FILES...
;
;	STKINI resets the stack pointer eliminating
;	GOSUB & FOR context. String temporaries are freed
;	up, SUBFLG is reset, continuing is disallowed,
;	and a dummy entry is put on the stack. This is so
;	FNDFOR will always find a non-"FOR" entry at the bottom
;	of the stack. [A]=0 and [D,E] is preserved.
;
STKINI	POP	BC		;GET RETURN ADDRESS HERE
	LD	HL,(FILTAB)
	DEC	HL		;TAKE INTO ACCOUNT FNDFOR STOPPER
	DEC	HL
	LD	(SAVSTK),HL	;MAKE SURE SAVSTK OK JUST IN CASE.
	INC	HL		;INCREMENT BACK FOR SPHL
	INC	HL
;	aka STKERR
XSTOP1	LD	SP,HL		;INITIALIZE STACK
	LD	HL,TEMPST
	LD	(TEMPPT),HL	;INITIALIZE STRING TEMPORARIES
	CALL	CLROVC		;BACK TO NORMAL OVERFLOW PRINT MODE
	CALL	OUTDOA
	CALL	FINPRT		;CLEAR PTRFIL, OTHER I/O FLAGS
	XOR	A		;ZERO OUT A
	LD	H,A		;ZERO OUT H
	LD	L,A		;ZERO OUT L
	LD	(PRMLEN),HL	;FLAG NO ACTIVE PARAMETERS
	LD	(NOFUNS),A	;INDICATE NO USER FUNCTIONS ACTIVE
	LD	(PRMLN2),HL	;NO PARAMETERS BEING BUILT
	LD	(FUNACT),HL	;SET NUMBER OF FUNCTIONS ACTIVE TO 0
	LD	(PRMSTK),HL	;AND NO PARAMETER BLOCKS ON THE STACK
	LD	(SUBFLG),A	;ALLOW SUBSCRIPTS
	PUSH	HL		;PUT ZERO (NON $FOR,$GOSUB)
				;ON THE STACK
	PUSH	BC		;PUT RETURN ADDRESS BACK ON
GTMPRT	LD	HL,(TEMP)	;GET SAVED [H,L]
	RET

;	COMPAR compares [H,L] with [D,E] unsigned
;
;	[H,L] less than [D,E] set Carry
;	[H,L] = [D,E] set Zero
;
;	[A] is the only register used
COMPAR	LD	A,H
	SUB	D
	RET	NZ
	LD	A,L
	SUB	E
	RET

;	SYNCHK looks at the current character to make sure it
;	is a specific thing (contained in the location after the call)
;	If not it calls the 'SYNTAX ERROR' routine. Otherwise it gobbles
;	the next character and returns. (by falling into CHRGET)
;
;	All registers are preserved except [A]=new char
;	and [H,L] ends up pointing at the character after the one
;	which was checked.
SYNCHR	LD	A,(HL)
	EX	(SP),HL
	CP	(HL)		;CMPC-IS CHAR THE RIGHT ONE?
	JP	NZ,JSNERR	;GIVE ERROR IF CHARS DONT MATCH
	INC	HL		;DUPLICATION OF CHRGET RST FOR SPEED
	EX	(SP),HL		;SEE CHRGET RST FOR EXPLANATION
	INC	HL
	LD	A,(HL)		;GET IT
	CP	':'		;IS IT END OF STATMENT OR BIGGER
	RET	NC		;DONE IF YES
	JP	CHRCON		;REST OF CHRGET

JSNERR	JP	SNERR		;IFE CYB


;-----------------------------------------------------------------------------
;	RESTORE, STOP, END, LINGET, CHRCON
; ## BIMISC.ASM:553 ##
;
RESTOR	EX	DE,HL		;SAVE [H,L] IN [D,E]
	LD	HL,(TXTTAB)
	JP	Z,RESTOR1	;RESTORE DATA POINTER TO BEGINNING OF PROGRAM
	EX	DE,HL		;TEXT POINTER BACK TO [H,L]
	CALL	LINGET		;GET THE FOLLOWING LINE NUMBER
	PUSH	HL		;SAVE TEXT POINTER
	CALL	FNDLIN		;FIND THE LINE NUMBER
	LD	H,B		;GET POINTER TO LINE IN [H,L]
	LD	L,C
	POP	DE		;TEXT POINTER BACK TO [D,E]
	JP	NC,USERR	;SHOULD HAVE FOUND LINE
RESTOR1	DEC	HL		;INITIALIZE DATPTR TO [TXTTAB]-1
RESFIN	LD	(DATPTR),HL	;READ FINISHES COME TO RESFIN
	EX	DE,HL		;GET THE TEXT POINTER BACK
	RET

STOP	RET	NZ		;RETURN IF NOT CONTROL-C AND MAKE
				;SURE "STOP" STATEMENTS HAVE A TERMINATOR
	INC	A
	JP	STPEND

ENDST	RET	NZ		;MAKE SURE "END" STATEMENTS HAVE A TERMINATOR
	PUSH	AF		;PRESERVE CONDITION CODES OVER CALL TO CLSALL
	CALL	Z,CLSALL
	POP	AF		;RESTORE CONDITION CODES
STPEND	LD	(SAVTXT),HL	;SAVE FOR "CONTINUE"
	LD	HL,TEMPST	;RESET STRING TEMP POINTER
	LD	(TEMPPT),HL	;SAVE IN CASE ^C PRINT USING
	DB	21H		;"LXI H," OVER NEXT TWO
STPEND1	OR	0FFH		;SET NON-ZERO TO FORCE PRINTING OF BREAK MESSAGE
	POP	BC		;POP OFF NEWSTT ADDRESS
ENDCON	LD	HL,(CURLIN)	;SAVE CURLIN
	PUSH	HL		;SAVE LINE TO PRINT
	PUSH	AF		;SAVE THE MESSAGE FLAG
				;ZERO MEANS DON'T PRINT "BREAK"
	LD	A,L
	AND	H		;SEE IF IT WAS DIRECT
	INC	A
	JP	Z,DIRIS		;IF NOT SET UP FOR CONTINUE
	LD	(OLDLIN),HL	;SAVE OLD LINE #
	LD	HL,(SAVTXT)	;GET POINTER TO START OF STATEMENT
	LD	(OLDTXT),HL	;SAVE IT
DIRIS	XOR	A
	LD	(CNTOFL),A	;FORCE OUTPUT
	CALL	OUTDOA
	CALL	CRDONZ		;PRINT CR IF TTYPOS .NE. 0
	POP	AF		;GET BACK ^C FLAG
	LD	HL,BRKTXT	;"BREAK"
	JP	NZ,ERRFIN	;CALL STROUT AND FALL INTO READY
	JP	STPRDY		;POP OFF LINE NUMBER & FALL INTO READY

CTROPT	LD	A,0FH		;PRINT AN ^O.
KILIN	PUSH	AF		;SAVE CURRENT CHAR
	SUB	03H		;CONTROL-C?
	JP	NZ,NTCTCT	;NO
	LD	(PRTFLG),A	;RESET ^O FLAG
	LD	(CNTOFL),A
NTCTCT	LD	A,'^'		;PRINT UP-ARROW.
	CALL	OUTDO		;SEND IT
	POP	AF		;GET BACK CONTROL CHAR.
	ADD	A,40H		;MAKE PRINTABLE
	CALL	OUTDO		;SEND IT
				;MARK LINE AS NOT FOR INPUT
	JP	CRDO		;AND THEN SEND CRLF.

CONT	LD	HL,(OLDTXT)	;A STORED TEXT POINTER OF
				;ZERO IS SETUP BY STKINI
				;AND INDICATES THERE IS NOTHING
				;TO CONTINUE
	LD	A,H		;"STOP","END",TYPING CRLF
	OR	L		;TO "INPUT" AND ^C SETUP OLDTXT
	LD	DE,ERRCN	;"CAN'T CONTINUE"
	JP	Z,ERROR
	EX	DE,HL
	LD	HL,(OLDLIN)
	EX	DE,HL
	EX	DE,HL
	LD	(CURLIN),HL	;SET UP OLD LINE # AS CURRENT LINE #
	EX	DE,HL
	RET

NULL	CALL	GETBYT
	RET	NZ		;MAKE SURE THERE IS A TERMINATOR
	INC	A		;CODE AT CRDO EXPECTS AT LEAST 1
	LD	(NULLS),A	;CHANGE NUMBER OF NULLS
	RET

TON	DB	3EH		;"MVI A," NON-ZERO QUANTITY

TOFF	XOR	A		;MAKE [A]=0 FOR NO TRACE
	LD	(TRCFLG),A	;UPDATE THE TRACE FLAG
	RET

SWAP	CALL	PTRGET		;[D,E]=POINTER AT VALUE #1
	PUSH	DE		;SAVE THE POINTER AT VALUE #1
	PUSH	HL		;SAVE THE TEXT POINTER
	LD	HL,SWPTMP	;TEMPORARY STORE LOCATION
	CALL	VMOVE		;SWPTMP=VALUE #1
	LD	HL,(ARYTAB)	;GET ARYTAB SO CHANGE CAN BE NOTED
	EX	(SP),HL		;GET THE TEXT POINTER BACK
				;AND SAVE CURRENT [ARYTAB]
	CALL	GETYPR
	PUSH	AF		;SAVE THE TYPE OF VALUE #1
	CALL	SYNCHR
	DB	','		;MAKE SURE THE VARIABLES ARE
				;DELIMITED BY A COMMA
	CALL	PTRGET		;[D,E]=POINTER AT VALUE #2
	POP	AF
	LD	B,A		;[B]=TYPE OF VALUE #1
	CALL	GETYPR		;[A]=TYPE OF VALUE #2
	CP	B		;MAKE SURE THEY ARE THE SAME
	JP	NZ,TMERR	;IF NOT, "TYPE MISMATCH" ERROR
	EX	(SP),HL		;[H,L]=OLD [ARYTAB] SAVE THE TEXT POINTER
	EX	DE,HL		;[D,E]=OLD [ARYTAB]
	PUSH	HL		;SAVE THE POINTER AT VALUE #2
	LD	HL,(ARYTAB)	;GET NEW [ARYTAB]
	CALL	COMPAR
	JP	NZ,GFCERR	;IF ITS CHANGED, ERROR
	POP	DE		;[D,E]=POINTER AT VALUE #2
	POP	HL		;[H,L]=TEXT POINTER
	EX	(SP),HL		;SAVE THE TEXT POINTER ON THE STACK
				;[H,L]=POINTER AT VALUE #1
	PUSH	DE		;SAVE THE POINTER AT VALUE #2
	CALL	VMOVE		;TRANSFER VALUE #2 INTO VALUE #1'S OLD
				;POSITION
	POP	HL		;[H,L]=POINTER AT VALUE #2
	LD	DE,SWPTMP	;LOCATION OF VALUE #1
	CALL	VMOVE		;TRANSFER SWPTMP=VALUE #1 INTO VALUE #2'S
				;OLD POSITION
	POP	HL		;GET THE TEXT POINTER BACK
	RET

GFCERR	JP	FCERR		;GIVE A FUNCTION CALL ERROR

ERASE	LD	A,01H
	LD	(SUBFLG),A	;THAT THIS IS "ERASE" CALLING PTRGET
	CALL	PTRGET		;GO FIND OUT WHERE TO ERASE
	JP	NZ,GFCERR	;PTRGET DID NOT FIND VARIABLE!
	PUSH	HL		;SAVE THE TEXT POINTER
	LD	(SUBFLG),A	;ZERO OUT SUBFLG TO RESET "ERASE" FLAG
	LD	H,B		;[B,C]=START OF ARRAY TO ERASE
	LD	L,C
	DEC	BC		;BACK UP TO THE FRONT
	DEC	BC		;NO VALUE TYPE WITHOUT LENGTH=2
	DEC	BC		;BACK UP ONE MORE
ERASE11	LD	A,(BC)		;GET A CHARACTER. ONLY THE COUNT HAS HIGH BIT=0
	DEC	BC		;SO LOOP UNTIL WE SKIP OVER THE COUNT
	OR	A		;SKIP ALL THE EXTRA CHARACTERS
	JP	M,ERASE11
	DEC	BC
	DEC	BC
	ADD	HL,DE		;[H,L]=THE END OF THIS ARRAY ENTRY
	EX	DE,HL		;[D,E]=END OF THIS ARRAY
	LD	HL,(STREND)	;[H,L]=LAST LOCATION TO MOVE UP
ERASE12	CALL	COMPAR		;SEE IF THE LAST LOCATION IS GOING TO BE MOVED
	LD	A,(DE)		;DO THE MOVE
	LD	(BC),A
	INC	DE		;UPDATE THE POINTERS
	INC	BC
	JP	NZ,ERASE12	;MOVE THE REST
	DEC	BC
	LD	H,B		;SETUP THE NEW STORAGE END POINTER
	LD	L,C
	LD	(STREND),HL
	POP	HL		;GET BACK THE TEXT POINTER
	LD	A,(HL)		;SEE IF MORE ERASURES NEEDED
	CP	','		;ADDITIONAL VARIABLES DELIMITED BY COMMA
	RET	NZ		;ALL DONE IF NOT
	CALL	CHRGTR
	JP	ERASE

PPAFHLR	POP	AF		;POP PSW
	POP	HL		;GET THE TEXT POINTER
	RET

;
;	Test for a letter / carry on=not a letter
;	                    carry off=a letter
;
ISLET	LD	A,(HL)
ISLET2	CP	'A'
	RET	C		;IF LESS THAN "A", RETURN EARLY
	CP	'Z'+1
	CCF
	RET

GCLERC	JP	CLEARC		;IFE LABEL  GO TO CLEARC

;	Syntax: CLEAR [[a][,b [,c]]]
;
;	a is a relic of pre-fiveo memory management and is ignored
;	b is the highest memory location available to BASIC-80
;	b is the the number of bytes in the data segment for BASIC-86
;	c is the number of bytes to be used as stack space by BASIC
;
CLEAR	JP	Z,GCLERC	;IF NO FORMULA JUST CLEAR
	CP	','		;ALLOW NO STRING SPACE
	JP	Z,CSKPCM
	CALL	INTID2		;Get Dummy Integer Parameter into [D,E]
	DEC	HL
	CALL	CHRGTR		;SEE IF ITS THE END
	JP	Z,GCLERC
CSKPCM	CALL	SYNCHR
	DB	','
	JP	Z,GCLERC
	EX	DE,HL
	LD	HL,(FILTAB)	;Use current top of memory as default
	EX	DE,HL
	CP	','		;SHOULD FINISH THERE
	JP	Z,CLEARS
	CALL	GETMPM		;GET MEMORY SIZE PARAMETER
CLEARS	DEC	HL		;BACK UP
	CALL	CHRGTR		;GET CHAR
	PUSH	DE		;SAVE NEW HIGH MEM
	JP	Z,CDFSTK	;USE SAME STACK SIZE
	CALL	SYNCHR
	DB	','
	JP	Z,CDFSTK
	CALL	GETMPM		;GET STACK SIZE PARAMETER
	DEC	HL
	CALL	CHRGTR
	JP	NZ,SNERR
CLEART	EX	(SP),HL		;SAVE TEXT POINTER
	PUSH	HL		;SAVE CANDIDATE FOR TOPMEM
				;(2*NUMLEV)+20 = 004EH
	LD	HL,NUMLEV*2+20	;CHECK STACK SIZE IS REASONABLE
	CALL	COMPAR
	JP	NC,GOMERR
	POP	HL		;[HL]=candidate for TOPMEM
	CALL	SUBDE		;DE=HL-DE=High Ram - Stack Size=new stack bottom
	JP	C,GOMERR	;WANTED MORE THAN TOTAL!
	PUSH	HL		;SAVE STACK BOTTOM
	LD	HL,(VARTAB)	;TOP LOCATION IN USE
	LD	BC,0014H	;LEAVE BREATHING ROOM
	ADD	HL,BC
	CALL	COMPAR		;ROOM?
	JP	NC,GOMERR	;NO, DON'T EVEN CLEAR
	EX	DE,HL		;NEW STACK BASE IN [H,L]
	LD	(MEMSIZ),HL	;SET UP NEW STACK BOTTOM
	POP	HL		;HL=Highest Ram available to BASIC
	LD	(FILTAB),HL	;SAVE IT, IT MUST BE OK
	POP	HL		;REGAIN THE TEXT POINTER
	JP	GCLERC		;GO CLEAR

GETMPM	CALL	FRMQNT		;EVALUATE FORMULA
	LD	A,D		;Memory size =0?
	OR	E
	JP	Z,FCERR		;Yes, error
	RET

GOMERR	JP	OMERR		;GIVE OM ERROR

;	CLEAR Default Stack Size to current stack size
CDFSTK	PUSH	HL
	LD	HL,(FILTAB)	;FIGURE OUT CURRENT STACK SIZE
	EX	DE,HL
	LD	HL,(MEMSIZ)
	LD	A,E		;SUB DX,STKLOW
	SUB	L
	LD	E,A
	LD	A,D
	SBC	A,H
	LD	D,A
	POP	HL
	JP	CLEART

;	SUBTRACT [H,L]-[D,E] INTO [D,E]
SUBDE	LD	A,L
	SUB	E
	LD	E,A
	LD	A,H
	SBC	A,D
	LD	D,A
	RET


;=============================================================================
;	NEXT CODE
; ## NEXT86.ASM ##
;
;	NOTE:
;
;	A FOR entry on the stack has the following format:
;
;	Low address
;		Token (FORTK in high byte) 			1 bytes
;		A pointer to the loop variable 			2 bytes
;		A pointer to the matching "NEXT"		2 bytes
;		A byte reflecting the sign of the increment	1 byte
;		A byte -1 for INT and +1 for SNG "FOR"		1 byte
;		The STEP 					4 bytes
;		The upper value 				4 bytes
;		The line # of the "FOR" statement 		2 bytes
;		A text pointer into the "FOR" statement 	2 bytes
;	High address
;
;	Total 19 bytes
;
NEXT	PUSH	AF
	DB	0F6H
NEXTS	XOR	A		;FLAG THAT "FOR IS USING "NEXT
	LD	(NXTFLG),A
	POP	AF
	LD	DE,0000H
NEXTC	LD	(NXTTXT),HL	;SAVE STARTING TEXT POINTER
	CALL	NZ,PTRGET
	LD	(TEMP),HL	;SAVE PTR TO LOOP VARIABLE
	CALL	FNDFOR		;TRY TO FING A "FOR ENTRY
	JP	NZ,NFERR	;NEXT W/O FOR ERROR
	LD	SP,HL		;SETUP STACK BY CHOPPING HERE
	PUSH	DE		;PUT THE VARIABLE PTR BACK ON
	LD	E,(HL)		;COMPARE TEXT POINTERS AT THE START
	INC	HL		;OF THIS "NEXT WITH THIS NEXT
	LD	D,(HL)
	INC	HL
	PUSH	HL		;SAVE THE POINTER INTO THE STACK ENTRY
	LD	HL,(NXTTXT)	;[H,L]=TEXT POINTER AT THE START OF THIS "NEXT"
	CALL	COMPAR
	JP	NZ,NFERR	;IF NO MATCH, "NEXT WITHOUT FOR"
	POP	HL
	POP	DE		;GET BACK THE VARIABLE POINTER
	PUSH	DE		;PUSH STEP ONTO THE STACK
	LD	A,(HL)
	PUSH	AF
	INC	HL
	PUSH	DE		;PUT THE POINTER TO THE LOOP VARIABLE ONTO THE STACK
	LD	A,(HL)		;GET FLAG WHETHER THIS IS AN INTEGER "FOR"
	INC	HL		;ADVANCE THE "FOR" ENTRY POINTER
	OR	A		;SET THE MINUS FLAG IF IT'S AN INTEGER "FOR"
	JP	M,INTNXT	;HANDLE INTEGERS SEPERATELY
	CALL	$MOVFM		;STEP VALUE INTO THE FAC
	EX	(SP),HL		;PUT THE POINTER INTO THE FOR ENTRY ONTO THE STACK
	PUSH	HL		;PUT THE POINTER TO THE LOOP VARIABLE BACK ONTO THE STACK
	LD	A,(NXTFLG)	;IS "FOR" USING "NEXT"
	OR	A
	JP	NZ,NXTDO	;NO, CONTINUE "NEXT"
	LD	HL,FVALSV	;FETCH THE INITIAL VALUE INTO THE FAC
	CALL	$MOVFM
	XOR	A		;CONTINUE THE "NEXT" WITH INITIAL VALUE
NXTDO	CALL	NZ,$FADDS
	POP	HL		;POP PTR TO LOOP VARIABLE
	CALL	$MOVMF		;WILL MOVE FAC TO THERE
	POP	HL		;GET ENTRY PTR.
	CALL	$MOVRM		;GET THE FINAL INTO THE REGISTERS
	PUSH	HL		;SAVE ENTRY PTR.
	CALL	FCOMP		;COMPARE THE NOS. RETURNING
				; 377 IF FAC LESS THAN REGS,0 IF =
				; AND 1 IF GREATER
	JP	FINNXT		;SKIP INTEGER CODE

INTNXT	INC	HL		;SKIP 4 DUMMY BYTES
	INC	HL
	INC	HL
	INC	HL
	LD	C,(HL)		;FETCH STEP TO CX
	INC	HL
	LD	B,(HL)
	INC	HL
	EX	(SP),HL		;PTR TO LOOP VARIABLE
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	PUSH	HL		;SAVE THE POINTER AT THE LOOP VARIABLE VALUE
	LD	L,C
	LD	H,B		;SETUP TO ADD [D,E] TO [H,L]
	LD	A,(NXTFLG)	;SEE IF "FOR IS USING "NEXT CODE
	OR	A
	JP	NZ,INXTDO	;NO, JUST CONTINUE
	LD	HL,(FVALSV)	;GET THE INITIAL VALUE
	JP	IFORIN		;CONTINUE FIRST ITERATION CHECK

INXTDO	CALL	IADD		;ADD THE STEP TO THE LOOP VARIABLE
	LD	A,(VALTYP)	;SEE IF THERE WAS OVERFLOW
	CP	04H		;TURNED TO SINGLE-PRECISION?
	JP	Z,OVERR		;INDICE GOT TOO LARGE
IFORIN	EX	DE,HL		;[D,E]=NEW LOOP VARIABLE VALUE
	POP	HL		;GET THE POINTER AT THE LOOP VARIABLE
	LD	(HL),D		;STORE THE NEW VALUE
	DEC	HL
	LD	(HL),E
	POP	HL		;GET BACK THE POINTER INTO THE "FOR" ENTRY
	PUSH	DE		;SAVE THE VALUE OF THE LOOP VARIABLE
	LD	E,(HL)		;[D,E]=FINAL VALUE
	INC	HL
	LD	D,(HL)
	INC	HL
	EX	(SP),HL		;SAVE THE ENTRY POINTER AGAIN, GET THE
				; VALUE OF THE LOOP VARIABLE INTO [H,L]
	CALL	ICOMP		;DO THE COMPARE
FINNXT	POP	HL		;POP "FOR ENTRY PTR
	POP	BC		;GET SGN OF INCREMENT
	SUB	B		;SUBTRACT THE INCREMENTS SIGN FROM
				; THAT OF (CURRENT VAL-FINAL VAL)
	CALL	$MOVRM		;GET LINE # OF "FOR INTO DX AND
				; TEXT PTR OF "FOR INTO CX
	JP	Z,LOOPDN	;JUMP IF DONE
	EX	DE,HL
	LD	(CURLIN),HL	;STORE LINE NO.
	LD	L,C		;SETUP THE TEXT PTR
	LD	H,B
	JP	NXTCON

LOOPDN	LD	SP,HL		;ELIMINATE FOR ENTRY & SAVE UPDATED
	LD	(SAVSTK),HL	;STACK
	LD	HL,(TEMP)	;RESTORE TEXT PTR
	LD	A,(HL)
	CP	','		;COMMA?
	JP	NZ,NEWSTT	;"NEXT
	CALL	CHRGTR		;Skip comma
	CALL	NEXTC		;Process Next NEXT
				;Doesn't return here.
				;Exit to NEWSTT (RUNCNT) or Loop


;=============================================================================
;	BIMISC  BASIC Interpreter miscellaneous routines/WHG/PGA etc.
; ## BIMISC.ASM:918 ##
;
;	Test if currently using file for I/O
ISFLIO	PUSH	HL		;SAVE [H,L]
	LD	HL,(PTRFIL)	;GET FILE POINTER
	LD	A,H		;NO ZERO?
	OR	L
	POP	HL		;RESTORE [H,L]
	RET


;=============================================================================
;	BISTRS  BASIC Interpreter String  routines/WHG/PGA etc.
;-----------------------------------------------------------------------------
;	STRING FUNCTIONS
; ## BISTRS.ASM:54 ##
;
;	The following routine compares two strings
;	One with desc in [D,E] other with desc. in [FACLO, FACLO+1]
;	A=0	if strings equal
;	A=377	if B,C,D,E .GT. FACLO
;	A=1	if B,C,D,E .LT. FACLO
STRCMP	CALL	FRESTR		;FREE UP THE FAC STRING, AND GET THE
				;POINTER TO THE FAC DESCRIPTOR IN [H,L]
	LD	A,(HL)		;SAVE THE LENGTH OF THE FAC STRING IN [A]
	INC	HL
	LD	C,(HL)		;SAVE THE POINTER AT THE FAC STRING
				;DATA IN [B,C]
	INC	HL
	LD	B,(HL)
	POP	DE		;GET THE STACK STRING POINTER
	PUSH	BC		;SAVE THE POINTER AT THE FAC STRING DATA
	PUSH	AF		;SAVE THE FAC STRING LENGTH
	CALL	FRETMP		;FREE UP THE STACK STRING AND RETURN
				;THE POINTER TO THE STACK STRING DESCRIPTOR
				;IN [H,L]
	POP	AF		;GET BACK LENGTH OF STRING
	LD	D,A		;[D]=LENGTH OF FAC STRING
	LD	E,(HL)		;[E]=LENGTH OF STACK STRING
	INC	HL
	LD	C,(HL)		;[B,C]=POINTER AT STACK STRING
	INC	HL
	LD	B,(HL)
	POP	HL		;GET BACK 2ND CHARACTER POINTER
CSLOOP	LD	A,E		;BOTH STRINGS ENDED
	OR	D		;TEST BY OR'ING THE LENGTHS TOGETHER
	RET	Z		;IF SO, RETURN WITH A ZERO
	LD	A,D		;GET FACLO STRING LENGTH
	SUB	01H		;SET CARRY AND MAKE [A]=255 IF [D]=0
	RET	C		;RETURN IF THAT STRING ENDED
	XOR	A		;MUST NOT HAVE BEEN ZERO, TEST CASE
	CP	E		;OF B,C,D,E STRING HAVING ENDED FIRST
	INC	A		;RETURN WITH A=1
	RET	NC		;TEST THE CONDITION
	DEC	D		;DECREMENT BOTH CHARACTER COUNTS
	DEC	E
	LD	A,(BC)		;GET CHARACTER FROM B,C,D,E STRING
	INC	BC
	CP	(HL)		;COMPARE WITH FACLO STRING
	INC	HL		;BUMP POINTERS (INX DOESNT CLOBBER CC'S)
	JP	Z,CSLOOP	;IF BOTH THE SAME, MUST BE MORE TO STRINGS
	CCF			;HERE WHEN STRINGS DIFFER
	JP	SIGNS		;SET [A] ACCORDING TO CARRY

;-----------------------------------------------------------------------------
;	STRING FUNCTIONS
; ## BISTRS.ASM:109 ##
;
;	The OCT$ (STRO$) function takes a number and gives
;	a string with the characters the number would give if
;	output in octal
OCT$	CALL	$FOUTO		;PUT OCTAL NUMBER IN FBUFFR
	JP	STRC		;JUMP INTO STR$ CODE

;	HEX$ (STRH$) same as OCT$ (STRO$) except uses hex instead of octal
HEX$	CALL	FOUTH1		;PUT HEX NUMBER IN FBUFFR
	JP	STRC		;JUMP INTO STR$ CODE

;	The STR$ function takes a number and gives
;	a string with the characters the output of the number
;	would have given
				;IS A NUMERIC
STR$	CALL	FOUT		;DO ITS OUTPUT
	;Common code for HEX$, OCT$ & STR$
STRC	CALL	STRLIT		;SCAN IT AND TURN IT INTO A STRING
	CALL	FREFAC		;FREE UP THE TEMP
	LD	BC,FINBCK
	PUSH	BC		;SET UP ANSWER IN NEW TEMP

;	STRCPY creates a copy of the string
;	whose descriptor is pointed to by [H,L].
;	On return [D,E] points to DSCTMP
;	which has the string info (length,where copied to)
STRCPY	LD	A,(HL)		;GET LENGTH
	INC	HL		;MOVE UP TO THE POINTER
	PUSH	HL		;GET POINTER TO POINTER OF ARG
	CALL	GETSPA		;GET THE SPACE
	POP	HL		;FIND OUT WHERE STRING TO COPY
	LD	C,(HL)
	INC	HL
	LD	B,(HL)
	CALL	STRAD2		;SETUP DSCTMP
	PUSH	HL		;SAVE POINTER TO DSCTMP
	LD	L,A		;GET CHARACTER COUNT INTO [L]
	CALL	MOVSTR		;MOVE THE CHARS IN
	POP	DE		;RESTORE POINTER TO DSCTMP
	RET			;RETURN

;	STRINI(A=1)
STRIN1	LD	A,01H		;MAKE ONE CHAR STRING (CHR$, INKEY$)
STRINI	CALL	GETSPA		;GET SOME STRING SPACE ([A] CHARS)
STRAD2	LD	HL,DSCTMP	;GET DESC. TEMP
	PUSH	HL		;SAVE DESC. POINTER
	LD	(HL),A		;SAVE CHARACTER COUNT
	INC	HL		;STORE [D,E]=POINTER TO FREE SPACE
	LD	(HL),E
	INC	HL
	LD	(HL),D
	POP	HL		;AND RESTORE [H,L] AS THE DESCRIPTOR POINTER
	RET

;	STRLT2 takes the string literal whose first character
;	is pointed by [H,L]+1 and builds a descriptor for it.
;	The descriptor is initially built in DSCTMP, but PUTNEW
;	transfers it into a temporary and leaves a pointer
;	at the temporary in FACLO. The characters other than
;	zero that terminate the string should be set up in [B]
;	and [D]. It the terminator is a quote, the quote is skipped
;	over. Leading quotes should be skipped before call. On return
;	the character after the string literal is pointed to
;	by [H,L] and is in [A], but the condition codes are
;	not set up.
STRLIT	DEC	HL
STRLT1	LD	B,'"'		;ASSUME STR ENDS ON QUOTE
STRLT3	LD	D,B
STRLT2	PUSH	HL		;SAVE POINTER TO START OF LITERAL
	LD	C,0FFH		;INITIALIZE CHARACTER COUNT
STRGET	INC	HL
	LD	A,(HL)		;GET CHAR
	INC	C		;BUMP CHARACTER COUNT
	OR	A		;IF 0, (END OF LINE) DONE
	JP	Z,STRFIN	;TEST
	CP	D
	JP	Z,STRFIN
	CP	B		;CLOSING QUOTE
	JP	NZ,STRGET	;NO, GO BACK FOR MORE
STRFIN	CP	'"'		;IF QUOTE TERMINATES THE STRING
	CALL	Z,CHRGTR	;SKIP OVER THE QUOTE
	PUSH	HL		;SAVE POINTER AT END OF STRING
	LD	A,B		;WERE WE SCANNING AN UNQUOTED STRING?
	CP	','
	JP	NZ,NTTRLS	;IF NOT, DON'T SUPPRESS TRAILING SPACES
	INC	C		;FIX [C] WHICH IS THE CHARACTER COUNT
LPTRLS	DEC	C		;DECREMENT UNTIL WE FIND A NON-SPACE CHARACTER
	JP	Z,NTTRLS	;DON'T GO PAST START (ALL SPACES)
	DEC	HL		;LOOK AT PREVIOUS CHARACTER
	LD	A,(HL)
	CP	' '
	JP	Z,LPTRLS	;IF SO CONTINUE LOOKING
NTTRLS	POP	HL
	EX	(SP),HL
	INC	HL
	EX	DE,HL		;GET POINTER TO TEMP
	LD	A,C		;GET CHARACTER COUNT IN A
	CALL	STRAD2		;SAVE STR INFO
;	Some string function is returning a result in DSCTMP
;	We want to setup a temp descriptor with DCSTMP in it,
;	put a pointer to the descriptor in FACLO and flag the
;	result as type string
PUTNEW	LD	DE,DSCTMP	;[D,E] POINT AT RESULT DESCRIPTOR
	DB	3EH		;SKIP THE NEXT BYTE ("MVI AL,")
PUTTMP	PUSH	DE		;SAVE A POINTER TO THE START OF THE STRING
	LD	HL,(TEMPPT)	;[H,L]=POINTER TO FIRST FREE TEMP
	LD	(FACLO),HL	;POINTER AT WHERE RESULT DESCRIPTOR WILL BE
	LD	A,03H
	LD	(VALTYP),A	;FLAG THIS AS A STRING
	CALL	VMOVE		;AND MOVE THE VALUE INTO A TEMPORARY
	LD	DE,FRETOP	;IF THE CALL IS TO PUTTMP, [D,E]
				;WILL NOT EQUAL DSCTMP +3
	CALL	COMPAR		;DSCTMP IS JUST BEYOND THE TEMPS
				;AND IF TEMPPT POINTS AT IT THERE
				;ARE NO FREE TEMPS
	LD	(TEMPPT),HL	;SAVE NEW TEMPORARY POINTER
	POP	HL		;GET THE TEXT POINTER
	LD	A,(HL)		;GET CURRENT CHARACTER INTO [A]
	RET	NZ
	LD	DE,ERRST	;"STRING TEMPORARY" ERROR
	JP	ERROR		;GO TELL HIM

;	Print the string pointed to by [H,L] which ends with a zero
;	If the string is below DSCTMP it will be copied into string space
STROUI	INC	HL		;POINT AT NEXT CHARACTER
STROUT	CALL	STRLIT		;GET A STRING LITERAL
;	Print the string whose descriptor is pointed to by FACLO.
STRPRT	CALL	FREFAC		;RETURN TEMP POINTER BY FACLO
	CALL	GETBCD		;[D]=LENGTH [B,C]=POINTER AT DATA
	INC	D		;INCREMENT AND DECREMENT EARLY
				;TO CHECK FOR NULL STRING
STRPR2	DEC	D		;DECREMENT THE LENGTH
	RET	Z		;ALL DONE
	LD	A,(BC)		;GET CHARACTER TO PRINT
	CALL	OUTDO
	CP	0DH
	CALL	Z,CRFIN
	INC	BC		;POINT TO THE NEXT CHARACTER
	JP	STRPR2		;AND PRINT IT...


;-----------------------------------------------------------------------------
;	STRING GARBAGE COLLECTION - GETSPA, GARBAG
; ## BISTRS.ASM:271 ##
;
;	GETSPA - Get space for character string
;	May force garbage collection.
;
;	# of chars (bytes) in [A]
;	Returns with pointer in [D,E] otherwise if cant get space
;	blows off to "Out of String Space" type error.
GETSPA	OR	A		;MUST BE NON ZERO. SIGNAL NO GARBAG YET
	DB	0EH
TRYGI2	POP	AF
	PUSH	AF		;SAVE IT BACK
	LD	HL,(STREND)
	EX	DE,HL		;IN [D,E]
	LD	HL,(FRETOP)	;GET TOP OF FREE SPACE IN [H,L]
	CPL			;-# OF CHARS
	LD	C,A		;IN [B,C]
	LD	B,0FFH
	ADD	HL,BC		;SUBTRACT FROM TOP OF FREE
	INC	HL
	CALL	COMPAR		;COMPARE THE TWO
	JP	C,GARBAG	;NOT ENOUGH ROOM FOR STRING, OFFAL TIME
	LD	(FRETOP),HL	;SAVE NEW BOTTOM OF MEMORY
	INC	HL		;MOVE BACK TO POINT TO STRING
	EX	DE,HL		;RETURN WITH POINTER IN [D,E]
PPSWRT	POP	AF		;GET CHARACTER COUNT
	RET			;RETURN FROM GETSPA

GARBAG	POP	AF		;HAVE WE COLLECTED BEFORE?
	LD	DE,ERROS	;GET READY FOR OUT OF STRING SPACE ERROR
	JP	Z,ERROR		;GO TELL USER HE LOST
	CP	A		;SET ZERO FLAG TO SAY WEVE GARBAGED
	PUSH	AF		;SAVE FLAG BACK ON STACK
	LD	BC,TRYGI2	;PLACE FOR GARBAG TO RETURN TO.
	PUSH	BC		;SAVE ON STACK
GARBA2	LD	HL,(MEMSIZ)	;START FROM TOP DOWN
FNDVAR	LD	(FRETOP),HL	;LIKE SO
	LD	HL,0000H	;GET DOUBLE ZERO
	PUSH	HL		;SAY DIDNT SEE VARS THIS PASS
	LD	HL,(STREND)	;FORCE DVARS TO IGNORE STRINGS
				;IN THE PROGRAM TEXT (LITERALS, DATA)
	PUSH	HL		;FORCE FIND HIGH ADDRESS
	LD	HL,TEMPST	;GET START OF STRING TEMPS
TVAR	EX	DE,HL		;SEE IF DONE
	LD	HL,(TEMPPT)
	EX	DE,HL
	CALL	COMPAR		;TEST
	LD	BC,TVAR		;FORCE JUMP TO TVAR
	JP	NZ,DVAR2	;DO TEMP VAR GARBAGE COLLECT
	LD	HL,PRMPRV	;SETUP ITERATION FOR PARAMETER BLOCKS
	LD	(TEMP9),HL
	LD	HL,(ARYTAB)	;GET STOPPING POINT IN [H,L]
	LD	(ARYTA2),HL	;STORE IN STOP LOCATION
	LD	HL,(VARTAB)	;GET STARTING POINT IN [H,L]
SVAR	EX	DE,HL		;GET STOPPING LOCATION
	LD	HL,(ARYTA2)
	EX	DE,HL
	CALL	COMPAR		;SEE IF AT END OF SIMPS
	JP	Z,ARYVAR
	LD	A,(HL)		;GET VALTYP
	INC	HL		;BUMP POINTER TWICE
	INC	HL		;
	INC	HL		;POINT AT THE VALUE
	PUSH	AF		;SAVE VALTYP
	CALL	IADAHL		;AND SKIP OVER EXTRA CHARACTERS AND COUNT
	POP	AF
	CP	03H		;SEE IF ITS A STRING
	JP	NZ,SKPVAR	;IF NOT, JUST SKIP AROUND IT
	CALL	DVARS		;COLLECT IT
	XOR	A		;AND DON'T SKIP ANYTHING MORE
SKPVAR	LD	E,A
	LD	D,00H		;[D,E]=AMOUNT TO SKIP
	ADD	HL,DE
	JP	SVAR		;GET NEXT ONE

ARYVAR	LD	HL,(TEMP9)	;GET LINK IN PARAMETER BLOCK CHAIN
	LD	E,(HL)		;GO BACK ONE LEVEL
	INC	HL
	LD	D,(HL)
	LD	A,D
	OR	E		;WAS THAT THE END?
	LD	HL,(ARYTAB)	;SETUP TO START ARRAYS
	JP	Z,ARYVAR?	;OTHERWISE GARBAGE COLLECT ARRAYS
	EX	DE,HL
	LD	(TEMP9),HL	;SETUP NEXT LINK IN CHAIN FOR ITERATION
	INC	HL		;SKIP CHAIN POINTER
	INC	HL
	LD	E,(HL)		;PICK UP THE LENGTH
	INC	HL
	LD	D,(HL)
	INC	HL
	EX	DE,HL		;SET [D,E]= ACTUAL END ADDRESS BY
	ADD	HL,DE		;ADDING BASE TO LENGTH
	LD	(ARYTA2),HL	;SET UP STOP LOCATION
	EX	DE,HL
	JP	SVAR

ARYVA2	POP	BC		;GET RID OF STACK GARBAGE
;	TODO: Duplicate ARYVAR ??
ARYVAR?	EX	DE,HL		;GET END OF ARRAYS
	LD	HL,(STREND)
	EX	DE,HL
	CALL	COMPAR		;SEE IF DONE WITH ARRAYS
	JP	Z,GRBPAS	;YES, SEE IF DONE COLLECTING
	LD	A,(HL)		;GET THE VALUE TYPE INTO [A]
	INC	HL
	PUSH	AF		;SAVE THE VALTYP
	INC	HL		;SKIP THE NAME CHARACTERS
	INC	HL
	CALL	IADAHL		;SKIP THE EXTRA CHARACTERS
	LD	C,(HL)		;PICK UP THE LENGTH
	INC	HL
	LD	B,(HL)
	INC	HL
	POP	AF		;RESTORE THE VALTYP
	PUSH	HL		;SAVE POINTER TO DIMS
	ADD	HL,BC		;ADD TO CURRENT POINTER POSITION
	CP	03H		;SEE IF ITS A STRING
	JP	NZ,ARYVA2	;IF NOT JUST SKIP IT
	LD	(TEMP8),HL	;SAVE END OF ARRAY
	POP	HL		;GET BACK CURRENT POSITION
	LD	C,(HL)		;PICK UP NUMBER OF DIMS
	LD	B,00H		;MAKE DOUBLE WITH HIGH ZERO
	ADD	HL,BC		;GO PAST DIMS
	ADD	HL,BC		;BY ADDING ON TWICE #DIMS (2 BYTE GUYS)
	INC	HL		;ONE MORE TO ACCOUNT FOR #DIMS.
ARYSTR	EX	DE,HL		;SAVE CURRENT POSIT IN [D,E]
	LD	HL,(TEMP8)	;GET END OF ARRAY
	EX	DE,HL		;FIX [H,L] BACK TO CURRENT
	CALL	COMPAR		;SEE IF AT END OF ARRAY
	JP	Z,ARYVAR?	;END OF ARRAY, TRY NEXT ARRAY
	LD	BC,ARYSTR	;ADDR OF WHERE TO RETURN TO
DVAR2	PUSH	BC		;GOES ON STACK
DVARS	XOR	A
	OR	(HL)		;SEE IF ITS THE NULL STRING
	INC	HL
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	INC	HL		;[D,E]=POINTER AT THE VALUE
	RET	Z		;NULL STRING, RETURN
	LD	B,H		;MOVE [H,L] TO [B,C]
	LD	C,L
	LD	HL,(FRETOP)	;GET POINTER TO TOP OF STRING FREE SPACE
	CALL	COMPAR		;IS THIS STRINGS POINTER .LT. FRETOP
	LD	H,B		;MOVE [B,C] BACK TO [H,L]
	LD	L,C
	RET	C		;IF NOT, NO NEED TO MESS WITH IT FURTHUR
	POP	HL		;GET RETURN ADDRESS OFF STACK
	EX	(SP),HL		;GET MAX SEEN SO FAR & SAVE RETURN ADDRESS
	CALL	COMPAR		;LETS SEE
	EX	(SP),HL		;SAVE MAX SEEN & GET RETURN ADDRESS OFF STACK
	PUSH	HL		;SAVE RETURN ADDRESS BACK
	LD	H,B		;MOVE [B,C] BACK TO [H,L]
	LD	L,C
	RET	NC		;IF NOT, LETS LOOK AT NEXT VAR
	POP	BC		;GET RETURN ADDR OFF STACK
	POP	AF		;POP OFF MAX SEEN
	POP	AF		;AND VARIABLE POINTER
	PUSH	HL		;SAVE NEW VARIABLE POINTER
	PUSH	DE		;AND NEW MAX POINTER
	PUSH	BC		;SAVE RETURN ADDRESS BACK
	RET			;AND RETURN

;	Here when made one complete pass thru string vars
GRBPAS	POP	DE		;POP OFF MAX POINTER
	POP	HL		;AND GET VARIABLE POINTER
	LD	A,H		;SEE IF ZERO POINTER
	OR	L
	RET	Z		;IF END OF COLLECTION,
				;THEN MAYBE RETURN TO GETSPA
	DEC	HL		;CURRENTLY JUST PAST THE DESCRIPTOR
	LD	B,(HL)		;[B]=HIGH BYTE OF DATA POINTER
	DEC	HL
	LD	C,(HL)		;[B,C]=POINTER AT STRING DATA
	PUSH	HL		;SAVE THIS LOCATION SO THE POINTER
				;CAN BE UPDATED AFTER THE STRING IS
				;MOVED
	DEC	HL
	LD	L,(HL)		;[L]=STRING LENGTH
	LD	H,00H		;[H,L] GET CHARACTER COUNT
	ADD	HL,BC		;[H,L]=POINTER BEYOND STRING
	LD	D,B
	LD	E,C		;[D,E]=ORIGINAL POINTER
	DEC	HL		;DON'T MOVE ONE BEYOND STRING
	LD	B,H		;GET TOP OF STRING IN [B,C]
	LD	C,L
	LD	HL,(FRETOP)	;GET TOP OF FREE SPACE
	CALL	BLTUC		;MOVE STRING
	POP	HL		;GET BACK POINTER TO DESC.
	LD	(HL),C		;SAVE FIXED ADDR
	INC	HL		;MOVE POINTER
	LD	(HL),B		;HIGH PART
	LD	H,B		;[H,L]=NEW POINTER
	LD	L,C
	DEC	HL		;FIX UP FRETOP
	JP	FNDVAR		;AND TRY TO FIND HIGH AGAIN


;-----------------------------------------------------------------------------
;	STRING CONCATENATION
; ## BISTRS.ASM:490 ##
;
;	The following routine concatenates two strings
;	The FACLO contains the first one at this point,
;	[H,L] points beyond the + sign after it
CAT	PUSH	BC		;PUT OLD PRECEDENCE BACK ON
	PUSH	HL		;SAVE TEXT POINTER
	LD	HL,(FACLO)	;GET POINTER TO STRING DESC.
	EX	(SP),HL		;SAVE ON STACK & GET TEXT POINTER BACK
	CALL	EVAL		;EVALUATE REST OF FORMULA
	EX	(SP),HL		;SAVE TEXT POINTER, GET BACK DESC.
	CALL	CHKSTR
	LD	A,(HL)
	PUSH	HL		;SAVE DESC. POINTER.
	LD	HL,(FACLO)	;GET POINTER TO 2ND DESC.
	PUSH	HL		;SAVE IT
	ADD	A,(HL)		;ADD TWO LENGTHS TOGETHER
	LD	DE,ERRLS	;SEE IF RESULT .LT. 256
	JP	C,ERROR		;ERROR "LONG STRING"
	CALL	STRINI		;GET INITIAL STRING
	POP	DE		;GET 2ND DESC.
	CALL	FRETMP
	EX	(SP),HL		;SAVE POINTER TO IT
	CALL	FRETM2		;FREE UP 1ST TEMP
	PUSH	HL		;SAVE DESC. POINTER (FIRST)
	LD	HL,(DSCTMPI)	;GET POINTER TO FIRST
	EX	DE,HL		;IN [D,E]
	CALL	MOVINS		;MOVE IN THE FIRST STRING
	CALL	MOVINS		;AND THE SECOND
	LD	HL,TSTOP	;CAT REENTERS FORMULA EVALUATION AT TSTOP
	EX	(SP),HL
	PUSH	HL		;TEXT POINTER OFF FIRST
	JP	PUTNEW		;THEN RETURN ADDRESS OF TSTOP

MOVINS	POP	HL		;GET RETURN ADDR
	EX	(SP),HL		;PUT BACK, BUT GET DESC.
	LD	A,(HL)		;[A]=STRING LENGTH
	INC	HL
	LD	C,(HL)		;[B,C]=POINTER AT STRING DATA
	INC	HL
	LD	B,(HL)
	LD	L,A		;[L]=STRING LENGTH
MOVSTR	INC	L
MOVLP	DEC	L		;SET CC'S
	RET	Z		;0, NO BYTE TO MOVE
	LD	A,(BC)		;GET CHAR
	LD	(DE),A		;SAVE IT
	INC	BC		;MOVE POINTERS
	INC	DE
	JP	MOVLP		;KEEP DOING IT


;-----------------------------------------------------------------------------
;	FREE UP STRING TEMPORARY - FRESTR, FREFAC, FRETMP, FRETMS
; ## BISTRS.ASM:560 ##
;
;	FRETMP is passed a pointer to a string descriptor in [D,E]
;	This value is returned in [H,L]. All the other registers are modified.
;	A check to is made to see if the string descriptor [D,E] points
;	to is the last temporary descriptor allocated by PUTNEW.
;	If so, the temporary is freed up by the updating of TEMPPT.
;	If a temporary is freed up, a further check is made to see if the
;	string data that that string temporary pointed to is the
;	the lowest part of string space in use.
;	If so, FRETMP is updated to reflect the fact that that space is no
;	longer is use.
;
FRESTR	CALL	CHKSTR		;MAKE SURE ITS A STRING
FREFAC	LD	HL,(FACLO)
FRETM2	EX	DE,HL		;FREE UP THE TEMP IN THE FACLO
FRETMP	CALL	FRETMS		;FREE UP THE TEMPORARY
	EX	DE,HL		;PUT THE STRING POINTER INTO [H,L]
	RET	NZ
	PUSH	DE		;SAVE [D,E] TO RETURN IN [H,L]
	LD	D,B		;[D,E]=POINTER AT STRING
	LD	E,C
	DEC	DE		;SUBTRACT ONE
	LD	C,(HL)		;[C]=LENGTH OF THE STRING FREED UP
	LD	HL,(FRETOP)	;SEE IF ITS THE FIRST
				;ONE IN STRING SPACE
	CALL	COMPAR
	JP	NZ,NOTLST	;NO SO DON'T ADD
				;MUST EXPLICITLY ZERO A
	LD	B,A		;MAKE [B]=0
	ADD	HL,BC		;ADD
	LD	(FRETOP),HL	;AND UPDATE FRETOP
NOTLST	POP	HL		;GET POINTER AT CURRENT DESCRIPTOR
	RET
	;(=L2 BKSPSTR)
FRETMS	LD	HL,(TEMPPT)	;GET TEMP POINTER
	DEC	HL		;LOOK AT WHAT IS IN THE LAST TEMP
	LD	B,(HL)		;[B,C]=POINTER AT STRING
	DEC	HL		;DECREMENT TEMPPT BY STRSIZ
	LD	C,(HL)
	DEC	HL
	CALL	COMPAR		;SEE IF [D,E] POINT AT THE LAST
	RET	NZ		;RETURN NOW IF NOW FREEING DONE
	LD	(TEMPPT),HL	;UPDATE THE TEMP POINTER SINCE
				;ITS BEEN DECREMENTED BY 4
	RET


;-----------------------------------------------------------------------------
;	STRING FUNCTIONS - LEN, ASC, CHR$
; ## BISTRS.ASM:611 ##
;
;	The function LEN($) returns the length of the
;	string passed as an argument
LEN	LD	BC,SNGFLT	;CALL SNGFLT WHEN DONE
	PUSH	BC		;LIKE SO
LEN1	CALL	FRESTR		;FREE UP TEMP POINTED TO BY FACLO
	XOR	A		;FORCE NUMERIC FLAG
	LD	D,A		;SET HIGH OF [D,E] TO ZERO FOR VAL
	LD	A,(HL)
	OR	A		;SET CONDITION CODES ON LENGTH
	RET			;RETURN

;	The following is the ASC($) function, it returns an integer
;	which is the decimal ASCII equivalent
ASC	LD	BC,SNGFLT	;WHERE TO GO WHEN DONE
	PUSH	BC		;SAVE RETURN ADDR ON STACK
ASC2	CALL	LEN1		;SET UP ORIGINAL STR
	JP	Z,FCERR		;NULL STR, BAD ARG.
	INC	HL		;BUMP POINTER
	LD	E,(HL)		;[D,E]=POINTER AT STRING DATA
	INC	HL
	LD	D,(HL)
	LD	A,(DE)		;[A]=FIRST CHARACTER
	RET

;	CHR$(#) creates a string which contains as its only
;	character the ASCII equivalent of the integer arg (#)
;	which must be .LT. 255.
CHR$	CALL	STRIN1		;GET STRING IN DSCTMP
	CALL	CONINT		;GET INTEGER IN RANGE
;	Put 1 char
PUT1CH	LD	HL,(DSCTMPI)	;GET ADDR OF STR
	LD	(HL),E		;SAVE ASCII BYTE
FINBCK	POP	BC		;RETURN TO HIGHER LEVEL &
				;SKIP THE CHKNUM CALL.
	JP	PUTNEW		;GO CALL PUTNEW

STRNG$	CALL	CHRGTR		;GET NEXT CHAR FOLLOWING "STRING$"
	CALL	SYNCHR
	DB	'('		;MAKE SURE LEFT PAREN
	CALL	GETBYT		;EVALUATE FIRST ARG (LENGTH)
	PUSH	DE		;SAVE IT
	CALL	SYNCHR
	DB	','		;COMMA
	CALL	FRMEVL		;GET FORMULA ARG 2
	CALL	SYNCHR
	DB	')'		;EXPECT RIGHT PAREN
	EX	(SP),HL		;SAVE TEXT POINTER ON STACK, GET REP FACTOR
	PUSH	HL		;SAVE BACK REP FACTOR
	CALL	GETYPR		;GET TYPE OF ARG
	JP	Z,STRSTR	;WAS A STRING
	CALL	CONINT		;GET ASCII VALUE OF CHAR
	JP	CALSPA		;NOW CALL SPACE CODE

STRSTR	CALL	ASC2		;GET VALUE OF CHAR IN [A]
CALSPA	POP	DE		;GET REP FACTOR IN [E]
	CALL	SPACE2		;INTO SPACE CODE, PUT DUMMY ENTRY
				;ON STACK POPPED OFF BY FINBCK
SPACE$	CALL	CONINT		;GET NUMBER OF CHARS IN [E]
	LD	A,' '		;GET SPACE CHAR
SPACE2	PUSH	AF		;SAVE CHAR
	LD	A,E		;GET NUMBER OF CHARS IN [A]
	CALL	STRINI		;GET A STRING THAT LONG
	LD	B,A		;COUNT OF CHARS BACK IN [B]
	POP	AF		;GET BACK CHAR TO PUT IN STRING
	INC	B		;TEST FOR NULL STRING
	DEC	B
	JP	Z,FINBCK	;YES, ALL DONE
	LD	HL,(DSCTMPI)	;GET DESC. POINTER
SPLP$	LD	(HL),A		;SAVE CHAR
	INC	HL		;BUMP PTR
				;DECR COUNT
	DEC	B
	JP	NZ,SPLP$	;KEEP STORING CHAR
	JP	FINBCK		;PUT TEMP DESC WHEN DONE


;-----------------------------------------------------------------------------
;	STRING FUNCTIONS - LEFT$, RIGHT$, MID$
; ## BISTRS.ASM:701 ##
;
;	The following is the LEFT$($,#) function.
;	It takes the leftmost # chars of the str.
;	If # is .GT. than the len of the str, it returns the whole str.
LEFT$	CALL	PREAM		;TEST THE PARAMETERS
	XOR	A		;LEFT NEVER CHANGES STRING POINTER
LEFT3	EX	(SP),HL		;SAVE TEXT POINTER
	LD	C,A		;OFFSET NOW IN [C]
	DB	3EH		;SKIP THE NEXT BYTE WITH "MVI A,"

;	This is PRINT USING's entry point into LEFT$
LEFTUS	PUSH	HL		;THIS IS A DUMMY PUSH TO OFFSET
				;THE EXTRA POP IN PUTNEW
LEFT2	PUSH	HL		;SAVE DESC. FOR  FRETMP
	LD	A,(HL)		;GET STRING LENGTH
	CP	B		;ENTIRE STRING WANTED?
	JP	C,ALLSTR	;IF #CHARS ASKED FOR.GE.LENGTH,YES
	LD	A,B		;GET TRUNCATED LENGTH OF STRING
	DB	11H		;SKIP OVER MVI USING "LXI D,"
ALLSTR	LD	C,00H		;MAKE OFFSET ZERO
	PUSH	BC		;SAVE OFFSET ON STACK
	CALL	GETSPA		;GET SPACE FOR NEW STRING
	POP	BC		;GET BACK OFFSET
	POP	HL		;GET BACK DESC POINTER.
	PUSH	HL		;BUT KEEP ON STACK
	INC	HL		;MOVE TO STRING POINTER FIELD
	LD	B,(HL)		;GET POINTER LOW
	INC	HL		;
	LD	H,(HL)		;POINTER HIGH
	LD	L,B		;GET LOW IN  L
	LD	B,00H		;GET READY TO ADD OFFSET TO POINTER
	ADD	HL,BC		;ADD  IT
	LD	B,H		;GET OFFSET POINTER IN [B,C]
	LD	C,L
	CALL	STRAD2		;SAVE INFO IN DSCTMP
	LD	L,A		;GET#  OF CHARS TO  MOVE IN L
	CALL	MOVSTR		;MOVE THEM IN
	POP	DE		;GET BACK DESC. POINTER
	CALL	FRETMP		;FREE IT UP.
	JP	PUTNEW		;PUT TEMP IN TEMP LIST

RIGHT$	CALL	PREAM		;CHECK ARG
	POP	DE		;GET DESC. POINTER
	PUSH	DE		;SAVE BACK FOR LEFT
	LD	A,(DE)		;GET PRESENT LEN OF STR
	SUB	B		;SUBTRACT 2ND PARM
	JP	LEFT3		;CONTINUE WITH LEFT CODE

;	MID$($,#) returns str with chars from # position
;	onward. If # is .GT. LEN($) then return null string.
;	MID$($,#,#2) returns str with chars from # position
;	for #2 chars. If #2 goes past end of string, return
;	as much as possible.
MID$	EX	DE,HL		;PUT THE TEXT POINTER IN [H,L]
	LD	A,(HL)		;GET THE FIRST CHARACTER
	CALL	PREAM2		;GET OFFSET OFF STACK AND MAKE
	INC	B
	DEC	B		;SEE IF EQUAL TO ZERO
	JP	Z,FCERR		;IT MUST NOT BE 0
	PUSH	BC		;PUT OFFSET ON TO THE STACK
	CALL	MIDRST		;DUPLICATE OF CODE CONDITIONED OUT
	POP	AF		;GET OFFSET BACK IN A
	EX	(SP),HL		;SAVE TEXT POINTER, GET DESC.
	LD	BC,LEFT2	;WHERE TO RETURN TO.
	PUSH	BC		;GOES ON STACK
	DEC	A		;SUB ONE FROM OFFSET
	CP	(HL)		;POINTER PAST END OF STR?
	LD	B,00H		;ASSUME NULL LENGTH STR
	RET	NC		;YES, JUST USE NULL STR
	LD	C,A		;SAVE OFFSET OF CHARACTER POINTER
	LD	A,(HL)		;GET PRESENT LEN OF STR
	SUB	C		;SUBTRACT INDEX (2ND ARG)
	CP	E		;IS IT TRUNCATION
	LD	B,A		;GET CALCED LENGTH IN B
	RET	C		;IF NOT USE PARTIAL STR
	LD	B,E		;USE TRUNCATED LENGTH
	RET			;RETURN TO LEFT2

;	The VAL function takes a string and turn it into
;	a number by interpreting the ascii digits, etc.
;	Except for the problem that a terminator must be supplied
;	by replacing the character beyond the string, VAL
;	is merely a call to Floating Input (FIN).
VAL	CALL	LEN1		;DO SETUP, SET RESULT=REAL
	JP	Z,SNGFLT
				;MAKE SURE TYPE SET UP OK IN EXTENDED
	LD	E,A		;GET LENGTH OF STR
	INC	HL		;TO HANDLE THE FACT THE IF
	LD	A,(HL)		;TWO STRINGS "1" AND "2" ARE STORED
	INC	HL		;NEXT TO EACH OTHER
	LD	H,(HL)
	LD	L,A
	PUSH	HL		;AND FIN IS CALLED POINTING TO
	ADD	HL,DE		;THE FIRST TWELVE WILL BE RETURNED
	LD	B,(HL)		;THE IDEA IS TO STORE 0 IN THE
	LD	(HL),D		;STRING BEYOND THE ONE VAL
	EX	(SP),HL		;IS BEING CALLED ON
	PUSH	BC		;THE FIRST CHARACTER OF THE NEXT STRING
	DEC	HL		;***CALL CHRGET TO MAKE SURE
	CALL	CHRGTR		;VAL(" -3")=-3
	CALL	FINDP		;IN EXTENDED, GET ALL THE PRECISION WE CAN
	POP	BC		;GET THE MODIFIED CHARACTER OF THE NEXT
				;STRING INTO [B]
	POP	HL		;GET THE POINTER TO THE MODIFIED CHARACTER
	LD	(HL),B		;RESTORE THE CHARACTER
				;IF STRING IS HIGHEST IN STRING SPACE
				;WE ARE MODIFYING [MEMSIZ] AND
				;THIS IS WHY [MEMSIZ] CAN'T BE USED TO STORE
				;STRING DATA BECAUSE WHAT IF THE
				;USER TOOK VAL OFF THAT HIGH STRING
	RET

;	Used by RIGHT$ and LEFT$ for parameter checking and setup
PREAM	EX	DE,HL		;PUT THE TEXT POINTER IN [H,L]
	CALL	SYNCHR
	DB	')'		;PARAM LIST SHOULD END

;	Used by MID$ for parameter checking and setup
PREAM2	POP	BC		;GET RETURN ADDR OFF STACK
	POP	DE		;GET LENGTH OF ARG OFF STACK
	PUSH	BC		;SAVE RETURN ADDR BACK ON
	LD	B,E		;SAVE INIT LENGTH
	RET

;-----------------------------------------------------------------------------
;	STRING FUNCTIONS - INSTR
; ## BISTRS.ASM:841 ##
;
;	This is the INSTR FUCNTION. It takes one of two
;	forms: INSTR(I%,S1$,S2$) or INSTR(S1$,S2$)
;	In the first form the string S1$ is searched for the
;	character S2$ starting at character position I%.
;	The second form is identical, except that the search
;	starts at position 1. INSTR returns the character
;	position of the first occurance of S2$ in S1$.
;	If S1$ is null, 0 is returned. If S2$ is null, then
;	I% is returned, unless I% .GT. LEN(S1$) in which
;	case 0 is returned.
INSTR	CALL	CHRGTR		;EAT FIRST CHAR
	CALL	FRMPRN		;EVALUATE FIRST ARG
	CALL	GETYPR		;SET ZERO IF ARG A STRING.
	LD	A,01H		;IF SO, ASSUME, SEARCH STARTS AT FIRST CHAR
	PUSH	AF		;SAVE OFFSET IN CASE STRING
	JP	Z,WUZSTR	;WAS A STRING
	POP	AF		;GET RID OF SAVED OFFSET
	CALL	CONINT		;FORCE ARG1 (I%) TO BE INTEGER
	OR	A		;DONT ALLOW ZERO OFFSET
	JP	Z,FCERR		;KILL HIM.
	PUSH	AF		;SAVE FOR LATER
	CALL	SYNCHR
	DB	','		;EAT THE COMMA
	CALL	FRMEVL		;EAT FIRST STRING ARG
	CALL	CHKSTR		;BLOW UP IF NOT STRING
WUZSTR	CALL	SYNCHR
	DB	','		;EAT COMMA AFTER ARG
	PUSH	HL		;SAVE THE TEXT POINTER
	LD	HL,(FACLO)	;GET DESCRIPTOR POINTER
	EX	(SP),HL		;PUT ON STACK & GET BACK TEXT PNT.
	CALL	FRMEVL		;GET LAST ARG
	CALL	SYNCHR
	DB	')'		;EAT RIGHT PAREN
	PUSH	HL		;SAVE TEXT POINTER
	CALL	FRESTR		;FREE UP TEMP & CHECK STRING
	EX	DE,HL		;SAVE 2ND DESC. POINTER IN [D,E]
	POP	BC		;GET TEXT POINTER IN B
	POP	HL		;DESC. POINTER FOR S1$
	POP	AF		;OFFSET
	PUSH	BC		;PUT TEXT POINTER ON BOTTOM
	LD	BC,POPHRT	;PUT ADDRESS OF POP H, RET ON
	PUSH	BC		;PUSH IT
	LD	BC,SNGFLT	;NOW ADDRESS OF [A] RETURNER
	PUSH	BC		;ONTO STACK
	PUSH	AF		;SAVE OFFSET BACK
	PUSH	DE		;SAVE DESC. OF S2
	CALL	FRETM2		;FREE UP S1 DESC.
	POP	DE		;RESTORE DESC. S2
	POP	AF		;GET BACK OFFSET
	LD	B,A		;SAVE UNMODIFIED OFFSET
	DEC	A		;MAKE OFFSET OK
	LD	C,A		;SAVE IN C
	CP	(HL)		;IS IT BEYOND LENGTH OF S1?
	LD	A,00H		;IF SO, RETURN ZERO. (ERROR)
	RET	NC
	LD	A,(DE)		;GET LENGTH OF S2$
	OR	A		;NULL??
	LD	A,B		;GET OFFSET BACK
	RET	Z		;ALL IF S2 NULL, RETURN OFFSET
	LD	A,(HL)		;GET LENGTH OF S1$
	INC	HL		;BUMP POINTER
	LD	B,(HL)		;GET 1ST BYTE OF ADDRESS
	INC	HL		;BUMP POINTER
	LD	H,(HL)		;GET 2ND BYTE
	LD	L,B		;GET 1ST BYTE SET UP
	LD	B,00H		;GET READY FOR DAD
	ADD	HL,BC		;NOW INDEXING INTO STRING
	SUB	C		;MAKE LENGTH OF STRING S1$ RIGHT
	LD	B,A		;SAVE LENGTH OF 1ST STRING IN [B]
	PUSH	BC		;SAVE COUNTER, OFFSET
	PUSH	DE		;PUT 2ND DESC (S2$) ON STACK
	EX	(SP),HL		;GET 2ND DESC. POINTER
	LD	C,(HL)		;SET UP LENGTH
	INC	HL		;BUMP POINTER
	LD	E,(HL)		;GET FIRST BYTE OF ADDRESS
	INC	HL
	LD	D,(HL)
	POP	HL		;RESTORE POINTER FOR 1ST STRING
CHK1	PUSH	HL		;SAVE POSITION IN SEARCH STRING
	PUSH	DE		;SAVE START OF SUBSTRING
	PUSH	BC		;SAVE WHERE WE STARTED SEARCH
CHK	LD	A,(DE)
	CP	(HL)		;GET CHAR FROM SUBSTRING
				; = CHAR POINTER TO BY [H,L]
	JP	NZ,OHWELL	;NO
	INC	DE		;BUMP COMPARE POINTER
	DEC	C		;END OF SEARCH STRING?
	JP	Z,GOTSTR	;WE FOUND IT!
	INC	HL		;BUMP POINTER INTO STRING BEING SEARCHED
	DEC	B		;DECREMENT LENGTH OF SEARCH STRING
	JP	NZ,CHK		;END OF STRING, YOU LOSE
	POP	DE		;GET RID OF POINTERS
	POP	DE		;GET RID OF GARB
	POP	BC		;LIKE SO
RETZR1	POP	DE
	XOR	A		;GO TO SNGFLT.
	RET			;RETURN

GOTSTR	POP	HL
	POP	DE		;GET RID OF GARB
	POP	DE		;GET RID OF EXCESS STACK
	POP	BC		;GET COUNTER, OFFSET
	LD	A,B		;GET ORIGINAL SOURCE COUNTER
	SUB	H		;SUBTRACT FINAL COUNTER
	ADD	A,C		;ADD ORIGINAL OFFSET (N1%)
	INC	A		;MAKE OFFSET OF ZERO = POSIT 1
	RET			;DONE

OHWELL	POP	BC
	POP	DE		;POINT TO START OF SUBSTRING
	POP	HL		;GET BACK WHERE WE STARTED TO COMPARE
	INC	HL		;AND POINT TO NEXT CHAR
				;DECR. # CHAR LEFT IN SOURCE STRING
	DEC	B
	JP	NZ,CHK1		;TRY SEARCHING SOME MORE
	JP	RETZR1		;END OF STRING, RETURN 0


;-----------------------------------------------------------------------------
;	STRING FUNCTIONS - LEFT HAND SIDE MID$
; ## BISTRS.ASM:973 ##
;
LHSMID	CALL	SYNCHR
	DB	'('		;MUST HAVE (
	CALL	PTRGET		;GET A STRING VAR
	CALL	CHKSTR		;MAKE SURE IT WAS A STRING
	PUSH	HL		;SAVE TEXT POINTER
	PUSH	DE		;SAVE DESC. POINTER
	EX	DE,HL		;PUT DESC. POINTER IN [H,L]
	INC	HL		;MOVE TO ADDRESS FIELD
	LD	E,(HL)		;GET ADDRESS OF LHS IN [D,E]
	INC	HL
	LD	D,(HL)
	LD	HL,(STREND)	;SEE IF LHS STRING IS IN STRING SPACE
	CALL	COMPAR		;BY COMPARING IT WITH STKTOP
	JP	C,NCPMID	;IF ALREADY IN STRING SPACE
				;DONT COPY.
	LD	HL,(TXTTAB)
	CALL	COMPAR		;Is this a fielded string?
	JP	NC,NCPMID	;Yes, Don't copy!!
	POP	HL		;GET BACK DESC. POINTER
	PUSH	HL		;SAVE BACK ON STACK
	CALL	STRCPY		;COPY THE STRING LITERAL INTO STRING SPACE
	POP	HL		;GET BACK DESC. POINTER
	PUSH	HL		;BACK ON STACK AGAIN
	CALL	VMOVE		;MOVE NEW DESC. INTO OLD SLOT.
NCPMID	POP	HL		;GET DESC. POINTER
	EX	(SP),HL		;GET TEXT POINTER TO [H,L] DESC. TO STACK
	CALL	SYNCHR
	DB	','		;MUST HAVE COMMA
	CALL	GETBYT		;GET ARG#2 (OFFSET INTO STRING)
	OR	A		;MAKE SURE NOT ZERO
	JP	Z,FCERR		;BLOW HIM UP IF ZERO
	PUSH	AF		;SAVE ARG#2 ON STACK
	LD	A,(HL)		;RESTORE CURRENT CHAR
	CALL	MIDRST		;USE MID$ CODE TO EVALUATE POSIBLE THIRD ARG.
	PUSH	DE		;SAVE THIRD ARG ([E]) ON STACK
				;MUST HAVE = SIGN
	CALL	FRMEQL		;EVALUATE RHS OF THING.
	PUSH	HL		;SAVE TEXT POINTER.
	CALL	FRESTR		;FREE UP TEMP RHS IF ANY.
	EX	DE,HL		;PUT RHS DESC. POINTER IN [D,E]
	POP	HL		;TEXT POINTER TO [H,L]
	POP	BC		;ARG #3 TO C.
	POP	AF		;ARG #2 TO A.
	LD	B,A		;AND [B]
	EX	(SP),HL		;GET LHS DESC. POINTER TO [H,L]
				; TEXT POINTER TO STACK
	PUSH	HL		;SAVE TEXT POINTER
	LD	HL,POPHRT	;GET ADDR TO RETURN TO
	EX	(SP),HL		;SAVE ON STACK & GET BACK TXT PTR.
	LD	A,C		;GET ARG #3
	OR	A		;SET CC'S
	RET	Z		;IF ZERO, DO NOTHING
	LD	A,(HL)		;GET LENGTH OF LHS
	SUB	B		;SEE HOW MANY CHARS IN EMAINDER OF STRING
	JP	C,FCERR		;CANT ASSIGN PAST LEN(LHS)!
	INC	A		;MAKE PROPER COUNT
	CP	C		;SEE IF # OF CHARS IS .GT. THIRD ARG
	JP	C,BIGLEN	;IF SO, DONT TRUNCATE
	LD	A,C		;TRUNCATE BY USING 3RD ARG.
BIGLEN	LD	C,B		;GET OFFSET OF STRING IN [C]
	DEC	C		;MAKE PROPER OFFSET
	LD	B,00H		;SET UP [B,C] FOR LATER DAD B.
	PUSH	DE		;SAVE [D,E]
	INC	HL		;POINTER TO ADDRESS FIELD.
	LD	E,(HL)		;GET LOW BYTE IN [E]
	INC	HL		;BUMP POINTER
	LD	H,(HL)		;GET HIGH BYTE IN [H]
	LD	L,E		;NOW COPY LOW BYTE BACK TO [L]
	ADD	HL,BC		;ADD OFFSET
	LD	B,A		;SET COUNT OF LHS IN [B]
	POP	DE		;RESTORE [D,E]
	EX	DE,HL		;MOVE RHS. DESC. POINTER TO [H,L]
	LD	C,(HL)		;GET LEN(RHS) IN [C]
	INC	HL		;MOVE POINTER
	LD	A,(HL)		;GET LOW BYTE OF ADDRESS IN [A]
	INC	HL
	LD	H,(HL)
	LD	L,A
	EX	DE,HL		;ADDRESS OF RHS NOW IN [D,E]
	LD	A,C		;IS RHS NULL?
	OR	A		;TEST
	RET	Z		;THEN ALL DONE.
;	Now all set up for assignment.
;	[H,L] = LHS pointer
;	[D,E] = RHS pointer
;	C = LEN(RHS)
;	B = LEN(LHS)
MID$LP	LD	A,(DE)
	LD	(HL),A		;GET BYTE FROM RHS.
	INC	DE		;STORE IN LHS
	INC	HL		;BUMP RHS POINTER
	DEC	C		;BUMP LHS POINTER.
				;BUMP DOWN COUNT OF RHS.
	RET	Z		;IF ZERO, ALL DONE.
				;IF LHS ENDED, ALSO DONE.
	DEC	B
	JP	NZ,MID$LP	;IF NOT DONE, MORE COPYING.
	RET			;BACK TO NEWSTT

MIDRST	LD	E,0FFH		;IF TWO ARG GUY, TRUNCATE.
	CP	')'
	JP	Z,MID2		;[E] SAYS USE ALL CHARS
				;IF ONE ARGUMENT THIS IS CORRECT
	CALL	SYNCHR
	DB	','		;COMMA? MUST DELINEATE 3RD ARG.
	CALL	GETBYT		;GET ARGUMENT  IN  [E]
MID2	CALL	SYNCHR
	DB	')'		;MUST BE FOLLOWED BY )
	RET			;ALL DONE.


;=============================================================================
;	FRE FUNCTION AND INTEGER TO FLOATING ROUTINES
; ## BISTRS.ASM:1096 ##
;
FRE	CALL	GETYPR
	JP	NZ,CLCDIF
	CALL	FREFAC		;FREE UP ARGUMENT AND SETUP
				;TO GIVE FREE STRING SPACE
	CALL	GARBA2		;DO GARBAGE COLLECTION
CLCDIF	EX	DE,HL
	LD	HL,(STREND)
	EX	DE,HL
	LD	HL,(FRETOP)	;TOP OF FREE AREA
	JP	GIVDBL		;RETURN [H,L]-[D,E]


;=============================================================================
;	SCNEDT  Screen Oriented Editor for GW-BASIC
; ## SCNEDT.ASM ##
;
;	PRINT "?" BEFORE GETTING INPUT
;	(not ref'd)
;
;	THIS IS THE LINE INPUT ROUTINE
;	IT READS CHARACTERS INTO BUF USING _ AS THE
;	CHARACTER DELETE CHARACTER AND @ AS THE LINE DELETE CHARACTER
;	IF MORE THAN BUFLEN CHARACTER ARE TYPED, NO ECHOING
;	IS DONE UNTIL A  _ @ OR CARRIAGE-RETURN IS TYPED.
;	CONTROL-G WILL BE TYPED FOR EACH EXTRA CHARACTER.
;	THE ROUTINE IS ENTERED AT INLIN, AT QINLIN TO TYPE A QUESTION MARK AND A SPACE FIRST
;
QINLIN	LD	A,'?'		;GET A QMARK
	CALL	OUTDO		;TYPE IT
	LD	A,' '		;SPACE
	CALL	OUTDO		;TYPE IT TOO
	JP	PINLIN		;NO CRUNCHING IN THIS CASE

MORINP	CALL	INCHR		; Get character and test ^O
	CP	01H		; CTL-A  (enter in EDIT MODE?))
	JP	NZ,INLNC1	; NO, TREAT NORMALLY
	LD	(HL),00H	; SAVE TERMINATOR
	JP	PINLIN1		; GO EDIT FROM HERE

QINLIN0	LD	(HL),B		; STORE ZERO IN BUF

;	Line input (aka RINPUT)
PINLIN	XOR	A
	LD	(NXTKEY),A
	XOR	A
	LD	(TEMPA),A	; FLAG TO DO CR

;	INPUT STATEMENT (REDO)
SINLIN	CALL	INCHR		; Get character and test ^O
	CP	01H		; CTL-A to enter in EDIT mode
	JP	NZ,TTYLIN
PINLIN1	CALL	CRDO
	LD	HL,0FFFFH
	JP	EDTLI1

RUBOUT	LD	A,(RUBSW)
	OR	A
	LD	A,'\'
	LD	(RUBSW),A
	JP	NZ,ECHDEL
	DEC	B
	JP	Z,QINLIN0
	CALL	OUTDO
	INC	B
ECHDEL	DEC	B
	DEC	HL
	JP	Z,OTKLN
	LD	A,(HL)
	CALL	OUTDO
	JP	MORINP
	DEC	B		;Unused.   This was the entry point
				;for 'DELCHR' in previous BASIC versions.
DELCHR	DEC	HL
	CALL	OUTDO
	JP	NZ,MORINP
OTKLN	CALL	OUTDO		; Output character in A
	CALL	CRDO		; Output CRLF

;	aka	PINSTREAM
;	Accepts a line from a file or device
TTYLIN	LD	HL,BUF
	LD	B,01H
	PUSH	AF
	XOR	A
	LD	(RUBSW),A
	POP	AF
INLNC1	LD	C,A
	CP	7FH		;RUBOUT ?
	JP	Z,RUBOUT
	LD	A,(RUBSW)
	OR	A
	JP	Z,TTYLIN1
	LD	A,'\'
	CALL	OUTDO
	XOR	A
	LD	(RUBSW),A
TTYLIN1	LD	A,C
	CP	07H
	JP	Z,PUTCTL
	CP	03H		;CTL-C ?
	CALL	Z,KILIN
	SCF
	RET	Z
	CP	0DH
	JP	Z,NEXTLIN
	CP	09H		;Is it TAB ?
	JP	Z,PUTCTL
	CP	0AH
	JP	NZ,TTYLIN2
	DEC	B
	JP	Z,PINLIN	;Clear typeahead before SINLIN
	INC	B
	JP	PUTCTL

TTYLIN2	CP	15H		;Is it control "U"?
	CALL	Z,KILIN		;Yes - Get another line (wipe current buffer)
	JP	Z,PINLIN
	CP	08H		;Is it delete (backspace: ctl-H) ?
	JP	NZ,NODELET	;No, skip over
	DEC	B
	JP	Z,SINLIN
	CALL	OUTDO
	LD	A,' '
	CALL	OUTDO
	LD	A,08H
	JP	DELCHR

	;NO_DELETE
NODELET	CP	18H		;CTL-X ?
	JP	NZ,TTYLIN3
	LD	A,'#'		;Print '#' to confirm
	JP	OTKLN		;.. and remove the current line being inserted

TTYLIN3	CP	12H		;Is it control "R"?
	JP	NZ,PUTBUF	;No - Put in buffer
	PUSH	BC		;Save buffer length
	PUSH	DE		;Save DE
	PUSH	HL		;Save buffer address
	LD	(HL),00H	;Mark end of buffer
	CALL	CRDO		;do CRLF
	LD	HL,BUF		;Point to buffer start
	CALL	OUTSTRZ		;Output buffer
	POP	HL		;Restore buffer address
	POP	DE		;Restore DE
	POP	BC		;Restore buffer length
	JP	MORINP		;Get another character

PUTBUF	CP	' '		;Is it a control code?
	JP	C,MORINP	;Yes - Ignore

PUTCTL	LD	A,B		;Get number of bytes in buffer
	OR	A
	JP	NZ,PUTBUF0
	PUSH	HL
	LD	HL,(PTRFIL)
	LD	A,H		;Test for line overflow
	OR	L
	POP	HL
	LD	A,07H		;CTRL-G: Set a bell
	JP	Z,OUTIT		;Ring bell if buffer full
	LD	HL,BUF
	CALL	LINGET
	EX	DE,HL
	LD	(CURLIN),HL
	JP	LBOERR

PUTBUF0	LD	A,C
	LD	(HL),C
	INC	HL
	INC	B
OUTIT	CALL	OUTDO
	SUB	0AH
	JP	NZ,MORINP
	LD	(TTYPOS),A
	LD	A,0DH
	CALL	OUTDO
PUTBUF1	CALL	INCHR
	OR	A
	JP	Z,PUTBUF1
	CP	0DH
	JP	Z,MORINP
	JP	INLNC1

NEXTLIN	LD	A,(TEMPA)
	OR	A
	JP	Z,FININL
	XOR	A
	LD	(HL),A
	LD	HL,BUFMIN	;"," ..
	RET

;	SCAN SEMICOLON FOR NO-CR
SCNSEM	PUSH	AF
	LD	A,00H
	LD	(TEMPA),A
	POP	AF
	CP	';'
	RET	NZ
	LD	(TEMPA),A
	JP	CHRGTR


;=============================================================================
;	FIVEO 5.0 FEATURES -WHILE/WEND, CALL, CHAIN, WRITE /P. ALLEN
; ## FIVEO.ASM:10 ##
;
;	This code handles the statements WHILE/WEND
;	The 8080 stack is used to put an entry on for each active WHILE
;	the same way active GOSUB and FOR entries are made.
;	The format is as follows:
;	      $WHILE - the token identifying the entry (1 byte)
;	      A text pointer at the character after the WEND of the WHILE body (2 bytes)
;	      A text pointer at the character after the WHILE of the WHILE body (2 bytes)
;	      The line number of the line that the WHILE is on (2 bytes)
;
;	      Total   7 bytes
;
WHILE:				;KEEP THE WHILE TEXT POINTER HERE
	LD	(ENDFOR),HL	;SAVE TEXT ADDRESS
	CALL	WNDSCN		;SCAN FOR THE MATCHING WEND
				;CAUSE AN ERRWH IF NO WEND TO MATCH
	CALL	CHRGTR		;POINT AT CHARACTWER AFTER WEND
	EX	DE,HL		;POSITION OF MATCHING WEND
	CALL	FNDWND		;SEE IF THERE IS A STACK ENTRY FOR THIS WHILE
	INC	SP		;GET RID OF THE NEWSTT ADDRESS ON THE STACK
	INC	SP
	JP	NZ,WNOTOL	;IF NO MATCH NO NEED TO TRUNCATE THE STACK
	ADD	HL,BC		;ELIMINATE EVERYTHING UP TO AND INCLUDING
				;THE MATCHING WHILE ENTRY
	LD	SP,HL
	LD	(SAVSTK),HL
WNOTOL	LD	HL,(CURLIN)	;MAKE THE STACK ENTRY
	PUSH	HL
	LD	HL,(ENDFOR)	;GET TEXT POINTER FOR WHILE BACK
	PUSH	HL
	PUSH	DE		;SAVE THE WEND TEXT POINTER
	JP	FNWEND		;FINISH USING WEND CODE

WEND	JP	NZ,SNERR	;STATEMENT HAS NO ARGUMENTS
	EX	DE,HL		;FIND MATCHING WHILE ENTRY ON STACK
	CALL	FNDWND
	JP	NZ,WEERR	;MUST MATCH OR ELSE ERROR
	LD	SP,HL		;TRUNCATE STACK AT MATCH POINT
	LD	(SAVSTK),HL	;[H,L] POINTING INTO STACK ENTRY
	EX	DE,HL
	LD	HL,(CURLIN)	;REMEMBER WEND LINE #
	EX	DE,HL
	EX	DE,HL
	LD	(NXTLIN),HL	;IN NXTLIN
	EX	DE,HL
	INC	HL		;INDEX INTO STACK ENTRY TO GET VALUES
	INC	HL		;SKIP OVER TEXT POINTER OF WEND
	LD	E,(HL)		;SET [D,E]=TEXT POINTER OF WHILE
	INC	HL
	LD	D,(HL)
	INC	HL
	LD	A,(HL)
	INC	HL
	LD	H,(HL)		;[H,L]=LINE NUMBER OF WHILE
	LD	L,A
	LD	(CURLIN),HL	;IN CASE OF ERROR OR CONTINUATION FIX CURLIN
	EX	DE,HL		;GET TEXT POINTER OF WHILE FORMULA INTO [H,L]
FNWEND	CALL	FRMEVL		;EVALUATE FORMULA
	PUSH	HL		;SAVE TEXT POINTER
	CALL	VSIGN		;GET IF TRUE OR FALSE
	POP	HL		;GET BACK WHILE TEXT POINTER
	JP	Z,FLSWHL	;GO BACK AT WEND IF FALSE
	LD	BC,WHILETK	;COMPLETE WHILE ENTRY
	LD	B,C		;NEED IT IN THE HIGH BYTE
	PUSH	BC
	INC	SP		;ONLY USE ONE BYTE
	JP	NEWSTT

FLSWHL	LD	HL,(NXTLIN)	;SETUP CURLIN FOR WEND
	LD	(CURLIN),HL
	POP	HL		;TAKE OFF TEXT OF WEND AS NEW TEXT POINTER
	POP	BC		;GET RID OF TEXT POINTER OF WHILE
	POP	BC		;TAKE OFF LINE NUMBER OF WHILE
	JP	NEWSTT

;
;	This subroutine searches the stack for an WHILE entry
;	whose WEND text pointer matches [D,E]. It returns with zero true
;	if a match is found and zero false otherwise. FOR entries
;	are skipped over, but GOSUB entries are not.
;
;	WHLSIZ=7
;
;	Note - 8086 versions force stack entries to be an even length
;	so stack accesses won't cross word boundaries.  This is done
;	for speed.  To accomplish this, an extra byte is pushed on
;	top of the WHILE token.  This extra byte is NOT reflected in
;	the value of WHLSIZ but is taken care of by the code.
;
FNDWND	LD	HL,0004H	;SKIP OVER RETURN ADDRESS AND NEWSTT
	ADD	HL,SP
FNDWN2	LD	A,(HL)		;GET THE ENTRY TYPE
	INC	HL
	LD	BC,FORTK
	CP	C		;SEE IF ITS $FOR
	JP	NZ,FNDWN3	;TODO: OO12H vs. 0010H next line
	LD	BC,0010H	;Yes, so skip over it.  Note that
	ADD	HL,BC		;the pointer has already been
	JP	FNDWN2		;incremented once.

FNDWN3	LD	BC,WHILETK
	CP	C
	RET	NZ
	PUSH	HL
	LD	A,(HL)
	INC	HL
	LD	H,(HL)
	LD	L,A
	CALL	COMPAR		;CMP [BX],DX-SAME WEND?
	POP	HL		;Note that the pointer has
	LD	BC,0006H	;already been incremented once.
	RET	Z		;RETURN IF ENTRY MATCHES
	ADD	HL,BC
	JP	FNDWN2

	;WHILE without WEND
WEERR	LD	DE,ERRWE
	JP	ERROR


;-----------------------------------------------------------------------------
;	'CALL' BASIC command
;
;	This is the CALL <simple var>[(<simple var>[,<simple var>]..)]
;	Stragegy:
;
;	1.) Make sure subroutine name is simple var, get value & save it
;	2.) Allocate space on stack for param adresses
;	3.) Evaluate params & stuff pointers on stack
;	3.) POP off pointers ala calling convention
;	4.) CALL subroutine with return address on stack
;
;
;	Extended and Disk BASIC-80 user function calls may also be made
;	 with the CALL statement.
;	The calling sequence used is the same as that in Microsoft's
;	 FORTRAN, COBOL and BASIC compilers.
MAXPRM	EQU	32

CALLST	LD	A,80H		;Flag PTRGET not to allow arrays
	LD	(SUBFLG),A
	CALL	PTRGET		;Evaluate var pointer
	PUSH	HL		;Save text pointer
	EX	DE,HL		;Var pointer to [H,L]
	CALL	GETYPR		;Get type of var
	CALL	VMOVFM		;Store value in FAC
	CALL	FRQINT		;Evaluate var
	LD	(TEMPA),HL	;Save it
	LD	C,MAXPRM	;Check to see if we have space for max parm block
	CALL	GETSTK		; Check for C levels on stack
	POP	DE		;Get text pointer off stack
	LD	HL,-MAXPRM*2	;Get space on stack for parms
	ADD	HL,SP
	LD	SP,HL		;Adjust stack
	EX	DE,HL		;Put text pointer in [H,L], stack pointer in [D,E]
	LD	C,MAXPRM	;Get # of params again
	DEC	HL		;Back up text pointer
	CALL	CHRGTR		;Get char
	LD	(TEMP),HL	;Save text pointer
	JP	Z,CALLST3	;If end of line, GO!   (jp if no parameters)
	CALL	SYNCHR		;Eat left paren
	DB	'('
CALLST1	PUSH	BC		;Save count
	PUSH	DE		;Save pointer into stack
	CALL	PTRGET		;Evaluate param address
	EX	(SP),HL		;Save text pointer get pointer into stack
	LD	(HL),E		;Save var address on stack
	INC	HL
	LD	(HL),D
	INC	HL
	EX	(SP),HL		;Save back var pointer, get text pointer
	POP	DE
	POP	BC
	LD	A,(HL)		;Look at terminator
	CP	','		;Comma?
	JP	NZ,CALLST2	;Test
	DEC	C		;Decrement count of params
	CALL	CHRGTR		;Get next char
	JP	CALLST1		;Back for more

CALLST2	CALL	SYNCHR		;check for terminating right paren
	DB	')'		; )
	LD	(TEMP),HL	;save text pointer
	LD	A,MAXPRM+1	;Calc # of params
	SUB	C
	POP	HL		;At least one, get its address in [H,L]
	DEC	A		;Was it one?
	JP	Z,CALLST3	;Yes
	POP	DE		;Next address in [D,E]
	DEC	A		;Two?
	JP	Z,CALLST3	;Yes
	POP	BC		;Final in [B,C]
	DEC	A		;Three?
	JP	Z,CALLST3	;Yes
	PUSH	BC		;Save back third parm
	PUSH	HL		;Save back first
	LD	HL,0002H	;Point to rest of parm list
	ADD	HL,SP
	LD	B,H		;Get into [B,C]
	LD	C,L
	POP	HL		;Restore parm three
CALLST3	PUSH	HL		;Save parm three
	LD	HL,CALLST4	;Where subroutines return
	EX	(SP),HL		;Put it on stack, get back parm three
	PUSH	HL		;Save parm three
	LD	HL,(TEMPA)	;Get subroutine address
	EX	(SP),HL		;Save, get back parm three
	RET			;Dispatch to subroutine

CALLST4	LD	HL,(SAVSTK)	;Restore stack to former state
	LD	SP,HL
	LD	HL,(TEMP)	;Get back text poiner
	JP	NEWSTT		;Get next statement


;-----------------------------------------------------------------------------
;	CHAIN
; ## FIVEO.ASM 165 ##
;
;	This is the code for the CHAIN statement
;	The syntax is:
;	CHAIN [MERGE]<file name>[,[<line number>][,ALL][,DELETE <range>]]
;	The steps required to execute a CHAIN are:
;
;	1.) Scan arguments
;
;	2.) Scan program for all COMMON statements and
;	    mark specified variables.
;
;	3.) Squeeze unmarked entries from symbol table.
;
;	4.) Copy string literals to string space
;
;	5.) Move all simple variables and arrays into the
;           bottom of string space.
;
;	6.) Load new program
;
;	7.) Move variables back down positioned after program.
;
;	8.) Run program
CHAIN	XOR	A		;Assume no MERGE
	LD	(MRGFLG),A
	LD	(MDLFLG),A	;Also no MERGE w/ DELETE option
	LD	A,(HL)		;Get current char
	LD	DE,MERGETK	;Is it MERGE?
	CP	E		;Test
	JP	NZ,NTCHNM	;NO
	LD	(MRGFLG),A	;Set MERGE flag
	INC	HL
NTCHNM	DEC	HL		;Rescan file name
	CALL	CHRGTR
	CALL	PRGFLI		;Evaluate file name and OPEN it
	PUSH	HL		;Save text pointer
	LD	HL,0000H	;Get zero
	LD	(CHNLIN),HL	;Assume no CHAIN line #
	POP	HL		;Restore text pointer
	DEC	HL		;Back up pointer
	CALL	CHRGTR		;Scan char
	JP	Z,NTCHAL	;No line number etc.
	CALL	SYNCHR
	DB	','		;Must be comma
	CP	','		;Omit line # (Use ALL for instance)
	JP	Z,NTLINF	;YES
	CALL	FRMEVL		;Evaluate line # formula
	PUSH	HL		;Save text poiner
	CALL	FRQINT		;Force to int in [H,L]
	LD	(CHNLIN),HL	;Save it for later
	POP	HL		;Restore text poiner
	DEC	HL		;Rescan last char
	CALL	CHRGTR
	JP	Z,NTCHAL	;No ALL i.e. preserve all vars across CHAIN
NTLINF	CALL	SYNCHR
	DB	','		;Should be comma here
	LD	DE,DELETETK	;Test for DELETE option
	CP	E		;Is it?
	JP	Z,CHMWDL	;Yes
	CALL	SYNCHR
	DB	'A'		;Check for "ALL"
	CALL	SYNCHR
	DB	'L'
	CALL	SYNCHR
	DB	'L'
	JP	Z,DNCMDA	;Goto step 3
	CALL	SYNCHR
	DB	','		;Force comma to appear
	CP	E		;Must be DELETE
	JP	NZ,SNERR	;No, give error
	OR	A		;Flag to goto DNCMDA
CHMWDL	PUSH	AF		;Save ALL flag
	LD	(MDLFLG),A	;Set MERGE w/ DELETE
	CALL	CHRGTR		;Get char after comma
	CALL	SCNLIN		;Scan line range
	PUSH	BC
	CALL	DEPTR		;Change pointers back to numbers
	POP	BC
	POP	DE		;Pop max line off stack
	PUSH	BC		;Save pointer to start of 1st line
	LD	H,B		;Save pointer to start line
	LD	L,C
	LD	(CMSPTR),HL
	CALL	FNDLIN		;Find the last line
	JP	NC,FCERRG	;Must have exact match on end of range
	LD	D,H		;[D,E] =  pointer at the start of the line
	LD	E,L		;beyond the last line in the range
	LD	(CMEPTR),HL	;Save pointer to end line
	POP	HL		;Get back pointer to start of range
	CALL	COMPAR		;Make sure the start comes before the end
FCERRG	JP	NC,FCERR	;If not, "Illegal function call"
	POP	AF		;Flag that says whether to go to DNCMDA
	JP	NZ,DNCMDA	;"ALL" option was present
NTCHAL	LD	HL,(CURLIN)	;Save current line number on stack
	PUSH	HL
	LD	HL,(TXTTAB)	;Start searching for COMMONs at program start
	DEC	HL		;Compensate for next instr
CLPSC1	INC	HL		;Look at first char of next line
	LD	A,(HL)		;Get char from program
	INC	HL
	OR	(HL)		;Are we pointing to program end?
	JP	Z,CLPFIN	;Yes
	INC	HL
	LD	E,(HL)		;Get line # in [D,E]
	INC	HL
	LD	D,(HL)
	EX	DE,HL
	LD	(CURLIN),HL	;Save current line # in CURLIN for errors
	EX	DE,HL
CSTSCN	CALL	CHRGTR		;Get statement type
AFTCOM	OR	A
	JP	Z,CLPSC1	;EOL Scan next one
	CP	':'		;Are we looking at colon
	JP	Z,CSTSCN	;Yes, get next statement
	LD	DE,COMMONTK	;Test for COMMON, avoid byte externals
	CP	E		;Is it a COMMON?
	JP	Z,DOCOMM	;Yes, handle it
	CALL	CHRGTR		;Get first char of statement
	CALL	DATA		;Skip over statement
	DEC	HL		;Back up to rescan terminator
	JP	CSTSCN		;Scan next one

DOCOMM	CALL	CHRGTR		;Get thing after COMMON
	JP	Z,AFTCOM	;Get next thing
NXTCOM	PUSH	HL		;Save text pointer
	LD	A,01H		;Call PTRGET to search for array
	LD	(SUBFLG),A
	CALL	PTRGTN		;This subroutine in F3 scans variables
	JP	Z,FNDAAY	;Found array
	LD	A,B		;Try finding array with COMMON bit set
	OR	80H
	LD	B,A
	XOR	A		;Set zero CC
	CALL	ERSFIN		;Search array table
	LD	A,00H		;Clear SUBFLG in all cases
	LD	(SUBFLG),A
	JP	NZ,NTFN2T	;Not found, try simple
	LD	A,(HL)		;Get terminator, should be "("
	CP	'('		;Test
	JP	NZ,SCNSMP	;Must be simple then
	POP	AF		;Get rid of saved text pointer
	JP	COMADY		;Already was COMMON, ignore it

NTFN2T	LD	A,(HL)		;Get terminator
	CP	'('		;Array specifier?
	JP	Z,FCERR		;Yes, undefined array - ?FC Error
SCNSMP	POP	HL		;Rescan variable name for start
	CALL	PTRGTN		;Evaluate as simple
	LD	A,D		;If var not found, [D,E]=0
	OR	E
	JP	NZ,COMFNS	;Found it
	LD	A,B		;Try to find in COMMON
	OR	80H		;Set COMMON bit
	LD	B,A
	LD	DE,COMPT2	;push on return address
	PUSH	DE
	LD	DE,PTRGTR	;address to common return point
	PUSH	DE
	LD	A,(VALTYP)	;Must have VALTYP in [D]
	LD	D,A
	JP	NOARYS		;Search symbol table

COMPT2	LD	A,D		;Found?
	OR	E
	JP	Z,FCERR		;No, ?FC Error
COMFNS	PUSH	HL		;Save text pointer
	LD	B,D		;Get pointer to var in [B,C]
	LD	C,E
	LD	HL,BCKUCM	;Loop back here
	PUSH	HL
CBAKBL	DEC	BC		;Point at first char of rest
LPBKNC	LD	A,(BC)		;Back up until plus byte
	DEC	BC
	OR	A
	JP	M,LPBKNC
	LD	A,(BC)		;Now point to 2nd char of var name
	OR	80H		;set COMMON bit
	LD	(BC),A
	RET			;done

FNDAAY	LD	(SUBFLG),A	;Array found, clear SUBFLG
	LD	A,(HL)		;Make sure really array spec
	CP	'('		;Really an array?
	JP	NZ,SCNSMP	;No, scan as simp
	EX	(SP),HL		;Save text pointer, get rid of saved text pointer
	DEC	BC		;Point at last char of name extension
	DEC	BC
	CALL	CBAKBL		;Back up before variable and mark as COMMON
BCKUCM	POP	HL		;Restore text pointer
	DEC	HL		;Rescan terminator
	CALL	CHRGTR
	JP	Z,AFTCOM	;End of COMMON statement
	CP	'('		;End of COMMON array spec?
	JP	NZ,CHKCST	;No, should be comma
COMADY	CALL	CHRGTR		;Fetch char after paren
	CALL	SYNCHR
	DB	')'		;Right paren should follow
	JP	Z,AFTCOM
CHKCST	CALL	SYNCHR
	DB	','		;Force comma to appear here
	JP	NXTCOM		;Get next COMMON variable

; 	Step 3 - Squeeze..
CLPFIN	POP	HL		;Restore previous CURLIN
	LD	(CURLIN),HL
	EX	DE,HL
	LD	HL,(ARYTAB)	;End of simple var squeeze to [D,E]
	EX	DE,HL
	LD	HL,(VARTAB)	;Start of simps
CLPSLP	CALL	COMPAR		;Are we done?
	JP	Z,DNCMDS	;Yes done, with simps
	PUSH	HL		;Save where this simp is
	LD	C,(HL)		;Get VALTYP
	INC	HL
	INC	HL
	LD	A,(HL)		;Get COMMON bit
	OR	A		;Set minus if COMMON
	PUSH	AF		;Save indicator
	AND	7FH		;Clear COMMON bit
	LD	(HL),A		;Save back
	INC	HL
	CALL	IADAHL		;Skip over rest of var name
	LD	B,00H		;Skip VALTYP bytes
	ADD	HL,BC
	POP	AF		;Get indicator whether to delete
	POP	BC		;Pointer to where var started
	JP	M,CLPSLP
	PUSH	BC		;This is where we will resume scanning vars later
	CALL	VARDLS		;Delete variable
	LD	HL,(ARYTAB)	;Now correct ARYTAB by # of bytes deleted
	ADD	HL,DE		;Add negative difference between old and new
	LD	(ARYTAB),HL	;Save new ARYTAB
	EX	DE,HL		;To [D,E]
	POP	HL		;Get current place back in [H,L]
	JP	CLPSLP

VARDLS	EX	DE,HL		;Point to where var ends
	LD	HL,(STREND)	;One beyond last byte to move
DLSVLP	CALL	COMPAR		;Done?
	LD	A,(DE)		;Grab byte
	LD	(BC),A		;Move down
	INC	DE		;Increment pointers
	INC	BC
	JP	NZ,DLSVLP
	LD	A,C		;Get difference between old and new
	SUB	L		;Into [D,E] ([D,E]=[B,C]-[H,L])
	LD	E,A
	LD	A,B
	SBC	A,H
	LD	D,A
	DEC	DE		;Correct # of bytes
	DEC	BC		;Moved one too far
	LD	H,B		;Get new STREND [H,L]
	LD	L,C
	LD	(STREND),HL	;Store it
	RET

DNCMDS	EX	DE,HL
	LD	HL,(STREND)	;Limit of array search
	EX	DE,HL
CLPAKP	CALL	COMPAR		;Done?
	JP	Z,DNCMDA	;Yes
	PUSH	HL		;Save pointer to VALTYP
	INC	HL		;Move down to COMMON bit
	INC	HL
	LD	A,(HL)		;Get it
	OR	A		;Set CC's
	PUSH	AF		;Save COMMON indicator
	AND	7FH		;Clear COMMON bit
	LD	(HL),A		;Save back
	INC	HL		;Point to length of array
	CALL	IADAHL		;Add length of var name
	LD	C,(HL)		;Get length of array in [B,C]
	INC	HL
	LD	B,(HL)
	INC	HL
	ADD	HL,BC		;[H,L] now points after array
	POP	AF		;Get back COMMON indicator
	POP	BC		;Get pointer to start of array
	JP	M,CLPAKP	;COMMON, dont delete!
	PUSH	BC		;Save so we can resume
	CALL	VARDLS		;Delete the array
	EX	DE,HL		;Returns with STREND in HL, so put in DE
	POP	HL		;Get back pointer to the next array
	JP	CLPAKP		;Check next array

;	Step 4 - Copy literals into string space
;	This code is very similar to the string garbage collect code
;	If BIGSTR is on, we also have to fix up the string back pointers.
DNCMDA	LD	HL,(VARTAB)	;Look at simple strings
CSVAR	EX	DE,HL
	LD	HL,(ARYTAB)	;Limit of search to [D,E]
	EX	DE,HL
	CALL	COMPAR		;Done?
	JP	Z,CAYVAR	;Yes
	LD	A,(HL)		;Skip name, Z if was a string
	INC	HL
	INC	HL
	INC	HL
	PUSH	AF
	CALL	IADAHL
	POP	AF
	CP	03H
	JP	NZ,CSKPVA	;Skip this var, not string
	CALL	CDVARS		;Copy this guy into string space if nesc
	XOR	A		;CDVARS has already incremented [H,L]
CSKPVA	LD	E,A
	LD	D,00H		;Add length of VALTYP
	ADD	HL,DE
	JP	CSVAR

CAYVA2	POP	BC		;Adjust stack
CAYVAR	EX	DE,HL
	LD	HL,(STREND)	;New limit of search
	EX	DE,HL
	CALL	COMPAR		;Done?
	JP	Z,DNCCLS	;Yes
	LD	A,(HL)		;Skip name, Z if was a string
	INC	HL
	INC	HL
	PUSH	AF
	INC	HL
	CALL	IADAHL
	LD	C,(HL)		;Get length of array
	INC	HL
	LD	B,(HL)		;Into [B,C]
	INC	HL
	POP	AF		;Get back VALTYP
	PUSH	HL		;Save pointer to array element
	ADD	HL,BC		;Point after array
	CP	03H		;String array?
	JP	NZ,CAYVA2	;No, look at next one
	LD	(TEMP3),HL	;Save pointer to end of array
	POP	HL		;Get back pointer to array start
	LD	C,(HL)		;Pick up number of DIMs
	LD	B,00H		;Make double with high zero
	ADD	HL,BC		;Go past DIMS
	ADD	HL,BC
	INC	HL		;One more to account for # of DIMs
CAYSTR	EX	DE,HL
	LD	HL,(TEMP3)	;Get end of array
	EX	DE,HL
	CALL	COMPAR		;See if at end of array
	JP	Z,CAYVAR	;Get next array
	LD	BC,CAYSTR	;Do next str in array
	PUSH	BC		;Save branch address on stack

CDVARS	LD	A,(HL)		;Get length of array entry
	INC	HL		;Also pick up pointer into [D,E]
	LD	E,(HL)		;Get data pointer
	INC	HL
	LD	D,(HL)
	INC	HL		;Set CC's on length
	OR	A
	RET	Z		;Ignore null strings
	PUSH	HL		;Save where we are
	LD	HL,(VARTAB)	;Is string in program text or disk buffers?
	CALL	COMPAR		;Compare
	POP	HL		;Restore where we are
	RET	C		;No, must be in string space
	PUSH	HL		;save where we are again.
	LD	HL,(TXTTAB)	;is it in buffers?
	CALL	COMPAR		;test
	POP	HL		;Restore where we are
	RET	NC		;in buffers, do nothing
	PUSH	HL		;Save where we are for nth time
	DEC	HL		;Point to start of descriptor
	DEC	HL
	DEC	HL
	PUSH	HL		;Save pointer to start
	CALL	STRCPY		;Copy string into DSCTMP
	POP	HL		;Destination in [H,L], source in [D,E]
	LD	B,03H		;# of bytes to move
	CALL	MOVE1		;Move em
	POP	HL		;Where we are
	RET

;	Step 5 - Move stuff up into string space!
DNCCLS	CALL	GARBA2		;Get rid of unused strings
	LD	HL,(STREND)	;Load end of vars
	LD	B,H		;Into [B,C]
	LD	C,L
	EX	DE,HL
	LD	HL,(VARTAB)	;Start of simps into [D,E]
	EX	DE,HL
	LD	HL,(ARYTAB)
	LD	A,L		;Get length of simps in [H,L]
	SUB	E
	LD	L,A
	LD	A,H
	SBC	A,D
	LD	H,A
	LD	(TEMP9),HL	;Save here
	LD	HL,(FRETOP)	;Destination of high byte
	LD	(SAVFRE),HL	;Save FRETOP to restore later
	CALL	BLTUC		;Move stuff up
	LD	H,B		;Now adjust top of memory below saved vars
	LD	L,C
	DEC	HL		;One lower to be sure
	LD	(FRETOP),HL	;Update FRETOP to reflect new value
	LD	A,(MDLFLG)	;MERGE w/ DELETE?
	OR	A		;Test
	JP	Z,NTMDLT	;No
	LD	HL,(CMSPTR)	;Start of lines to delete
	LD	B,H		;Into [B,C]
	LD	C,L
	LD	HL,(CMEPTR)	;End of lines to delete
	CALL	DEL		;Delete the lines
	LD	(ARYTAB),HL	;***also set up ARYTAB and STREND
	LD	(STREND),HL	;in case we get error in CHAIN
				;because of file lookup and then have to
				;look at variables later (shouldnt be any)
				;***PGA 7/7/81
	CALL	LINKER		;Re-link lines just in case

;	Step 6 - load new program
NTMDLT	LD	A,01H		;Set CHAIN flag
	LD	(CHNFLG),A
	LD	A,(MRGFLG)	;MERGEing?
	OR	A		;Set cc'S
	JP	NZ,OKGETM	;Do MERGE
	LD	A,(NFILES)
	LD	(NFILSSV),A	;Save NFILES
	JP	CHNENT		;Jump to LOAD code

;	Step 7 - Move stuff back down
CHNRET	XOR	A		;Clear CHAIN, MERGE flags
	LD	(CHNFLG),A
	LD	(MRGFLG),A
	LD	HL,(VARTAB)	;Get current VARTAB
	LD	B,H		;Into [B,C]
	LD	C,L
	LD	HL,(TEMP9)	;Get length of simps
	ADD	HL,BC		;Add to present VARTAB to get new ARYTAB
	LD	(ARYTAB),HL
	LD	HL,(FRETOP)	;Where to start moving
	INC	HL		;One higher
	EX	DE,HL		;Into [D,E]
	LD	HL,(SAVFRE)	;Last byte to move
	LD	(FRETOP),HL	;Restore FRETOP from this
MVBKVR	CALL	COMPAR		;Done?
	LD	A,(DE)		;Move byte down
	LD	(BC),A
	INC	DE		;Increment pointers
	INC	BC
	JP	NZ,MVBKVR
	DEC	BC		;Point to last var byte
	LD	H,B		;[H,L]=last var byte
	LD	L,C
	LD	(STREND),HL	;This is new end
	EX	DE,HL
	LD	HL,(CHNLIN)	;Get CHAIN line # in [D,E]
	EX	DE,HL
	LD	HL,(TXTTAB)	;Get prog start in [H,L]
	DEC	HL		;Point at zero before program
	LD	A,D
	OR	E		;line number zero?
	JP	Z,NEWSTT	;line #=0, go...
	CALL	FNDLIN		;Try to find destination line
	JP	NC,USERR	;Not there...
	DEC	BC		;Point to zero on previous line
	LD	H,B		;Make text pointer for NEWSTT
	LD	L,C
	JP	NEWSTT		;Bye...

	;UNREF'D !
COMMON	JP	DATA


;-----------------------------------------------------------------------------
;	WRITE
; ## FIVEO.ASM 716 ##
;
WRITE	LD	C,02H		;Setup output file
	CALL	FILGET
	DEC	HL
	CALL	CHRGTR		;Get another character
	JP	Z,WRTFIN	;Done with WRITE
WRTMLP	CALL	FRMEVL		;Evaluate formula
	PUSH	HL		;Save the text pointer
	CALL	GETYPR		;See if we have a string
	JP	Z,WRTSTR	;We do
	CALL	FOUT		;Convert to a string
	CALL	STRLIT		;Literalize string
	LD	HL,(FACLO)	;Get pointer to string
	INC	HL		;Point to address field
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	LD	A,(DE)		;Is number positive?
	CP	' '		;Test
	JP	NZ,WRTNEG	;No, must be negative
	INC	DE
	LD	(HL),D
	DEC	HL
	LD	(HL),E
	DEC	HL
	DEC	(HL)		;Adjust length of string (length.LT.255 so OK)
WRTNEG	CALL	STRPRT		;Print the number
NXTWRV	POP	HL		;Get back text pointer
	DEC	HL		;Back up pointer
	CALL	CHRGTR		;Get next char
	JP	Z,WRTFIN	;end
	CP	';'		;Semicolon?
	JP	Z,WASEMI	;Was one
	CALL	SYNCHR
	DB	','		;Only possib left is comma
	DEC	HL		;to compensate for later CHRGET
WASEMI	CALL	CHRGTR		;Fetch next char
	LD	A,','		;put out comma
	CALL	OUTDO
	JP	WRTMLP		;Back for more

WRTSTR	LD	A,'"'		;put out double quote
	CALL	OUTDO		;Send it
	CALL	STRPRT		;print the string
	LD	A,'"'		;Put out another double quote
	CALL	OUTDO		;Send it
	JP	NXTWRV		;Get next value

WRTFIN	PUSH	HL		;Save text pointer
	LD	HL,(PTRFIL)	;See if disk file
	LD	A,H
	OR	L
	JP	Z,NTRNDW	;No
	LD	A,(HL)
	CP	03H		;Random?
	JP	NZ,NTRNDW	;NO
	CALL	CMPFBC		;See how many bytes left
	LD	A,L		;do subtract
	SUB	E
	LD	L,A
	LD	A,H
	SBC	A,D
	LD	H,A
				;Number of bytes in CR/LF sequence
	LD	DE,0FFFEH	;Subtract bytes in <cr>
	ADD	HL,DE
	JP	NC,NTRNDW	;Not enough, give error eventually
CRLFSQ	LD	A,' '		;Put out spaces
	CALL	OUTDO		;Send space
	DEC	HL		;Count down
	LD	A,H		;Count down
	OR	L
	JP	NZ,CRLFSQ
NTRNDW	POP	HL		;Restore [H,L]
	CALL	CRDO		;Do crlf
	JP	FINPRT


;=============================================================================
;	BASIC-86 INTERPRETER DEVICE INDEPENDENT I/O MODULE
; ## GIO86.ASM ###
;-----------------------------------------------------------------------------
;	Misc. Parsing Routines
FILINP	LD	C,01H		;MUST BE SEQUENTIAL INPUT
FILGET	CP	'#'		;NUMBER SIGN THERE?
	RET	NZ		;NO, NOT FILE INPUT
	PUSH	BC		;SAVE EXPECTED MODE
	CALL	FILSCN		;READ AND GET POINTER
				;ERROR IF FILE NOT OPEN
	POP	DE		;[DL]=FILE MODE
	CP	E		;IS IT RIGHT?
	JP	Z,GDFILM	;GOOD FILE MODE
	CP	03H		;ALLOW STUFF WITH RANDOM FILES
	JP	NZ,DERBFM	;IF NOT, "BAD FILE MODE"
GDFILM	CALL	SYNCHR
	DB	','		;GO PAST THE COMMA
FILSET	LD	D,B		;SETUP PTRFIL
	LD	E,C
	EX	DE,HL
	LD	(PTRFIL),HL
	EX	DE,HL
	RET

;	FILSCN - parse file number
;	Entry - [BX]=text pointer
;	Exit  - [DL]=file number, [SI], [CX] point to file data block for file [DL]
;		[AL]=file mode, FLAGS.Z is set if file is not open.
;		note - if file is not open, no FDB exists
FILSCN	DEC	HL
	CALL	CHRGTR
	CP	'#'
	CALL	Z,CHRGTR
	CALL	FRMEVL		;Formula evaluator

;	FACFPT - Transform file number into File-Data-Block pointer
;	Entry - [FAC] = file number (0..n)
;	Exit  - if File-Data-Block is allocated,
;	           SI points to 1st byte of File-Data-Block
;	        else FLAGS.Z is true
;	        DX, AX are used
;
FACFPT	CALL	CONINT		;[AL] = file number
				;fall into FDBPTR

;	FDBPTR - Transform file number into File-Data-Block pointer
;	Entry - [AL] = file number (0..n)
;	Exit  - if File-Data-Block is allocated,
;	           SI points to 1st byte of File-Data-Block
;	        else FLAGS.Z is true
;	        All other registers are preserved
;
FDBPTR	LD	E,A
;	FDBPTR with file number in E
FILID2	LD	A,(NFILES)	;Number of files
	CP	E
	JP	C,DERBFN	;Bad File Number
	LD	D,00H
	PUSH	HL
	LD	HL,FDBTAB	;Table of up to 16 FDB's
	ADD	HL,DE
	ADD	HL,DE
	LD	C,(HL)
	INC	HL
	LD	B,(HL)
	LD	A,(BC)
	OR	A
	POP	HL
	RET

;	GETPTR is called from VARPTR(#<expression>)
;	Entry - [AL]=file number
;	Exit  - [DX] points to random file buffer, or sector buffer of file
;
GETPTR	CALL	FILID2		;FDBPTR with file number in E
	LD	HL,0029H
	CP	03H
	JP	NZ,NTFIVD
	LD	HL,00B2H	;Return pointer to MODE
NTFIVD	ADD	HL,BC
	EX	DE,HL		;Return result in [DE]
	RET


;=============================================================================
;	DSKCOM - - COMMON ROUTINES FOR DISK BASICS
; ## DSKCOM.ASM ##
;
;	CONVERSION ROUTINES
;
MKI$	LD	A,02H		;VALUE TYPE FOR INTEGER AND NUMBER
				;OF CHARACTERS RESULT WILL NEED
	DB	01H		;SKIP NEXT TWO BYTES WITH "LXI  B,"
MKS$	LD	A,04H		;VALUE TYPE OF SINGLE PRECISION
	DB	01H		;SKIP NEXT TWO BYTES
MKD$	LD	A,08H		;VALUE TYPE OF DOUBLE-PRECISION
	PUSH	AF		;SAVE THE NUMBER OF BYTES OF
				;STRING SPACE WE NEED
	CALL	DOCNVF		;CONVERT FAC TO PROPER TYPE
	POP	AF		;GET THE NUMBER OF BYTES NEEDED
	CALL	STRINI		;GET A PLACE FOR THE STRING DATA
	LD	HL,(DSCTMPI)	;POINT TO THE PLACE TO STORE THE DATA
	CALL	VMOVMF		;MOVE THE FAC VALUE INTO THE STRING CREATION
	JP	FINBCK

CVI	LD	A,01H		;SET [A] TO BE VALTYP-1
	DB	01H		;SKIP THE NEXT TWO BYTES WITH "LXI B,"
CVS	LD	A,03H		;ALSO SET [A] TO NUMBER OF CHARACTERS REQUIRED -1
	DB	01H		;SKIP THE NEXT TWO BYTES
CVD	LD	A,07H		;DOUBLE PRECISION VALUE TYPE -1
	PUSH	AF		;SAVE THE VALTYP
	CALL	FRESTR		;MAKE SURE THE ARGUMENT IS A STRING
				;AND GET A POINTER TO THE DESCRIPTOR
	POP	AF		;GET BACK NUMBER OF CHARACTERS REQUIRED-1
	CP	(HL)		;MAKE SURE THE STRING IS LONGER THAN THAT
	JP	NC,FCERR	;IF NOT, "ILLEGAL FUNCTION CALL"
	INC	A		;[A]=TRUE VALUE TYPE
	LD	(VALTYP),A	;SETUP VALUE TYPE FOR MOVE
	INC	HL
	LD	A,(HL)		;[H,L]=POINTER AT STRING DATA
	INC	HL		;AND FOR IDENTIFICATION
	LD	H,(HL)
	LD	L,A
	JP	VMOVFM


;-----------------------------------------------------------------------------
;	READ ITEMS FROM A SEQUENTIAL FILE
;
FILIND	CALL	GETYPR		;SEE IF INPUT IS STRING OR NUMBER
	LD	BC,DOASIG	;RETURN ADDRESS TO SETUP [FAC]
	LD	DE,2C20H	;SETUP TERMINATORS SPACE AND COMMA
	JP	NZ,INPDOR	;IF NUMERIC, GO READ THE FILE
	LD	E,D		;MAKE BOTH TERMINATORS COMMA
	JP	INPDOR		;GO READ THE FILE

;	Entry for line input and read code for item fetching from
;	sequential input files
DLINE	CALL	FILINP		;GET FILE NUMBER SET UP
	CALL	PTRGET		;READ STRING TO STORE INTO
	CALL	CHKSTR		;MAKE SURE IT WAS A STRING
	LD	BC,FINPRT	;RESET TO CONSOLE WHEN DONE READING
	PUSH	BC		;SAVE ON STACK
	PUSH	DE		;SAVE POINTER AT VARIABLE
	LD	BC,LETCON	;GOOD RETURN ADDRESS FOR ASSIGNMENT
	XOR	A		;SET A=0 FOR STRING VALUE TYPE
	LD	D,A		;ZERO OUT BOTH TERMINATORS
	LD	E,A
INPDOR	PUSH	AF		;SAVE VALUE TYPE
	PUSH	BC		;SAVE RETURN ADDRESS
	PUSH	HL		;SAVE POINTER AT DATA COMING IN
				;A DUMMY POINTER AT BUFMIN
NOTWNT	CALL	INDSKC		;Read a character from file PTRFIL
	JP	C,DERRPE	;Input past end
	CP	' '		;SKIP LEADING SPACES
	JP	NZ,NOTSPC1	;EXCEPT FOR LINE INPUT
	INC	D		;CHECK FOR LINEINPUT
	DEC	D
	JP	NZ,NOTWNT	;SKIP ANY NUMBER
NOTSPC1	CP	'"'		;QUOTED STRING COMING IN?
	JP	NZ,NOTQTE
	LD	B,A
	LD	A,E		;MUST BE INPUT OF A STRING
	CP	','		;WHICH HAS [E]=44 (",")
	LD	A,B		;QUOTE BACK INTO [A]
	JP	NZ,NOTQTE
	LD	D,B		;TERMINATORS ARE QUOTES ONLY
	LD	E,B
	CALL	INDSKC		;READ PAST QUOTATION
	JP	C,QUITSI	;IF EOF, ALL DONE
NOTQTE	LD	HL,BUF		;BUFFER FOR DATA
	LD	B,0FFH		;MAXIMUM NUMBER OF CHARACTERS (255)
LOPCRS	LD	C,A		;SAVE CHARACTER IN [C]
	LD	A,D		;CHECK FOR QUOTED STRING
	CP	'"'
	LD	A,C		;RESTORE CHARACTER
	JP	Z,NOTQTL	;DON'T IGNORE CR OR STOP ON LF
	CP	0DH		;CR?
	PUSH	HL		;SAVE DEST PTR. ON STACK
	JP	Z,ICASLF	;EAT LINE FEED IF ONE
	POP	HL		;RESTORE DEST. PTR.
	CP	0AH		;LF?
	JP	NZ,NOTQTL	;NO, TEST OTHER TERMINATORS
				;** 5/14/82 BUG FIX(MULTIPLE LF FOR UNQUOTED STRING)
	LD	C,A		;SAVE CURRENT CHAR
	LD	A,E		;GET TERMINATOR 2
	CP	','		;CHECK FOR COMMA (UNQUOTED STRING)
	LD	A,C		;RESTORE CHARACTER
	CALL	NZ,STRCHR	;IF NOT, STORE LF (?)
	CALL	INDSKC		;GET NEXT CHAR
	JP	C,QUITSI	;IF EOF, ALL DONE.
;	CP	0AH		;** 5/14/82 BUG FIX(MULTIPLE LF FOR UNQUOTED STRING)
;	JR	Z,LPISLF	;** 5/14/82 BUG FIX(MULTIPLE LF FOR UNQUOTED STRING)
	CP	0DH		;IS IT A CR?
	JP	NZ,NOTQTL	;IF NOT SEE IF STORE NORMALLY
	LD	A,E		;GET TERMINATOR
	CP	' '		;IS IT NUMERIC INPUT?
	JP	Z,LPCRGT	;IF SO, IGNORE CR, DONT PUT IN BUFFER
	CP	','		;IS IT NON-QUOTED STRING (TERM=,)
	LD	A,0DH		;GET BACK CR.
	JP	Z,LPCRGT	;IF SO, IGNORE CR.
NOTQTL	OR	A		;IS CHAR ZERO
	JP	Z,LPCRGT	;ALWAYS IGNORE, AS IT IS TERMINATOR
				;FOR STRLIT (SEE QUIT2B)
	CP	D		;TERMINATOR ONE?
	JP	Z,QUITSI	;STOP THEN
	CP	E		;TERMINATOR TWO?
	JP	Z,QUITSI
	CALL	STRCHR		;SAVE THE CHAR
LPCRGT	CALL	INDSKC		;READ ANOTHER CHARACTER
	JP	NC,LOPCRS	;IF NOT, CHECK AS TERMINATOR
QUITSI	PUSH	HL		;SAVE PLACE TO STUFF ZERO
	CP	'"'		;STOPPED ON QUOTE?
	JP	Z,MORSPC	;DON'T SKIP SPACES THEN
				;BUT DO SKIP FOLLOWING COMMA OR
				;CRLF THOUGH
	CP	' '		;STOPPED ON SPACE?
	JP	NZ,NOSKCR	;NO, DON'T SKIP SPACES
				;OR ANY FOLLOWING COMMAS OR CRLFS EITHER
MORSPC	CALL	INDSKC		;READ SPACES
	JP	C,NOSKCR	;EOF, ALL DONE.
	CP	' '
	JP	Z,MORSPC
	CP	','		;COMMA?
	JP	Z,NOSKCR	;OK, SKIP IT
	CP	0DH		;CARRIAGE RETURN?
	JP	NZ,BAKUPT	;BACK UP PAST THIS CHARACTER
ICASLF	CALL	INDSKC		;READ ANOTHER
	JP	C,NOSKCR	;EOF, ALL DONE.
	CP	0AH		;LINE FEED?
	JP	Z,NOSKCR	;OK, SKIP IT TOO
BAKUPT	LD	HL,(PTRFIL)	;GO TO NUMBER OF CHARACTERS
	LD	BC,0028H	;NMLOFS
	ADD	HL,BC
	INC	(HL)		;BACK UP BY INCREMENTING CHARACTER COUNT
NOSKCR	POP	HL		;GET BACK PLACE TO STORE TERMINATOR
QUIT2B	LD	(HL),00H	;STORE THE TERMINATOR
	LD	HL,BUFMIN	;ITEM IS NOW STORED AT THIS POINT +1
	LD	A,E		;WAS IT A NUMERIC INPUT?
	SUB	20H		;IF SO, [E]=" "
	JP	Z,NUMIMK	;USE FIN TO SCAN IT
	LD	B,D		;SET [B]=44 IF SCANNING UNQUOTED STRING
	LD	D,00H
	CALL	STRLT2
	POP	HL		;GET BACK [H,L]
	RET			;DO ASSIGNMENT

NUMIMK	CALL	GETYPR		;GET TYPE OF NUMERIC VARIABLE BEING READ
	PUSH	AF		; PUSH PSW
	CALL	CHRGTR		;READ FIRST CHARACTER
	POP	AF		;RESTORE TYPE OF VARIABLE
	PUSH	AF		; PUSH PSW
	CALL	C,FIN		;SINGLE PRECISION INPUT
	POP	AF		; POP PSW
	CALL	NC,FINDP	;DOUBLE PRECISION INPUT
	POP	HL		;GET [H,L]
	RET			;DO THE ASSIGNMENT

STRCHR	OR	A		;TRYING TO STORE NULL BYTE
	RET	Z		;RETURN, DONT STORE IT
	LD	(HL),A		;STORE THE CHARACTER
	INC	HL
	DEC	B		;128 YET?
	RET	NZ		;MORE SPACE IN BUFFER, RETURN
	POP	BC		;GET RID OF SUPERFLUOUS STACK ENTRY
	JP	QUIT2B		;SPECIAL QUIT


;-----------------------------------------------------------------------------
;	LOAD AND RUN ROUTINES
;
PRGFLI:				;MD.SQI: SEQUENTIAL INPUT MODE
				;INTERNAL FILE NUMBER IS ALWAYS ZERO
	LD	D,01H		;SCAN FILE NAME AND DISK NUMMER
PRGFL2	XOR	A		;AND DO THE RIGHT THING USING MD.KIL
	JP	PRGFIL		;AS A FLAG

LRUN	DB	0F6H		;SET NON ZERO TO FLAG "RUN" COMMAND
LOAD	XOR	A		;FLAG ZERO FOR "LOAD"
				;A>0 INDICATES CLOSE FILES, RUN
				;A=0 INDICATES CLOSE FILES, NO RUN
	PUSH	AF		;SAVE "RUN"/"LOAD" FLAG
	CALL	PRGFLI		;FIND THAT FILE AND SETUP FOR
				;USING INDSKC SUBROUTINE
	LD	A,(NFILES)	;Number of files
	LD	(NFILSSV),A	;(LSTFRE+1) SINCE WE MAKE IT LOOK LIKE ZERO
				;SO ,R OPTION CAN LEAVE FILES OPEN
	DEC	HL		;SEE IF NO RUN OPTION
	CALL	CHRGTR
	JP	Z,NOTRNL	;NO, JUST LOAD
	CALL	SYNCHR
	DB	','		;GOTTA HAVE A COMMA
	CALL	SYNCHR
	DB	'R'		;ONLY OPTION IS RUN
	JP	NZ,SNERR	;AND THAT BETTER BE THE END
	POP	AF		;GET RID OF "RUN"/"LOAD" FLAG
;	RUN fn closes all files, loads program, and executes program
;	RUN fn,R loads program, and executes program
;	LOAD fn closes all files, loads program
;	LOAD fn,R loads program, and executes program
;	MERGE fn merges an ASCII program
;	CHAIN fn loads/merges a program, and executes program (leaving files opened)
CHNENT	XOR	A		;SO FILES AREN'T CLOSED
	LD	(NFILES),A	; HIGHEST FILE NUMBER ALLOWED
	DB	0F6H		; FLAG RUN WITH NON-ZERO
NOTRNL	POP	AF		;restore [A]>0 for RUN, =0 for LOAD
				;Variable  ",R"    CHAIN   LOAD   RUN
	LD	(RUNFLG),A	;RUNFLG=  ^O201    ^O201      0     1
	LD	HL,DIRTMP	;
	LD	(HL),00H	;NLONLY=  ^O201    ^O201      1     1
	LD	(FDBTAB),HL	;If NLONLY and 200 <> 0, don't close any files
				;If NLONLY and 001 <> 0, don't close file 0
	CALL	SCRTCH		;Clear variables, close files
	LD	A,(NFILSSV)	;RESTORE MAXFIL
	LD	(NFILES),A	;THAT WAS KLUDGED
	LD	HL,(FILPT1)
	LD	(FDBTAB),HL	;RESTORE BACK TO NORMAL
	LD	(PTRFIL),HL	;PTRFIL GOT ZEROED SO FIX IT TOO
; BELOW IS FIX (TO LABEL NOTINI) SO THAT IF ^C DURING MBASIC FOO, WONT EXIT TO SYSTEM
	LD	HL,(CURLIN)	;GET LINE NUMBER
	INC	HL		;SEE IF IN INITIALIZATION
	LD	A,H
	AND	L
	INC	A
	JP	NZ,NOTINI	;NO
	LD	(CURLIN),HL	;SAVE DIRECT LINE NUMBER
NOTINI	CALL	INDSKC		;READ THE FIRST CHARACTER
	JP	C,MAIN		;ALL DONE IF NOTHING IN FILE
	CP	0FEH		;IS THIS A PROTECTED FILE?
	JP	NZ,NTPROL	;NO
	LD	(PROFLG),A	;SET PROTECTED FILE
	JP	BINLOD		;DO BINARY LOAD

NTPROL	INC	A		;IS IT A BINARY FILE?
	JP	NZ,MAINGO	;NO, SINCE PTRFIL IS NON-ZERO

;	INCHR will use INDSKC instead of polling the terminal
;	When EOF is hit PTRFIL will be restored
;	and LSTFRE will be used as a flag
;	to indicate whether to run the loaded program

;	Time for a binary load.
;	After the load, the file is linked together
;	LSTFRE is used as a flag whether to RUN or not
;
;	Note: in DSKCOM.ASM, BINLOD is calling FSTLOD,
;	a Fast Load code.
BINLOD	LD	HL,(TXTTAB)	;GET PLACE TO START STORING INTO
BINLLP	EX	DE,HL		;SEE IF THERE IS ROOM TO SPARE
	LD	HL,(FRETOP)
	LD	BC,0FFAAH
	ADD	HL,BC
	CALL	COMPAR
	EX	DE,HL
	JP	C,OUTLOD	;ERROR AND WIPE OUT PARTIAL GARBAGE
				;UNLINKED!! NO ZEROES AT THE END!!
	CALL	INDSKB		;READ THE A DATA BYTE
				;THIS IS SEMI-WEAK SINCE MEMORY
				;IS LEFT IN A BAD BAD STATE
				;IF AN I/O ERROR OCCURS
	LD	(HL),A		;STORE BYTE
	INC	HL		;INCREMENT POINTER
	JP	NC,BINLLP	;READ THE NEXT CHAR
	LD	(VARTAB),HL	;SAVE END TEMP FOR DECODING
	LD	A,(PROFLG)	;IS THIS A PROTECTED FILE?
	OR	A		;SET CC'S
	CALL	NZ,PROLOD	;TRANSLATE TO GOOD STUFF
	CALL	LINKER		;FIX THE LINKS
	INC	HL		;WHEN LINKER RETURNS, [H,L]
	INC	HL		;POINTS TO DOUBLE ZERO
	LD	(VARTAB),HL	;UPDATE [VARTAB]
	LD	HL,NFILES	;ONLY CLOSE FILE ZER0
	LD	A,(HL)
	LD	(NFILSSV),A	;SAVE NUMBER OF FILES
	LD	(HL),00H
	CALL	RUNC		;SETUP ARYTAB, STREND
	LD	A,(NFILSSV)	;RESTORE NUMBER OF FILES
	LD	(NFILES),A	;Number of files
	LD	A,(CHNFLG)	;CHAIN IN PROGRESS
	OR	A		;TEST
	JP	NZ,CHNRET	;YES, GO BACK TO CHAIN CODE
	LD	A,(RUNFLG)	;RUN OR NOT?
	OR	A
	JP	Z,READY
	JP	NEWSTT

PRGFIN	CALL	FINPRT		;ZERO PTRFIL
	CALL	CLSFIL		;CLOSE FILE ZERO
	JP	GTMPRT		;REFETCH TEXT POINTER

;	OUTLOD, ERROR AND WIPE OUT PARTIAL GARBAGE
OUTLOD	CALL	SCRTCH
	JP	OMERR

;	MERGE filespec  Statement
; 	(OKGETM is called by CHAIN MERGE ... in FIVEO)
;
MERGE	POP	BC		;ELIMINATE NEWSTT RETURN
	CALL	PRGFLI		;READ THE NAME AND DISK
	DEC	HL		;MUST END THERE
	CALL	CHRGTR
	JP	Z,OKGETM	;READ THE FILE
	CALL	PRGFIN		;CLOSE OUT TIME
	JP	SNERR		;AND "SYNTAX ERROR"

OKGETM	XOR	A		;NO RUN OPTION WITH "MERGE"
	LD	(RUNFLG),A	;SET UP THE RUN FLAG
	CALL	INDSKC		;READ FROM [PTRFIL] FILE
	JP	C,MAIN		;GO BACK IF EOF
	INC	A		;IS IT A BINARY FILE??
	JP	Z,DERBFM	;BINARY IS WRONG FILE MODE
	INC	A		;OR PROTECTED BINARY FILE??
	JP	Z,DERBFM	;ALSO GIVE BAD FILE MODE
MAINGO	LD	HL,(PTRFIL)	;GET FILE POINTER
	LD	BC,0028H	; (NMLOFC) POINT TO NUMBER OF CHARS IN BUFFER
	ADD	HL,BC		;BY ADDING OFFSET
	INC	(HL)		;BACK UP FILE BY INCREMENTING COUNT
	JP	MAIN


;-----------------------------------------------------------------------------
; ## GIO86.ASM:746 ##
;	DIRDO is called to make sure direct statement is not found when loading file
;	 If device is keyboard, control transfers to GONE with AX used.
;
DIRDO	PUSH	HL
	LD	HL,(PTRFIL)
	LD	A,H
	OR	L		;IS PTRFIL ZERO SO NOT FILE READING?
	LD	DE,ERRFDR	;Err $42 - (DS_ERR)
				; "Direct statement in file"
	JP	NZ,ERROR	;NOTE: LXI D, IS USED TO MAKE SOURCE
				; CONVERSIONS EASIER
	POP	HL		;GET BACK POINTER AT BUFMIN
	JP	GONE		;EXECUTE DIRECT STATEMENT


;-----------------------------------------------------------------------------
;	DISPATCH FOR DIRECT STATEMENT
;
;	Make sure we're not reading a file in
;
;	SAVE command -- ASCII or binary
;
SAVE	LD	D,02H		;(MD.SQO) ELIMINATE EARLIER VERSION
				; AND CREATE EMPTY FILE
	CALL	PRGFL2		;READ FILE NAME AND DISK NUMBER
				; AND LOOK IT UP
	DEC	HL
	CALL	CHRGTR		;END OF STATEMENT?
	JP	Z,BINSAV	;BINARY SAVE!!
	CALL	SYNCHR
	DB	','		;ONLY OPTIONS ARE ",A" AND ",P"
	CP	'P'		;PROTECTED SAVE?
	JP	Z,PROSAV	;DO IT
	CALL	SYNCHR
	DB	'A'		;FOR ASCII SAVE
	JP	LISTS		;USE THE LIST CODE TO DO THE OUTPUT
				; CONTROL-CS ARE NOT ALLOWED
				; AND AT THE END PTRFIL IS ZEROED

;-----------------------------------------------------------------------------
;	BINSAV - Binary SAVE support.
; ## GIO86.ASM:1088 ##
;	Note: in DSKCOM.ASM, BINSAV is implemented using
;	a fast block save code.
BINSAV	CALL	SCCPTR		;GET RID OF POINTERS BEFORE SAVING
	CALL	PROCHK		;DONT ALLOW BINARY SAVES OF PROTECTED PROGRAMS
	LD	A,0FFH		;ALWAYS START WITH 255
BINPSV	CALL	FILOU3		;SEND TO FILE
	EX	DE,HL
	LD	HL,(VARTAB)	;GET STOP POINT
	EX	DE,HL
	LD	HL,(TXTTAB)	;GET START POINT
BNPSLP	CALL	COMPAR		;REACHED THE END?
	JP	Z,PRGFIN	;REGET TEXT POINTER AND CLOSE FILE 0
	LD	A,(HL)		;GET LINE DATA
	INC	HL		;POINT AT NEXT DATA
	PUSH	DE		;SAVE LIMIT
	CALL	FILOU3		;SEND CHAR TO FILE
	POP	DE		;RESTORE LIMIT
	JP	BNPSLP		;CONTINUE WITH LINE DATA


;-----------------------------------------------------------------------------
;	CLOSE [, WIDTH] Statements
; ## GIO86.ASM:207 ##
;
;	CLOSE Statement
; 	 Syntax: CLOSE [[#]n [,[#]n ...]]
;
CLOSE	LD	BC,CLSFIL	;SERVICE ROUTINE ADDRESS
	LD	A,(NFILES)	;HIGHEST POSSIBLE ARGUMENT,
				;WHICH MEANS DO ALL POSSIBLE
	JP	NZ,CLOSE1	;NOT END OF STATEMENT, SO SCAN ARGUMENTS
	PUSH	HL		;SAVE THE TEXT POINTER
CLOS1	PUSH	BC		;SAVE ROUTINE ADDRESS
	PUSH	AF		;SAVE CURRENT VALUE
	LD	DE,CLOS2	;RETURN ADDRESS
	PUSH	DE		;SAVE IT TO COME BACK WITH
	PUSH	BC		;DISPATCH TO SERVICE ROUTINE
	RET

;	aka RETALL
CLOS2	POP	AF		;GET BACK OLD ARGUMENT
	POP	BC		;GET BACK SERVICE ROUTINE ADDRESS
	DEC	A		;DECREMENT ARGUMENT
	JP	P,CLOS1		;LOOP ON MORE VALUES
	POP	HL		;GET BACK THE TEXT POINTER
	RET

;	Close next file
;	aka RETRTS
CLOSNX	POP	BC		;GET BACK SERVICE ROUTINE ADDRESS
	POP	HL		;GET BACK THE TEXT POINTER
	LD	A,(HL)		;SEE IF MORE ARGUMENTS
	CP	2CH		;DELIMITED BY COMMA
	RET	NZ
	CALL	CHRGTR		;READ FIRST CHARACTER OF FORMULA
	;Close one or more files
CLOSE1	PUSH	BC		;SAVE THE SERVICE ROUTINE ADDRESS
	LD	A,(HL)		;GET POSSBLE "#"
	CP	'#'		;IS IT
	CALL	Z,CHRGTR	;SKIP IT, ITS OPTIONAL
	CALL	GETBYT		;READ THE ARGUMENT
	EX	(SP),HL		;SAVE THE TEXT POINTER ON THE STACK
				;AND SET [H,L]=SERVICE ADDRESS
	PUSH	HL		;SAVE THE SERVICE ADDRESS
	LD	DE,CLOSNX	;PUT A RETURN ADDRESS ON THE STACK
	PUSH	DE
	JP	(HL)		;DISPATCH TO DO THE FUNCTION

; ## GIO86.ASM:1088 ##
;	CLSALL - close all opened files
;	 Entry - none
;	 Exit  - All registers preserved
CLSALL	PUSH	DE
	PUSH	BC		;SAVE [B,C] FOR STKINI
	XOR	A		;MAKE IT CLOSE ALL DISKS
	CALL	CLOSE
	POP	BC
	POP	DE		;GET BACK [D,E]
	XOR	A		;RETURN WITH [A]=0 AND Z ON
	RET


;-----------------------------------------------------------------------------
;	"FIELD" STATEMENT FOR SETTING UP I/O STRINGS
; ## DSKCOM.ASM 531 ##
FIELD	CALL	FILSCN		;GET DATA BLOCK POINTER IN [B,C]
	JP	Z,DERBFN	;error if File Not Opened
	SUB	03H		;MAKE SURE ITS A RANDOM FILE
	JP	NZ,DERBFM	;IF NOT, "BAD FILE MODE"
	EX	DE,HL		;SAVE TEXT POINTER
	LD	HL,00A9H	;POINT TO RECORD SIZE
	ADD	HL,BC
	LD	A,(HL)		;GET IT
	INC	HL
	LD	H,(HL)
	LD	L,A
	LD	(TEMPA),HL	;STORE MAX ALLOWED
	LD	HL,0000H
	LD	(RECORD),HL
	LD	A,H		;MAKE [A]=0
	EX	DE,HL		;GET BACK TEXT POINTER
	LD	DE,00B2H	;POINT TO 5.0 FIELD BUFFER
LOPFLD	EX	DE,HL		;SAVE TEXT POINTER IN [D,E]
	ADD	HL,BC		;ADD ON DATA POINTER SO [H,L] NOW POINTS
				;AT THE START OF THE DATA
	LD	B,A		;SETUP COUNT OF CHARACTERS PAST BY
				;IN DATA AREA, SO TOTAL IS NEVER GREATER THAN 128
	EX	DE,HL		;TEXT POINTER BACK INTO [H,L]
				;[D,E]=POINTER INTO DATA AREA
	LD	A,(HL)		;MORE "AS"S TO SCAN?
	CP	2CH		;COMMA STARTS THE CLAUSE
	RET	NZ		;BACK TO NEWSTT IF NOT
	PUSH	DE		;SAVE THE POINTER INTO THE DATA BLOCK
	PUSH	BC		;SAVE [B]=NUMBER OF CHARACTERS ALLOCATED
	CALL	GTBYTC		;Fetch and get a byte in E and A
	PUSH	AF		;READ NUMBER INTO [A] FROM TEXT
				;SAVE THIS NUMBER
	CALL	SYNCHR
	DB	'A'		;SCAN THE "AS"
	CALL	SYNCHR
	DB	'S'
	CALL	PTRGET		;GET A POINTER AT THE STRING DESCRIPTOR
	CALL	CHKSTR		;INTO [D,E]
	POP	AF		;GET THE NUMBER OF CHARACTERS
	POP	BC		;GET THE NUMBER ALREADY USED
	EX	(SP),HL		;SAVE THE TEXT POINTER AND
				;[H,L]=POINTER INTO DATA BLOCK
	LD	C,A		;SAVE # OF CHARACTERS IN [C]
	PUSH	DE		;SAVE [D,E]
	PUSH	HL		;SAVE [H,L]
	LD	HL,(RECORD)	;GET TOTAL SO FAR
	LD	B,00H		;ACCUMULATE COUNT
	ADD	HL,BC
	LD	(RECORD),HL	;SAVE TOTAL AGAIN
	EX	DE,HL		;TOTAL TO [D,E]
	LD	HL,(TEMPA)	;GET MAX ALLOWED
	CALL	COMPAR		;IN RANGE?
	JP	C,DERFOV	;NO, GIVE ERROR
	POP	HL		;RESTORE [H,L]
	POP	DE		;RESTORE [D,E]
	EX	DE,HL		;[H,L] POINT AT STRING DESCRIPTOR
	LD	(HL),C		;STORE THE LENGTH
	INC	HL
	LD	(HL),E		;STORE THE POINTER INTO THE DATA BLOCK
	INC	HL
	LD	(HL),D
	POP	HL		;GET BACK THE TEXT POINTER
	JP	LOPFLD		;CONTINUE SCANNING "AS" CLAUSES IF MORE


;-----------------------------------------------------------------------------
;	RANDOM NON-I/O -- LSET/RSET/FIELD
; ## DSKCOM.ASM 605 ##
;
;	LSET/RSET stringvar = stringexp
;
;	If stringvar points to an I/O buffer, use the string size to
;	justify string. If stringvar is a literal, make new var with length
;	of literal. If stringvar points to string space, use it. If the
;	length of the variable is zero, return the null string. If a copy
;	must be created, and stringexp is a temporary, use this space over
;	unless length stringvar greater than stringexp.
RSET	DB	0F6H		;clear carry

LSET	SCF			;Set carry if lset
	PUSH	AF		;Save LSET/RSET flag
	CALL	PTRGET		;Get pointer to stringvar
	CALL	CHKSTR		;Must be a string variable
	PUSH	DE		;Save pointer to descriptor
	CALL	FRMEQL		;EAT "=" AND EVALUATE STRINGEXP
	POP	BC		; [B,C] = ptr to descr.
	EX	(SP),HL		;Text ptr on bottom of stack
	PUSH	HL		;LSET/RSET flag next
	PUSH	BC		;Put descr. ptr back on
	CALL	FRESTR		;Error if not string, free temp.
	LD	B,(HL)		;Get length of stringexp
	EX	(SP),HL		; [H,L] = descr. of var,save othr
	LD	A,(HL)		;Get length of stringvar
	LD	C,A		;Save in [C]
	PUSH	BC		;Save lengths of both
	PUSH	HL		;Save descriptor pointer
	PUSH	AF		;PSW zero if was temp.
	INC	HL
	LD	E,(HL)		;get point to stringvar text
	INC	HL
	LD	D,(HL)
	OR	A		;stringvar null?
	JP	Z,RETCUR	;Yes, don't change
	LD	HL,(TXTTAB)
	CALL	COMPAR		;Stringvar in disk buffer?
	JP	NC,OLDSTR	;Yes, use it
	LD	HL,(VARTAB)
	CALL	COMPAR		;stringvar in program(literal)?
	JP	C,OLDSTR	;No, in string space so use it
;	Need to make new string for result since stringvar points to a literal.
;	If stringexp was a temporary, it has been freed. If the length of
;	stringexp is greater than or equal to the length of stringvar, GETSPA
;	can be called and no garbage collection can occur so temp. can be reused.
;	If stringvar is greater, must get a temp. to point to stringexp if it
;	was a temp. , then call GETSPA which in this case can garbage collect.
	LD	E,C
	LD	D,00H		;# BYTES TO ALLOCATE FOR RESULT
	LD	HL,(STREND)
	ADD	HL,DE
	EX	DE,HL
	LD	HL,(FRETOP)
	CALL	COMPAR		;will GETSPA garbage collect?
	JP	C,MAKDSC	;Yes, better have stringexp temp.
	POP	AF		;get rid of temp indicator
MADESC	LD	A,C		;Get length of stringvar
	CALL	GETSPA		;Get space for result
	POP	HL		;Get stringvar descr.
	POP	BC		;Get lengths off stack
	EX	(SP),HL		;Get what we wanted, stringexp descr.
	PUSH	DE
	PUSH	BC
	CALL	FRESTR		;Free temp if any
	POP	BC
	POP	DE
	EX	(SP),HL
	PUSH	BC		;Restore stack to previous state
	PUSH	HL
	INC	HL
	PUSH	AF
	LD	(HL),E		;set pointer to stringvar copy
	INC	HL
	LD	(HL),D
OLDSTR	POP	AF
	POP	HL		;Get stringvar descr.
	INC	HL
	LD	E,(HL)		;Get pointer to text area
	INC	HL
	LD	D,(HL)
	POP	BC		;Get lengths off stack
	POP	HL		;Get pointer to stringexp descr.
	INC	HL		;point to address part
	LD	A,(HL)		;Get ptr to stringexp text
	INC	HL
	LD	H,(HL)
	LD	L,A
	LD	A,C		;Get length of field(stringvar)
	CP	B
	JP	NC,FILDOK	;Jump if field large enough for result
	LD	B,A		;Save # of bytes to copy
FILDOK	SUB	B
	LD	C,A		;[C] = # blanks to fill
	POP	AF		;Get LSET/RSET flag
	CALL	NC,BLKFIL	;Fill leading if RSET
	INC	B		;In case zero
COPLOP	DEC	B		;Decr. # to copy
	JP	Z,LRSTDN	;Done when all copied
	LD	A,(HL)		;Get byte from stringexp
	LD	(DE),A		;Copy to stringvar
	INC	HL
	INC	DE
	JP	COPLOP

RETCUR	POP	BC
	POP	BC
	POP	BC
	POP	BC
	POP	BC		;Get garb off stack
LRSTDN	CALL	C,BLKFIL	;Fill trailing if LSET
	POP	HL		;Restore text pointer
	RET

BLKFIL	LD	A,20H		;Fill with spaces
	INC	C		;In case zero
BLKFL1	DEC	C		;Decr. # to fill
	RET	Z		;Return when done
	LD	(DE),A		;Store space
	INC	DE
	JP	BLKFL1

;	If stringexp was a temporary, create a new temporary to point to
;	stringexp since old one was freed. This must be done since GETSPA
;	will be called and garbage collection might occur. If stringexp is
;	not a temporary, return.
MAKDSC	POP	AF		;Get temp flag
	POP	HL
	POP	BC
	EX	(SP),HL		;Dig down to stringexp descr.
	EX	DE,HL		;FRETMS wants [D,E]
	JP	NZ,MAKDS1	;Not a temp, don't reallocate
	PUSH	BC
	LD	A,B
	CALL	STRINI		;Make a temp point to stringexp
	CALL	PUTNEW		;Get a temp to point to it
	POP	BC
MAKDS1	EX	(SP),HL
	PUSH	BC
	PUSH	HL		;Restore stack to previous state
	JP	MADESC


;-----------------------------------------------------------------------------
;	Program I/O -- Fixed Length INPUT
; ## DSKCOM.ASM 763 ##
;
;	Format:
;               stringvar = INPUT$(#bytes[,[#] file#])
;	If no file # is given, characters will be read from the user's
;	terminal. No echoing will be done and no editing will be allowed
;	(i.e. rubout,@,_,^U are just input as characters).
FIXINP	CALL	CHRGTR
	CALL	SYNCHR
	DB	'$'		;STRING FUNCTION
	CALL	SYNCHR
	DB	'('
	CALL	GETBYT		;Get # of bytes to read
	PUSH	DE		;Save # of bytes to read
	LD	A,(HL)
	CP	2CH		;Read from disk file?
	JP	NZ,REDTTY	;No, from user's terminal
	CALL	CHRGTR
	CALL	FILSCN		;Set up file #
	CP	02H		;SEQUENTIAL OUTPUT FILE?
	JP	Z,DERBFM	;THEN BAD FILE MODE
	CALL	FILSET		;SET UP PTRFIL
	XOR	A		;SET ZERO FOR FLAG
REDTTY	PUSH	AF		;NON ZERO SET IF TERMINAL I/O
	CALL	SYNCHR
	DB	')'		;Must have paren
	POP	AF		;Get flag off stack
	EX	(SP),HL		;Save text ptr, [L]=# to read
	PUSH	AF		;Save flag
	LD	A,L
	OR	A		;Read no characters?
	JP	Z,FCERR		;Yes, error
	PUSH	HL		;Save #
	CALL	STRINI		;Get space for string
	EX	DE,HL
	POP	BC		;[C] = # to read
FIXLOP	POP	AF
	PUSH	AF		;NON-ZERO set if should read from TTY
	JP	Z,DSKCHR	;Read from disk file
	CALL	GNXTKY		;GET CHAR IF ONE
	JP	NZ,CHARCW	;WAS ONE
	CALL	SVCKBD		;Read a char from terminal
;	Note: will check flag on interrupt ^c
CHARCW	CP	03H		;Control-C?
	JP	Z,FICTLC	;Yes, stop
PUTCHR	LD	(HL),A		;Put char into string
	INC	HL
	DEC	C		;Read enough yet?
	JP	NZ,FIXLOP	;No, read more
	POP	AF		;Get flag off stack
	CALL	FINPRT
	JP	PUTNEW		;Return string as result

;	Got <Ctrl>-C
;	aka INTCTC
FICTLC	LD	HL,(SAVSTK)	;GET SAVED STACK POINTER
	LD	SP,HL		;SET [H,L] TO IT
	JP	ENDCON		;STOP PROGRAM

DSKCHR	CALL	INDSKC		;Get char from file
	JP	C,DERRPE	;If carry, read past EOF
	JP	PUTCHR		;Put char in string


;-----------------------------------------------------------------------------
;	EOF, LOC, LOF  Functions
; ## GIO86.ASM:587 ##
;
;			Revision history
;			-------- -------
;
;11/7/77         FIXED EOF() CODE TO USE ORNOFS INSTEAD OF NMLOFS, PGA
;12/2/77  (4.41) FIXED RANDOM ACCESS TO CLOSE EXTENTS PGA
;12/17/77        ADDITIONAL CODE TO SUPPORT ONTEL DOS BASIC, P.ZILBER
;12/29/77 (4.42) FIXED BUG WHERE GET, PUT W/O REC NOT INC CURLOC, PGA
;1/5/78   (4.43) FIXED ANOTHER RANDOM BUG, LINE PRINTER ^C PROB. PGA
;7/23/79         Add Beehive interace and cleanup conditionals
;
;
;
;File information:
;
;        1	Mode of the file
;F.BLK1	(n)	1st block of space, usually FCB
;      ( CPM: 33, ONTEL: 42 MOSTEK: 47 ). Zero for others
;LOCOFS  2	CURLOC, # of sectors read or writeen for sequential.
;		For random files, it is the last record # +1.
;ORNOFS	 1/2	Seq Input: 	# of bytes in sector when read.
;		Seq Output:	# bytes in output sector( # written )
;		Random:		Set to DATPSC by PUT and GET, sometimes
;				zeroed by OUTSQ2 setup for DSKOUT code.
;NMLOFS	 1/2	Seq Input:	# bytes left in input buffer
;		Seq Output:	Position of imaginary print head
;F.BLK2	(n)	2nd block of space( 6 byte chain links for ONTEL,
;		160 bytes for DMC, 10 bytes for BEEHIV )
;DATOFS	(n)	Sector buffer, Length = DATPSC
;
;Extra information for 5.0 version only:
;
;FD.SIZ	 2	Variable length record size( default = 128 )
;FD.PHY	 2	Current physical record #
;FD.LOG	 2	Current logical record #
;FD.CHG	 1	Future flag for across record PRINTs, etc.
;FD.OPS	 2	Output print position for PRINT, INPUT, WRITE
;FD.DAT	(n)	Data buffer for FIELD, size is (FD.SIZ). FD.MAX is max.
;
;
;File modes
;MD.000	0	The mode number for No File, internal use only as an escape from OPEN
;MD.RND	3	The mode number for random files
;MD.SQI	1	The mode number for sequential input files never written into a file
;MD.SQO	2	The mode for sequential output files and program file
;
;	EOF(n) Function - returns -1 if eof, else 0
;	Entry - [FAC] = file number
;	Exit  - [FAC] = -1 if EOF, else 0.
EOF	CALL	FACFPT		;CONVERT ARGUMENT TO FILE NUMBER AND
				; SET [B,C] TO POINT TO FILE DATA BLOCK
	JP	Z,DERBFN	;BAD FILE NUMBER - NOT FOUND !!!
	CP	02H		;IS IT A SEQUENTIAL OUTPUT FILE?
	JP	Z,DERBFM	;THEN GIVE BAD FILE MODE
ORNCHK	LD	HL,0027H	; (0+ORNOFS)
				;SEE IF ANY BYTES ARRIVED IN THIS BUFFER
	ADD	HL,BC
	LD	A,(HL)		;ZERO IFF IT IS END OF FILE
	OR	A		;SET CC'S
	JP	Z,WASEOF	;NO BYTES LEFT
	LD	A,(BC)		;** 5.11 **  GET FILE MODE
	CP	03H		;IS IT A RANDOM FILE?
	JP	Z,WASEOF	;** 5.11 **  (A) .NE. 0 - not EOF
	INC	HL		;POINT TO NUMBER LEFT IN BUFFER
	LD	A,(HL)		;GET NUMBER OF BYTES IN BUFFER
	OR	A		;NON-ZERO?
	JP	NZ,CHKCTZ	;THEN CHECK FOR CONTROL-Z
	PUSH	BC		;SAVE [B,C]
	LD	H,B		;GET FCB POINTER IN [B,C]
	LD	L,C
	CALL	READIN		;READ ANOTHER BUFFER
	POP	BC		;RESTORE [B,C]
	JP	ORNCHK		;HAVE NEW BUFFER, USE PREVIOUS PROCEDURE

CHKCTZ	LD	A,DATSPC	;GET # OF BYTES IN FULL BUFFER
	SUB	(HL)		;SUBTRACT LEFT
	LD	C,A		;PUT IN [B,C] FOR DAD
	LD	B,00H
	ADD	HL,BC		;ADD TO ORNOFS OFFSET
	INC	HL		;ADD ONE TO POINT TO BYTE IN BUFFER
	LD	A,(HL)		;GET BYTE
	SUB	1AH		;IF CONTROL-Z, EOF (CONTROL-\ IS FS)
WASEOF	SUB	01H
	SBC	A,A
	JP	CONIA		;CONVERT TO AN INTEGER AND RETURN

;
;	[B,C] POINTS AT FILE DATA BLOCK
;
OUTSEQ	LD	D,B		;PUT FILE BLOCK OFFSET IN [D,E]
	LD	E,C
	INC	DE		;POINT TO FCB

OUTSEQ0	LD	HL,0027H	;(0+ORNOFS) POINT TO NUMBER IN BUFFER
	ADD	HL,BC		;ADD START OF FILE DATA BLOCK
	PUSH	BC		;SAVE FILE DATA POINTER
	XOR	A
	LD	(HL),A		;ZERO OUT NUMBER IN DATA BUFFER

;	OUTPUT NEXT RECORD IN FILE
;
;	(A) = 0
;	(HL) points to NMLOFS-1
;	(DE) points to File Data Block + 1 ( FCB if SPC2ND=0)
;	(BC) points to File Data Block

	CALL	SETBUF		;SET BUFFER ADDRESS
	LD	A,(CPMWRI)	; BDOS FN code
	CALL	ACCFIL		;Access file
	CP	0FFH
	JP	Z,DERTMF	;Too many files - 5.11
	DEC	A		;ERROR EXTENDING FILE? (1)
	JP	Z,DERDIO	;YES
	DEC	A		;DISK FULL? (2)
	JP	NZ,OUTSOK	;NO
	POP	DE		;GET BACK FILE POINTER
	XOR	A		;GET ZERO
	LD	(DE),A		;MARK AS CLOSED
	LD	C,10H		;CLOSE IT (BDOS function 16 - Close file)
	INC	DE		;POINT TO FCB
	CALL	CPMENT		;CALL CP/M
	JP	DERDFL		;GIVE "DISK FULL" ERROR MESSAGE

OUTSOK	INC	A		;TOO MANY FILES?
	JP	Z,DERTMF	;YES
	POP	BC		;GET POINTER AT CURLOC
	LD	HL,0025H	;BY ADDING OFFSET TO FILE POINTER
	ADD	HL,BC
	LD	E,(HL)		;INCREMENT IT
	INC	HL
	LD	D,(HL)
	INC	DE
	LD	(HL),D
	DEC	HL
	LD	(HL),E
	RET


;-----------------------------------------------------------------------------
;	CLSFIL - close file [AL]
;	 Exit  - Flags, AX, SI used, all other registers are preserved
;
;	FILE NUMBER IS IN [A]
;	ZERO ALL INFORMATION. IF FILE IS OPEN, RAISE ITS DISKS HEAD
;	IF FILE IS SEQUENTIAL OUTPUT, SEND FINAL SECTOR OF DATA
;
; ## GIO86.ASM:876 ##
;
CLSFIL	CALL	FDBPTR		;GET POINTER TO DATA
	JP	Z,NTOPNC	;RETURN IF NOT OPEN
				;SAVE FILE #
	PUSH	BC		;SAVE FILE POINTER
	LD	A,(BC)		;GET FILE MODE
	LD	D,B		;PUT FILE BLOCK OFFSET IN [D,E]
	LD	E,C
	INC	DE		;POINT TO FCB
	PUSH	DE		;SAVE [D,E] FOR LATER
	CP	02H		;SEQENTIAL OUTPUT?
	JP	NZ,NOFORC	;NO NEED TO FORCE PARTIAL OUTPUT BUFFER
	LD	HL,CLSFL1	;RETURN HERE
	PUSH	HL		;SAVE ON STACK
	PUSH	HL		;NEED EXTRA STACK ENTRY
	LD	H,B		;GET FILE POINTER
	LD	L,C		;INTO [H,L]
	LD	A,1AH		;PUT OUT CONTROL-Z (OR FS)
	JP	FILOU4		;JUMP INTO CHAR OUTPUT CODE

CLSFL1	LD	HL,0027H	; (0+ORNOFS) CHARS IN BUFFER
	ADD	HL,BC		;TEST
	LD	A,(HL)		;TEST ORNOFS
	OR	A
	CALL	NZ,OUTSEQ0	;FORCE OUT BUFFER
NOFORC	POP	DE		;GET BACK FCB POINTER

;	CLOSE FILE
;
;	(DE) points to FCB
;	((SP)) points to File Data Block
	CALL	SETBUF
	LD	C,10H		;THE CLOSE (BDOS function 16 - Close file)
	CALL	CPMENT		;CALL CPM
	POP	BC		;RESTORE FILE POINTER
NTOPNC	LD	D,29H		; (DATOFS) NUMBER OF BYTES TO ZERO
	XOR	A
MORCZR	LD	(BC),A
	INC	BC
	DEC	D
	JP	NZ,MORCZR
	RET

;	LOC(n) Function
;	Entry - [FAC] = file number
;	Exit  - [FAC] = current record number
LOC	CALL	FACFPT		;CONVERT ARGUMENT AND POINT AT DATA BLOCK
	JP	Z,DERBFN	;IF NOT OPEN, "BAD FILE NUMBER"
	CP	03H		;Random mode?
	LD	HL,0026H	;0+LOCOFS+1: Assume not
	JP	NZ,LOC1		;No, use CURLOC
	LD	HL,00AEH	;0+FD.LOG+1: POINT AT LOGICAL RECORD NUMBER
LOC1	ADD	HL,BC
	LD	A,(HL)
	DEC	HL
	LD	L,(HL)
	JP	GIVINT

;	LOF(n) Function
;	Entry - [FAC] = file number
;	Exit  - [FAC] = length of file in bytes
LOF	CALL	FACFPT		;CONVERT ARGUMENT AND INDEX
	JP	Z,DERBFN	;"BAD FILE NUMBER" IF NOT OPEN
	LD	HL,0010H	;0+FCB.RC+1: Point to record number
	ADD	HL,BC		;(BC) points to File Data Block
	LD	A,(HL)		;GET RC
	JP	SNGFLT		;FLOAT IT

;	FILOUT -- PUT A CHARACTER IN AN OUTPUT BUFFER AND OUTPUT IF NECESSARY
;
;	CALL AT FILOUT WITH [H,L] TO BE SAVED ON THE STACK
;	AND THE CHARACTER IN THE HIGH ORDER BYTE BELOW THE [H,L]
;	ON THE STACK. THE CURRENT DATA IS OUTPUT IF THERE ARE 128
;	CHARACTER STUFFED INTO THE DATA AREA.
;	FILOUT IS NORMALLY CALLED FROM OUTDO (OUTCHR)
;
;	Output to file
FILOUT	POP	HL		;GET SAVED [H,L] OFF STACK
	POP	AF		;GET SAVE CHAR OFF STACK
FILOU3	PUSH	HL		;SAVE THE [H,L]
	PUSH	AF		;SAVE THE CHARACTER AGAIN
	LD	HL,(PTRFIL)	;GET THE POINTER TO THE FILE
	LD	A,(HL)		;WHAT IS THE MODE?
	CP	01H		;MUST BE ECHOING OR "EXTRA IGNORED"
				;DURING THE READING OF A FILE
	JP	Z,PPAFHLR	;SO IGNORE THIS OUTCHR
	CP	03H		;RANDOM?
	JP	Z,FILOFV	;YES, FINISH UP IN FIVDK.MAC
	POP	AF		;TAKE THE CHARACTER OFF
FILOU4	PUSH	DE
	PUSH	BC
	LD	B,H		;SETUP [B,C] FOR OUTSEQ
	LD	C,L
	PUSH	AF		;RE-SAVE OUTPUT CHARACTER
	LD	DE,0027H	;0+ORNOFS:
				;POINT AT THE NUMBER OF CHARACTERS IN THE
	ADD	HL,DE		;BUFFER CURRENTLY
	LD	A,(HL)
	CP	DATSPC		;IS THE BUFFER FULL?
	PUSH	HL		;SAVE POINTER AT CHARACTER COUNT
	CALL	Z,OUTSEQ	;OUTPUT IF FULL
	POP	HL		;GET BACK DATA BLOCK POINTER
	INC	(HL)		;INCREMENT THE NUMBER OF CHARACTERS
	LD	C,(HL)		;FETCH FOR OFFSET INTO DATA
	LD	B,00H
	INC	HL		;POINT AT PRINT POSITION
	POP	AF		;GET THE OUTPUT CHARACTER
	PUSH	AF		;RESAVE FOR OUTPUT
	LD	D,(HL)		;[D]=CURRENT POSITION
	CP	0DH		;BACK TO ZERO POSITION WITH RETURN?
	LD	(HL),B		;ASSUME RESET TO ZERO SINCE [B]=0
	JP	Z,ISCRDS	;ALL DONE UPDATING POSITION
	ADD	A,0E0H		;SET CARRY FOR SPACES AND HIGHER
	LD	A,D		;[A]=CURRENT POSITION
	ADC	A,B		;ADD ON CARRY SINCE [B]=0
	LD	(HL),A		;UPDATE THE POSITION IN THE DATA BLOCK
ISCRDS	ADD	HL,BC
	POP	AF		;GET THE CHARACTER
	POP	BC
	POP	DE
	LD	(HL),A		;SAVE IT IN THE DATA AREA
	POP	HL		;GET BACK SAVED [H,L]
	RET

FIVDPT	DEC	DE		;MAP RECORD NUMBER 1=0 LOGICAL
	DEC	HL
	LD	(HL),E
	INC	HL
	LD	(HL),D		;SETUP CURLOC AGAIN
	INC	HL		;POINT TO ORN
	LD	(HL),DATSPC	;SET NUMBER IN THE BUFFER TO DATSPC
	INC	HL
	LD	(HL),DATSPC
	POP	HL		;[H,L]=TEXT POINTER
	EX	(SP),HL		;SAVE TEXT POINTER, [H,L]=START OF DATA BLOCK
	LD	B,H
	LD	C,L
;	RANDOM FILE ACCESS
;
;	(DE) = physical block #
;	(BC) points to File Data Block
;	(HL) points to File Data Block
	PUSH	HL		;SAVE DATA BLOCK POINTER
	LD	A,(CPMVER)	;Get CP/M version number
	OR	A
	JP	Z,RNDVR1	;Version 1.x
	LD	HL,0022H	;0+FCB.RN+1: Offset to random record number
	ADD	HL,BC
	LD	(HL),E		;Set new random record number
	INC	HL
	LD	(HL),D
	INC	HL
	LD	(HL),00H
	JP	RNDDON		;Finished setting record number

RNDVR1	LD	HL,000DH	;POINT TO EXTENT
	ADD	HL,BC		;ADD START OF FILE CONTROL BLOCK
	LD	A,E		;GET LOW BYTE OF OFFSET
	RLA			;GET HIGH BIT IN CARRY
	LD	A,D		;GET HIGH BYTE
	RLA			;ROTATE IN HIGH BYTE OF LOW PART
	LD	D,(HL)		;PUT ORIGINAL EXTENT IN [D]
	CP	D		;ARE NEW AND OLD EXTENT THE SAME?
	JP	Z,SAMEXT	;SAME EXTENT, DONT RE-OPEN
	PUSH	DE		;SAVE RECORD NUMBER
	PUSH	AF		;SAVE NEW EXTENT
	PUSH	HL		;SAVE POINTER TO EXTENT
	PUSH	BC		;SAVE FILE POINTER
	LD	DE,DIRTMP	;READ DIRECTORY IN HERE FOR OPEN
	LD	C,1AH		;SET CPM BUFFER ADDRESS
				;BDOS function 26 - Set DMA address
	CALL	CPMENT		;CALL CP/M
	POP	DE		;GET CPM FCB POINTER
	PUSH	DE		;SAVE BACK
	INC	DE		;POINT TO FCB
	LD	C,10H		;CLOSE PREVIOUS EXTENT (?!)
				;(BDOS function 16 - Close file)
	CALL	CPMENT		;CALL CP/M
	POP	DE		;GET BACK FCB POINTER
	POP	HL		;RESTORE POINTER TO EXTENT FIELD
	POP	AF		;GET BACK NEW EXTENT
	LD	(HL),A		;STORE NEW EXTENT
	PUSH	DE
	INC	DE		;POINT TO FCB
	LD	C,0FH		;OPEN NEW EXTENT
				;(BDOS function 15 - Open file)
	PUSH	DE		;SAVE EXTENT POINTER
	CALL	CPMENT		;BY CALLING CP/M
	POP	DE		;RESTORE FCB POINTER
	INC	A		;DOES EXTENT EXIST?
	JP	NZ,RNDOK	;YES
	LD	C,16H		;MAKE THE EXTENT EXIST
				;(BDOS function 22 - create file)
	CALL	CPMENT		;CALL CP/M
	INC	A		;ROOM IN DIRECTORY?
	JP	Z,DERTMF	;NO
RNDOK	POP	BC		;RESTORE [B,C]
	POP	DE		;RESTORE RECORD NUMBER
SAMEXT	LD	HL,0021H	;0+FCB.NR+1: NEXT RECORD FIELD
	ADD	HL,BC		;POINT TO IT
	LD	A,E		;GET LOW 7 BITS OF RECORD #
	AND	7FH
	LD	(HL),A		;SET RECORD #
RNDDON	POP	HL		;[H,L] POINT AT FILE DATA BLOCK
;	(BC) points to File Data Block
;	(HL) points to File Data Block
	LD	A,(PGTFLG)	;GET FLAG FOR "PUT" OR "GET"
	OR	A
	JP	NZ,PUTFIN	;DO THE PUTTING
	CALL	READIN		;PERFORM THE GET
	POP	HL		;GET THE TEXT POINTER
	RET

PUTFIN	LD	HL,0021H	;0+FCB.NR+1: LOOK AT RECORD #
	ADD	HL,BC		;[H,L] POINTS TO IT
	LD	A,(HL)		;GET IT
	CP	7FH		;LAST RECORD IN EXTENT?
	PUSH	AF		;SAVE INDICATOR
	LD	DE,DIRTMP	;DIRTMP: SAVE HERE
	LD	HL,0029H	;DATOFS: POINT TO DATA
	ADD	HL,BC
	PUSH	DE		;SAVE DIRTMP POINTER
	PUSH	HL		;SAVE DATA POINTER
	CALL	Z,BUFMOV	;NOT LAST EXTENT
	CALL	OUTSEQ		;OUTPUT THE DATA
	POP	DE		;RESTORE DATA POINTER
	POP	HL		;RESTORE POINTER TO DIRTMP
	POP	AF		;RESTORE INDICATOR
	CALL	Z,BUFMOV	;MOVE SECTOR
	POP	HL		;GET THE TEXT POINTER
	JP	FINPRT		;ZERO PTRFIL

BUFMOV	PUSH	BC		;SAVE [B,C]
	LD	B,80H		;# OF BYTES TO MOVE
BUFSLP	LD	A,(HL)		;GET BYTE FROM BUFFER
	INC	HL		;BUMP POINTER
	LD	(DE),A		;SAVE IN DIRTMP
	INC	DE		;BUMP POINTER
	DEC	B
	JP	NZ,BUFSLP	;KEEP MOVING BYTES
	POP	BC		;RESTORE [B,C]
	RET


;-----------------------------------------------------------------------------
;	INCHR - get next byte from file PTRFIL
;	 Exit  - [AL]=byte, [FLAGS], [AH] destroyed.
;	         All other regs preserved
;	         if END-OF-FILE then
;	            if program load was in progress, file 0 closed etc.
;	            else Read-Past-End error is generated
; ## GIO86.ASM:903 ##
INDSKB	PUSH	BC		;SAVE CHAR COUNTER
	PUSH	HL		;SAVE [H,L]
INDSKB0	LD	HL,(PTRFIL)	;GET DATA BLOCK POINTER
	LD	A,(HL)
	CP	03H		;GET FILE MODE
	JP	Z,FILIFV	;MD.RND RANDOM?
	LD	BC,0028H	;DO INPUT
	ADD	HL,BC		;SEE HOW MANY CHARACTERS LEFT
	LD	A,(HL)
	OR	A		;GET THE NUMBER
	JP	Z,INDSKB1
	DEC	HL		;MUST GO READ SOME MORE -- IF CAN
	LD	A,(HL)		;POINT AT ORNOFS
	INC	HL		;GET ORIGINAL NUMBER
	DEC	(HL)		;POINT AT NUMBER LEFT AGAIN
	SUB	(HL)		;DECREMENT THE NUMBER
	LD	C,A		;SUBTRACT TO GIVE OFFSET
	ADD	HL,BC		;[C]=OFFSET
	LD	A,(HL)
	OR	A		;GET THE DATA
	POP	HL		;RESET CARRY FLAG FOR NO EOF
	POP	BC		;RESTORE [H,L]
	RET

;	aka FILLSQ
INDSKB1	DEC	HL		;BACK UP POINTER
	LD	A,(HL)		;TO ORNOFS
	OR	A		;DID WE HIT EOF ON PREVIOUS READ?
	JP	Z,INDSKB3	;YES
	CALL	READ2		;READ A RECORD
;	OR	A 	<---	;USED TO BE - WAS IT EOF?
	JP	NZ,INDSKB0	;RETURN WITH A CHAR (GET A CHAR BY INDSKB)
INDSKB3	SCF			;CARRY IS EOF FLAG
	POP	HL		;RESTORE [H,L]
	POP	BC		;EOF DETECTED
	LD	A,1AH		;RETURN WITH EOF: CHAR=CONTROL-Z (OR =FS)
	RET

READ2	LD	HL,(PTRFIL)	;GET DATA POINTER
READIN	PUSH	DE
	LD	D,H		;PUT FCB POINTER IN [D,E]
	LD	E,L
	INC	DE
	LD	BC,0025H	;0+LOCOFS: POINT TO CURLOC
	ADD	HL,BC
	LD	C,(HL)
	INC	HL
	LD	B,(HL)
	INC	BC
	DEC	HL
	LD	(HL),C		;UPDATE [CURLOC]
	INC	HL
	LD	(HL),B
	INC	HL		;POINT TO NUMBER READ
	INC	HL		;POINT TO NMLOFS
	PUSH	HL		;SAVE [H,L]

;	ZERO OUT THE BUFFER IN CASE NOTHING READ
	LD	C,80H		;NUMBER OF BYTES/BUFFER
ZRRND	INC	HL		;INCREMENT BUFFER POINTER
	LD	(HL),00H	;ZERO IT
	DEC	C		;DECREMENT COUNT
	JP	NZ,ZRRND	;KEEP ZEROING


;	READ SPECIFIED RECORD IN FILE
;
;	(DE) points to FCB
;
;	If SW2BYT = 0,
;		(A) = number of bytes read
;	If SW2BYT = 1,
;		(DE) = number of bytes read
;
;	If EOF, return with (A) or (DE) zero and
;		jump to READI2
;
;	Returns 'Z' set if EOF

	CALL	SETBUF		; SET CPM BUFFER ADDRESS
	LD	A,(CPMREA)	;Get read code
	CALL	ACCFIL		;Access file
	OR	A		;EOF?
	LD	A,00H		;Return 0 if EOF
	JP	NZ,READI2	;Assume EOF if error
	LD	A,80H		;OTHERWISE, HAVE 128 BYTES
READI2	POP	HL		;POINT BACK TO # READ
	LD	(HL),A		;STORE NUMBER READ
	DEC	HL		;POINT AT NUMBER ORIGINALLY
	LD	(HL),A		;STORE NUMBER READ
	OR	A		;Test for EOF
	POP	DE		;GET [D,E] BACK
	RET

;	Set DMA address
SETBUF	PUSH	BC		;SAVE [B,C]
	PUSH	DE		;SAVE [D,E]
	PUSH	HL		;SAVE [H,L]
	LD	HL,0028H	;DATOFS-1: POINT TO BUFFER
	ADD	HL,DE		;ADD
	EX	DE,HL		;PUT BUFFER ADDRESS IN [D,E]
	LD	C,1AH		;SET UP BUFFER ADDRESS
				;(BDOS function 26 - Set DMA address)
	CALL	CPMENT		;CALL CPM
	POP	HL		;RESTORE [H,L]
	POP	DE		;RESTORE [D,E]
	POP	BC		;RESTORE [B,C]
	RET

;	Input char from Ctrl-Z-terminated file
;	Read byte, C flag is set if EOF
;
;	a.k.a. RDBYT
INDSKC	CALL	INDSKB		; GET A CHARACTER FROM A SEQUENTIAL
				; FILE IN [PTRFIL]
				;=INCHR in other dialects...
	RET	C		;IF EOF, RETURN WITH END OF FILE CHARACTER
	CP	1AH		;WAS IT A CONTROL-Z (OR FS)?
	SCF			;SET CARRY
	CCF			;MAKE SURE CARRY RESET
	RET	NZ		;NO
	PUSH	BC		;SAVE [B,C]
	PUSH	HL		;SAVE [H,L]
	LD	HL,(PTRFIL)	;GET POINTER TO FILE DATA BLOCK
	LD	BC,0027H	;0+ORNOFS: POINT TO NUMBER
				; ORIGINALLY IN BUFFER
	ADD	HL,BC
	LD	(HL),00H	;FORCE IT TO ZERO
	INC	HL		;POINT TO NUMBER IN BUFFER
	LD	(HL),00H	;FORCE TO ZERO.
	SCF			;SET EOF FLAG
	POP	HL		;RESTORE [H,L]
	POP	BC		;RESTORE [B,C]
	RET

;	NAMFIL - Scan a file name for NAME, KILL, or FILES command
;	 Entry - [HL] = text pointer
;	 Exit  - [HL] = text pointer
;	         [DE] points to 1st byte of filename
;	         [??] = number of bytes in filename string
;	 Uses  - [A]
;	Get file name, etc..
;	a.k.a. FNAME -- SCAN A FILE NAME AND NAME COMMAND
NAMFIL	CALL	FRMEVL		;EVALUATE STRING
	PUSH	HL		;SAVE TEXT POINTER
	CALL	FRESTR		;FREE UP THE TEMP
	LD	A,(HL)		;GET LENGTH OF STRING
	OR	A		;NULL STRING?
	JP	Z,DERIFN	;YES, ERROR
	PUSH	AF		;NO "." SEEN
	INC	HL		;PICK UP POINTER TO STRING
	LD	E,(HL)		;BY GETTING ADDRESS
	INC	HL		;OUT OF DESCRIPTOR
	LD	H,(HL)
	LD	L,E		;[H,L] POINTS TO STRING
	LD	E,A		;SAVE LENGTH

	CP	02H		;CAN THERE BE A DEVICE?
	JP	C,NODEV		;NO, NAME TOO SHORT
	LD	C,(HL)		;[C]=POSSIBLE DEVICE NAME
				;('drive letter' in file specifier)
	INC	HL		;POINT TO NEXT CHAR
	LD	A,(HL)		;GET IT
	DEC	E		;DECREMENT COUNT FOR DEVICE NAME
	CP	':'		;COLON FOR DEVICE NAME?
	JP	Z,NAMFIL1	;YES, SO NOW GET FILE NAME
	DEC	HL		;BACK UP POINTER BY ONE
	INC	E		;COMPENSATE FOR DCR
NODEV	DEC	HL		;BACK UP POINTER
	INC	E		;INCREMENT CHAR COUNT TO COMPENSATE FOR NEXT DECR
	LD	C,'@'		;USE CURRENTLY SELECTED DRIVE
				;(Force to drive number '0' {default}
				;if no drv letter in the filename)
NAMFIL1	DEC	E		;DECREMENT CHAR COUNT
	JP	Z,NMERR		;ERROR IF NO FILENAME
	LD	A,C		;Get drive letter as written in file specifier
	AND	0DFH		;Convert..
	SUB	40H		;..to drive number (LOGICAL NUMBER).
	JP	C,NMERR		;NOT IN RANGE
	CP	1BH		;BIGGER THAN 27
	JP	NC,NMERR	;NOT ALLOWED
	LD	BC,FCBDRV	;WHERE TO PUT NAME
	LD	(BC),A		;Set drive number in FCB
	INC	BC		;Point to name
				;(POINT TO WHERE FIRST CHAR OF FILE NAME IS STORED)
	LD	D,0BH		;LENGTH OF NAME
				;(11-2*0 ??  ..I'd rather put 8+3)
FILINX	INC	HL		;BUMP POINTER
FILLOP	DEC	E		;END OF STRING
	JP	M,FILSPC	;YES, FILL REST OF FIELD WITH BLANKS
	LD	A,(HL)		;GET CHAR
	CP	'.'		;EXTENSION?
	JP	NZ,FILLOP1	;NO
	CALL	FILLNM		;YES, FILL NAME WITH BLANKS
	POP	AF		;RESTORE CC'S
	SCF			;FLAG "." SEEN
	PUSH	AF		;SAVE CC'S BACK
	JP	FILINX		;YES, IGNORE "."
FILLOP1	LD	(BC),A		;COPY CHAR
	INC	BC
	INC	HL
	DEC	D		;DECREMENT POSSIBLE COUNT OF CHARS
	JP	NZ,FILLOP
GOTNAM	XOR	A
	LD	(FXBXTND),A
	POP	AF
	POP	HL
	RET

FILLNM	LD	A,D		;GET # OF CHARS
	CP	0BH		;INITIAL POSITION?  (11+8*0-2*0)
	JP	Z,NMERR		;DONT ALLOW NULL FILENAME
	CP	03H		;FILLED FIELD?
	JP	C,NMERR		;NO, BUT 2ND "."
	RET	Z		;YES, BACK TO LOOP
	LD	A,' '		;FILL WITH SPACE
	LD	(BC),A
	INC	BC
	DEC	D
	JP	FILLNM

FILSPC	INC	D		;CHARS LEFT IN FILE BUFFER
	DEC	D		;TEST
	JP	Z,GOTNAM	;NO
FILSP2	LD	A,' '		;SPACE
	LD	(BC),A		;STORE
	INC	BC
	DEC	D		;FILLED WHOLE FIELD?
	JP	NZ,FILSP2	;NO, MORE SPACES
	JP	GOTNAM		;YES, MAKE SURE NAME OK

NMERR	JP	DERIFN		;Bad file name

;-----------------------------------------------------------------------------
; ## GIODSK.ASM:1044 ##
;	'NAME' BASIC command  (file rename)
;
;	NAME oldname AS newname
;	 Entry - [HL] = text pointer
;	 Exit  - [HL] = text pointer
NAME	CALL	NAMFIL		;PICK UP THE OLD NAME TO USE
	PUSH	HL		;SAVE THE TEXT POINTER
	LD	DE,DIRTMP	;READ DIRECTORY IN HERE
	LD	C,1AH		;SET BUFFER ADDRESS
				; (BDOS function 26 - Set DMA address)
	CALL	CPMENT		;CALL CP/M
	LD	DE,FCBDRV	;SEE IF ORIGINAL NAME EXISTS
	LD	C,0FH		;BY OPENING
				; (BDOS function 15 - Open file)
	CALL	CPMENT		;CALL CP/M
	INC	A		;DOES IT EXIST?
	JP	Z,DERFNF	;FILE NOT FOUND
	LD	HL,FILNA2	;SAVE FILE NAME IN FILNA2
	LD	DE,FCBDRV	;12+3*0-2*0+2*0+3*0-3*0 (???)
				;SET [C]=MAX FILE NAME LENGTH
	LD	B,0CH
NAMRMV	LD	A,(DE)		;GET BYTE FROM FILE
	LD	(HL),A		;SAVE BYTE IN "OLD" FILE NAME
	INC	HL		;BUMP POINTERS
	INC	DE
	DEC	B
	JP	NZ,NAMRMV
	POP	HL		;GET THE TEXT POINTER BACK
	CALL	SYNCHR
	DB	'A'		;MAKE SURE "AS" IS THERE
	CALL	SYNCHR
	DB	'S'
	CALL	NAMFIL		;READ THE NEW NAME
	PUSH	HL		;SAVE THE TEXT POINTER
	LD	A,(FCBDRV)	;GET DISK # OF FILE NAME
	LD	HL,FILNA2	;POINT TO ORIG FILE
	CP	(HL)		;COMPARE
	JP	NZ,FCERR	;DISKS MUST BE THE SAME
	LD	DE,FCBDRV	;SEE IF ORIGINAL NAME EXISTS
	LD	C,0FH		;BY OPENING
				; (BDOS function 15 - Open file)
	CALL	CPMENT		;CALL CP/M
	INC	A		;DOES IT EXIST?
	JP	NZ,DERFAE	;YES
	LD	C,17H		;RENAME OPERATION
				; (BDOS function 23 - Rename file)
	LD	DE,FILNA2	;POINT AT OLD NAME FCB
	CALL	CPMENT		;CALL CP/M
	POP	HL		;RESTORE TEXT POINTER
	RET


;=============================================================================
;	GIO86   - BASIC-86 Interpreter Device Independent I/O Module
; ## GIO86.ASM:10 ##
;
;	--------- --- ---- -- ---------
;	COPYRIGHT (C) 1982 BY MICROSOFT
;	--------- --- ---- -- ---------
;
;	        T. Corbett      Microsoft Inc.
;
;-----------------------------------------------------------------------------
;	OPEN statement
; ## GIO86.ASM:51 ##
;
;	OPEN Statement
;	Syntax:
;	 OPEN mode,fnum,filespec[,random-record-length]
;	Different versions pick the file access mode directly from the
;	FCB structure.
;	It looks like this version is avoiding to trust CP/M.
;	Note: this syntax is invalid on TRSDOS 6 BASIC:
;	 OPEN filespec FOR mode AS fnum [LEN=random-record-length]
OPEN	LD	BC,FINPRT	;ZERO PTRFIL WHEN DONE
	PUSH	BC
	CALL	FRMEVL		;READ THE FILE MODE
	PUSH	HL		;SAVE THE TEXT POINTER
	CALL	FRESTR		;FREE STRING TEMP & CHECK STRING
	LD	A,(HL)		;MAKE SURE ITS NOT A NULL STRING
	OR	A
	JP	Z,DERBFM	;IF SO, "BAD FILE MODE"
	INC	HL
	LD	C,(HL)		;[B,C] POINT AT MODE CHARACTER
	INC	HL
	LD	B,(HL)
	LD	A,(BC)		;[A]=MODE CHARACTER
	AND	0DFH		;FORCE TO UPPER CASE
	LD	D,02H		;ASSUME IT'S "O" (2=MD.SQO)
	CP	'O'		;IS IT?
	JP	Z,HAVMOD	;[D] HAS CORRECT MODE
	LD	D,01H		;ASSUME SEQUENTIAL (1=MD.SQI)
	CP	'I'		;IS IT?
	JP	Z,HAVMOD	;[D] SAYS SEQUENTIAL INPUT
	LD	D,03H		;MUST BE RANDOM (3=MD.RND)
	CP	'R'
	JP	NZ,DERBFM	;IF NOT, NO MATCH SO "BAD FILE MODE"
HAVMOD	POP	HL		;GET BACK THE TEXT POINTER
	CALL	SYNCHR
	DB	','		;SKIP COMMA BEFORE FILE NUMBER
	PUSH	DE		;SAVE THE FILE MODE
	CP	'#'		;SKIP A POSSIBLE "#"
	CALL	Z,CHRGTR
	CALL	GETBYT		;READ THE FILE NUMBER (Get integer 0-255)
	CALL	SYNCHR
	DB	','		;SKIP COMMA BEFORE NAME
	LD	A,E		;[A]=FILE NUMBER
	OR	A		;MAKE SURE FILE WASN'T ZERO
	JP	Z,DERBFN	;IF SO, "BAD FILE NUMBER"
	POP	DE		;GET BACK FILE MODE

;	PRGFIL is called to open file #0 (SAVE/LOAD/MERGE etc.)
;	 Entry - [HL]=text pointer, pointing at filename
;	         [D]=file mode
;	 Exit  - [PTRFIL] points to files FDB (directing all future I/O to file)
;	         [HL]=[TEMP]=updated text pointer
PRGFIL	LD	E,A		;SAVE FILE NUMBER IN [E]
	PUSH	DE		;SAVE THE MODE IN [D] SINCE PROGRAM FILE [A]=0
	CALL	FDBPTR		;[B,C] POINT AT FILE DATA BLOCK
	JP	NZ,DERFAO	;IF NON ZERO MODE, "FILE ALREADY OPEN"
	POP	DE		;[D]=FILE MODE
	PUSH	BC		;SAVE POINTER AT FILE DATA BLOCK
	PUSH	DE		;SAVE BACK FILE MODE AND NUMBER
	CALL	NAMFIL		;READ THE NAME
	POP	DE		;RESTORE FILE NUMBER
	POP	BC		;GET BACK FILE DATA BLOCK POINTER
	PUSH	BC		;SAVE BACK
	PUSH	AF		;SAVE EXTENSION FLAG
	LD	A,D		;GET FILE MODE
	CALL	VARECS		;SCAN RECORD LENGTH FIELD
	POP	AF		;GET BACK EXTENSION FLAG
	LD	(TEMP),HL	;SAVE THE TEXT POINTER FOR A WHILE
	JP	C,PRGDOT	;IF "." SEEN, DONT DEFAULT EXTENSION
	LD	A,E		;GET FILE NUMBER
	OR	A		;SET CONDITION CODES
	JP	NZ,PRGDOT	;NOT FILE 0, DONT DEFAULT FILE NAME
	LD	HL,FCBFTYP	;(FILNAM+9-0-0-2*0):
				; POINT TO FIRST CHAR OF EXTENSION
	LD	A,(HL)		;GET IT
	CP	' '		;BLANK EXTENSION
	JP	NZ,PRGDOT	;NON-BLANK EXTENSION, DONT USE DEFAULT
	LD	(HL),'B'	;SET DEFAULT EXTENSION
	INC	HL
	LD	(HL),'A'
	INC	HL
	LD	(HL),'S'	;SET ".BAS"
PRGDOT	POP	HL		;[H,L]=POINTER AT FILE DATA BLOCK
	LD	A,D
	PUSH	AF
	LD	(PTRFIL),HL	;SETUP AS CURRENT FILE
	PUSH	HL		;SAVE BACK FILE DATA BLOCK POINTER
	INC	HL		;POINT TO FCB ENTRY
	LD	DE,FCBDRV	;GET POINTER TO SCANNED FILE NAME
	LD	C,0CH		;(12+0+0*3+2*0+3*0 ??) NUMBER OF BYTES TO COPY

OPNLP	LD	A,(DE)		;GET BYTE FROM FILNAM
	LD	(HL),A		;STORE IN FILE DATA BLOCK
	INC	DE
	INC	HL
	DEC	C		;DECREMENT COUNT OF BYTES TO MOVE
	JP	NZ,OPNLP	;KEEP LOOPING

;	OPEN FILE
;
;	((SP)) points to File Data Block
;	((SP)+2) contains the file mode - DMC!X3200!R2E

	XOR	A
	LD	(HL),A		;MAKE SURE EXTENT FIELD IS ZERO
	LD	DE,0014H	;POINT TO NR FIELD
	ADD	HL,DE
	LD	(HL),A		;SET TO ZERO
	POP	DE		;GET POINTER TO FILE DATA BLOCK BACK IN [D]
	PUSH	DE		;SAVE AGAIN FOR LATER
	INC	DE
	CALL	SETBUF		;SET BUFFER ADDRESS
	POP	HL		;GET BACK FILE DATA BLOCK PTR
	POP	AF		;GET MODE
	PUSH	AF
	PUSH	HL		;SAVE BACK
	CP	02H		; (MD.SQO) SEQUENTIAL OUTPUT?
	JP	NZ,OPNFIL	;NO, DO CPM OPEN CALL
	PUSH	DE		;SAVE FCB POINTER
	LD	C,13H		;DELETE EXISTING OUTPUT FILE, IF ANY
				;(BDOS function 19 - delete file)
	CALL	CPMENT		;CALL CP/M
	POP	DE		;RESTORE FCB POINTER

MAKFIL	LD	C,16H		; BDOS function 22 - create file
	CALL	CPMENT		;CALL CPM
	INC	A		;TEST FOR TOO MANY FILES
	JP	Z,DERTMF	;THAT WAS THE CASE
	JP	OPNSET		;FINISH SETUP OF FILE DATA BLOCK

OPNFIL	LD	C,0FH		;CPM CODE FOR OPEN
				;(BDOS function 15 - Open file)
	CALL	CPMENT		;CALL CPM
	INC	A		;FILE NOT FOUND
	JP	NZ,OPNSET	;FOUND
	POP	DE		;GET BACK FILE POINTER
	POP	AF		;GET MODE OF FILE
				;(LD A,(DE) somewhere else)
	PUSH	AF
	PUSH	DE		;SAVE BACK FILE POINTER
	CP	03H		;RANDOM?
	JP	NZ,DERFNF	;NO, SEQENTIAL INPUT, FILE NOT FOUND
	INC	DE		;MAKE [D,E]=FCB POINTER
	JP	MAKFIL		;MAKE FILE

;	((SP)) points to File Data Block
;	((SP)+2) contains the file mode - DMC!X3200!R2E

OPNSET	POP	DE		;POINT TO FILE INFO
	POP	AF
	LD	(DE),A
	PUSH	DE		;SAVE POINTER BACK
	LD	HL,0025H	;(0+LOCOFS) POINT TO CURLOC
	ADD	HL,DE
	XOR	A		;ZERO CURLOC IN CASE THIS FILE WAS JUST KILLED
	LD	(HL),A
	INC	HL
	LD	(HL),A
	INC	HL
	LD	(HL),A		;ZERO NUMBER OF BYTES IN THE BUFFER
	INC	HL
	LD	(HL),A		;ZERO PRINT POSITION
	POP	HL		;GET POINTER AT MODE
	LD	A,(HL)		;SEE WHAT HAS TO BE DONE
	CP	03H		;IS IT RANDOM MODE?
	JP	Z,RNDFIN	;YES RANDOM FINISH UP
	CP	01H		;IF SEQUENTIAL ALL THAT IS LEFT TO DO
	JP	NZ,GTMPRT	;FETCH TEXT POINTER AND DONE
;
; FINISH UP SEQUENTIAL INPUT AFTER FINDING FILE
;
	CALL	READ2
	LD	HL,(TEMP)
	RET

RNDFIN	LD	BC,0029H	;DATOFS: NOW ADVANCE POINTER TO DATA
	ADD	HL,BC		;BY ADDING PROPER OFFSET
	LD	C,DATSPC	;# OF BYTES TO ZERO
ZRRNDT	LD	(HL),B
	INC	HL
	DEC	C
	JP	NZ,ZRRNDT
	JP	GTMPRT		;Restore code string address

;	'SYSTEM' BASIC command
;
;	SYSTEM - EXIT BASIC
;
SYSTEM	RET	NZ		;IF WASN'T EOS
	CALL	CLSALL		;CLOSE ALL DATA FILES
SYSTEMX	JP	CPMWRM		;WARM START CP/M

;	'RESET' Statement
;
;	Entry/exit:   [HL] = text pointer
;
RESET	RET	NZ		;SHOULD TERMINATE
	PUSH	HL		;SAVE TEXT POINTER
	CALL	CLSALL		;CLOSE ALL FILES
	LD	C,19H		;GET DRIVE CURRENTLY SELECTED
				; (BDOS function 25 - Get current drive)
	CALL	CPMENT		;GET IT IN [A]
	PUSH	AF		;SAVE CURRENT DRIVE #
	LD	C,0DH		;DO THE RESET CALL
				; (BDOS function 13 - Reset discs)
	CALL	CPMENT
	POP	AF		;GET DRIVE TO SELECT
	LD	E,A		;INTO [E]
	LD	C,0EH		;SET DRIVE,
				; (BDOS function 14 - Select disc (set current drive))
	CALL	CPMENT		;CALL CPM
	POP	HL		;RESTORE TEXT POINTER
	RET

;	KILL filename
;	 Entry - [BX] = text pointer
;	 Exit -  [BX] = text pointer
;
; ## GIODSK.ASM:1018 ##
KILL	CALL	NAMFIL		;SCAN FILE NAME
	PUSH	HL		;SAVE TEXT POINTER
	LD	DE,DIRTMP	;(DIRTMP) READ DIRECTORY IN HERE
	LD	C,1AH		;SET BUFFER ADDRESS
				;(BDOS function 26 - Set DMA address)
	CALL	CPMENT		;FOR CP/M
	LD	DE,FCBDRV	;TRY TO OPEN FILE
	PUSH	DE		;SAVE FCB POINTER
	LD	C,0FH		;BDOS function 15 - Open file
	CALL	CPMENT
	INC	A		;FILE FOUND?
	POP	DE		;GET BACK POINTER TO FCB
	PUSH	DE		;SAVE BACK
	PUSH	AF		;SAVE FOUND FLAG
	LD	C,10H		;THIS MAY NOT BE NESC.
				;(BDOS function 16 - Close file)
	JP	Z,KILL0
	CALL	CPMENT		;CLOSE FILE
KILL0	POP	AF		;RESTORE FOUND INDICATOR
	POP	DE		;RESTORE FCB POINTER
	JP	Z,DERFNF	;YES
	LD	C,13H		;BDOS function 19 - delete file
	CALL	CPMENT		;CALL CPM
	POP	HL		;GET BACK TEXT POINTER
	RET

;	FILES [ filename ]
;
;	FILES command [List the Directory]
;	If filename is omitted, all files on the logged
;	disk are listed.
;	If supplied, all files matching filename or wildcards
;	are listed.
;
FILES	JP	NZ,FILES0	;FILE NAME WAS SPECIFIED
	PUSH	HL		;SAVE TEXT POINTER
	LD	HL,FCBDRV	;POINT TO FILE NAME
	LD	(HL),00H	;SET CURRENT DRIVE
	INC	HL		;BUMP POINTER
	LD	C,0BH		;MATCH ALL FILES
	CALL	FNAMFL		;SET FILE NAME AND EXTENSION TO QUESTION MARKS
	POP	HL		;RESTORE TEXT POINTER
FILES0	CALL	NZ,NAMFIL	;SCAN FILE NAME
	XOR	A		;MAKE SURE EXTENT IS ZERO
	LD	(FXBXTND),A	; (FILNAM+12)
	PUSH	HL		;SAVE TEXT POINTER
	LD	HL,FCBFNAM	;GET FIRST CHAR OF FILE NAME
	LD	C,08H		;FILL NAME WITH QUESTION MARKS
	CALL	FNAMQS
	LD	HL,FCBFTYP	;POINT TO EXTENSION
	LD	C,03H		;3 CHARS IN EXTENSION
	CALL	FNAMQS		;FILL IT WITH QMARKS
	LD	DE,DIRTMP	;SET BUFFER TO 80 HEX
	LD	C,1AH		;(C.BUFF)
				;BDOS function 26 - Set DMA address
	CALL	CPMENT
	LD	DE,FCBDRV	;POINT TO FCB
	LD	C,11H		;DO INITIAL SEARCH FOR FILE
				;BDOS function 17 - search for first
	CALL	CPMENT
	CP	0FFH		;FIND FIRST INCARNATION OF FILE
	JP	Z,DERFNF	;NO
FILNXT	AND	03H		;MASK OFF LOW TWO BITS
	ADD	A,A		;MULTIPLY BY 32
	ADD	A,A
	ADD	A,A
	ADD	A,A
	ADD	A,A
	LD	C,A		;PUT OFFSET IN [B,C]
	LD	B,00H
	LD	HL,DIRTMP+1	;POINT TO DIRECTORY BUFFER
	ADD	HL,BC		;POINT TO FCB ENTRY IN DIRECTORY
	LD	C,0BH		; (11+5*0+11*0 ??) CHARS IN NAME
FILES2	LD	A,(HL)		;GET FILE NAME CHAR
	INC	HL		;BUMP POINTER
;	AND	7FH		;Force it to 7 bit ASCII
;				;(does not exist on previous versions)
	CALL	OUTDO		;PRINT IT
	LD	A,C		;GET  CHAR POSIT
	CP	04H		; (4+5*0) ABOUT TO PRINT EXTENSION?
	JP	NZ,NOTEXT	;NO
	LD	A,(HL)		;GET FIRST CHAR OF EXTENSION
	CP	' '		;IF SO, NOT SPACE
	JP	Z,PRISPA	;PRINT SPACE
	LD	A,'.'		;PRINT DOT
PRISPA	CALL	OUTDO
NOTEXT	DEC	C		;DECREMENT CHAR COUNT
	JP	NZ,FILES2	;MORE OF NAME TO PRINT
	LD	A,(TTYPOS)	;GET CURRENT TTY POSIT
	ADD	A,0DH		;SPACE FOR NEXT NAME?
	LD	D,A		;SAVE IN D
	LD	A,(TTYSIZ)	;GET LENGTH OF TERMINAL LINE
	CP	D		;COMPRE TO CURRENT POSIT
	JP	C,NWFILN	;NEED TO FORCE CRLF
	LD	A,' '		;TWO SPACES BETWEEN FILE NAMES
	CALL	OUTDO
	CALL	OUTDO
				;OR THREE
NWFILN	CALL	C,CRDO		;TYPE CRLF
	LD	DE,FCBDRV	;POINT AT FCB
	LD	C,12H		;SEARCH FOR NEXT ENTRY
				;(BDOS function 18 - search for next)
	CALL	CPMENT		;SEARCH FOR NEXT INCARNATION
	CP	0FFH		;NO MORE?
	JP	NZ,FILNXT	;MORE.
	POP	HL		;RESTORE TEXT POINTER
	RET

; 	Deal with file name
;	aka FILENAME_QS
FNAMQS	LD	A,(HL)		;GET CHAR
	CP	'*'		;WILD CARD?
	RET	NZ		;NO, RETURN
;	aka FILENAME_FILL
FNAMFL	LD	(HL),'?'	;STORE QUESTION MARK
	INC	HL		;BUMP POINTER
	DEC	C		;DECREMENT COUNT OF QMARKS
	JP	NZ,FNAMFL	;KEEP SAVING QMARKS
	RET


; DSKF FUNCTION
; Miscellaneous Operating System I/O

; Enter BDOS for file read or write operations
;
; Used by the routines at __EOF and INDSKB.
; Called after picking the current function from CPMREA / CPMWRT
ACCFIL	PUSH	DE		;Save FCB address
	LD	C,A
	PUSH	BC
	CALL	CPMENT
	POP	BC
	POP	DE
	PUSH	AF
	LD	HL,0021H	;(0+FCB.RN) Point to random record number
	ADD	HL,DE		;Now HL points to the random access record number
	INC	(HL)		;Increment record number LSB (R0)
	JP	NZ,ACCFL1
	INC	HL		;Increment record number R1
	INC	(HL)		;Increment record number R2
	JP	NZ,ACCFL1	;NO
	INC	HL
	INC	(HL)
ACCFL1	LD	A,C		;Get back CPM call code
	CP	'"'		;was it a 'random write' BDOS call ?
	JP	NZ,ACCFL2
	POP	AF		;Get error code and map into 1.4 errors
	OR	A
	RET	Z		;Return if write OK
	CP	05H
	JP	Z,DERTMF	;JP if Directory full (Too many files)
	CP	03H
	LD	A,01H		;Turn into I/O error
	RET	Z		;Return if Disk full
	INC	A		;DEFAULT TO DISK SPACE FULL (2)
	RET

ACCFL2	POP	AF
	RET

;
;	The 5.0 Disk code is essentially an extra level of buffering for random disk I/O files.
;	Sequential I/O is not affected by the 5.0 code.
;	Great care has been taken to ensure compatibility with existing code
;	to support diverse operating systems.
;	The 5.0 disk code has its own data structure for handling the variable length records in random files.
;	This data structure sits right after the regular data block for the file and consumes an amount of
;	memory equal to MAXREC (The maximum allowed record size) plus 9 bytes.
;
;	Here is the content of the data block:
;
;	FD.SIZ size 2          ;Variable length record size default 128
;	FD.PHY size 2          ;Current physical record #
;	FD.LOG size 2          ;Current logical record number
;	FD.CHG size 1          ;Future flag for accross block PRINTs etc.
;	FD.OPS size 2          ;Output print position for PRINT, INPUT, WRITE
;	FD.DAT size FD.SIZ     ;Actual FIELD data buffer
;                              ;Size is FD.SIZ bytes long
;
;	DATE				FIX
;	----				---
;	8/6/1979           Make PUT, GET increment LOC correctly
;	8/14/1979          PUT in BASIC compiler switch (main source)

;	VARECS - Variable record scan for OPEN

;	Enter VARECS with file mode in [A]

VARECS	CP	03H		;Random?
	RET	NZ		;No, give error later if he gave record length
	DEC	HL		;Back up pointer
	CALL	CHRGTR		;Test for eol
	PUSH	DE		;Save [D,E]
	LD	DE,0080H	;Assume record length=DATPSC
	JP	Z,NOTSEP	;No other params for OPEN
	PUSH	BC		;Save file data block pointer
	CALL	INTIDX		;Get record length
	POP	BC		;Get back file data block
NOTSEP	PUSH	HL
	LD	HL,(RECSIZ)	;Save text pointer
	CALL	COMPAR		;Is size ok?
	JP	C,FCERR
	LD	HL,00A9H
	ADD	HL,BC		;Stuff into data block:
				;(FD.SIZ) POINT TO RECORD SIZE
	LD	(HL),E
	INC	HL
	LD	(HL),D
	XOR	A
	LD	E,07H		;Clear other bytes in data block
ZOFIVB	INC	HL		;# of bytes to clear;Increment pointer
	LD	(HL),A		;Clear byte
	DEC	E		;Count down
	JP	NZ,ZOFIVB	;Go back for more
	POP	HL		;Text pointer
	POP	DE		;Restore [D,E]
	RET

;	Syntax - GET fn [,recnum]   (if no recnum next relative record assumed)
;	         PUT fn [,recnum]
;	 Entry - [BX] = text pointer
;	         [CX] = 0 for GET, 1 for PUT
PUTST	DB	0F6H		;Set PUT flag

GETST	XOR	A		;Set GET flag
	LD	(PUTGETF),A	;PUT/GET Flag
	CALL	FILSCN		;[A]=file mode
	CP	03H		;Not random - bad file mode
	JP	NZ,DERBFM	;Bad file mode
	PUSH	BC		;Save pointer at file data block
	PUSH	HL		;Save text pointer
	LD	HL,00ADH	;(FD.LOG) Fetch current logical posit
	ADD	HL,BC
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	INC	DE		;Compensate for "DCX D" when call INTIDX
	EX	(SP),HL		;Save data block pointer and get text pointer
	LD	A,(HL)
	CP	','		;Is there a record number
	CALL	Z,INTIDX	;Read it if there, 1-indexed
	DEC	HL		;Make sure statement ends
	CALL	CHRGTR
	JP	NZ,SNERR
	EX	(SP),HL		;Save text pointer, get data block pointer
	LD	A,D		;Get record #
	OR	E		;Make sure its not zero
	JP	Z,DERBRN	;If so, "Bad record number"
	DEC	HL
	LD	(HL),E
	INC	HL
	LD	(HL),D
	DEC	DE
	POP	HL		;Get back text pointer
	POP	BC
	PUSH	HL		;Save back text pointer
	PUSH	BC		;Pointer to file data block
	LD	HL,00B0H	;FD.OPS: Zero output file posit
	ADD	HL,BC
	XOR	A
	LD	(HL),A
	INC	HL
	LD	(HL),A
	LD	HL,00A9H	;(FD.SIZ) Get logical record size in [D,E]
	ADD	HL,BC
	LD	A,(HL)
	INC	HL
	LD	H,(HL)
	LD	L,A
	EX	DE,HL		;Record size to [D,E], posit in [H,L]
	PUSH	DE		;Save record size (count of bytes)

;	Record size in [D,E]
;	Logical position in [H,L]
;	This code computes physical record # in [H,L]
;	offset into buffer in [D,E]
	PUSH	HL		;Save logical posit
	LD	HL,0080H	;Get sector size
	CALL	COMPAR		;Compare the two
	POP	HL		;Restore logical posit
	JP	NZ,NTLSAP	;If record size=sector size, done
	LD	DE,0000H	;Set offset to zero
	JP	DONCLC		;Done with calculations

NTLSAP	LD	B,D		;Copy record size to [B,C]
	LD	C,E
	LD	A,10H		;16 by 16 multiply
	EX	DE,HL		;Put multiplier in [D,E]
	LD	HL,0000H	;Set both parts of product to zero
	PUSH	HL		;2nd part is on stack
FRMUL1	ADD	HL,HL
	EX	(SP),HL
	JP	NC,FNOCRY
	ADD	HL,HL
	INC	HL
	JP	FNOCY0

FNOCRY	ADD	HL,HL
FNOCY0	EX	(SP),HL
	EX	DE,HL
	ADD	HL,HL		;Rotate [D,E] left one
	EX	DE,HL
	JP	NC,FNOCY2	;Add in [B,C] if Ho=1
	ADD	HL,BC
	EX	(SP),HL
	JP	NC,FNOINH
	INC	HL
FNOINH	EX	(SP),HL
FNOCY2	DEC	A		;are we done multiplying
	JP	NZ,FRMUL1	;No, go back for next bit of product
	LD	A,L		;Get low byte of result
	AND	7FH		;Get rid of high bit
	LD	E,A		;this is it
	LD	D,00H		;Set high byte of remainder to zero
	POP	BC		;Get high word of product
	LD	A,L		;Get MSB of low word
	LD	L,H
	LD	H,C
	ADD	HL,HL		;Make space for it
	JP	C,FCERR		;UH-OH record # to big!
	RLA			;Is it set?
	JP	NC,DONINH	;Not set
	INC	HL		;Copy it into low bit
DONINH	LD	A,B		;Get high byte of record #
	OR	A		;Is it non-zero?
	JP	NZ,FCERR

;	At this point, record # is in [H,L]
;	offset into record in [D,E]
;	Stack:
;		COUNT of bytes to read or write
;		data block
;		Text pointer
;		Return Address
DONCLC	LD	(RECORD),HL	;Save record size
	POP	HL		;Get count
	POP	BC		;Pointer to file data block
	PUSH	HL		;Save back count
	LD	HL,00B2H	;FD.DAT: Point to Field buffer
	ADD	HL,BC		;Add start of data block
	LD	(LBUFF),HL	;Save pointer to FIELD buffer
NXTOPD	LD	HL,0029H	;DATOFS: Point to physical buffer
	ADD	HL,BC		;Add file block offset
	ADD	HL,DE
	LD	(PBUFF),HL	;PBUFF: Save
	POP	HL		;Get count
	PUSH	HL		;Save count
	LD	HL,0080H	;[H,L]=DATPSC-offset
	LD	A,L
	SUB	E
	LD	L,A
	LD	A,H
	SBC	A,D
	LD	H,A
	POP	DE		;Get back count (destroy offset)
	PUSH	DE		;Save COUNT
	CALL	COMPAR		;Which is smaller, count or DATPSC-offset?
	JP	C,DATMOF	;The latter
	LD	H,D		;Copy count into bytes
	LD	L,E
DATMOF	LD	A,(PUTGETF)	;PUT or GET
	OR	A		;Set cc's
	JP	Z,FIVDRD	;Was Read
	LD	DE,0080H	;If bytes .LT. DATPSC then read(sector)
	CALL	COMPAR
	JP	NC,NOFVRD	;(Idea-if writing full buffer, no need to read)
	PUSH	HL		;Save bytes
	CALL	GETSUB		;Read record.
	POP	HL		;Bytes
NOFVRD	PUSH	BC
	LD	B,H
	LD	C,L
	EX	DE,HL
	LD	HL,(PBUFF)
	EX	DE,HL
	LD	HL,(LBUFF)	;Get ready to move bytes between buffers
	CALL	FDMOV		;Move bytes to physical buffer
	LD	(LBUFF),HL	;Store updated pointer
	LD	D,B		;COUNT TO [D,E]
	LD	E,C
	POP	BC		;Restore FDB pointer
	CALL	PUTSUB		;Do write
NXFVBF	LD	HL,(RECORD)
	INC	HL		;Increment it
	LD	(RECORD),HL	;Save back
	POP	HL		;Count

	LD	A,L		;Make count correct
	SUB	E
	LD	L,A
	LD	A,H
	SBC	A,D
	LD	H,A
	LD	A,H
	OR	L		;Is count zero?
	LD	DE,0000H	;Set offset=0
	PUSH	HL		;Save COUNT
	JP	NZ,NXTOPD	;Keep working on it
	POP	HL		;Get rid of COUNT
	POP	HL		;Restore text pointer
	RET			;Done

; Read code
; [H,L]=bytes
; [D,E]=count
FIVDRD	PUSH	HL		;Save bytes
	CALL	GETSUB		;Do read
	POP	HL		;Get back bytes
	PUSH	BC
	LD	B,H
	LD	C,L
	EX	DE,HL
	LD	HL,(LBUFF)	;Point to logical buffer
	EX	DE,HL
	LD	HL,(PBUFF)
	CALL	FDMOV
	EX	DE,HL		;Get pointer to FIELD buffer in [H,L]
	LD	(LBUFF),HL	;Save back updated logical buffer
	LD	D,B		;COUNT TO [D,E]
	LD	E,C
	POP	BC
	JP	NXFVBF

PUTSUB	DB	0F6H		;"OR n" to Mask 'XOR A'

GETSUB	XOR	A
	LD	(PGTFLG),A	;GET/PUT Fflag
	PUSH	BC
	PUSH	DE
	PUSH	HL
	EX	DE,HL
	LD	HL,(RECORD)
	EX	DE,HL
	LD	HL,00ABH	;FD.PHY: Point to physical record #
	ADD	HL,BC		;Add offset to file buffer
	PUSH	HL		;Save this pointer
	LD	A,(HL)		;Get current phys. rec #
	INC	HL
	LD	H,(HL)
	LD	L,A
	INC	DE
	CALL	COMPAR		;Do we already have record in buffer
	POP	HL		;Restore pointer
	LD	(HL),E
	INC	HL
	LD	(HL),D		;Store new record number
	JP	NZ,NTREDS	;Curent and previos record numbers are different
	LD	A,(PGTFLG)	;Trying to do read?
	OR	A
	JP	Z,SUBRET	;If trying to read and record already in buffer, do nothing
NTREDS	LD	HL,SUBRET	;Where to return to
	PUSH	HL
	PUSH	BC		;File data block
	PUSH	HL		;Dummy text pointer
	LD	HL,0026H	;LOCOFS+1: where [H,L] is expected to be
	ADD	HL,BC
	JP	FIVDPT		;Call old PUT/GET

SUBRET	POP	HL		;Restore all regs and return to caller
	POP	DE
	POP	BC
	RET

;	LDIR on the 8080/8085
;	Move bytes from [H,L] to [D,E] [B,C] times
FDMOV	PUSH	BC		;Save count
FDMOV1	LD	A,(HL)		;Get byte
	LD	(DE),A		;Store it
	INC	HL
	INC	DE
	DEC	BC		;Decrement count
	LD	A,B		;Gone to zero?
	OR	C
	JP	NZ,FDMOV1	;Go back for more
	POP	BC		;Return with count
	RET

FILOFV	POP	AF		;Get character off stack
	PUSH	DE		;Save [D,E]
	PUSH	BC		;Save [B,C]
	PUSH	AF		;Save back char
	LD	B,H		;[B,C]=file data block
	LD	C,L
	CALL	CMPFPS		;Any room in buffer
	JP	Z,DERFOV1	;No
	CALL	SETFPI		;save new position
	LD	HL,00B1H	;FD.DAT-1: Index into data buffer
	ADD	HL,BC		;Add start of file control block
	ADD	HL,DE		;Add offset into buffer
	POP	AF		;Get back char
	LD	(HL),A		;Store in buffer
	PUSH	AF		;Save char
	LD	HL,0028H	;NMLOFS: Set up [H,L] to point at print posit
	ADD	HL,BC
	LD	D,(HL)		;Get present position
	LD	(HL),00H	;Assume set it to zero
	CP	0DH		;Is it <Cr>?
	JP	Z,FISCR		;Yes
	ADD	A,0E0H		;Set carry for spaces & higher
	LD	A,D		;Add one to current posit
	ADC	A,00H
	LD	(HL),A
FISCR	POP	AF		;Restore all regs
	POP	BC
	POP	DE
	POP	HL
	RET

DERFOV1	JP	DERFOV		;FIELD overflow

FILIFV	PUSH	DE
	CALL	CMPFBC		;Save [D,E]
	JP	Z,DERFOV1	;Compare to present posit
	CALL	SETFPI		;Return with null
	LD	HL,00B1H	;FD.DAT-1: Set new position
	ADD	HL,BC		;Point to data
	ADD	HL,DE
	LD	A,(HL)		;Get the byte
	OR	A		;Clear carry (no EOF)
	POP	DE		;Restore [D,E]
	POP	HL		;Restore [H,L]
	POP	BC		;Restore [B,C]
	RET

GETFSZ	LD	HL,00A9H	;FD.SIZ: Point to record size
	JP	GETFP1		;Continue

GETFPS	LD	HL,00B0H	;FD.OPS: Point to output position
GETFP1	ADD	HL,BC		;Add offset into buffer
	LD	E,(HL)		;Get value
	INC	HL
	LD	D,(HL)
	RET

SETFPI	INC	DE		;Increment current posit
	LD	HL,00B0H	;FD.OPS: Point to output position
	ADD	HL,BC		;Add file control block address
	LD	(HL),E
	INC	HL
	LD	(HL),D
	RET

CMPFBC	LD	B,H		;Copy file data block into [B,C]
	LD	C,L
CMPFPS	CALL	GETFPS		;Get present posit
	PUSH	DE		;Save it
	CALL	GETFSZ		;Get file size
	EX	DE,HL		;into [H,L]
	POP	DE		;Get back posit
	CALL	COMPAR		;See if were at end
	RET


;-----------------------------------------------------------------------------
;	PROSAV - Protected SAVE
; ## GIODSK.ASM:807 ##
;
;	PROSAV: Encode and save the BASIC program text
PROSAV	CALL	CHRGTR		;skip "P"
	LD	(TEMP),HL	;Save text pointer
	CALL	SCCPTR		;Get rid of GOTO pointers
	CALL	PENCOD		;Encode binary
	LD	A,0FEH		;ID byte for Protected files
	CALL	BINPSV		;Do the SAVE
	CALL	PROLOD		;Decode binary
	JP	GTMPRT		;return to NEWSTT

;	C=N1=11D Number of bytes to use from ATNCON
;	B=N2=13D Number of bytes to use from SINCON
;	Note: GW-BASIC uses constant tables $EXPCN and $LOGP
;
N1	EQU	11		;Number of bytes to use from ATNCON
N2	EQU	13  		;Number of bytes to use from SINCON
;
;	PENCOD: Encode the BASIC program text in memory
PENCOD	LD	BC,N2*256+N1	;Initialize both counters
	LD	HL,(TXTTAB)	;Starting point
	EX	DE,HL		;Into [DX]
ENCDBL	LD	HL,(VARTAB)	;At end?
	CALL	COMPAR		;Test
	RET	Z		;Yes
	LD	HL,$ATNCN	;Point to first scramble table
	LD	A,L		;Use [C] to index into it
	ADD	A,C
	LD	L,A
	LD	A,H
	ADC	A,00H
	LD	H,A
	LD	A,(DE)		;[A]=byte from program
	SUB	B		;Subtract counter for no reason
	XOR	(HL)		;XOR entry
	PUSH	AF		;Save result
	LD	HL,$SINCN	;calculate offset into SINCON using [B]
	LD	A,L
	ADD	A,B
	LD	L,A
	LD	A,H
	ADC	A,00H
	LD	H,A
	POP	AF		;Get back current byte
	XOR	(HL)		;XOR on this one too
	ADD	A,C		;Add counter for randomness
	LD	(DE),A		;store back in program
	INC	DE		;Increment pointer
	DEC	C		;decrement first table index
	JP	NZ,CNTZER	;Still non-Zero
	LD	C,N1		;Re-initialize counter 1
CNTZER	DEC	B		;dedecrement counter-2
	JP	NZ,ENCDBL	;Still non-zero, go for more
	LD	B,N2		;Re-initialize counter 2
	JP	ENCDBL		;Keep going until done

;	PDECOD: Decode the BASIC program text in memory
PROLOD	LD	BC,N2*256+N1	;Initialize both counters
	LD	HL,(TXTTAB)	;Starting point
	EX	DE,HL		;Into [D,E]
DECDBL	LD	HL,(VARTAB)	;At end?
	CALL	COMPAR		;Test
	RET	Z		;Yes
	LD	HL,$SINCN	;calculate offset into SINCON using [B]
	LD	A,L
	ADD	A,B
	LD	L,A
	LD	A,H
	ADC	A,00H
	LD	H,A
	LD	A,(DE)		;[AL]=byte from program
	SUB	C		;Subtract counter for randomness
	XOR	(HL)		;XOR on this one too
	PUSH	AF		;Save result
	LD	HL,$ATNCN	;Point to first scramble table
	LD	A,L		;Use [CL] to index into it
	ADD	A,C
	LD	L,A
	LD	A,H
	ADC	A,00H
	LD	H,A
	POP	AF		;Get back current byte
	XOR	(HL)		;XOR entry
	ADD	A,B		;Add counter for no reason
	LD	(DE),A		;store [AL] back in program
	INC	DE		;Increment pointer
	DEC	C		;decrement first table index
	JP	NZ,CNTZR2	;Still non-Zero
	LD	C,N1		;Re-initialize counter 1
CNTZR2	DEC	B
	JP	NZ,DECDBL	;Decrement counter-2, Still non-zero, go for more
	LD	B,N2		;Re-initialize counter 2
	JP	DECDBL		;Keep going until done

;	PRODIR: Check protection flag in direct mode and
;	raise FC Error if yes
PRODIR	PUSH	HL		;Save [H,L]
	LD	HL,(CURLIN)	;Get current line #
	LD	A,H		;Direct?
	AND	L
	POP	HL		;Restore [H,L]
	INC	A		;Direct? (if A=0, direct)
	RET	NZ

;	PROCHK: Check protection flag and
;	raise FC Error if yes
PROCHK	PUSH	AF		;Save flags
	LD	A,(PROFLG)	;Is this a protected file?
	OR	A		;Set CC's
	JP	NZ,FCERR	;Yes, give error
	POP	AF		;Restore flags
	RET

;-----------------------------------------------------------------------------
; ## GWDATA.ASM:1424 ##
;
RECORD	DW	0000H		;Record #
LBUFF	DW	0000H		;Logical buffer address
PBUFF	DW	0000H		;Physical buffer address
PUTGETF	DB	00H		;PUT/GET flag (Non zero=PUT)

;-----------------------------------------------------------------------------
INITSA	CALL	NODSKS		;Clear Files Table
	LD	HL,(TXTTAB)
	DEC	HL
	LD	(HL),00H
	LD	HL,(TEMP8)	;POINT TO START OF COMMAND LINE
	LD	A,(HL)		;GET BYTE POINTED TO
	OR	A		;IF ZERO, NO FILE SEEN
	JP	NZ,LRUN		;TRY TO RUN FILE
	JP	READY

;-----------------------------------------------------------------------------
;	BASIC-80 Initialization
;
;	INIT - System Initialization Code
;
; ## GWINIT.ASM:56 ##
;
;	CP/M Page 0 Map:
;	0000	Warm Start Jump
;	0001	BIOS area start + 3
;	0003	IOBYTE
;	0004	Current Drive (b0-b3) & User Code (b4-b7)
;	0005	Jump to BDOS (and BDOS area start)
;	0006	BDOS area start + 6
;	0008	RST 1
;	:	:
;	0030	RST 6
;	0038	RST 7 (DDT & SID)
;	0040-4F	BIOS work area
;	0050-5B	...
;	005C-7F	Default FCB
;	0080-FF	Default file buffer
;		and command tail from 0081H, with length in 0080H
;
;	BIOS calls
;	-1 -03	BOOT	Cold boot
;	 0  00	WBOOT	Warm boot
;	 1  03	CONST	Console status
;	 2  06	CONIN	Console input
;	 3  09	CONOUT	Console output
;	 4  0C	LIST	Listing output
;	 5  0F	PUNCH	Logical punch (paper tape)
;	 6  12	READER	Logical reader (paper tape)
;	14  2A	LISTST	Listing status
;
;
;	BDOS Services: (C := service code)
;	1	Get a byte from console -> A
;	2	Write a byte to console <- E
;	9	Write a string to console <- DE - Terminator is '$'
;	10	Get a line from console <- DE
;	11	Check console status -> A = 0 if no key pressed
;	15	Open existing file <- DE = FCB
;	16	Close the file <- DE = FCB
;	19	Delete a file <- DE = FCB, A = ret code
;	20	Sequential read <- DE = FCB -> A = ret code, 0 if not EOF
;	21	Sequential write <- DE = FCB -> A = ret code, 0 if OK
;	22	Make a file <- DE = FCB
;	23	Rename a file <- DE = FCB
;	26	Set buffer address <- DE = Buffer
;	33	Direct read <- DE = FCB
;	34	Direct write <- DE = FCB
;	36	Get direct address <- DE = FCB
;	40	Write with zero fill <- DE = FCB
;
DSKDAT	DW	0000H

INIT:
	LD	HL,TSTACK	;SET UP TEMP STACK
	LD	SP,HL
	XOR	A
	LD	(PROFLG),A	;INITIALIZE PROTECT FLAG
	LD	(FILTAB),HL
	LD	SP,HL
	LD	HL,KBUF1	;INITIALIZE KBUF-1 WITH A COLON
	LD	(HL),':'	;DIRECT INPUTS RESTART OK.
	CALL	STKINI		;REALLY SET UP INIT'S TEMPORARY STACK
	LD	(TTYPOS),A
	LD	(SAVSTK),HL	;WE RESTORE STACK WHEN ERRORS
	LD	HL,(0001H)	;GET START OF BIOS VECTOR TABLE
	LD	BC,0004H	;CSTS
	ADD	HL,BC		;ADD FOUR
	LD	E,(HL)		;PICK UP CSTS ADDRESS
	INC	HL
	LD	D,(HL)
	EX	DE,HL		;GET CSTS ADDRESS
	LD	(VCONST1),HL	;BIOS CONST vector #1 -> THIRD CONTROL-C CHECK
	LD	(VCONST2),HL	;BIOS CONST vector #2 -> SAVE
	LD	(VCONST3),HL	;BIOS CONST vector #3 -> FAST CONTROL-C CHECK
	EX	DE,HL		;POINTER BACK TO [H,L]
	INC	HL		;POINT AT CI ADDRESS
	INC	HL
	LD	E,(HL)		;GET LOW BYTE OF CI ADDRESS
	INC	HL
	LD	D,(HL)		;GET HIGH BYTE
	EX	DE,HL		;INPUT ADDRESS TO [H,L]
	LD	(VCONIN),HL	;BIOS CONIN vector -> CONSOLE INPUT CALL
	EX	DE,HL		;POINTER BACK TO [H,L]
	INC	HL		;SKIP "JMP" OPCODE
	INC	HL		;BUMP POINTER
	LD	E,(HL)		;GET OUTPUT ROUTINE ADDRESS
	INC	HL
	LD	D,(HL)
	EX	DE,HL		;INTO [H,L]
	LD	(VCONOUT),HL	;BIOS CONOUT vector -> OUTPUT ROUTINE
	EX	DE,HL		;POINTER BACK TO [H,L]
	INC	HL		;NOW POINT TO PRINTER OUTPUT
	INC	HL		;ROUTINE ADDRESS
	LD	E,(HL)		;PICK IT UP
	INC	HL
	LD	D,(HL)
	EX	DE,HL		;GET ADDRESS INTO [D,E]
	LD	(VLIST),HL	;BIOS LIST vector -> PRINT ROUTINE ADDRESS
;
;       Check CP/M Version Number
;
	LD	C,0CH		;BDOS function 12 - Get BDOS version number
	CALL	CPMENT
	LD	(CPMVER),A	;CP/M version in BCD format (22)
	OR	A
	LD	HL,1514H	;FN2=$15 (CP/M 1 WR), FN1=14 (CP/M 1 RD)
	JP	Z,CPMVR1	;JP if BDOS Version 1
	LD	HL,2221H	;FN2=$22 (Write record FN),
				; FN1=$21 (Read record FN)
CPMVR1	LD	(CPMREA),HL	;Load the BDOS FN code pair (FN1+FN2)
	LD	HL,0FFFEH	;SAY INITIALIZATION IS EXECUTING
	LD	(CURLIN),HL	;IN CASE OF ERROR MESSAGE
	XOR	A
	LD	(CNTOFL),A
	LD	(ENDBUF),A	;MAKE SURE OVERRUNS STOP
	LD	(CHNFLG),A	;MAKE SURE CHAINS AND MERGES..
	LD	(MRGFLG),A	;..DONT TRY TO HAPPEN
	LD	(ERRFLG),A	;DON'T ALLOW EDIT TO BE CALLED ON ERRORS
	LD	HL,0000H
	LD	(LPTPOS),HL	;ZERO FLAG AND POSITION
	LD	HL,0080H	;The default record size is 128 bytes.
	LD	(RECSIZ),HL	;=MAXREC
	LD	HL,TEMPST
	LD	(TEMPPT),HL
	LD	HL,PRMSTK	;INITIALIZE PARAMETER BLOCK CHAIN
	LD	(PRMPRV),HL

;-----------------------------------------------------------------------------
;	Read Operating System Parameters (memsiz etc.)
;
; ## GWINIT.ASM:170 ##
;
;	THE FOLLOWING CODE SCANS A CP/M COMMAND LINE FOR BASIC.
;	THE FORMAT OF THE COMMAND IS:
;
;	  BASIC <FILE NAME>[/M:<TOPMEM>][/F:<FILES>][/S:<MAX RECORD SIZE>]
;
;	THE FOLLOWING SWITCHES ARE RECOGNIZED:
;
;       	/M:<TOPMEM>
;       	/F:<FILES>
;       	/S:<MAX RECORD SIZE>
;
	LD	HL,(0006H)	;HL=BDOS entry address (=LAST LOC IN MEMORY)
	LD	(MEMSIZ),HL	;-> USE AS DEFAULT
	LD	A,03H
	LD	(NFILES),A
	LD	HL,ZEROB	;LOCATED ZERO BYTE
	LD	(TEMP8),HL	;SO IF RE-INITAILIZE OK
	LD	A,(COMAGN)
	OR	A		;IS THERE A COMMAND?
	JP	NZ,DONCMD	;NOTHING IN COMMAND BUFFER
	INC	A
	LD	(COMAGN),A	;WE HAVENT SCANNED COMMAND YET
	LD	HL,BASE+0080H	;POINT TO FIRST CHAR OF COMMAND BUFFER
	LD	A,(HL)		;WHICH CONTAINS # OF CHARS IN COMMAND
	OR	A		;IS THERE A COMMAND?
	LD	(TEMP8),HL	;SAVE POINTER TO THIS ZERO
	JP	Z,DONCMD	;NOTHING IN COMMAND BUFFER
	LD	B,(HL)		;AND [B]
	INC	HL		;POINT TO FIRST CHAR IN BUFFER
TBF_LP	LD	A,(HL)		;GET CHAR FROM BUFFER
	DEC	HL		;BACK UP POINTER
	LD	(HL),A		;STORE CHAR BACK
	INC	HL		;NOW ADVANCE CHAR TO ONE PLACE
	INC	HL		;AFTER PREVIOUS POSIT.
	DEC	B
	JP	NZ,TBF_LP	;KEEP MOVING CHARS
	DEC	HL		;BACK UP POINTER
	LD	(HL),00H	;STORE TERMINATOR FOR CHRGET (0)
	LD	(TEMP8),HL	;SAVE POINTER TO NEW ZERO (OLD DESTROYED)
	LD	HL,BASE+007FH	;POINT TO CHAR BEFORE BUFFER
	CALL	CHRGTR		;IGNORE LEADING SPACES
	OR	A
	JP	Z,DONCMD	;END OF COMMAND
;
;	Command line parameters usage example
;
;	A>MBASIC PRGM/F:2/M:&H9000
;	Use first 36K of memory, 2 files, and execute PRGM.BAS.
	CP	'/'		;IS IT A SLASH
	JP	Z,FNDSLH	;YES
	DEC	HL		;BACK UP POINTER
	LD	(HL),'"'	;STORE DOUBLE QUOTE
	LD	(TEMP8),HL	;SAVE POINTER TO START OF FILE NAME
	INC	HL		;BUMP POINTER
ISSLH	CP	'/'		;OPTION?
	JP	Z,FNDSLH	;YES
	CALL	CHRGTR		;SKIP OVER CHAR IN FILE NAME
	OR	A		;SET CC'S
	JP	NZ,ISSLH	;KEEP LOOKING FOR OPTION
	JP	DONCMD		;THATS IT

FNDSLH	LD	(HL),00H	;STORE TERMINATOR OVER "/"
	CALL	CHRGTR		;GET CHAR AFTER SLASH
SCANS1	CALL	MAKUPL		;CONVERT SWITCH TO UPPER CASE
	CP	'S'		;[/S:<maximum record size>]
	JP	Z,WASS
	CP	'M'		;[/M:<highest memory location>]
	PUSH	AF		;SAVE INDICATOR
	JP	Z,WASM		;WAS MEMORY OPTION
	CP	'F'		;[/F:<number of files>]
	JP	NZ,SNERR	;NOT "M" OR "F" ERROR

WASM	CALL	CHRGTR		;GET NEXT CHAR
	CALL	SYNCHR
	DB	':'		;MAKE SURE COLON FOLLOWS
	CALL	CNSGET		;[DE]=VALUE FOLLOWING COLON
	POP	AF		;GET BACK M/F FLAG
	JP	Z,MEM		;WAS MEMORY OPTION

;	If /F:<number of files> is present, it sets the number of disk data files that may be
;	open at anyone time during the execution of a BASIC program.
;	Each file data block allocated in this fashion requires 166 bytes of memory.
;	If the /F option is omitted, the number of files defaults to 3.
	LD	A,D		;FILES CANT BE .GT. 255
	OR	A		;SET CC'S
	JP	NZ,FCERR	;FUNCTION CALL ERROR
	LD	A,E		;GET LOW BYTE
	CP	10H		;MUST BE .LT. 16
	JP	NC,FCERR
	LD	(NFILES),A	;HIGHEST FILE NUMBER ALLOWED
	JP	FOK		;DONE

MEM	EX	DE,HL		;[HL]=requested MEMSIZ
	LD	(MEMSIZ),HL	;Record memory request
	EX	DE,HL
FOK	DEC	HL		;RESCAN LAST CHAR
	CALL	CHRGTR		;BY CALLING CHRGET
	JP	Z,DONCMD	;END OF COMMAND
	CALL	SYNCHR
	DB	'/'		;SLASH SHOULD FOLLOW
	JP	SCANS1		;SCAN NEXT SWITCH

; /S:<maximum record size> may be added at the end of the command
; line to set the maximum record size for use with random files.
; The default record size is 128 bytes.
WASS	CALL	CHRGTR		;GET CHAR AFTER "S"
	CALL	SYNCHR
	DB	':'		;MAKE SURE COLON FOLLOWS
	CALL	CNSGET		;[DE]=VALUE FOLLOWING COLON
	EX	DE,HL
	LD	(RECSIZ),HL	;SAVE IT
	EX	DE,HL
	JP	FOK		;CONTINUE SCANNING

ZEROB	DB	00H		;ZERO BYTE

COMAGN	DB	00H		;WE HAVENT SCANNED COMMAND YET

	;Done command
DONCMD:
;-----------------------------------------------------------------------------
;	Allocate Space for Disk Buffers
;
; ## GWINIT.ASM:299 ##

	DEC	HL		;useless ?
	LD	HL,(MEMSIZ)	;get size of memory
	DEC	HL		;always leave top byte unused because
				;val(string) makes byte in memory
				;beyond last char of string=0
	LD	(MEMSIZ),HL	;save in real memory size
	DEC	HL		;one lower is stktop
	PUSH	HL		;save it on stack

;-----------------------------------------------------------------------------
;	INIT TXTAB, STKTOP, VARTAB, MEMSIZ, FRETOP, STREND
;
; ## GWINIT.ASM:318 ##
;
;	Memory map for GW-BASIC:
;
;               [MAXMEM]--}     highest byte of physical memory in system
;                               user managed memory
;               [TOPMEM]--}     highest byte available to BASIC
;                               basic stack
;               [STKLOW]--}     lowest byte available for STACK
;                           +--}FDB---}[STKEND] {end of chain}
;                           +---FDB{--+
;               [FILTAB]-------}FDB---+ (FILTAB points to lowest byte of lowest FDB)
;                               0 (1 byte string space terminator for VAL)
;               [MEMSIZ]--}     highest byte of IN-USE string space
;               [FRETOP]--}     highest byte of FREE string space
;               [STREND]--}     lowest  byte of FREE string space
;               [ARYTAB]--}     lowest  byte of Array Table
;               [VARTAB]--}     lowest  byte of Variable Table
;               [TXTTAB]--}     lowest  byte of BASIC Program Text
;
;	note:	when [FILTAB] = [STKLOW], no FDB's are allocated.
;		when [FRETOP] = [MEMSIZ], IN-USE string space is empty.
;		when [SP] = [STKLOW], STACK is full.

; At this point, MEMSIZ-1 is on stack, [HL]=TXTTAB-1
;
;	Disk Initialization Routine

;	Setup file info blocks
;	the number of each and information for
;	getting to pointers to each is stored. No locations are
;	initialized, this is done by NODSKS, first closing all files.
;	The number of files is the file pointer table.
;
	LD	A,(NFILES)	;HIGHEST FILE NUMBER ALLOWED
	LD	HL,DSKDAT	;GET START OF MEMORY
	LD	(FILPT1),HL
	LD	DE,FDBTAB	;POINT TO TABLE TO SET UP
	LD	(NFILES),A	;REMEMBER HOW MANY FILES
				;HIGHEST FILE NUMBER ALLOWED
	INC	A		;ALWAYS FILE 0 FOR INTERNAL USE
	LD	BC,00A9H	;(DBLK.C) - SIZE OF A FILE INFO BLOCK PLUS $CODE
LOPFLB	EX	DE,HL		;[H,L] POINT INTO POINTER BLOCK
	LD	(HL),E		;STORE THE POINTER AT THIS FILE
	INC	HL
	LD	(HL),D
	INC	HL
	EX	DE,HL
	ADD	HL,BC		;[H,L] POINT TO NEXT INFO BLOCK
	PUSH	HL		;SAVE [H,L]
	LD	HL,(RECSIZ)	;GET MAX RECORD SIZE
				;The default record size is 128 bytes.
	LD	BC,00B2H	;(FNZBLK) GET SIZE OF OTHER STUFF
	ADD	HL,BC
	LD	B,H
	LD	C,L		;RESULT TO [B,C]
	POP	HL		;RESTORE [H,L]
	DEC	A		;ARE THERE MORE?
	JP	NZ,LOPFLB
	INC	HL		;INCREMENT POINTER
	LD	(TXTTAB),HL	;SAVE BOTTOM OF MEMORY
	LD	(SAVSTK),HL	;WE RESTORE STACK WHEN ERRORS
	POP	DE		;GET  CURRENT MEMSIZ
	LD	A,E		;CALC TOTAL FREE/8
	SUB	L
	LD	L,A
	LD	A,D
	SBC	A,H
	LD	H,A
	JP	C,OMERR
	LD	B,03H		;SHIFT RIGHT THREE BITS (DIVIDE BY 8)
SHFLF3	OR	A
	LD	A,H
	RRA
	LD	H,A
	LD	A,L
	RRA
	LD	L,A
	DEC	B
	JP	NZ,SHFLF3
	LD	A,H		;SEE HOW MUCH
	CP	02H		;IF LESS THAN 512 USE 1 EIGHTH
	JP	C,SMLSTK
	LD	HL,0200H
SMLSTK	LD	A,E		;SUBTRACT STACK SIZE FROM TOP MEM
	SUB	L
	LD	L,A
	LD	A,D
	SBC	A,H
	LD	H,A
	JP	C,OMERR
	LD	(MEMSIZ),HL	;Save lowest legal value for [SP]
	EX	DE,HL
	LD	(FILTAB),HL	;Initially there are no FDB's
	LD	(FRETOP),HL	;REASON USES THIS...
	LD	SP,HL		;SET UP NEW STACK
	LD	(SAVSTK),HL
	LD	HL,(TXTTAB)
	EX	DE,HL
	CALL	REASON
	LD	A,L		;SUBTRACT MEMSIZ-TXTTAB
	SUB	E
	LD	L,A
	LD	A,H
	SBC	A,D
	LD	H,A
	DEC	HL		;SINCE TWO ZEROS EXIST BETWEEN
	DEC	HL		;TXTTAB AND STREND, ADJUST
	PUSH	HL		;SAVE NUMBER OF BYTES TO PRINT
	LD	HL,HEDING	;GET HEADING ("BASIC VERSION...")
	CALL	STROUT		;PRINT IT
	POP	HL		;RESTORE NUMBER OF BYTES TO PRINT
	CALL	LINPRT		;PRINT # OF BYTES FREE
	LD	HL,WORDS	;TYPE THE HEADING
	CALL	STROUT		;"BYTES FREE"
	LD	HL,STROUT
	LD	(REPINI),HL
	CALL	CRDO		;PRINT CARRIAGE RETURN
	LD	HL,READYR
	JP	INITSA

;-----------------------------------------------------------------------------
;	Sign on message and other data to be discarded after INIT
;
; ## GWRAM.ASM:34 ##
;
AUTTXT	DB	0DH,0AH,0AH
	DB	'Owned by Microsoft',0DH,0AH,00H
WORDS	DB	' Bytes free',00H
HEDING	DB	'BASIC-80 Rev. 5.21',0DH,0AH
	DB	'[CP/M Version]',0DH,0AH
	DB	'Copyright 1977-1981 (C) by Microsoft',0DH,0AH
	DB	'Created: 28-Jul-81',0DH,0AH,00H
	DB	00H,00H,00H,00H,00H,00H,00H

	DS	003FH
TSTACK	DS	0001H
L6040	DS	0063H
L60A3	EQU	$

	END	START

