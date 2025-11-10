;BMOVE/ASM
	TITLE	<BASICG/OV2>
;
;
*GET	BUILDVER/ASM		;<631>
*GET	NEWB/EQU
CR	EQU	13
NUMTOKE	EQU	0EH		;Token for line ref.
FIND	EQU	5AD6H		;FNDLIN	
READY	EQU	5942H		;READY	Print ready msg
NXTCHR	EQU	5EEBH		;CHRGTR	parse down line
NUM2DE	EQU	5FF2H		;LINSPC	HL => value in DE
HIPT	EQU	7204H		;FRETOP	Ptr to highest avail
LOPT	EQU	722FH		;STREND	Ptr to lowest avail
PGMBGN	EQU	6F2AH		;TXTTAB	Ptr to start of pgm
OV1	EQU	9841H		;LODOV1	Entry to reload ov1
NUMPOSN	EQU	6006H		;LINGT3	Find refed number
IPOS	EQU	71D1H		;CONTXT	Continuation pt after ref. #
INBUF	EQU	70C5H		;BUF	Input buffer
UNTOKE	EQU	7AA3H		;BUFLIN	
LBUF	EQU	6F85H		;KBUF-1
CURLIN	EQU	721EH		;DOT
FUNCERR	EQU	5FEDH		;FCERR
OMERR	EQU	4263H		;OMERR
BASDSP	EQU	46AFH		;STROUT
NUMDSP	EQU	3139H		;LINPRT
;
;
	ORG	DOSOVR@
;
;
;	MOVE - Upon entry, HL => cmdline fm basic
;
;
	LD	(SAVSTK),SP	;Stack after clear done
	CP	5
	JP	Z,COPY
	CP	2
	JP	NZ,DOFIND
;
MOVE:	CALL	NXTLIN		;Get a line #
	LD	(NUM1),DE	;Start line
	LD	(STRTLIN),BC	;Line start pointer
	PUSH	DE
	CALL	NXTLIN		;Get ending line
	LD	(NUM2),DE
	LD	(ENDLIN),BC	;Line start pointer
	LD	B,H		;Save HL
	LD	C,L
	EX	DE,HL		;End to HL
	POP	DE
	OR	A
	SBC	HL,DE		;End > start!
	JP	C,BADLIN
	LD	H,B
	LD	L,C		;Restore buffer ptr
	CALL	NXTCHR		;Must be last now
	JP	Z,BADLIN
	CALL	NUM2DE		;Get number
	JP	NZ,BADLIN
	CALL	NXTLIN1
	LD	(NUM3),DE	;Save endline
	LD	(POSN3),BC	;Save line start
	JR	GOTLIN
;
;	Get and check a line
;
NXTLIN	CALL	NXTCHR		;Pt to a line #
	JP	Z,BADLIN	;Must have something
	CALL	NUM2DE		;Get the value
	JP	Z,BADLIN
NXTLIN1	LD	A,D		;Not line zero
	OR	E
	JP	Z,BADLIN
	PUSH	HL		;Save buffer ptr
	CALL	FIND
	POP	HL
	RET	C		;Back if real line
	JP	BADLIN
;
GOTLIN:	LD	DE,(NUM3)	;Get newline posn
	CALL	RANGE		;Check that not in block!
	JP	NC,BADLIN	;Quit if it is
	CALL	FIND		;Find num3
	INC	HL
	INC	HL		;Pt to following line #
	LD	E,(HL)
	INC	HL
	LD	D,(HL)		;Line # to DE
	LD	HL,(NUM1)
	OR	A
	SBC	HL,DE		;Be sure a move will be made
	JP	Z,BADLIN
	LD	HL,0		;First, zero line ctr
	LD	(BLANKS),HL
	LD	HL,$-$		;Move line posn
POSN3	EQU	$-2
	LD	E,(HL)		;Get next line address
	INC	HL
	LD	D,(HL)
	LD	(NEWLIN),DE	;Save for move
	EX	DE,HL		;HL to nxt line
	LD	A,(HL)		;See if end
	INC	HL		;Pt to line #
	OR	(HL)
	INC	HL
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	EX	DE,HL		;Nxt line to HL
	JR	Z,GOODRM	;If so, have enuf lines!
	DEC	HL		;Cant be this line
	LD	DE,$-$
NUM3	EQU	$-2
	SBC	HL,DE		;Get blanks avail.
	JP	Z,BADLIN	;Quit if no room
	LD	(BLANKS),HL	;Save this number
;
;	Now, count the lines to move
;
GOODRM:	LD	HL,$-$
STRTLIN	EQU	$-2
	LD	DE,$-$
ENDLIN	EQU	$-2
	LD	BC,1		;Line counter
CNTLP:	OR	A
	PUSH	HL
	SBC	HL,DE		;See if at end
	POP	HL
	JP	Z,ENDCNT	;Go if so
	LD	A,(HL)		;Get nxt line start
	INC	HL
	LD	H,(HL)
	LD	L,A
	OR	H		;Just in case, dont go to end
	JP	Z,BADLIN
	INC	BC		;Show another coming
	JR	CNTLP
;
ENDCNT:	LD	HL,$-$		;Get lines avail
BLANKS	EQU	$-2
	LD	A,H
	OR	L		;If 0, always enough
	JR	Z,OKLINS
	SBC	HL,BC		;Sub count fm avail
	JP	C,BADLIN	;"Not enuf lines avail...
OKLINS:	LD	(LTM),BC	;Save count of lines
;
;	Now check the memory needed
;
	LD	HL,(HIPT)
	LD	DE,(LOPT)
	OR	A
	SBC	HL,DE		;HL has free memory
	PUSH	HL
	LD	HL,(ENDLIN)	;Ptr to last lin
	LD	E,(HL)		;This pts to end of
	INC	HL		; line+1
	LD	D,(HL)
	EX	DE,HL		;End+1 to HL
	LD	DE,(STRTLIN)	;Start posn
	OR	A
	SBC	HL,DE		;This is needed space
	LD	(TOMOVE),HL	;Save byte count
	EX	DE,HL		;Needed to DE
	POP	HL		;Available space
	SBC	HL,DE
	JP	C,NOMEM
;
;	Determine direction of shuffle
;
	LD	HL,(STRTLIN)
	LD	DE,(NEWLIN)
	OR	A
	SBC	HL,DE		;CF set means up
	PUSH	AF		;Save dir flag
;
;	Now, mark all program ref's to these lines
;	First, get offset fm current locn in mem to
;	where the lines to be moved will end up.
;
	LD	HL,(POSN3)	;Ptr to line to go after
	LD	E,(HL)		;Now get nxt posn
	INC	HL		; as this will be start
	LD	D,(HL)		; of insert point
	LD	HL,(STRTLIN)	;Starting posn
	EX	DE,HL
	PUSH	AF
	OR	A
	SBC	HL,DE		;This is the offset
	POP	AF		;Get back dir
	JR	NC,DIR2		;Done if move down
	LD	DE,(TOMOVE)	;Else need to sub this also
	OR	A
	SBC	HL,DE
DIR2:	LD	(OFFSET),HL
;
;	Now find line refs and insert a 0D token and the line
;	address PLUS the offset. This must be done to get
;	the renumbered lines after the move when they are in
;	their new positions.
;
	OR	1
TIME2	LD	(DOPASS1),A	;Make NZ
	LD	HL,(PGMBGN)	;Get the start of the pgrm
	DEC	HL
PAS1	INC	HL		;Pt to next char
	LD	A,(HL)
	INC	HL
	OR	(HL)		;Ck for end
	JP	Z,PAS1DUN	;Go if done
	INC	HL
	LD	E,(HL)		;Get the line # into DE
	INC	HL
	LD	D,(HL)
PAS1A	CALL	NXTCHR		;Get another char fm line
PAS1B	OR	A		;See if line end
	JR	Z,PAS1		;New line if so
	LD	C,A		;Save the token
	LD	A,(DOPASS1)	;Is this pass 1 or 2?
	OR	A
	LD	A,C		;Recover token
	JR	Z,PAS2		;Go if pass 2
	CP	0A8H		;Ck for ON ERROR
	JR	NZ,PAS1C	;Go if not
	CALL	NXTCHR
	CP	89H		;See if goto
	JR	NZ,PAS1B
	CALL	NXTCHR
	CP	0EH		;Line number next?
	JR	NZ,PAS1B
	PUSH	DE		;Save the line number
	CALL	NUMPOSN		;get refed line # posn
	LD	A,D		;See if "0"
	OR	E
	JR	NZ,PAS1D	;Go if not
	JR	PAS1F		; else skip - cant adjust a 0
PAS1C:	CP	0EH		;Line # follows?
	JR	NZ,PAS1A	;No, get another token
	PUSH	DE		;Save curr line
	CALL	NUMPOSN		;Get refed line # posn
PAS1D:	PUSH	HL		;Save posn in line
	CALL	FIND		;Get posn of refed line
	JR	NC,UNDEF
	CALL	RANGE		;See if a moved ref.
	JR	C,PAS1E		;Dont do if not in range
	DEC	BC
	LD	A,0DH
	JR	PAS2A		;Go if line found
;
;	Undefined line error report
;
UNDEF:	CALL	41F0H		;CRDONZ
	LD	HL,UNDEF$
	PUSH	DE
	CALL	46AFH		;STROUT	Dsply undefined
	POP	HL
	CALL	3139H		;LINPRT	Dsply the line #
	POP	BC
	POP	HL
	PUSH	HL
	PUSH	BC
	CALL	3131H		;INPRT	Dsply source line #
;
PAS1E:	POP	HL
PAS1F:	POP	DE
	DEC	HL
PAS1G:	JR	PAS1A
UNDEF$	DB	'Undef. line ',00H
;
;	Install new line # references
;
PAS2:	CP	0DH		;Token fm pass 1?
	JR	NZ,PAS1G	;Go if not
	PUSH	DE
	CALL	NUMPOSN		;Get refed line posn
	PUSH	HL
	EX	DE,HL
	INC	HL
	INC	HL
	INC	HL
	LD	C,(HL)		;P/u new line #
	INC	HL
	LD	B,(HL)		; into BC
	LD	A,0EH		;Restore line ref token
PAS2A:	LD	HL,PAS1E	;Return point
	PUSH	HL
	LD	HL,(IPOS)	;Posn in line +1
	PUSH	HL		;Save for nxtchr
	DEC	HL		;Ref'ed line #
	LD	(HL),B		;Insert whatever
	DEC	HL
	LD	(HL),C
	DEC	HL
	LD	(HL),A		;Insert 0D or 0E
	POP	HL		;Set for scan continue
	RET
;
;	See if line # in DE is in the move range
;	BC,DE must not be altered
;
RANGE:	LD	HL,(NUM2)	;Ending line #
	OR	A
	SBC	HL,DE		;Sub refed line
	RET	C		;Back if line greater
	LD	HL,(NUM1)	;Get starting line
	OR	A
	SBC	HL,DE
	JR	Z,RNG1
	CCF
	RET	C		;Back if less
RNG1:	LD	HL,$-$		;Get the offset
OFFSET	EQU	$-2
	ADD	HL,BC
	LD	B,H
	LD	C,L		;New offset to BC
	OR	A
	RET
;
;	Done with adjust, where did we come from?
;
PAS1DUN:
	LD	A,(DOPASS1)
	OR	A		;NZ=pass1
	JP	Z,EXIT		;Go if done
;
;	Now, we need to move the lines and change the memory
;	pointers on the move lines and the shuffled lines.
;	Start by jerking out the move block
;
	LD	HL,(STRTLIN)	;Ptr to 1st line to move
	LD	DE,(LOPT)	;Free mem
	LD	BC,(TOMOVE)
	LDIR			;Block now moved into memory
	LD	(ENDPT),DE	;For block adjust
;
;	CF has direction, CF=UP
;	Init upward move
;
	POP	AF
	PUSH	AF
	LD	HL,(ENDLIN)	;Pt past end line
	LD	A,(HL)
	INC	HL
	LD	H,(HL)
	LD	L,A		;Ptr to HL
	PUSH	HL
	PUSH	HL
	POP	BC		;Endlin+1 to BC
	LD	HL,(NEWLIN)
	OR	A
	SBC	HL,BC		;This is bytes to move
	LD	B,H
	LD	C,L		;Count to BC
	POP	HL
	LD	DE,(STRTLIN)
	POP	AF
	LD	A,0B0H		;LDIR byte
	PUSH	AF		;Keep CF
	JR	C,DOMOV
;
;	No, is downward
;
	EX	DE,HL		;HL=>STRT, DE=>END
	DEC	HL
	DEC	DE
	PUSH	HL
	LD	BC,(NEWLIN)	;Start-1
	OR	A
	SBC	HL,BC
	INC	HL
	LD	B,H		;Count to BC
	LD	C,L
	POP	HL
	LD	A,0B8H		;LDDR inst.
;
DOMOV:	LD	(DIRECT),A	;Set direction
	LD	(TOCNG),BC	;Count for adj later
	DB	0EDH		;LDxx opcode
DIRECT	DB	0B0H		;Either DR or IR opcode
	JR	NC,ADJ0		;Go if downward
	LD	(UPSTART),DE	;Save is move was up
;
;	Move done, adjust block pointers and line #'s
;
ADJ0:	LD	HL,(LOPT)	;Pt to moved block
	PUSH	HL		;Save start
	LD	DE,(STRTLIN)	;Where it came from
	OR	A
	SBC	HL,DE
	LD	(OFFSET1),HL	;Save this offset
	POP	HL		;HL to blk start
	LD	BC,(OFFSET)	;Amount moved
ADJ1:	LD	E,(HL)		;LSB old ptr
	INC	HL
	LD	D,(HL)		;MSB old ptr
	PUSH	DE		;Save old ptr valu
	EX	DE,HL		;Old ptr to HL
	ADD	HL,BC		;New ptr in HL
	EX	DE,HL		;New ptr in DE
;
	PUSH	HL		;Save, make new line #
	PUSH	DE
	LD	DE,(NUM3)	;Request line
	INC	DE
	LD	(NUM3),DE
	INC	HL
	LD	(HL),E		;Insert new line #
	INC	HL
	LD	(HL),D
	POP	DE
	POP	HL
;
	LD	(HL),D		;Put in new ptr
	DEC	HL
	LD	(HL),E
	POP	HL		;Old ptr value
	LD	DE,$-$		;Mem offset
OFFSET1	EQU	$-2
	ADD	HL,DE		;This next HL posn
	EX	DE,HL		;Save in DE
	LD	HL,$-$		;End of move blk
ENDPT	EQU	$-2
	OR	A
	SBC	HL,DE		;At the end?
	JR	Z,ADJ2		;Go if so
	EX	DE,HL		;HL pts to new ptr
	JR	ADJ1
;
ADJ2:	LD	DE,(NEWLIN)	;Get new posn in pgm area
	POP	AF		;See shich direction
	PUSH	AF
	JR	NC,ADJ2A	;Ok if is down
	LD	DE,$-$		;Else this is new locn
UPSTART	EQU	$-2
ADJ2A:	LD	HL,(LOPT)	;Where we stored the block
	LD	BC,(TOMOVE)	;Block len
	LDIR
;
;	Now all line pointers need adjustment except
;	those in the moved block. AF still on stack with
;	CF indicating direction.
;
	LD	HL,(STRTLIN)	;Init was upward
	LD	BC,(TOMOVE)
	POP	AF
	JR	C,WASUP		;Go if ok
	LD	HL,(NEWLIN)
	ADD	HL,BC
;
;	Now, get the offset to adjust. TOMOVE has the right
;	adjustment, but must be made negative if the move
;	was in an upward position
;
WASUP:	LD	BC,$-$		;Len of moved pgm
TOCNG	EQU	$-2
	PUSH	AF		;Save flags
	PUSH	HL		;Save start
	ADD	HL,BC		;This is end
	LD	(ENDPT1),HL
	POP	HL
	POP	AF
	LD	BC,(TOMOVE)	;This is move offset
	JR	NC,ADJ3		;Go if move was down
	PUSH	HL
	OR	A		;Reset CF
	SBC	HL,HL		;HL = 0
	SBC	HL,BC		;Offset negative
	LD	B,H		; and back to BC
	LD	C,L
	POP	HL
ADJ3:	LD	E,(HL)		;LSB old ptr
	INC	HL
	LD	D,(HL)		;MSB old ptr
	EX	DE,HL		;Old ptr to HL
	ADD	HL,BC		;New ptr in HL
	EX	DE,HL		;New ptr in DE
	LD	(HL),D		;Put in new ptr
	DEC	HL
	LD	(HL),E
	LD	HL,$-$		;End of move blk
ENDPT1	EQU	$-2
	OR	A
	SBC	HL,DE		;At the end?
	JR	Z,ADJ4		;Go if so
	EX	DE,HL		;HL pts to new ptr
	JR	ADJ3
;
;	Now, go back and fix line # refs
;
ADJ4:	XOR	A		;Set pass 2
	LD	(DOPASS1),A	;This must be 0
	JP	TIME2
;
;
;	Find something 
;
DOFIND:
	LD	(DTYPE),A	;Save display mode
	XOR	A
	LD	(FTIME),A	;Dsply flag
	LD	HL,LBUF+1	;Pt to command
	CALL	NXTCHR		;Get 1st char of command
	JP	Z,AGAIN		;Repeat previous
	CP	'"'		;Literal string?
	JP	Z,BADLIN	;Error - cant find strings
	CP	'A'		;See if variable
	JR	C,CPNUM		;Go if not
	CP	'Z'+1
	JR	C,VARFIND	;Go if it is variable
CPNUM:	CP	'0'
	JP	C,BADLIN	;Can't be less
	CP	'9'+1
	JP	C,NUMFIND	;Go if number
;
;	Find a keyword
;
KEYFIND:
	LD	(LSTTYP$),A	;Save the type
	CP	0FFH		;Special 2 byter?
	LD	B,0FFH		;Init one part only
	JR	NZ,KF1		;Go if not
	INC	HL
	LD	B,(HL)		;Get the 2nd half
KF1:	LD	C,A		;Save main token
	LD	A,B
	LD	(LSTKEY$),A	;Save 2nd type
	LD	HL,(PGMBGN)	;Find the program start
KEYREP:	DEC	HL
KEYLP:	CALL	GTPTR
	JP	Z,EXIT		;Go if no more
	INC	HL
	LD	E,(HL)		;Get this line #
	INC	HL
	LD	D,(HL)
	LD	(LSTLIN$),DE	;Save for next time
KEYLP1:	CALL	NXTCHR		;Get a char
	OR	A
	JR	Z,KEYLP		;Go on line end
	CP	0FFH		;Spcl?
	JR	Z,KEY2		;Yes, ck it out
	CP	C		;Was not spcl
	JR	Z,KEY3		;Go if match
	JR	KEYLP1
KEY2:	CP	C		;Looking for FF?
	EX	AF,AF'		;Save result flag
	CALL	NXTCHR		; and get next always
	OR	A		;EOL?
	JR	Z,KEYLP		;Done if so
	CP	B		;Set for this char cp
	EX	AF,AF'		;Was C a match?
	JR	NZ,KEYLP1	;No, do more
	EX	AF,AF'		;C was, was B?
	JR	NZ,KEYLP1	;No, domore
KEY3:	CALL	SHOW
	JR	KEYLP
;
VARFIND:
	LD	DE,LSTVAR$	;Save variable
	LD	B,10		;Max cmp len
VAR1:	LD	A,(HL)		;Get a char
	INC	HL
	CALL	VALID		;See if real char
	JR	NZ,VAR2		;Found end if not
	LD	(DE),A		;Save char
	INC	DE
	DJNZ	VAR1		;Find more
VAR2:	EX	DE,HL
	LD	(HL),0		;Terminate variable
	LD	A,10
	SUB	B		;Get len of variable
	JP	Z,BADLIN	;No len is immpossible
	LD	(LSTTYP$),A	;Len = type
	LD	B,A		;Save len in B and
	LD	C,A		; in C
	LD	HL,(PGMBGN)	;Pt to start
VARREP:	DEC	HL
VARLP:	CALL	GTPTR
	JP	Z,EXIT
	LD	B,C		;Always reset counter
	INC	HL		;Now get line #
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	LD	(LSTLIN$),DE	;Save for a match
VARLP3:	LD	DE,LSTVAR$	;Pt to stored var.
VARLP1:	CALL	NXTCHR		;Get a char
	OR	A		;Line end?
	JR	Z,VARLP		;Go if so
	CP	8FH		;REM line
VP0:	CALL	Z,DUMPLIN
	JR	Z,VARLP
	IF	@BLD631
	CP	84H		;<631>
	JR	Z,VP0		;<631>
	CP	0FFH		;<631>
	JR	NZ,VP0A		;<631>
	INC	HL		;<631>
	JR	VARLP1		;<631>
	ELSE
	JP	PATCH1		;Go to patch #6
	NOP
	ENDIF
VP0A	CP	'"'		;String start?
	JR	Z,BYSTR		;Bypass if so
	CP	'A'		;Some variable?
	JR	C,VARLP1	;No, not a variable
	CP	'Z'+1
	JR	NC,VARLP1	;Go if not variable
VARLP2:	EX	DE,HL		;DE to line, HL to var storage
	CP	(HL)		;A match?
	EX	DE,HL		;Back to normal
	JR	NZ,BYVAR	;No, bypass entire variable
	INC	DE		;Next var. posn
	DEC	B		;Count down var len
	JP	NZ,VAR3		;Go if not end
	CP	'('		;If subscripted, always ok
	JR	Z,VAR5
	LD	A,C		;Was it full 10 chars?
	CP	10
VAR5:	CALL	Z,SHOW		;If so, can be spcl
	JR	Z,VARLP
	INC	HL		;Ck if next is spcl
	LD	B,A		;Save orig char
	LD	A,(HL)		;If it is, not a match
	DEC	HL
	CALL	VALID
	LD	A,B		;Get back char
	JR	Z,BYVAR		;Bypass if spcl
	CALL	SHOW		;Else show line
	JR	VARLP		; and on to next
VAR3:	CALL	NXTCHR		;Get nxt char
	OR	A
	JR	Z,VARLP		;Go on line end
	JR	VARLP2		;Ck another char
BYSTR:	CALL	NXTCHR		;Get either str or var char
	OR	A
	JR	Z,VARLP		;Go on line end
	CP	'"'		;End of string?
	JR	Z,VARLP3	;Yes, cont search
	JR	BYSTR		;If not, do another
BYVAR:	LD	B,C		;Reset counter!
	CP	'('		;Subscript (groan!)
	JR	Z,VARLP3	;Yes, ck subs.
BYV1:	CALL	NXTCHR		;Get a char
	OR	A
	JR	Z,VARLP		;Go on line end
	CP	'('		;Start subscript?
	JR	Z,VARLP3	;Yes, return
	CALL	VALID		;End of var?
	JR	NZ,VARLP3	;Go if so
	JR	BYV1		;Not end, another char
;
DUMPLIN:
	CALL	NXTCHR
	OR	A		;Line end?
	JR	NZ,DUMPLIN
	RET	
;
;
GTPTR:	INC	HL
	LD	A,(HL)		;Get ptr lsb
	LD	(NXTPO),A
	PUSH	AF
	INC	HL
	LD	A,(HL)		;Get ptr msb
	LD	(NXTPO+1),A
	POP	AF
	OR	(HL)		;End of pgm?
	RET
;
;
;	See if display line or number
;
SHOW:	LD	A,$-$
DTYPE	EQU	$-1		;Get the command
	CP	3		;Search?
	JR	Z,SHOLIN	;Go if Search
;
SHONUM:
	PUSH	DE
	PUSH	BC		;These must be saved
	LD	A,$-$
FTIME	EQU	$-1		;Z if first time
	OR	A
	LD	A,1
	LD	(FTIME),A	;Show not first
	JR	Z,SHO1		;Go if was first
	LD	HL,COMSTR
	CALL	BASDSP
SHO1:	LD	HL,(LSTLIN$)	;Get number
	CALL	NUMDSP		;Let basic show it
	LD	HL,$-$
NXTPO	EQU	$-2
	DEC	HL		;1 before nxt line
	XOR	A		;Show was done
	POP	BC
	POP	DE
	RET
;
COMSTR	DB	', ',0
;
;
;	Show the line found
;
SHOLIN:
	LD	DE,(LSTLIN$)	;Get the number
	LD	(CURLIN),DE	;Make it current
	LD	A,'.'		;Set show current
	JP	DOLINE		;Show it!
;
;	Find a reference to a line number
;
NUMFIND:
	CALL	NUM2DE		;Get the number in DE
	JP	NZ,EXIT		;Quit on error
	LD	(LSTNUM$),DE	;Save for retry
	LD	HL,(PGMBGN)	;Get start of pgm
	LD	A,NUMTOKE	;What to look for
	LD	(LSTTYP$),A	;Save also for repeat
NUMREP:	DEC	HL
	LD	C,A		;token to match
	PUSH	DE		;Save cp valu on stack
NUMLP:	CALL	GTPTR
	JP	Z,EXIT
	INC	HL
	LD	E,(HL)		;Get this line #
	INC	HL
	LD	D,(HL)
	LD	(LSTLIN$),DE	;Save for next time
NUMLP1:	EX	(SP),HL		;Save HL, posn in line
	PUSH	HL		;Save #
	OR	A
	SBC	HL,DE		;See if a match
	POP	HL
	EX	(SP),HL		;All back in posn now
	LD	DE,(LSTLIN$)	;Get line # back
	JR	NZ,NUMLP2
	CALL	SHOW		;All done, found a match
	JR	NUMLP
NUMLP2:	CALL	NXTCHR		;Get the next char fm line
	OR	A
	JR	Z,NUMLP		;Go if line end
	CP	C		;Is it a ref token?
	JR	NZ,NUMLP2	;Cont if not
	CALL	NUM2DE
	LD	HL,(IPOS)	;Get back buff posn
	DEC	HL
	JR	NUMLP1		;See if a match
;
;	Entry was F with no parms, use last ones
;
AGAIN:	LD	A,(DTYPE)	;Get command type
	CP	4		;Is it find?
	JP	Z,EXIT		;Can't "again" a find command
	LD	A,(LSTTYP$)	;Get the last search type
	OR	A		;Was their one?
	JP	Z,BADLIN		;Back if not
	PUSH	AF		;Save type
	LD	DE,(LSTLIN$)	;P/u where we left off
	CALL	FIND		;Get the address of next
	JP	NC,BADLIN	;Something goofed
	POP	AF
	LD	DE,(LSTNUM$)	;Get previous
	CP	NUMTOKE		;Numeric?
	JR	Z,NUMREP	;Go if so
	CP	80H		;Keyword?
	JR	C,AGN1		;Go if not
	LD	C,A		;Token to C
	LD	A,(LSTKEY$)	;Get 2nd half
	LD	B,A
	JP	KEYREP		;Start find
AGN1:	LD	B,A		;Must be var. set up len
	LD	C,A
	LD	DE,LSTVAR$	;Pt to old var.
	PUSH	DE		;Get on stack
	JP	VARREP		;Go do it
;
;	Check for valid variable chars - NZ on end
;
VALID:	CP	'.'		;Valid char
	RET	Z
	CP	'A'
	JR	C,VAL1		;Not a letter
	CP	'Z'+1
	JR	C,VALZ		;Yes - back
VAL1:	CP	'0'
	JR	C,SPCL		;Not digit, ck more
	CP	'9'+1
	JR	C,VALZ		;Yes, a number
SPCL:	CALL	CKSPCL		;Ck for special
	RET			;Return result
VALZ:	CP	A		;Set Z
	RET
;
CKSPCL:	CP	'!'
	RET	Z
	CP	'#'
	RET	Z
	CP	'$'
	RET	Z
	CP	'%'
	RET	Z
	CP	'('
	RET
;
;
COPY:
	LD	HL,INBUF
	CALL	NXTCHR		;Pt to possible parm
	JR	Z,ILLEG		;Bad if at end
	CALL	NUM2DE		;Get the 1st number
	JR	Z,ILLEG		;Bad if end
	LD	A,D
	OR	E
	JR	Z,ILLEG		;Go if not a number
	PUSH	HL		;Must save buffer
	CALL	FIND
	JR	NC,ILLEG	;Go if can't find
	OR	A
	INC	BC
	INC	BC		;Past line ptr
	SBC	HL,BC		;Len to HL
	DEC	HL
	DEC	HL		;Past line #
	EX	(SP),HL		;Save len on stack
	CALL	NXTCHR		;On to next number
	JR	Z,ILLEG
	CALL	NUM2DE
	JR	NZ,ILLEG	;Now must be end
	LD	A,D
	OR	E
	JR	Z,ILLEG		;Go if not number
	PUSH	BC		;Save old line address
	CALL	FIND
	POP	HL		;New posn in BC,old in HL
	JR	C,ILLEG		;Can't have new line already
	POP	BC		;Get len old line
;
;	Copy a line
;
	PUSH	DE		;Save new line #
	INC	HL
	INC	HL		;Pass nxt line ptr
	LD	DE,LBUF+1	;  else setup for move
	PUSH	DE		;Save buffer
	LDIR
MAKLIN:	POP	HL		;Back to buffer
	DEC	HL
	CALL	UNTOKE
	PUSH	BC		;Save current eol
	LD	HL,5		;Max ascii line #
	ADD	HL,BC		;New end
	EX	DE,HL		;End to DE
	LD	H,B		;HL to buffer end
	LD	L,C
	LD	BC,INBUF	;Line start
	OR	A
	SBC	HL,BC		;Len of line
	LD	B,H
	LD	C,L		;Len to BC
	POP	HL		;Pt to curr end
	LDDR			;Make room for line #
	EX	DE,HL		;HL to buff start
	LD	B,6		;Clear line # area
CLRLP:	LD	(HL),20H	;  with spaces
	DEC	HL
	DJNZ	CLRLP
	POP	DE		;Line # back
	EX	DE,HL
	INC	DE		;Input buffer start
	PUSH	DE
	LD	A,97		;HEXDEC, make # ascii
	RST	28H
ENDL:	INC	DE		;Find 0 byte
	LD	A,(DE)
	OR	A
	JR	NZ,ENDL
	POP	HL		;Line start
	PUSH	DE		;Line end
	LD	A,13
	LD	(DE),A		;For dsp
	LD	A,10		;@@dsply
	RST	40
	POP	DE
	XOR	A
	LD	(DE),A
	LD	HL,INBUF-1	;Treat as typed line
	JP	59B5H		;NTAUTO+3
ILLEG:	JR	BADLIN
;
;
DOLINE:	LD	HL,NOBRK@
	DB	0DDH
BADLIN:	LD	HL,FUNCERR
	DB	0DDH
NOMEM:	LD	HL,OMERR
	DB	0DDH
EXIT:	LD	HL,READY
EXIT1:	LD	SP,$-$
SAVSTK	EQU	$-2
	JP	(HL)
;
DOPASS1	DB	0
NUM1	DW	0
NUM2	DW	0
LTM	DW	0
TOMOVE	DW	0
NEWLIN	DW	0
;
	IFGT	$,23FFH
	ERR	'Overflow into LIB region'
	ENDIF
;
;
	IF	@BLD631
	ELSE
PATCH1	CP	84H
	JP	Z,VP0
	CP	0FFH
	JP	NZ,VP0A
	INC	HL
	JP	VARLP1
	ENDIF
	END	1E00H

