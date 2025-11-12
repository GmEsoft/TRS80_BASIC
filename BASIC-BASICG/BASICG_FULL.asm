;=============================================================================
;	BASIC[G] 01.01.00 for TRSDOS Version 6
;	Copyright (c) 1984 By Microsoft, licensed to Tandy Corporation.
;	All rights reserved.
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

;	CONFIGURATION

;	-P0=1	enables Graphics extensions (BASICG)
IF	@@0
GFX	EQU	1
ELSE
GFX	EQU	0
ENDIF

;	-p9=1	enables TEST build:
;		ORG 3000H instead of ORG 2600H
;		No need to split into BASIC[G]/CMD and BASIC[G]/OV1
;		2.5K less free bytes
IF	@@9
TEST	EQU	1
GMESOFT	EQU	1
ELSE
TEST	EQU	0
GMESOFT	EQU	0
ENDIF


;=============================================================================
;	SOME EXPLANATION
;
;	BASIC-80 configures memory as follows:
;
;LOW LOCATIONS
;
;	RST	SUBROUTINES
;
;	0	STARTUP
;		Initially a JMP to the initialization code
;		but changed to a JMP to READY.
;		Restarting the machine at 0 during program
;		execution can leave things messed up.
;
;	1	SYNCHK
;		A check is made to make sure the
;		character pointer points at a specific
;		character. If not the "SYNTAX ERROR"
;		routine is called. If so,
;		the CHRGET RST is dropped into so
;		the character after the matched
;		one will be put in [A] and
;		the condition codes will reflect this
;		Example: SYNCHK THENTK (The match character is
;		given in the location after the RST)
;		Would check to make sure [H,L] pointed to a THENTK
;		and if so fetch the next character into [A].
;		If not, a "SYNTAX ERROR" would be given.
;
;	2	CHRGET
;		Using [H,L] as the text pointer
;		The text pointer is incremented
;		and the next character is fetched into [A]
;		If the character is a " " it is skipped
;		over and the next character is fetched.
;		The statement terminators ":" and 0
;		leave the Zero flag set.
;		The numerics "O" through "9" leave the Carry
;		flag set. The current character can be
;		refetched into [A] by doing a MOV A,M.
;		If the condition codes must be set up again
;		DCX H,CHRGET will work. It is very difficult
;		to reexamine the character before the current
;		one since spaces may be in-between.
;		DCX H,DCX H,CHRGET will not always work.
;
;	3	OUTCHR
;		The character in [A] is printed on
;		the user's terminal. [A] and the
;		condition codes are preserved.
;
;	4	COMPAR
;		[D,E] and [H,L] are compared as unsigned
;		oouble-byte integers. Carry is set if
;		[H,L] is less than [D,E]. Zero is set if they
;		are equal. [A] is smashed. The only definite
;		thing that can be said about [A] on return
;		is that if the Zero flag is set, [A] will
;		equal 0.
;
;	5	FSIGN
;		The FAC (Floating ACcumulator)
;		which is used to store numeric results
;		is checked to see what sign its
;		value has.
;
;	6	PUSHM
;		A double byte quantity pointed
;		to by [H,L] is pushed onto the
;		stack. [B,C] is set equal to the
;		value pushed. [H,L] is incremented by two.
;
;	7	In the 4K version RST 7 is unused and the locations
;		associated with it are used to continue
;		the code for RST 6. In the 8K a JMP is made
;		around the first three RST 7 locations
;		during RST 6 execution. RST 7 initially
;		contains a RET, but the user can change it to
;		a JMP to an interrupt service routine.
;
;	FUNCTION DISPATCH ADDRESSES
;		FUNDSP contains the addresses of the
;		function routines in the order of the
;		function names in the CRUNCH LIST.
;		The functions that take more than one argument
;		are at the end. See the explanation at ISFUN.
;
;	THE OPERATOR TABLE
;		The OPTAB table contains an operators precedence
;		followed by the address of the routine to perform
;		the operation. The index into the
;		operator table is made by subtracting off the CRUNCH value
;		of the lowest numbered operator. The order
;		of operators in the CRUNCH LIST and in OPTAB is identical.
;		The precedences are arbitrary, except for their
;		comparative sizes. Note that the precedence for
;		unary operators such as NOT and Negation are
;		setup specially without using a table.
;
;	THE RESERVED WORD OR CRUNCH LIST
;		When a command or program line is typed in
;		it is stored in BUF. As soon as the whole line
;		has been typed in (INLIN returns) CRUNCH is
;		called to convert all reserved words to their
;		CRUNCH values. This reduces the size of the
;		program and speeds up execution by allowing
;		table dispatches to perform functions, statements,
;		and operations. This is because all the statement
;		names are stored consecutively in the CRUNCH LIST.
;		When a match is found between a string
;		of characters and a word in the CRUNCH LIST,
;		the entire text of the matched word is taken out of
;		the input line and a reserved word token is put
;		in its place. A reserved word token is always equal
;		to Octal 200 plus the position of the matched word
;		in the CRUNCH LIST.
;
;	STATEMENT DISPATCH ADDRESSES
;		When a statement is to be executed, the first
;		character of the statement is examined
;		to see if it is less than the reserved
;		word token for the lowest numbered statement name.
;		If so, the "LET" code is called to
;		treat the statement as an assignment statement.
;		Otherwise a check is made to make sure the
;		reserved word number is not too large to be a
;		statement type number. If not the address
;		to dispatch to is fetched from STMDSP (the Statement
;		Dispatch Table) using the reserved word
;		number for the statement to calculate an index into
;		the table.
;
;	ERROR MESSAGES
;		When an error condition is detected
;		[E] must be set up to indicate which error
;		message is appropriate and a branch must be made
;		to ERROR. The stack will be reset and all
;		program context will be lost. Variables
;		values and the actual program remain intact.
;		Only the value of [E] is important when
;		the branch is made to ERROR. [E] is used as an
;		index into ERRTAB which gives the two
;		character error message that will be printed on the
;		user's terminal.
;
;	IMPURE STORAGE
;		All temporaries, flags, pointers, the buffer area,
;		the floating accumulator, and anything else that
;		is used to store a changing value should be located
;		in this area. Care must be made in moving locations
;		in this area since the juxtaposition of two locations
;		is often depended upon.
;
;	TEXTUAL MESSAGES
;		Constant messages are stored here. Unless
;		the code to check if a string must be copied
;		is changed these strings must be stored above
;		DSCTMP, or else they will be copied before
;		they are printed.
;
;	FNDFOR
;		Most small routines are fairly simple
;		and are documented in place. FNDFOR is
;		used for finding "FOR" entries on
;		the stack. Whenever a "FOR" is executed an
;		18 byte entry is pushed onto the stack.
;		Before this is done, however, a check
;		must be made to see if there
;		are any "FOR" entries already on the stack
;		for the same loop variable. If so, that "FOR" entry
;		and all other "FOR" entries that were made after it
;		are eliminated from the stack. This is so a
;		program that jumps out of the middle
;		of a "FOR" loop and then restarts the loop again
;		and again won't use up 18 bytes of stack
;		space every time. The "NEXT" code also
;		calls FNDFOR to search for a "FOR" entry with
;		the loop variable in
;		the "NEXT". At whatever point a match is found
;		the stack is reset. If no match is found a
;		"NEXT WITHOUT FOR" error occurs. GOSUB execution
;		also puts a 6 byte entry on stack.
;		When a RETURN is executed FNDFOR is
;		called with a variable pointer that can't
;		be matched. When "FNDFOR" has run
;		through all the "FOR" entries on the stack
;		it returns and the return code makes
;		sure the entry that was stopped
;		on is a GOSUB entry. This assures that
;		if you GOSUB to a section of code
;		in which a FOR loop is entered but never
;		exited the RETURN will still be
;		able to find the most recent
;		GOSUB ENTRY. The "RETURN" code eliminates the
;		"GOSUB" entry and all "FOR" entries made after
;		the GOSUB entry.
;
;	NON-RUNTIME STUFF
;		The code to input a line, crunch it, give errors,
;		find a specific line in the program,
;		perform a "NEW", "CLEAR", and "LIST" are
;		all in this area. Given the explanation of
;		program storage given below these are
;		all straightforward.
;
;	NEWSTT
;		Whenever a statement finishes execution it
;		does a "RET" which takes
;		execution back to NEWSTT. Statements that
;		create or look at semi-permanent stack entries
;		must get rid of the return address of NEWSTT and
;		JMP to NEWSTT when done. NEWSTT always
;		CHRGETs the first character after the statement
;		name before dispatching. When returning
;		back to NEWSTT the only thing that
;		must be set up is the text pointer in
;		[H,L]. NEWSTT will check to make sure
;		[H,L] is pointing to a statement terminator.
;		if a statement shouldn't be performed unless
;		it is properly formatted (i.e. "NEW") it can
;		simply do a "RNZ" after reading all of
;		its arguments. since the Zero flag
;		being off indicates there is not
;		a statement terminator NEWSTT will
;		do the JMP to the "SYNTAX ERROR"
;		routine. If a statement should be started
;		over it can do LHLD TEMP,RET since the [H,L]
;		at NEWSTT is always stored in TEMP. Of course
;		care must be taken that no routine
;		that smashes TEMP has been called.
;		The ^C code stores TEMP in OLDTXT and CURLIN (the
;		current line number) in OLDLIN since the ^C check
;		is made before the statement pointed to is
;		executed. "STOP" and "END" store the text pointer
;		in [H,L] which points at their terminating
;		character in OLDTXT.
;
;	STATEMENT CODE
;		The individual statement code comes
;		next. The approach used in executing each
;		statement is documented in the statement code
;		itself.
;
;	FRMEVL, THE FORMULA EVALUATOR
;		Given an [H,L] pointing to the starting
;		character of a formula FRMEVL
;		evaluates the formula and leaves
;		the value in the Floating Accumulator (FAC).
;		[H,L] is returned pointing to the first character
;		that could not be interpreted as part of the
;		formula. The algorithm uses the stack
;		to store temporary results:
;
;			0. Put a dummy precedence of zero on
;				the stack.
;			1. Read lexeme (constant, function,
;				variable, formula in parens)
;				and take the last precedence value
;				off the stack.
;			2. See if the next character is an operator.
;				If not, return. This may cause
;				operator application or an actual
;				return from FRMEVL.
;			3. If it is, see what precedence it has
;				and compare it to the precedence
;				of the last operator on the stack.
;			4. If = or less remember the text
;				pointer at the start of this dperator
;				and do a return to cause
;				application of the last operator.
;				Eventually return to step 2
;				by returning to RETAOP.
;			5. If greater put the last precedence
;				back on, save the current
;				temporary result, operator address
;				and precedence and return to step 1.
;
;
;		Relational operators are all handled through
;		a common routine. Special
;		care is taken to detect type mismatches such as 3+"F"
;
;	EVAL -- THE ROUTINE TO READ A LEXEME
;		EVAL checks for the different types of
;		entities it is supposed to detect.
;		Leading pluses are ignored,
;		Digits and "." cause FIN (Floating INput)
;		to be called. Function names cause the
;		formula inside the parentheses to be evaluated
;		and the function routine to be called. Variable
;		names cause PTRGET to be called to get a pointer
;		to the value, and then the value is put into
;		the FAC. An open parenthesis causes FRMEVL
;		to be called (recursively), and the ")" to
;		be checked for. Unary operators (NOT AND
;		Negation) put their precedence on the stack
;		and enter formula evaluation at step 1, so
;		that everything up to an operator greater than
;		their precedence or the end of the formula
;		will be evaluated. When FRMEVL does a return
;		because it sees an operator of higher precedence
;		it does not pass the text pointer in [H,L], so
;		after the unary operation has been performed
;		on the FAC the text pointer must be fetched from
;		a temporary location that FRMEVL uses and
;		a return back to FRMEVL done.
;
;	DIMENSION AND VARIABLE SEARCHING
;		Space is allocated for variables as they are
;		encountered. Thus "DIM" statements must be
;		executed to have effect. 6 bytes are allocated
;		for each simple variable, whether it is a string,
;		number or user defined function. The first two
;		bytes give the name of the variable ano the last four
;		give its value. [VARTAB] gives the first location
;		where a simple variable name is found and [ARYTAB]
;		gives the location to stop searching for simple
;		variables. A "FOR" entry has a text pointer
;		and a pointer to a variable value so neither
;		the program or the simple variables can be
;		moved while there are active "FOR" entries on the stack.
;		User defined function values also contain
;		pointers into simple variable space so no user-defined
;		function values can be retained if simple variables
;		are moved. Adding a simple variable
;		adding six to ARYTAB and STREND, block transfering
;		the array variables up by six and making sure the
;		new [STREND] is not too close to the stack.
;		This movement of array variables means
;		that no pointer to an array will stay valid when
;		new simple variables can be encountered. This is
;		why array variables are not allowed "FOR"
;		loop variables, Setting up a new array variable
;		merely involves building the descriptor,
;		updating STREND, and making sure there is
;		still enough room between STREND and the
;		stack. Without multiple dimensions the format
;		of an array variable is simply:
;			Second character
;			First character
;			Number of bytes used by values
;			Values
;		The format when multiply dimensioned variables
;		are allowed is described in the "MULDIM" code.
;		PTRGET, the routine which returns a pointer
;		to a variable value, has two important flags. One is
;		"DIMFLG" which indicated whether "DIM" called PTRGET
;		or not. If so, no prior entry for the variable in
;		question should be found, and the index indicates
;		how much space to set aside. Simple variables can
;		be "DIMENSIONED", but the only effect will be to
;		set aside space for the variable if it hasn't been
;		encountered yet. The other important flag is SUBFLG
;		which indicates whether a subscripted variable should be
;		allowed in the current context. If SUBFLG is non-zero
;		the open parenthesis for a subscripted variable
;		will not be scanned by PTRGET, and PTRGET will return
;		with a text pointer pointing to the "(", if
;		there was one.
;
;	STRINGS
;		In the variable table strings are stored just like
;		numeric variables. Simple strings have four value
;		bytes which are initialized to all zeros (which
;		represents the null string). The only difference
;		in handling is that when PTRGET sees a "$" after the
;		name of a variable, PTRGET sets VALTYP to one and turns
;		on the MSB (Most-Signifigant-Bit) of the value of
;		the first character of the variable name.
;		Having this bit on in the name of the variable ensures
;		that the search routine will not match
;		'A' with 'A$' or 'A$' with 'A'. The meaning of
;		the four value bytes are:
;			LOW
;				Length of the string
;				Unused
;				Low 8 bits
;				Higm 8 bits of the address
;					of the characters in the
;					string if length<>0.
;					meaningless otherwise.
;			HIGH
;		The value of a string variable (these 4 bytes)
;		is called the string descriptor to distinguish
;		it from the actual string data. Whenever a
;		string constant is encountered in a formula or as
;		part of an input string, or as part of data, STRLIT
;		is called, causing a descriptor to be built for
;		the string. If the string constant is in BUF (which
;		it will be if the string is being "INPUT", or the
;		string is part of some formula in a direct statement)
;		the value is copied into string space since BUF
;		is always changing. "STRCPY" is used to copy
;		strings.
;
;		String functions and the one string operator "+"
;		always return their values in string space.
;		Assigning a string a constant value in a program
;		through a "READ" or assignment statement
;		will not use any string space since
;		the string descriptor will point into the
;		program itself. In general, copying is done
;		when a string value is in BUF, or it is in string
;		space and there is an active pointer to it.
;		Thus F$=G$ will cause copying if G$ has its
;		string data in string space. F$=CHR$(7)
;		will use one byte of string space to store the
;		new one character string created by "CHR$", but
;		the assignment itself will cause no copying since
;		the only pointer at the new string is a
;		temporary descriptor created by FRMEVL which will
;		go away as soon as the assignment is done.
;		It is the nature of garbage collection that
;		disallows having two string descriptors point to the same
;		area in string space. String functions and operators
;		must proceed as follows:
;			1) Figure out the length of their result
;			2) Call GETSPA to find space for their
;			result. The arguments to the function
;			or operator may change since garbage collection
;			may be invoked. The only thing that can
;			be saved during the call to GETSPA is a pointer
;			to the descriptors of the arguments.
;			3) Construct the result descriptor in DSCTMP.
;			GETSPA returns the location of the available
;			space.
;			4) Create the new value by copying parts
;			of the arguments or whatever.
;			5) Free up the arguments by calling FRETMP.
;			6) Jump to PUTNEW to get the descriptor in
;			DSCTMP transferred into a new string temporary.
;		The reason for string temporaries is that garbage
;		collection has to know about all active string descriptors
;		so it knows what is and isn't in use. String temporaries are
;		used to store the descriptors of string expressions.
;
;		Instead of maving an actual value stored in the
;		FAC, and having the value of a temporary result
;		being saved on the stack, as happens with numeric
;		variables, strings have the pointer to a string descriptor
;		stored in the FAC, and it is this pointer
;		that gets saved on the stack by formula evaluation.
;		String functions cannot free their arguments up right
;		away since GETSPA may force
;		garbage collection and the argument strings
;		may be over-written since garbage collection
;		will not be able to find an active pointer to
;		them. Function and operator results are built in
;		DSCTMP since string temporaries are allocated
;		(PUTNEW) and dealloated (FRETMP) in a FIFO ordering
;		(I.e. a stack) so the new temporary cannot
;		be set up until the old one(s) are freed. Trying
;		to build a result in a temporary after
;		freeing up the argument temporaries could result
;		in one of the argument temporaries being overwritten
;		too soon by the new result.
;
;		String space is allocateo at the very top
;		of memory. MEMSIZ points beyono the last location of
;		string space. String are stored in high locations
;		first. Whenever string space is allocated (GETSPA)
;		FRETOP, which is initialized to [MEMSIZ], is updated
;		to give the highest location in string space
;		that is not in use. The result is that
;		FRETOP gets smaller and smaller, until some
;		allocation would make [FRETOP] less than or equal to
;		[STKTOP]. This means string space has run into the
;		stack and that garbage collection must be called.
;
;		GARBAGE COLLECTION:
;			0. MINPTR= [STKTOP]  [FRETOP]=[MEMSIZ]
;			1. REMMIN=0
;			2. For each string descriptor
;			(temporaries, simple strings, string arrays)
;			if the string is not null and its pointer is
;			> MINPTR and < FRETOP,
;			MINPTR=this string descriptors pointer
;			REMMIN=pointer at this string descriptor
;			end
;			3. If REMMIN<>0 (we found an uncollected string)
;			block transfer the string data pointed
;			to in the string descriptor pointed to by REMMIN
;			so that the last byte of string data is at
;			[FRETOP], update FRETOP so that it
;			points to the location just below the one
;			the string data was moved into. update
;			the pointer in the descriptor so it points
;			to the new location of the string data.
;			Go to step 1.
;
;		After calling garbage collection GETSPA again checks
;		to see if [A] characters are available between
;		[STKTOP] and [FRETOP], if not an "OUT OF STRING"
;		error is invoked.
;
;	MATH PACKAGE
;		The math package contains floating input (FIN),
;		floating output (FOUT) floating compare (FCOMP)
;		... and all the numeric operators and functions.
;		the formats, conventions and entry points are all
;		described in the Math Package itself.
;
;	INIT -- THE INITIALIZATION ROUTINE
;		Initialization first looks at the SWITCH register
;		to see what type of I/O should be done.
;		Any non-standard I/O causes locations in BASIC
;		to be changed. Then the amount of memory,
;		terminal width, and which functions to be retained
;		are ascertained from the user. A Zero is put down
;		at the first location not used by the Math-Package
;		and TXTTAB is set up to point at the next location.
;		This determines where program storage will start. The
;		highest memory location minus the amount of defaulted
;		string space (50) gives the first location used by the
;		stack. Special checks are made to make sure
;		all questions in INIT are answered reasonably, since
;		once init finishes the locations it uses are
;		used for program storage. The last thing INIT does is
;		change location Zero to be a JUMP to READY instead
;		of init. Once this is done there is no way to restart
;		INIT.
;
;
;	STORAGE
;				A Zero.
;		[TXTTAB]	Pointer to next line's pointer
;				Line # of this line (2 bytes)
;				Characters on this line
;				Zero
;				Pointer at next line's pointer
;					(pointed to by the above pointer)
;				... repeats ...
;		LAST LINE:	Pointer at Zero pointer
;				Line # of this line
;				Characters on this line
;				Zero
;				Double zero (pointed to by the above pointer)
;		[VARTAB]	Simple variables, 6 bytes per value.
;				2 bytes give the name, 4 bytes the value
;				... repeats ...
;		[ARYTAB]	Array variables. 2 bytes name, 2 byte
;				length, value (extra if MULDIM on)
;				... repeats ...
;		[STREND]	Free space
;				... repeats ...
;				Most recent stack entry
;				... repeats ...
;		[STKTOP]	First stack entry
;				Free string space
;				... repeats ...
;		[FRETOP]	String space in use
;				... repeats ...
;		[MEMSIZ]	Highest machine location
;				Unused except by the VAL function.
;HIGH LOCATIONS

;-----------------------------------------------------------------------------
;	MACROS FOR DOS SERVICE CALLS
;
$SVC    MACRO   #N
        LD      A,#N
        RST     0028H
        ENDM

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


;========================================================
;  EQUATES/ASM: a list of useful equates for the Model 4
;========================================================
;
@IPL	EQU	00H		;re-boot system
@KEY	EQU	01H		;wait for key press
@DSP	EQU	02H		;display character
@GET	EQU	03H		;get byte from device
@PUT	EQU	04H		;write byte to device
@CTL	EQU	05H		;make control request
@PRT	EQU	06H		;send character to printer
@WHERE	EQU	07H		;locate origin of CALL
@KBD	EQU	08H		;scan keyboard
@KEYIN	EQU	09H		;accept line of input
@DSPLY	EQU	0AH		;display message line
@LOGER	EQU	0BH		;log message
@LOGOT	EQU	0CH		;display and log message
@MSG	EQU	0DH		;message line handler
@PRINT	EQU	0EH		;print message line
@VDCTL	EQU	0FH		;control video display
@PAUSE	EQU	10H		;wait for delay
@PARAM	EQU	11H		;parse parameters
@DATE	EQU	12H		;get system date
@TIME	EQU	13H		;get system time
@CHNIO	EQU	14H		;pass control downstream
@ABORT	EQU	15H		;abort program execution
@EXIT	EQU	16H		;return to LS-DOS
@CMNDI	EQU	18H		;execute command
@CMNDR	EQU	19H		;execute command, return
@ERROR	EQU	1AH		;post error message
@DEBUG	EQU	1BH		;enter DEBUG
@CKTSK	EQU	1CH		;check task slot
@ADTSK	EQU	1DH		;add task
@RMTSK	EQU	1EH		;remove task
@RPTSK	EQU	1FH		;replace task
@KLTSK	EQU	20H		;remove current task
@CKDRV	EQU	21H		;check drive availability
@DODIR	EQU	22H		;do a directory
@RAMDIR	EQU	23H		;get directory records
@DCSTAT	EQU	28H		;test if drive assigned
@SLCT	EQU	29H		;select new drive
@DCINIT	EQU	2AH		;initialize FDC
@DCRES	EQU	2BH		;reset FDC
@RSTOR	EQU	2CH		;issue FDC RESTORE
@STEPI	EQU	2DH		;issue FDC STEP IN
@SEEK	EQU	2EH		;seek a cylinder
@RSLCT	EQU	2FH		;test drive for busy-ness
@RDHDR	EQU	30H		;read sector header
@RDSEC	EQU	31H		;read sector
@VRSEC	EQU	32H		;verify sector
@RDTRK	EQU	33H		;read track
@HDFMT	EQU	34H		;hard disk format
@WRSEC	EQU	35H		;write sector
@WRSSC	EQU	36H		;write system sector
@WRTRK	EQU	37H		;write track
@RENAM	EQU	38H		;rename file
@REMOV	EQU	39H		;remove file or device
@INIT	EQU	3AH		;open new or existing file
@OPEN	EQU	3BH		;open existing file
@CLOSE	EQU	3CH		;close file
@BKSP	EQU	3DH		;backspace one log. rec.
@CKEOF	EQU	3EH		;check for end-of-file
@LOC	EQU	3FH		;calculate LRN
@LOF	EQU	40H		;calculate EOF LRN
@PEOF	EQU	41H		;position to EOF
@POSN	EQU	42H		;position file to LRN
@READ	EQU	43H		;read record from file
@REW	EQU	44H		;rewind file to beginning
@RREAD	EQU	45H		;re-read current sector
@RWRIT	EQU	46H		;re-write current sector
@SEEKSC	EQU	47H		;seek specified sector
@SKIP	EQU	48H		;skip next record
@VER	EQU	49H		;write and verify record
@WEOF	EQU	4AH		;write EOF
@WRITE	EQU	4BH		;write record to file
@LOAD	EQU	4CH		;load program file
@RUN	EQU	4DH		;run program file
@FSPEC	EQU	4EH		;parse filename
@FEXT	EQU	4FH		;set up default extension
@FNAME	EQU	50H		;get filename/extension
@GTDCT	EQU	51H		;get drive code table
@GTDCB	EQU	52H		;get device control block
@GTMOD	EQU	53H		;find module in memory
@RDSSC	EQU	55H		;read system sector
@DIRRD	EQU	57H		;read directory record
@DIRWR	EQU	58H		;write directory record
@MUL8	EQU	5AH		;multiply 8 by 8:  A = C * E
@MUL16	EQU	5BH		;multiply 16 by 8: HL:A = HL * C
@DIV8	EQU	5DH		;divide 8 by 8
@DIV16	EQU	5EH		;divide 16 by 8
@HEXD	EQU	5FH		;hex to decimal ASCII
@DECHEX	EQU	60H		;decimal ASCII to hex
@HEXDEC	EQU	61H		;hex to decimal ASCII
@HEX8	EQU	62H		;1-byte to hex
@HEX16	EQU	63H		;2-byte to hex
@HIGH$	EQU	64H		;get or set HIGH$
@FLAGS	EQU	65H		;get system flags
@BANK	EQU	66H		;memory banking
@BREAK	EQU	67H		;get or set <BREAK> vector
@SOUND	EQU	68H		;beep through speaker
@VDCLS	EQU	69H		;clear video screen
@CKBRKC	EQU	6AH		;check and reset <BREAK>
@VDPRT	EQU	6BH		;screen print


;-----------------------------------------------------------------------------
;	CONSTANTS
;

; --- TRS-80 Specific definitions ---
	IF	!BASE
BASE	EQU	0
	ENDIF

; --- Sizes ---

NUMLEV 	EQU	55		;NUMBER OF STACK LEVELS RESERVED
				; BY AN EXPLICIT CALL TO GETSTK
				;GWBASIC: 110
				;CP/M: 0*20+19+2*5=29
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

LPTLEN 	EQU	255		; Max column size on printer
CLMWID 	EQU	16		; MAKE COMMA COLUMNS FOURTEEN CHARACTERS
LNCMPS 	EQU	(((LPTLEN/CLMWID)-1)*CLMWID)
				;LAST COMMA FIELD POSIT

DATSPC 	EQU	128    		;NUMBER OF DATA BYTES IN DISK SECTOR



;=============================================================================
	IF	TEST
	ORG	3000H		;No need to split
	ELSE
	ORG	2600H		;Need to split to BASIC$/CMD and BASIC$/OV1
	ENDIF
;=============================================================================


;=============================================================================
;	GWDATA copied from BINTRP.MAC
; ## GWDATA.ASM:204 ##
;
START	JP	INIT

;	Signature 1 ? (DB 0F4H,29H,53H,2AH)
SIG1	DB	0F4H,29H,53H,2AH

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
$DADDH	CALL	VMOVAF		;--->	TODO cmts
	LD	HL,$DHALF
	CALL	VMOVFM
	CALL	DADD
	RET			;<---

$FADDH	LD	HL,$SHALF	;ADD .5 TO FAC
;	Entry to FADD with pointer to arg in (HL)
;	($FAC)=(BXDX)+($FAC)
$FADDS	CALL	$MOVRM		;GET ARGUMENT INTO THE REGISTERS
	JR	FADD		;DO THE ADDITION

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
	JR	NC,FADD1	;IS FAC SMALLER?
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
	JR	NC,ROUND	;ROUND RESULT IF THERE WAS NO OVERFLOW
				;THE MOST IT CAN OVERFLOW IS ONE BIT
	INC	HL		;THERE WAS OVERFLOW
	INC	(HL)		;INCREMENT EXPONENT
	JP	Z,$OVFLS
	LD	L,01H		;SHIFT RESULT RIGHT ONE, SHIFT CARRY IN
	CALL	SHRADD
	JR	ROUND		;ROUND RESULT AND WE ARE DONE

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
	JR	NZ,NORM3	;NO, SHIFT ONE PLACE AT A TIME
;	THIS LOOP SPEEDS THINGS UP BY SHIFTING 8 PLACES AT ONE TIME
	LD	C,D		;YES, SHIFT OVER 1 BYTE
	LD	D,H
	LD	H,L
	LD	L,A		;SHIFT IN 8 ZEROS FOR THE LOW ORDER
	LD	A,B		;UPDATE SHIFT COUNT
	SUB	08H
	CP	0E0H		;DID WE SHIFT IN 4 BYTES OF ZEROS?
	JR	NZ,NORM1	;NO, TRY TO SHIFT OVER 8 MORE
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
	JR	NZ,ZERO3	;DO USUAL THING
	LD	A,C		;GET BYTE TO SHIFT
ZERO2	DEC	B		;DECREMENT SHIFT COUNT
	RLA			;SHIFT LEFT
	JR	NC,ZERO2	;NORMALIZE LIKE SOB
	INC	B		;CORRECT SHIFT COUNT
	RRA			;WE DID IT ONE TOO MANY TIMES
	LD	C,A		;RESULT TO [C]
	JR	NORM4		;ALL DONE

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
	JR	Z,ROUND
	LD	HL,FAC		;LOOK AT FAC'S EXPONENT
	ADD	A,(HL)		;UPDATE EXPONENT
	LD	(HL),A
	JR	NC,$ZERO	;CHECK FOR UNDERFLOW
	JR	Z,$ZERO		;NUMBER IS ZERO, ALL DONE
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
	JR	C,SHFTR2	;NO, SHIFT IT ONE PLACE AT A TIME
				;THIS LOOP SPEEDS THINGS UP BY
				; SHIFTING 8 PLACES AT ONE TIME
	LD	B,E		;SHIFT NUMBER 1 BYTE RIGHT
	LD	E,D
	LD	D,C
	LD	C,00H		;PUT 0 IN HO
	JR	SHFTR1		;TRY TO SHIFT 8 RIGHT AGAIN

; 	Shift right number in BCDE
;
SHFTR2	ADD	A,09H		;CORRECT SHIFT COUNT
	LD	L,A		;SAVE SHIFT COUNT
;	TEST FOR CASE (VERY COMMON) WHERE SHIFTING SMALL INTEGER RIGHT.
;	THIS HAPPENS IN FOR LOOPS, ETC.
	LD	A,D		;SEE IF THREE LOWS ARE ZERO.
	OR	E
	OR	B
	JR	NZ,SHFTR22	;IF SO, DO USUAL.
	LD	A,C		;GET HIGH BYTE TO SHIFT
SHFTR21	DEC	L		;DONE SHIFTING?
	RET	Z		;YES, DONE
	RRA			;ROTATE ONE RIGHT
	LD	C,A		;SAVE RESULT
	JR	NC,SHFTR21	;ZAP BACK AND DO NEXT ONE IF NONE
	JR	SHFTR5		;CONTINUE SHIFTING

SHFTR22	XOR	A		;CLEAR CARRY
	DEC	L		;ARE WE DONE SHIFTING?
	RET	Z		;RETURN IF WE ARE
SHFTR4	LD	A,C		;GET HO
SHRADD	RRA			;ENTRY FROM FADD, SHIFT IT RIGHT
	LD	C,A		;SAVE IT
SHFTR5	LD	A,D		;SHIFT NEXT BYTE RIGHT
	RRA
	LD	D,A
	LD	A,E		;SHIFT LOW ORDER RIGHT
	RRA
	LD	E,A
SHFTR3	LD	A,B		;SHIFT OVERFLOW BYTE RIGHT
	RRA
	LD	B,A
	JR	SHFTR22		;SEE IF WE ARE DONE


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
	JR	FMULT		;COMPLETE LOG CALCULATION

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
	JR	Z,FMULT3	;ARE WE MULTIPLYING BY ZERO?
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
	JR	NC,FMULT4	;DON'T ADD IN NUMBER IF BIT WAS ZERO
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
	JR	Z,FMULT6	;IF NOT DON'T WORRY
	LD	A,B		;RE FETCH LO
	OR	20H		;"OR" IN STICKY
	LD	B,A		;BACK TO LO
FMULT6	DEC	E		;ARE WE DONE?
	LD	A,D		;GET NUMBER WE ARE MULTIPLYING BY
	JR	NZ,FMULT3?	;MULTIPLY AGAIN IF WE ARE NOT DONE
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
	JR	NC,FDIV2	; Restore divisor if borrow
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
	JR	Z,FDIV21	;IF NOT IGNORE
	LD	A,20H		;SET BIT
FDIV21	POP	HL		;AND THE REST OF REMAINDER
	OR	H		;"OR" IN REST
	JP	ROUNDB		;USE REMAINDER

FDIV22	RLA			;WE AREN'T, GET OLD CARRY BACK
	LD	A,E		;ROTATE EVERYTHING LEFT ONE
	RLA			;ROTATE NEXT BIT OF QUOTIENT IN
	LD	E,A
NROUNDB	LD	A,D
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
	JR	NZ,FDIV1	;THIS ISN'T THE CASE
	PUSH	HL		;SAVE PART OF NUMBER
	LD	HL,FAC		;GET POINTER TO FAC
	DEC	(HL)		;DECREMENT EXPONENT
	POP	HL		;GET NUMBER BACK
	JR	NZ,FDIV1	;DIVIDE MORE IF NO OVERFLOW OCCURED
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
	JR	Z,MULDV2	;IT IS, ZERO FAC AND WE ARE DONE
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
	JR	ICOMPS		;GO SET A CORRECTLY


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
	JR	MOVE1		;CONTINUE WITH THE MOVE

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
	JR	VMVVFM		; AN "XCHG" AND FALLS INTO MOVE1

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
	JR	NZ,ICOMP1	;GO SET UP A
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
	JR	NZ,FCOMPD	;THEY ARE DIFFERENT, GO SET UP A
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
	JR	CINT12

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
	JR	CINT14

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
	JR	NC,CONIS2	;IT IS, BUT IT MIGHT BE -32768
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
	JR	CONIS1		;STORE IT IN THE FAC AND SET VALTYP

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
	JR	CONISD

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
	JR	NC,DINT		;CONVERT THE DOUBLE PRECISION NUMBER
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
	JR	NZ,DINT2	;CHECK FOR -32768
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
	JR	NZ,DINT11
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
	JR	Z,DINTA1	;CONTINUE IF NECESSARY
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
	JR	Z,UMULTX	;THIS IS DONE FOR SPEED
	LD	A,10H		;SET UP A COUNT
UMULT1	ADD	HL,HL		;ROTATE (HL) LEFT ONE
	JP	C,BSERR		;CHECK FOR OVERFLOW, IF SO,
	EX	DE,HL		; BAD SUBSCRIPT (BS) ERROR
	ADD	HL,HL		;ROTATE (DE) LEFT ONE
	EX	DE,HL
	JR	NC,UMULT2	;ADD IN (BC) IF HO WAS 1
	ADD	HL,BC
	JP	C,BSERR		;CHECK FOR OVERFLOW
UMULT2	DEC	A		;SEE IF DONE
	JR	NZ,UMULT1
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
	JR	IADDS		;GO ADD THE NUMBERS

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
	JR	C,IMULTS	;CHECK FOR OVERFLOW
	EX	DE,HL		;ROTATE FIRST ARGUMENT LEFT ONE TO SEE IF
	ADD	HL,HL		; WE ADD IN (BC) OR NOT
	EX	DE,HL
	JR	NC,IMULT2	;DON'T ADD IN ANYTHING
	ADD	HL,BC		;ADD IN (BC)
	JR	C,IMULTS	;CHECK FOR OVERFLOW
IMULT2	DEC	A		;ARE WE DONE?
	JR	NZ,IMULT1	;NO, DO IT AGAIN
	POP	BC		;WE ARE DONE, GET SIGN OF RESULT
	POP	DE		;GET ORIGINAL FIRST ARGUMENT
IMULT21	LD	A,H		;ENTRY FROM IDIV, IS RESULT .GE. 32768?
	OR	A
	JP	M,IMULT3	;IT IS, CHECK FOR SPECIAL CASE OF -32768
	POP	DE		;RESULT IS OK, GET SECOND ARGUMENT OFF STACK
	LD	A,B		;GET THE SIGN OF RESULT IN A
	JR	INEGA		;NEGATE THE RESULT IF NECESSARY

IMULT3	XOR	80H		;IS RESULT 32768?
	OR	L		;NOTE: IF WE GET HERE FROM IDIV, THE RESULT
	JR	Z,IMULT4	; MUST BE 32768, IT CANNOT BE GREATER
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
	JR	IDIV14		;GO DIVIDE

IDIV12	PUSH	AF		;SAVE COUNT
	PUSH	HL		;SAVE (HL) I.E. CURRENT NUMERATOR
	ADD	HL,BC		;SUBTRACT DENOMINATOR
	JR	NC,IDIV13	;WE SUBTRACTED TOO MUCH, GET OLD (HL) BACK
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
	JR	NZ,IDIV12	;NO, DIVIDE AGAIN
	EX	DE,HL		;GET QUOTIENT IN (HL), REMAINDER IN (DE)
	POP	BC		;GET SIGN OF RESULT
	PUSH	DE		;SAVE REMAINDER SO STACK WILL BE ALRIGHT
	JR	IMULT21		;CHECK FOR SPECIAL CASE OF 32768

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
	JR	INEGA


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
	JR	NC,DADD2	;PUT THE SMALLER NUMBER IN FAC
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
	JR	NZ,DADD1	;NO, DO THE NEXT LO BYTE
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
	JR	NC,DROUND	;ROUND THE RESULT IF NO CARRY
	EX	DE,HL		;GET POINTER TO FAC IN (HL)
	INC	(HL)		;ADD 1 TO EXPONENT
	JP	Z,$OVFLS
;**************************************************************
; WE ARE NOW SET TO SHIFT THE FAC RIGHT 1 BIT. RECALL WE GOT HERE WITH CF=1.
; THE INSTRUCTIONS SINCE WE GOT HERE HAVEN'T AFFECTED
; CF SO WHEN WE SHIFT RIGHT WE WILL SHIFT CF INTO THE HIGH MANTISSA BIT.
;*************************************************************
	CALL	DSHRRA1		;SHIFT NUMBER RIGHT ONE, SHIFT IN CARRY
	JR	DROUND		;ROUND THE RESULT

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
	JR	NZ,DNORM5	;WE CAN'T, SEE IF NUMBER IS NORMALIZED
	LD	HL,DFACLO1	;WE CAN, GET POINTER TO LO
	LD	C,08H		;SET UP A COUNT
DNORM2	LD	D,(HL)		;GET A BYTE OF FAC
	LD	(HL),A		;PUT IN BYTE FROM LAST LOCATION,
				; THE FIRST TIME THROUGH A IS ZERO
	LD	A,D		;PUT THE CURRENT BYTE IN A FOR NEXT TIME
	INC	HL		;INCREMENT POINTER TO NEXT HIGHER ORDER
	DEC	C		;ARE WE DONE?
	JR	NZ,DNORM2	;NO, DO THE NEXT BYTE
	LD	A,B		;SUBTRACT 8 FROM SHIFT COUNT
	SUB	08H
	CP	0C0H		;HAVE WE SHIFTED ALL BYTES TO ZERO?
	JR	NZ,DNORM1	;NO, TRY TO SHIFT 8 MORE
	JP	$ZERO		;YES, THE NUMBER IS ZERO

DNORM3	DEC	B		;DECREMENT SHIFT COUNT
	LD	HL,DFACLO1	;GET POINTER TO LO
	CALL	DSHFLC		;SHIFT THE FAC LEFT
	OR	A		;SEE IF NUMBER IS NORMALIZED
DNORM5	JP	P,DNORM3	;SHIFT FAC LEFT ONE IF IT IS NOT NORMALIZED
	LD	A,B		;GET THE SHIFT COUNT
	OR	A		;SEE IF NO SHIFTING WAS DONE
	JR	Z,DROUND	;NONE WAS, PROCEED TO ROUND THE NUMBER
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
	JR	DADDS

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
	JR	NZ,DADDL	;NO, DO THE NEXT HIGHER ORDER BYTE
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
	JR	C,DSHFR3	;WE CAN'T, CHECK IF WE ARE DONE
	POP	HL		;GET POINTER BACK
DSHFRM	PUSH	HL		;ENTRY FROM DMULT, SAVE POINTER TO HO
	LD	DE,0800H	;SHIFT A ZERO INTO THE HO, SET UP A COUNT
DSHFR2	LD	C,(HL)		;SAVE A BYTE OF FAC
	LD	(HL),E		;PUT THE LAST BYTE IN ITS PLACE
	LD	E,C		;SET UP E FOR NEXT TIME THROUGH THE LOOP
	DEC	HL		;POINT TO NEXT LOWER ORDER BYTE
	DEC	D		;ARE WE DONE?
	JR	NZ,DSHFR2	;NO, DO THE NEXT BYTE
	JR	DSHFR1		;YES, SEE IF WE CAN SHIFT OVER 8 MORE

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
	JR	NZ,DSHFR5	;NO, ROTATE THE NEXT LOWER ORDER BYTE
	JR	DSHFR4		;YES, SEE IF WE ARE DONE SHIFTING

;	ENTRY TO DSHFTR FROM DADD, DMULT
;
DSHRRA1	LD	HL,FACHI	;GET POINTER TO HO OF FAC
	LD	D,01H		;SHIFT RIGHT ONCE
	JR	DSHFRA		;GO DO IT

;	Rotate FAC left one
;	Alters A,C,H,L
DSHFLC	LD	C,08H		;SET UP A COUNT
DSHFTL	LD	A,(HL)		;GET A BYTE OF FAC
	RLA			;ROTATE IT LEFT ONE
	LD	(HL),A		;UPDATE BYTE IN FAC
	INC	HL		;INCREMENT POINTER TO NEXT HIGHER ORDER BYTE
	DEC	C		;ARE WE DONE?
	JR	NZ,DSHFTL	;NO, ROTATE THE NEXT BYTE
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
	JR	Z,DMULT5	;WE ARE
	LD	C,08H		;SET UP A COUNT
DMULT3	PUSH	BC		;SAVE COUNTERS
	RRA			;ROTATE MULTIPLIER RIGHT
	LD	B,A		;SAVE IT
	CALL	C,DADDAR	;ADD IN OLD FAC IF BIT OF MULTIPLIER WAS ONE
	CALL	DSHRRA1		;ROTATE PRODUCT RIGHT ONE
	LD	A,B		;GET MULTIPLIER IN A
	POP	BC		;GET COUNTERS BACK
	DEC	C		;ARE WE DONE WITH THIS BYTE OF ARG?
	JR	NZ,DMULT3	;NO, MULTIPLY BY THE NEXT BIT OF THE MULTIPLIER
DMULT4	POP	DE		;YES, GET POINTER INTO ARG BACK
	DEC	B		;ARE WE DONE?
	JP	NZ,DMULT2	;NO, MULTIPLY BY NEXT HIGHER ORDER BY OF ARG
				;POINT IS TO RIGHT OF UNDERSTOOD ONE
	JP	DNORML		;ALL DONE, NORMALIZE AND ROUND RESULT

DMULT5	LD	HL,FACHI	;GET POINTER TO HO OF FAC
	CALL	DSHFRM		;SHIFT PRODUCT RIGHT ONE BYTE, WE ARE
	JR	DMULT4		; MULTIPLYING BY ZERO

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
	JR	NC,DIV11	;REACHING ZERO
	LD	DE,$DMP01	;POINT TO .1D0
	LD	HL,ARGLO	;POINT TO ARG
	CALL	VMOVE		;(DE) := (HL), (VALTYP) bytes
	JR	DMULT		;FAC := FAC * ARG (DP)

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
	JR	NZ,DIV13
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
	JR	NZ,DARG161	;Loop if no underflow
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
	JR	NZ,PSHARG1
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
	JR	NZ,POPARG1
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
	JR	C,DDIV2		;WAS IT OK?
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
	JR	NZ,DDIV1	; CHANGED ON THE FIRST OR SECOND SUBTRACTION)
	LD	HL,FAC		;YES, SUBTRACT ONE FROM EXPONENT TO CORRECT
	DEC	(HL)		; SCALING
	JR	NZ,DDIV1	;CONTINUE DIVIDING IF NO UNDERFLOW
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
	JR	Z,FINC		;IGNORE MINUS SIGN
	CP	'+'		;IGNORE A LEADING SIGN
	JR	Z,FINC
	DEC	HL		;SET CHARACTER POINTER BACK ONE
;	Here to check for a digit, a decimal point, "E" or "D"
FINC	CALL	CHRGTR		;GET THE NEXT CHARACTER OF THE NUMBER
	JP	C,FINDIG	;WE HAVE A DIGIT
	CP	'.'		;CHECK FOR A DECIMAL POINT
	JR	Z,FINDP1	;WE HAVE ONE, I GUESS
	CP	'e'		;LOWER CASE "E"
	JR	Z,FINC1
	CP	'E'		;CHECK FOR A SINGLE PRECISION EXPONENT
FINC1	JR	NZ,FINC4	;NO
	PUSH	HL		;SAVE TEXT PTR
	CALL	CHRGTR		;GET NEXT CHAR
	CP	'l'		;SEE IF LOWER CASE "L"
	JR	Z,FINC2		;IF SO POSSIBLE ELSE
	CP	'L'		;IS THIS REALLY AN "ELSE"?
	JR	Z,FINC2		;WAS ELSE
	CP	'q'		;SEE IF LOWER CASE "Q"
	JR	Z,FINC2		;IF SO POSSIBLE "EQV"
	CP	'Q'		;POSSIBLE "EQV"
FINC2	POP	HL		;RESTORE [H,L]
	JR	Z,FINC3		;IT WAS JUMP!
	LD	A,(VALTYP)	;IF DOUBLE DON'T DOWNGRADE TO SINGLE
	CP	08H		;SET CONDITION CODES
	JR	Z,FINC5
	LD	A,00H		;MAKE A=0 SO NUMBER IS A SINGLE
	JR	FINC5

FINC3	LD	A,(HL)		;RESTORE ORIGINAL CHAR
FINC4	CP	'%'		;TRAILING % (RSTS-11 COMPATIBILITY)
	JR	Z,FINDP11	;MUST BE INTEGER.
	CP	'#'		;FORCE DOUBLE PRECISION?
	JR	Z,FINDP12	;YES, FORCE IT & FINISH UP.
	CP	'!'		;FORCE SINGLE PREC.
	JR	Z,FINDP13
	CP	'd'		;LOWER CASE "D"
	JR	Z,FINC5
	CP	'D'		;CHECK FOR A DOUBLE PRECISION EXPONENT
	JR	NZ,FINE		;WE DON'T HAVE ONE, THE NUMBER IS FINISHED
FINC5	OR	A		;DOUBLE PRECISION NUMBER -- TURN OFF ZERO FLAG
	CALL	FINFRC		;FORCE THE FAC TO BE SNG OR DBL
	CALL	CHRGTR		;GET THE FIRST CHARACTER OF THE EXPONENT
	CALL	MINPLS		;EAT SIGN OF EXPONENT  ( test '+', '-'..)
;	Here to get the next digit of the exponent
FINEC	CALL	CHRGTR		;GET THE NEXT CHARATER
	JP	C,FINEDG	;PACK THE NEXT DIGIT INTO THE EXPONENT
	INC	D		;IT WAS NOT A DIGIT, PUT THE CORRECT SIGN ON
	JR	NZ,FINE		; THE EXPONENT, IT IS POSITIVE
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
	JR	NZ,FINEF2	;MULTIPLY OR DIVIDE AGAIN IF WE ARE NOT DONE
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
	JR	NZ,FINE		;WE HAD 2 DECIMAL POINTS, NOW WE ARE DONE
	CALL	C,FINFRC	;THIS IS THE FIRST ONE, CONVERT FAC TO SNG
				; IF WE DON'T ALREADY HAVE A DOUBLE
	JP	FINC		;CONTINUE LOOKING FOR DIGITS

FINDP11	CALL	CHRGTR		; Gets next character (or token) from BASIC text.
	POP	AF		;GET SIGN OFF THE STACK
	PUSH	HL		;SAVE TEXT POINTER
	LD	HL,POPHRT	;ADDRESS POP (HL) AND RETURN
				;2 lines below added in MBASIC
;	PUSH	HL		; * (added in the late March edition)
;	LD	HL,CINT		; *
	PUSH	HL		;WILL WANT TO FORCE ONCE D.P. DONE
	PUSH	AF		;PUT SIGN BACK ON THE STACK
	JR	FINE		;ALL DONE

FINDP12	OR	A		;SET NON-ZERO TO FORCE DOUBLE PREC
FINDP13	CALL	FINFRC		;FORCE THE TYPE
	CALL	CHRGTR		;READ AFTER TERMINATOR
	JR	FINE		;ALL DONE

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
	JR	NC,FINDG2	; (HL) .LT. (DE), SO THE NUMBER IS TOO BIG
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
;
; NOTE: The following deviation from MBASIC has the effect to
;	convert numbers above 100'000 to DP where MBASIC
;	converts numbers above 1'000'000 to DP.
;
;	If we type this command:
;	  PRINT 999999,9999999,99999999
;	In MBASIC we get:
;	  999999 (SP)    1E+07 (SP)  99999999 (DP)
;	In TRSDOS we get:
;	  999999 (SP)  9999999 (DP)  99999999 (DP)
;
;	Here to decide if we have a single or double precision number
FINDGV	JR	NC,FINDGD	;FALL THROUGH IF VALTYP WAS 4 I.E. SNG PREC
	;Other constants in MBASIC: 9474H,2400H = 1'000'000 !!
	LD	BC,9143H	;--->	GET 100'000, DO WE HAVE 6 DIGITS ALREADY?
	LD	DE,5000H	;<---
	CALL	FCOMP		;IF SO, FAC .GE. 100'000
	JP	P,FINDG3	;WE DO, CONVERT TO DOUBLE PRECISION
	CALL	MUL10		;MULTIPLY THE OLD NUMBER BY TEN
	POP	AF		;GET THE NEXT DIGIT
	CALL	FINLOG		;PACK IT INTO THE FAC
	JR	FINDGE		;GET FLAGS OFF STACK AND WE ARE DONE

;	Here to convert a 7 digit single precision number to double precision
FINDG3	CALL	CONDS		;CONVERT SINGLE TO DOUBLE PRECISION
;	Here to pack in the next digit of a double precision number
FINDGD	CALL	DMUL10		;MULTIPLY THE FAC BY 10
	CALL	VMOVAF		;SAVE THE FAC IN ARG
	POP	AF		;GET THE NEXT DIGIT
	CALL	$FLT		;CONVERT THE DIGIT TO SINGLE PRECISION
	CALL	CONDS		;NOW, CONVERT THE DIGIT TO DOUBLE PRECISION
	CALL	DADD		;ADD IN THE DIGIT
	JR	FINDGE		;GET THE FLAGS OFF THE STACK AND WE ARE DONE

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
	JR	NC,FINEDG0	;WE ALREADY HAVE TWO DIGITS
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
	JR	FINEDG3

FINEDG2	LD	A,C
FINEDG3	XOR	(HL)		;SIGN IN HIGH BIT OF (A)
	RLA			;SIGN IN CARRY
	POP	HL
	JR	DV010		;GO PRINT OVERFLOW

FINEDG4	POP	AF		; This entry is used by __EXP
	POP	AF		; (RESZER exits here)
;	Deal with various overflow conditions
$OVFLS2	LD	A,(FACHI)
	RLA
	JR	DV010		;GO PRINT OVERFLOW

$OVFLS1	POP	AF		;DO A POP THEN FALL INTO OVERR_1
$OVFLS	LD	A,(FAC_1)	;GET SIGN BYTE
	CPL			;SIGN WAS STORED COMPLEMENTED
	RLA			;SIGN TO CARRY
	JR	DV010		;GO PRINT OVERFLOW

$DIV0S1:
	OR	B		;---> These 2 lines don't appear
	JR	Z,$DIV0S	;<--- in MBASIC
	LD	A,C
	JR	$DIV0S

; Division (exponent is 0)
;
$DIV0S2	LD	A,(FACHI)	;TODO Check: (ARGHI in MBASIC)
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
	JR	NZ,INF00	;JUMP PRINT IF TRAPPING OTHERWISE +INFINITY
	LD	HL,FLGOVC	;PRINT INDICATOR FLAG
	LD	A,(HL)
	OR	A		;PRINT IF 0,1;SET TO 2 IF 1
	JR	Z,DV011
	DEC	A
	JR	NZ,INF00
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
	JR	NC,INF10
	LD	DE,$INFMS	;MINUS INFINITY
INF10	CALL	MOVMM		;MOVE INTO FAC
	CALL	GETYPR
	JR	C,INF20		;SP ALL OK
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
	JR	Z,INF30		;JUMP IF NOT TRAPPING
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
	JR	FOUT2		;CONVERT THE NUMBER INTO DIGITS

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
	JR	Z,FOUT1		;THEY DON'T
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
	JR	NC,FOUFRV	;WE HAVE A SNG OR DBL
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
	JR	Z,FOTSZ1	;WE DON'T
	LD	A,B		;WE DO, SEE IF THE SIGN WAS A SPACE
	CP	C		;ZERO FLAG IS SET IF IT WAS
	LD	C,'*'		;SET FILL CHARACTER TO AN ASTERISK
	JR	NZ,FOTSZ1	;SET THE SIGN TO AN ASTERISK
				; IF IT WAS A SPACE
	LD	A,E		;GET FORMAT SPECS AGAIN
	AND	04H		;SEE IF SIGN IS TRAILING
	JR	NZ,FOTSZ1	;IF SO DON'T ASTERISK FILL
	LD	B,C		;B HAS THE SIGN, C THE FILL CHARACTER
FOTSZ1	LD	(HL),C		;FILL IN THE ZERO OR THE SIGN
	CALL	CHRGTR		;GET THE NEXT CHARACTER IN THE BUFFER
				; SINCE THERE ARE NO SPACES, "CHRGET"
				; IS EQUIVALENT TO "INX	H"/"MOV	A,M"
	JR	Z,FOTSZ11	;IF WE SEE A REAL ZERO, IT IS THE END OF THE NUMBER,
				; AND WE MUST BACK UP AND PUT IN A ZERO.
				;CHRGET SETS THE ZERO FLAG ON REAL ZEROS OR COLONS,
				; BUT WE WON'T SEE ANY COLONS IN THIS BUFFER.
	CP	'E'		;BACK UP AND PUT IN A ZERO IF WE SEE
	JR	Z,FOTSZ11	; AN "E" OR A "D" SO WE CAN PRINT 0 IN
	CP	'D'		; FLOATING POINT NOTATION WITH THE C FORMAT ZERO
	JR	Z,FOTSZ11
	CP	'0'		;DO WE HAVE A ZERO?
	JR	Z,FOTSZ1	;YES, SUPPRESS IT
	CP	','		;DO WE HAVE A COMMA?
	JR	Z,FOTSZ1	;YES, SUPPRESS IT
	CP	'.'		;ARE WE AT THE DECIMAL POINT?
	JR	NZ,FOTSZ2	;NO, I GUESS NOT
FOTSZ11	DEC	HL		;YES, BACK UP AND PUT A ZERO BEFORE IT
	LD	(HL),'0'
FOTSZ2	LD	A,E		;GET THE FORMAT SPECS TO CHECK FOR A FLOATING
	AND	10H		; DOLLAR SIGN
	JR	Z,FOTSZ3	;WE DON'T HAVE ONE
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
	JR	Z,FOUFRV5	;IF SO, JUMP
	CP	':'		;IF CARRY NOT SET NOT A DIGIT
	JR	NC,FOUFRV3
	CP	'0'		;IF CARRY SET NOT A DIGIT
	JR	C,FOUFRV3
	INC	C		;INCREMENTED DIGITS TO PRINT
FOUFRV3	INC	HL		;POINT TO NEXT BUFFER CHARACTER
	LD	A,(HL)		;FETCH NEXT CHARACTER
	OR	A		;0(BINARY) AT THE END OF CHARACTERS
	JR	NZ,FOUFRV2	;CONTINUE SEARCH IF NOT AT END
	LD	A,'D'		;NOW TO CHECK TO SEE IF SEARCHED FOR D
	CP	B
	LD	B,A		;IN CASE NOT YET SEARCHED FOR
	POP	HL		;NOW TO CHECK FOR "D"
	LD	C,00H		;ZERO DIGIT COUNT
	JR	NZ,FOUFRV1	;GO SEARCH FOR "D" IF NOT DONE SO
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
	JR	Z,FOUFRVA	;IF SO NO BETTER PRINTOUT
	CP	'-'		;MUST BE NEGATIVE!
	JR	Z,FOUFRV7	;MUST PROCESS THE DIGITS
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
	JR	NC,FOUFRVA	;IF SO STOP TRYING
FOUFRV7	INC	HL		;POINT TO NEXT CHARACTER
	LD	A,(HL)		;FETCH UP
	OR	A		;BINARY ZERO AT END
	JR	NZ,FOUFRV6	;CONTINUE IF NOT AT END
	LD	H,B		;SAVE EXPONENT
	POP	BC		;FETCH TYPE, DIGIT COUNT
	LD	A,B		;DETERMINE TYPE
	CP	'E'		;SINGLE PRECISION?
	JR	NZ,FOUFRV9	;NO - GO PROCESS AS DOUBLE PRECISION
	LD	A,C		;DIGIT COUNT
	ADD	A,H		;ADD EXPONENT VALUE
	CP	09H
	POP	HL		;POP OLD BUFFER POINTER
	JR	NC,FOUFRV4	;CAN'T DO BETTER
FOUFRV8	LD	A,80H
	LD	(FLGSCN),A
	JR	FOUFRVB		;DO FIXED POINT PRINTOUT

FOUFRV9	LD	A,H		;SAVE EXPONENT
	ADD	A,C		;TOTAL DIGITS NECESSARY
	CP	12H		;MUST PRODUCE CARRY TO USE FIXED POINT
	POP	HL		;GET STACK RIGHT
	JR	NC,FOUFRV4
	JR	FOUFRV8		;GO PRINT IN FIXED POINT

FOUFRVA	POP	BC
	POP	HL		;GET ORIGINAL BUFFER PTR BACK
	JR	FOUFRV4

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
	JR	FOUFRVE		;FIXED OUTPUT

FOUFRVD	POP	AF		;NORMAL ROUTE
	ADD	A,D		;SEE IF NUMBER SHOULD BE PRINTED IN E NOTATION
	JP	M,FOFRS1	;IT SHOULD, IT IS .LT. .01
	INC	D		;CHECK IF IT IS TOO BIG
	CP	D
	JR	NC,FOFRS1	;IT IS TOO BIG, IT IS .GT. 10^D-1
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
	JR	Z,FOFRS2	;IT WAS, CONTINUE SUPPRESSING
	CP	'.'		;HAVE WE SUPPRESSED ALL THE FRACTIONAL DIGITS?
	CALL	NZ,INXHRT	;YES, IGNORE THE DECIMAL POINT ALSO
	POP	AF		;GET THE EXPONENT BACK
	JR	Z,FOUTZR1	;WE ARE DONE IF WE ARE IN FIXED POINT NOTATION
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
	JR	NC,FOUCE2	;DO IT AGAIN IF RESULT WAS POSITIVE
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
	JR	NC,FOUFXV	;WE HAVE A SNG OR A DBL

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
	JR	Z,FFXIX1	;CHECK IF WE HAVE A TRAILING SIGN
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
	JR	Z,FOUBE1	; IGNORE IT AND MAKE THE FIELD SHORTER WITH
	CP	'*'		; NO ILL EFFECTS
	JR	Z,FOUBE1
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
	JR	NZ,FOUBE4	;NO, WE CAN NOT GET RID OF ANOTHER CHARACTER
	INC	HL		;SKIP OVER THE DECIMAL POINT
	CALL	CHRGTR		;GET THE NEXT CHARACTER
	JR	NC,FOUBE4	;IT IS NOT A DIGIT, WE CAN'T SHORTEN THE FIELD
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
	JR	Z,FOUBE3	;PUT IT BACK IN THE BUFFER
				; IF IT IS NOT THE LAST ONE
	POP	BC		;GET THE BUFFER POINTER OFF THE STACK
	JR	FOUBE5		;SEE IF THE FIELD IS NOW SMALL ENOUGH

;	Here if the number is too big for the field
FOUBE4	POP	AF		;GET THE CHARACTERS OFF THE STACK
	JR	Z,FOUBE4	;LEAVE THE NUMBER IN THE BUFFER ALONE
	POP	HL		;GET THE POINTER TO THE BEGINNING
				; OF THE NUMBER MINUS 1
	LD	(HL),'%'	;PUT IN A PERCENT SIGN TO INDICATE
				; THE NUMBER WAS TOO LARGE FOR THE FIELD
	RET			;ALL DONE -- RETURN FROM FOUT

;	Here to print a SNG or DBL in fixed format
FOUFXV	PUSH	HL		;SAVE THE BUFFER POINTER
	RRA			;GET FIXED OR FLOATING NOTATION FLAG IN CARRY
	JP	C,FFXFLV	;PRINT THE NUMBER IN E-NOTATION
	JR	Z,FFXSFX	;WE HAVE A SNG HERE TO PRINT A DBL IN
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
	JR	FFXXV31		;CONVERT THE DIGITS AND DO THE TRIMMING UP

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
	JR	NZ,FFXXV32	;CHECK IF THERE WERE ANY DECIMAL PLACES AT ALL
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
FFXFLV	JR	Z,FFXSFL	;IF WE HAVE A SNG, SET THE RIGHT FLAGS
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
	XOR	A		;--->
	OR	B
	JR	Z,FFXSFL1	;<---
	LD	A,D		;GET THE "A" FIELD SPEC
	AND	04H		;SEE IF THE SIGN IS A TRAILING SIGN
	CP	01H		;SET CARRY IF A IS ZERO
	SBC	A,A		;SET D=0 IF WE HAVE A TRAILING SIGN,
FFXSFL1	LD	D,A		; D=377 IF WE DO NOT
	ADD	A,C
	LD	C,A		;SET C=NUMBER OF SIGNIFICANT DIGITS TO PRINT
	SUB	E		;IF WE HAVE LESS THAN E, THEN WE MUST GET RID
	PUSH	AF		;SAVE COMPARISON # OF SIG DIGITS
				; AND THE # OF DIGITS WE WILL PRINT
	PUSH	BC		;SAVE THE "B" FIELD SPEC AND # OF SIG DIGITS
	PUSH	DE		;--->
	PUSH	HL
	PUSH	AF		;<---
FFXLV1	CALL	M,FINDIV	; OF SOME BY DIVIDING BY TEN AND ROUNDING
	JP	M,FFXLV1
	XOR	A		;--->
	LD	(EXPAF),A
	LD	HL,EXPTMP
	CALL	VMOVMF
	CALL	GETYPR
	JP	PE,FFXLV11
	CALL	$FADDH
	LD	A,0FAH
	JP	FFXLV12
FFXLV11	CALL	$DADDH
	LD	A,0F0H
FFXLV12	POP	DE
	SUB	D
FFXLV13	CALL	M,FINDIV
	JP	M,FFXLV13
	LD	A,(FAC)
	SUB	81H
	PUSH	AF
	LD	HL,EXPTMP
	CALL	VMOVFM
	POP	AF
	JP	M,FFXLV14
	LD	A,01H
	LD	(EXPAF),A
	CALL	FINDIV
FFXLV14	POP	HL		;<---
	POP	DE
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
	JR	NZ,FFXLV16	;IF NON-ZERO PROCEED
	CALL	DECHRT		;SEE IF D.P. THERE
	LD	A,(HL)		;FETCH TO MAKE SURE D.P.
	CP	'.'		;IF NOT MUST BE ZERO
	CALL	NZ,INXHRT	;IF NOT MUST LEAVE AS IS
	LD	(TEMP2),HL	;NEED D.P. LOCATION IN TEMP2
				; SO IGNORE IT.
FFXLV16	POP	AF		;GET THE EXPONENT BACK
	JR	C,FFXLV17	;EXPONENT=0 IF THE NUMBER IS ZERO
	ADD	A,E		;SCALE IT CORRECTLY
	SUB	B
	SUB	D
FFXLV17	PUSH	BC		;SAVE THE "B" FIELD SPEC
	LD	B,A		;--->
	LD	A,(EXPAF)
	ADD	A,B		;<---
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
	JR	NC,FOUNS10	;NO, DONT MULTPLY
	LD	DE,$D1E10$	;MULTIPLY BY 1D10
	LD	HL,ARGLO	;MOVE INTO ARG
	CALL	VMOVE		;PUT IN ARG
	CALL	DMULT		;MULTIPLY BY IT
	POP	AF		;GET ORIG EXPONENT OFF STACK
	SUB	0AH		;GET PROPER OFFSET FOR EXPONENT
	PUSH	AF		;SAVE EXPONENT BACK
	JR	FOUTNV1		;FORCE IT BIGGER IF POSSIBLE

FOUNS10	CALL	FOUNSC		;IS THE FAC TOO BIG OR TOO SMALL?
FOUNS1	CALL	GETYPR		;SEE WHAT KIND OF VALUE WE HAVE SO
				; WE CAN SEE IF THE FAC IS BIG ENOUGH
	JP	PE,FOUNS1D	;WE HAVE A DBL
	LD	BC,9143H	;GET 99999.95 TO SEE IF THE FAC IS BIG
	LD	DE,4FF9H	; ENOUGH YET
	CALL	FCOMP
	JR	FOUNS1C		;GO DO THE CHECK

FOUNS1D	LD	DE,$D1E15$	;GET POINTER TO 999,999,999,999,999.5
	CALL	DCOMPD		;SEE IF THE NUMBER IS STILL TOO SMALL
FOUNS1C	JP	P,FOUNS3	;IT ISN'T ANY MORE, WE ARE DONE
	POP	AF		;IT IS, MULTIPLY BY TEN
	CALL	FINMLT
	PUSH	AF		;SAVE THE EXPONENT AGAIN
	JR	FOUNS1		;NOW SEE IF IT IS BIG ENOUGH

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
	JR	FOUNSCC		;GO DO THE CHECK

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
	JR	FOUNSC1		;GO SEE IF WE ARE NOW DONE

;	Here to put zeros in the buffer with commas or a decimal point in the middle.
;	The count is in A, it can be zero, but the Zero flag must be set.
;	B the decimal point count and C the comma count are updated
;	A,B,C,H,L are altered
FOUNSC2	JR	NZ,FOUNSC4	;ENTRY AFTER A "CALL FOUTCV"
FOUNSC3	RET	Z		;RETURN IF WE ARE DONE
	CALL	FOUNSCA		;SEE IF WE HAVE TO PUT A COMMA
				; OR A DECIMAL POINT BEFORE THIS ZERO
FOUNSC4	LD	(HL),'0'	;PUT A ZERO IN THE BUFFER
	INC	HL		;UPDATE THE BUFFER POINTER
	DEC	A		;DECREMENT THE ZERO COUNT
	JR	FOUNSC3		;GO BACK AND SEE IF WE ARE DONE

;	Here to put a possible comma count in C, and zero C if we are not
;	using the comma specification
FOUNSC5	LD	A,E		;SETUP DECIMAL POINT COUNT
	ADD	A,D
	INC	A
	LD	B,A
	INC	A		;SETUP COMMA COUNT
FOUNSC6	SUB	03H		;REDUCE [A] MOD 3
	JR	NC,FOUNSC6
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
	JR	NZ,FOUNSC9
	INC	HL		;PUT IN LEADING ZEROS UNTIL B ZERO
	LD	C,B		;POINT TO NEXT AVAILABLE BUFFER LOCATION
	RET

FOUNSCA	DEC	B		;TIME FOR D.P.?
FOUNSCB	JR	NZ,FOUNSCF	;IF NOT D.P. TIME, SEE IF COMMA TIME

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
	PUSH	BC		;--->
	PUSH	HL		;<---
	CALL	GETYPR		;SEE WHAT KIND OF A NUMBER WE HAVE
	JP	PO,FOUNSCJ	;WE HAVE A SNG
;	Here to convert a double precision number to decimal digits
	CALL	$DADDH		;<-->	ADD .5 TO THE ORIGINAL NUMBER TO ROUND IT
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
	JR	NC,FOUNSCI	;IF THE NUMBER WAS NOT LESS THAN THE POWER OF TEN,
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
	JR	NZ,FOUNSCH	;NO, GO DO THE NEXT ONE
	PUSH	BC		;YES, CONVERT REMAINING DIGITS USING SINGLE
	PUSH	HL		; PRECISION, THIS IS FASTER, MOVE THE NUMBER
	LD	HL,DFACLO	; THAT IS LEFT INTO THE SNG FAC
	CALL	$MOVFM
	JR	FOUNSCK

;	Here to convert a single precision number to decimal digits
FOUNSCJ:			;<-->	PUSH BC : PUSH HL removed since MBASIC
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
	JR	NC,FOUNSCM	;SUBTRACT AGAIN IF RESULT WAS POSITIVE
	CALL	FADDA		;IT WASN'T, ADD POWER OF TEN BACK IN
	INC	HL		;INCREMENT POINTER TO NEXT POWER OF TEN
	CALL	$MOVFR		;SAVE C,D,E IN FAC
	EX	DE,HL		;GET POWER OF TEN POINTER IN (DE)
	POP	HL		;GET BUFFER POINTER
	LD	(HL),B		;PUT CHARACTER IN BUFFER
	INC	HL		;INCREMENT BUFFER POINTER
	POP	AF		;GET DIGIT COUNT (THE CARRY) BACK
	POP	BC		;GET COMMA AND DP INFORMATION BACK
	JR	C,FOUNSCL	;CALCULATE NEXT DIGIT IF WE HAVE NOT DONE 2
	INC	DE		;WE HAVE, INCREMENT POINTER TO CORRECT PLACE
	INC	DE		; IN THE INTEGER POWER OF TEN TABLE
	LD	A,04H		;GET THE DIGIT COUNT
	JR	FCI20		;COMPUTE THE REST OF THE DIGITS LIKE INTEGERS
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
	JR	NC,FCI30	;IF (HL) WAS .GE. (DE) THEN SUBTRACT AGAIN
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
	JR	NZ,FCI20	;NO, GO DO THE NEXT ONE
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
	JR	Z,FOUTH4	;DO FIRST OCTAL DIGIT
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
	JR	NZ,FOUTH5	;NO, MAKE A DIGIT
	LD	A,C		;GET DIGIT COUNTER
	DEC	A		;WAS IT GOING TO GO TO ZERO (LAST DIG?)
	JR	Z,FOUTH5	;IF SO, FORCE ONE ZERO DIGIT
	LD	A,(DE)		;HAVE WE PRINTED A NON-ZERO DIGIT?
	OR	A		;SET CC'S
	JR	Z,FOUTH7	;NO, DONT PRINT THIS LEADING ZERO
	XOR	A		;GET ZERO
FOUTH5	ADD	A,30H		;MAKE NUMERIC DIGIT
	CP	':'		;IS IT A BIG HEX DIGIT? (A-F)
	JR	C,FOUTH6	;NO, DONT ADD OFFSET
	ADD	A,07H		;(A..F) ADD OFFSET
FOUTH6	LD	(DE),A		;SAVE DIGIT IN FBUFFR
	INC	DE		;BUMP POINTER
	LD	(DE),A		;SAVE HERE TO FLAG PRINTED SIG. DIG.
FOUTH7	XOR	A		;MAKE A ZERO
	DEC	C		;ALL DONE PRINTING?
	JR	Z,FOUTH8	;YES, RETURN
	DEC	B		;SEE IF HEX OR OCTAL
	INC	B		;TEST
	JR	Z,FOUTH3	;WAS OCTAL
	JR	FOUTH2		;WAS HEX

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
	JR	SQRC		;SKIP OVER THE NEXT 3 BYTES

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
	PUSH	AF		;--->
	PUSH	DE
	PUSH	BC
	CALL	FINT
	POP	BC
	POP	DE
	PUSH	AF
	CALL	FCOMP
	JP	NZ,FP301
	OR	C
	PUSH	AF
	AND	7FH
	LD	(FACHI),A
	LD	BC,9000H
	LD	DE,0000H
	CALL	FCOMP
	JP	P,FP30		;Y is an integer
	POP	AF
	JP	P,FP201
	POP	AF
	RRCA
	POP	BC
	AND	B
	POP	BC
	POP	DE
	PUSH	AF
	CALL	TODO371		;TODO: identify - called by SQR and FPWR
	POP	AF
	RLA
	LD	A,(FAC)
	INC	A		;Zero to negative power?
	DEC	A
	JP	Z,DV010		;Yes - ?/0 Error
	LD	BC,8100H	;1.0
	LD	DE,0000H
	JP	FDIV

FP201	POP	AF
	POP	AF
	POP	BC
	POP	DE
	JP	TODO371		;TODO: identify - called by SQR and FPWR

	;Y is an integer
FP30	POP	AF
	LD	(FACHI),A
	CALL	$MOVRF		;GET Y IN THE REGISTERS
FP301	POP	AF
	POP	AF		;<---
	JP	P,FP303		;NO PROBLEMS IF X IS POSITIVE
	PUSH	AF		;Save exponent
	LD	A,(FAC)		;Check the sign of power
	CP	99H
	JR	C,FP302		;Negative
	POP	AF		;Restore exponent
	JR	FP303		;Positive

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
	JR	NC,EXP110	;IF SO OVERFLOW
	CP	68H		;IF TOO SMALL ANSWER IS 1
	JR	C,EXP200
	CALL	PUSHF		;SAVE y
	CALL	FINT		;DETERMINE INTEGER POWER OF 2
	ADD	A,81H		;INTEGER WAS RETURNED IN A
				;BIAS IS $81 BECAUSE BINARY POINT
				; IS TO LEFT OF UNDERSTOOD 1
	JR	Z,EXP100	;<-->	Moved from MBASIC after POP BC : POP DE
	POP	BC
	POP	DE		;RECALL y
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

;	TODO: identify - called by SQR and FPWR
TODO371	PUSH	DE		;--->
	PUSH	BC
	CALL	QINT
	LD	HL,ARGHI	;=ARG-1 (MSB mantissa)
	LD	(HL),E
	INC	HL
	LD	(HL),D
	LD	HL,$SONE
	CALL	$MOVFM
TODO372	LD	HL,(ARGHI)	;=ARG-1 (MSB mantissa)
	LD	A,H
	OR	A
	RRA
	LD	H,A
	LD	A,L
	RRA
	LD	L,A
	LD	(ARGHI),HL	;=ARG-1 (MSB mantissa)
	JP	NC,TODO373
	POP	BC
	POP	DE
	PUSH	DE
	PUSH	BC
	CALL	FMULT
TODO373	LD	HL,ARGHI	;=ARG-1 (MSB mantissa)
	LD	A,(HL)
	INC	HL
	OR	(HL)
	JP	Z,TODO374
	POP	BC
	POP	HL
	CALL	PUSHF
	LD	D,H
	LD	E,L
	CALL	$MOVFR
	EX	DE,HL
	CALL	FMULT
	POP	BC
	POP	HL
	CALL	PUSHF
	EX	DE,HL
	CALL	$MOVFR
	JP	TODO372

TODO374	POP	BC
	POP	DE
	RET			;<---

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
	JR	POLY1		;SEE IF DONE

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
;	(Note: MBASIC has a different implementation)
RND	CALL	CINT		;--->	=$FI - L2=FRCINT
	LD	A,H
	OR	A
	JP	M,FCERR		;Illegal function call
	OR	L
	JR	Z,RNDSUB	;Generate next pseudo-random number
	PUSH	HL
	CALL	RNDSUB		;Generate next pseudo-random number
	CALL	$MOVRF
	EX	DE,HL
	EX	(SP),HL
	PUSH	BC
	CALL	CONSIH
	POP	BC
	POP	DE
	CALL	FMULT
	LD	HL,$SONE
	CALL	$FADDS
	JP	FINT

	;Generate next pseudo-random number
RNDSUB	LD	HL,RNDTMP	;TRS-80: RND temp value (3 bytes)
	PUSH	HL
	LD	DE,0000H
	LD	C,E
	LD	H,03H
	;Outer loop
RNDSL1	LD	L,08H
	;Inner loop
RNDSL2	EX	DE,HL
	ADD	HL,HL
	EX	DE,HL
	LD	A,C
	RLA
	LD	C,A
	EX	(SP),HL
	LD	A,(HL)
	RLCA
	LD	(HL),A
	EX	(SP),HL
	JR	NC,RNDSJ1
	PUSH	HL
	LD	HL,(RNDVAL)
	ADD	HL,DE
	EX	DE,HL
	LD	A,(RNDVAH)
	ADC	A,C
	LD	C,A
	POP	HL
RNDSJ1	DEC	L
	JR	NZ,RNDSL2	;Inner loop
	EX	(SP),HL
	INC	HL
	EX	(SP),HL
	DEC	H
	JR	NZ,RNDSL1	;Outer loop
	POP	HL
	LD	HL,0B065H
	ADD	HL,DE
	LD	(RNDVAL),HL
	CALL	SETSNG		;Set VALTYP=4 for SNG
	LD	A,05H
	ADC	A,C
	LD	(RNDVAH),A
	EX	DE,HL
	LD	B,80H
	LD	HL,FAC_1	;FAC+1
	LD	(HL),B
	DEC	HL
	LD	(HL),B
	LD	C,A
	LD	B,00H
	JP	NORMAL		;<---


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
	JR	C,ATAN30
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
	JR	C,NOSEC		;TOO SMALL FOR ANYTHING REASONABLE
	JR	Z,ISSEC		;"." IS VALID VAR CHAR
	CP	':'		;TOO BIG FOR NUMERIC?
	JR	NC,PTRGT3	;YES
	CP	'0'		;IN RIGHT RANGE?
	JR	NC,ISSEC	;YES, WAS NUMERIC
PTRGT3	CALL	ISLET2		;SET CARRY IF NOT ALPHABETIC
	JR	C,NOSEC		;ALLOW ALPHABETICS
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
	JR	NC,VMORC1	;YES
	CP	'0'		;IN RANGE FOR DIGIT
	JR	NC,VMORCH	;YES, VALID CHAR
VMORC1	CALL	ISLET2		;AS ARE ALPHABETICS
	JR	NC,VMORCH
	CP	'.'		;DOTS ALSO OK
	JR	Z,VMORCH	;SO EAT IT
	LD	A,B		;CHECK FOR MAXIMUM COUNT
	CP	NAMLEN-1	;LIMITED TO SIZE OF NAMBUF ONLY
	JP	NC,SNERR	;MUST BE BAD SYNTAX
	POP	BC		;GET BACK THE STORED [B,C]
	LD	(NAMCNT),A	;ALWAYS SET UP COUNT OF EXTRAS
	LD	A,(HL)		;RESTORE TERMINATING CHAR
NOSEC	CP	'%'+1		;NOT A TYPE INDICATOR
	JR	NC,TABTYP	;THEN DONT CHECK THEM
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
	JR	Z,SNFUNS	;NO FUNCTIONS SO NO SPECIAL SEARCH
	LD	HL,(PRMLEN)	;GET THE SIZE TO SEARCH
	LD	DE,PARM1	;GET THE BASE OF THE SEARCH
	ADD	HL,DE		;[H,L]= PLACE TO STOP SEARCHING
	LD	(ARYTA2),HL	;SET UP STOPPING POINT
	EX	DE,HL		;[H,L]=START [D,E]=END
	JR	LOPFND


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
	JR	NZ,NOTIT1
	LD	A,(VALTYP)	;GET TYPE WERE LOOKING FOR
	CP	L		;COMPARE WITH OUR VALTYP
	JR	NZ,NOTIT1	;NOT RIGHT KIND -- SKIP IT
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
	JR	NZ,LOPTOP	;NO, KEEP LOOKING
;-----------------------------------------------------------------------------
; ## BIPTRG.ASM:198 ##
;
	LD	A,(PRMFLG)	;HAS PARM1 BEEN SEARCHED
	OR	A
	JR	Z,SMKVAR	;IF SO, CREATE VARIABLE
	XOR	A		;FLAG PARM1 AS SEARCHED
	LD	(PRMFLG),A
SNFUNS	LD	HL,(ARYTAB)	;STOPPING POINT IS [ARYTA2]
	LD	(ARYTA2),HL
	LD	HL,(VARTAB)	;SET UP STARTING POINT
	JR	LOPFND

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
	JR	Z,VARNOT	;YES.
	LD	DE,RETVAR	;DID EVAL CALL US?
	CALL	COMPAR		;IF SO, DON'T MAKE A NEW VARIABLE
	POP	DE		;RESTORE THE POSITION
	JR	Z,FINZER	;MAKE FAC ZERO (ALL TYPES) AND SKIP RETURN
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
	JR	NZ,ZEROER	;POINTS TO THE START OF THE VARIABLE
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
	JR	NZ,ISIT11	;NO, MORE CHARS TO LOOK AT
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
	JR	NZ,POPHR2	;IF NOT, DONE
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
	JR	Z,SHTNAM	;YES, SHORT NAME
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
	JR	NZ,LPPSNM
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
	JR	NZ,LPLNAM
	LD	HL,(NAMTMP)
	JR	LNGNAM		;WAS LONG ONE

SHTNAM	CALL	INTIDX		;EVALUATE IT
	XOR	A		;MAKE SURE NAMCNT=0
	LD	(NAMCNT),A
LNGNAM	LD	A,(OPTVAL)	;SEE WHAT THE OPTION BASE IS
	OR	A
	JR	Z,OPTB0		;IF BASE 0 DO NOTHING
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
	JR	Z,DOCHRT	;DO CHRGET FOR NEXT ONE
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
	LD	DE,(STREND)	;GET THE PLACE TO STOP INTO [D,E]
	CALL	COMPAR		;STOPPING TIME?
	JR	Z,NOTFDD	;YES, COULDN'T FIND THIS ARRAY
	LD	E,(HL)		;GET VALTYP IN [E]
	INC	HL
	LD	A,(HL)		;GET FIRST CHARACTER
	INC	HL
	CP	C		;SEE IF IT MATCHES
	JR	NZ,NMARY1	;NOT THIS ONE
	LD	A,(VALTYP)	;GET TYPE OF VAR WERE LOOKING FOR
	CP	E		;SAME AS THIS ONE?
	JR	NZ,NMARY1	;NO, SKIP THIS VAR
	LD	A,(HL)		;GET SECOND CHARACTER
	CP	B		;ANOTHER MATCH?
	JR	Z,ISARY1	;MATCH, CHECK OUT REST OF NAME
NMARY1	INC	HL		;POINT TO LENGTH OF VAR
NMARY2	LD	E,(HL)		;GET VAR NAME LENGTH IN [E]
	INC	E		;ADD ONE TO GET CORRECT LENGTH
	LD	D,00H		;HIGH BYTE OF ZERO
	ADD	HL,DE		;ADD OFFSET
CNOMAT	LD	E,(HL)		;[D,E]=LENGTH
	INC	HL		;OF THE ARRAY BEING LOOKED AT
	LD	D,(HL)
	INC	HL
	JR	NZ,LOPFD1	;IF NO MATCH, SKIP THIS ONE AND TRY AGAIN
ARYEXT	LD	A,(DIMFLG)	;SEE IF CALLED BY "DIM"
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
	JR	NZ,NMARY2	;BAD NAME SIZE JUST SKIP AND SET NZ CC
	INC	HL		;POINT ONE BYTE AFTER LENGTH FIELD
	OR	A		;LENGTH ZERO?
	JR	Z,CNOMAT	;THEN FOUND, EXIT
	DEC	HL		;MOVE BACK ONE
	CALL	MATSUB		;OTHERWISE TRY TO MATCH CHARACTERS
	JR	CNOMAT		;USING COMMON SUBROUTINE


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
LOPPTA	JR	C,POPDIM
	PUSH	AF
	LD	A,(OPTVAL)	;GET THE OPTION BASE
	XOR	0BH		;MAP 0 TO 11 AND 1 TO 10
	LD	C,A		;[B,C]=DEFAULT DIMENSION
	LD	B,00H
	POP	AF
	JR	NC,NOTDIM	;DEFAULT DIMENSIONS TO TEN
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
	JR	NZ,LOPPTA	;HANDLE THE OTHER INDICES
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
	JR	NZ,ZERITA	;NO, ZERO MORE
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
	JR	C,FINNOW

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
	JR	NZ,INLPNM	;PROCESS THE REST OF THE INDICES
	LD	A,(VALTYP)	;SEE HOW BIG THE VALUES ARE
				;AND MULTIPLY BY THAT SIZE
	LD	B,H		;SAVE THE ORIGINAL VALUE FOR MULTIPLYING
	LD	C,L		;BY THREE
	ADD	HL,HL		;MULTIPLY BY TWO AT LEAST
	SUB	04H		;FOR INTEGERS AND STRINGS
				;NO MORE MULTIPLYING BY TWO
	JR	C,SMLVAL
	ADD	HL,HL		;NOW MULTIPLIED BY FOUR
	JR	Z,DONMUL	;RE-GEN CONDITION CODES
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
	JR	NZ,SLPLNG	;FOR THE COUNT AND DATA
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
	JR	Z,ISMAT2	;IF SO, ITS A MATCH
	LD	A,(DE)		;GET ANOTHER CHARACTER
	INC	DE		;SEE IF ITS THE SAME
	CP	(HL)		;MOVE FORWARD IN DEFINITION TABLE
	INC	HL		;MORE FORWARD IN STORED NAME
	JR	Z,SLPMAT	;IF MATCH KEEP GOING UNTIL END
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
	JR	EREDIT

;	EDIT command
;	(TRS-80 implementation)
EDIT	CALL	LINSPC		;GET THE ARGUMENT LINE NUMBER
	RET	NZ		;ERROR IF NOT END OF LINE
;	Enter EDIT mode after an SN error
EREDIT	POP	HL		;GET RID OF NEWSTT ADDRESS
;	Edit again (reload the line)
EDTAGN	LD	(DOT),DE	;SAVE CURRENT LINE IN DOT FOR LATER EDIT OR LIST
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
	JR	NZ,EDTCNT	;IF NOT ZERO (END OF LINE) KEEP COUNTING...
	POP	HL		;GET BACK POINTER TO LINE
	LD	B,A		;SET CURRENT LINE POSIT TO ZERO
;	Get command with repeat count (main loop)
;	Optional number of repetitions of the next subcommand.
EDTCMD	LD	D,00H		;ASSUME REPITION COUNT IS ZERO
;	Wait command keystroke
EDTWTC	CALL	SVCKBD		;GET A CHAR FROM USER
	OR	A		;IGNORE NULLS
	JR	Z,EDTWTC
	CALL	MAKUPS		;MAKE UPPER CASE COMMAND
	SUB	30H		; convert from ASCII
	JR	C,EDTDOC	;GET RID OF OFFSET
	CP	0AH		;...
	JR	NC,EDTDOC
	LD	E,A		;SAVE CHAR
	LD	A,D		;GET ACCUM REPITITION
	RLCA			;MULTIPLY BY 2
	RLCA			;BY 4
	ADD	A,D		;AND ADD TO GET 5*D
	RLCA			;*2 TO GET 10*D
	ADD	A,E		;ADD DIGIT
	LD	D,A		;SAVE BACK NEW ACCUM
	JR	EDTWTC		;GET NEXT CHAR

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
	CP	0DH-'0'		; CR:   $0D - '0' = $DD
	JP	Z,EDTENT	;ENTER: Display the line and validate the changes
	CP	' '-'0'
	JR	Z,EDTSPC	;SPACE: display next char and advance cursor
	CP	'a'-'0'		;COMMAND IN LOWER CASE?
	JR	C,EDTNLC	;Jump if not an LC letter
	SUB	20H		;TO UPPERCASE
;	Jump if not an LC letter
EDTNLC	CP	'Q'-'0'		;'Q': QUIT?
	JP	Z,EDTQQ		;IF SO, QUIT & PRINT "OK" OR RETURN TO INLIN
	CP	'L'-'0'
	JP	Z,EDTLL		;BRANCH
	CP	'S'-'0'
	JR	Z,EDTSS		;SEARCH
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
	JR	Z,EDTKK		;(SAME AS "S" BUT DELETES CHARS)
	CP	'H'-'0'		;HACK??
	JP	Z,EDTHH		;HACK OFF THE REST OF THE LINE & INSERT
	CP	'A'-'0'		;AGAIN??
	RET	NZ
;	The A subcommand lets you begin editing a line over again.
;	It restores the original line and repositions the cursor at the beginning.
	CALL	CRDO
	POP	BC		;DISPI RETURN ADDRESS
	POP	DE		;LINE NUMBER INTO [D,E]
	JP	EDTAGN		;RESTART EDITING

;	SPACE: display next char and advance cursor
EDTSPC	LD	A,(HL)		;GET CHAR FROM CURENT POSIT
	OR	A		;ARE WE AT END OF LINE?
	RET	Z		;IF SO, RETURN
	INC	B		;BUMP CURRENT POSITION
	CALL	OUTCHR		;TYPE CHARACTER
	INC	HL		;MOVE POINTER TO NEXT CHAR
	DEC	D		;TEST IF DONE WITH REPITITIONS
	JR	NZ,EDTSPC	;REPEAT
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
	JR	Z,EDTPPW
	CALL	OUTCHR		;TYPE THE CHAR
	POP	AF		;GET KILL FLAG
	PUSH	AF		;SAVE BACK
	CALL	C,EDTIDL	;DELETE THE CHAR IF K COMMAND.
	JR	C,EDTSSK	;AND DONT MOVE POINTER AS DELCHR ALREADY DID
	INC	HL
	INC	B		;INCREMENT LINE POSIT
EDTSSK	LD	A,(HL)		;ARE WE AT END ?
	CP	E		;ARE CURRENT CHAR & SEARCH
	JR	NZ,EDTSCH	;CHAR THE SAME? IF NOT, LOOK MORE
	DEC	D		;LOOK FOR N MATCHES
	JR	NZ,EDTSCH	;IF NOT 0, KEEP LOOKING
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
	JR	Z,EDTBSL	;TYPE SLASH
	CALL	OUTCHR		;TYPE CHAR WE'RE GOING TO DELETE
	CALL	EDTIDL		;DELETE CURRENT CHAR
	DEC	D		;DECREMENT DELETE COUNT
	JR	NZ,EDTDD1	;KEEP DOING IT
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
	JR	NC,EDTCCC	;NO
	CP	0AH		;IS IT LF?
	JR	Z,EDTCCC	;YES
	CP	07H		;OR BELL?
	JR	Z,EDTCCC	;OK
	CP	09H		;OR TAB?
	JR	Z,EDTCCT	;OK
	LD	A,07H		;GET BELL
	CALL	OUTDO		;SEND IT
	JR	EDTCCL

EDTCCC	LD	(HL),A		;SAVE IN MEMORY
	CALL	OUTCHR		;ECHO THE CHAR WERE USING TO REPLACE
	INC	HL		;BUMP POINTER
	INC	B		;INCREMENT POSITION WITHIN LINE
EDTCCR	DEC	D		;ARE WE DONE CHANGING?
	JR	NZ,EDTCC	;IF NOT, CHANGE SOME MORE.
	RET			;DONE

EDTCCT	LD	A,' '		;--->
	LD	(HL),A
	CALL	OUTCHR
	INC	HL
	INC	B
	LD	A,B
	AND	07H
	PUSH	DE
	CALL	NZ,EDTITB	;Insert a tab
	POP	DE
	JR	EDTCCR		;<---

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
	CP	08H		;DELETE?? (7FH in MBASIC)
	JR	Z,EDTIBS	;YES, ACT LIKE "_"
	CP	08H		;Backspace? (twice??)
	JR	Z,EDTIB1	;Do delete
	CP	0DH		;IS IT A CARRIAGE RETURN?
	JP	Z,EDTENT	;DONT INSERT, AND SIMULATE <CR>
	CP	1BH		;IS IT ESCAPE?
	RET	Z		;IF SO, DONE.
	CP	0AH		;LINE FEED?
	JR	Z,EDTIIN	;ALLOW IT
	CP	07H		;BELL?
	JR	Z,EDTIIN	;ALLOW IT
	CP	09H		;TAB?
	JR	Z,EDTIIT	;ALLOW IT
	CP	' '		;IS IT ILLEGAL CHAR
	JR	C,EDTII		;TOO SMALL
	JR	EDTIIN		;IF NOT, JUMP AROUND NEXT CODE

;	Backspace in insert mode
EDTIBS	LD	A,08H
;	Backspace in insert mode (never used)
EDTIB1	DEC	B		;ARE WE AT START OF LINE?
	INC	B		;LETS SEE
	JR	Z,EDTSCR	;IF SO, TYPE DING.
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
	JR	EDTID1		;KEEP CRUNCHING

;	Insert a tab in insert mode
EDTIIT	CALL	EDTITB		;---> Insert a tab
	JP	EDTII		;INSERT: enter insert mode

;	Insert a tab
EDTITB	LD	A,' '
	CALL	EDTINS		;Insert a char in buffer if line not too long
	RET	C
	LD	A,B
	AND	07H
	JR	NZ,EDTITB	;Insert a tab
	RET

;	Insert a char in buffer in insert mode
EDTIIN	CALL	EDTINS		;Insert a char in buffer if line not too long
	JR	EDTII		;<--- INSERT: enter insert mode

;	Insert a char in buffer if line not too long
EDTINS	PUSH	AF		;SAVE THE CHAR TO BE INSERTED
	LD	A,C		;GET LENGTH OF LINE
;	If an attempt is made to insert a character that will make the line longer than
;	249 characters, a bell (Control-G) is typed and the character is not printed.
	CP	249		;SEE IF WE ARENT TRYING TO MAKE LINE TOO LONG
	JR	C,EDTIN1	;IF LENGTH OK, GO INSERT
	POP	AF		;GET THE UNLAWFUL CHAR
EDTSCR	SCF			;--->
	RET			;<---

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
	OR	A		;---> AND GO GET MORE CHARS
	RET			;<---

;	BACK: Do a backspace
EDTBKS	LD	A,B		;ARE WE MOVING BACK PAST THE
	OR	A		;FIRST CHARACTER
	RET	Z		;DON'T ALLOW IT
	DEC	HL		;MOVE CHAR POINTER BACK
	LD	A,08H
	CALL	OUTCHR		;ECHO IT
	DEC	B		;CHANGE CURRENT POSITION
	DEC	D		;ARE WE DONE MOVING BACK?
	JR	NZ,EDTBKS	;IF NOT, GO BACK MORE
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
	JR	INIUS		;DONT POP OFF OR LOOK AT USFLG

REUSST	LD	A,(SARYFL)	;DID WE PRINT OUT A VALUE LAST SCAN?
	OR	A		;SET CC'S
	JR	Z,FCERR3	;NO, GIVE ERROR
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
	INC	B		;---> Is the using string null?
	DEC	B		;<--- ;(Bug fix? Is [A] null?)
FCERR3	JP	Z,FCERR		;IF SO, "ILLEGAL FUNCTION CALL"
	INC	HL		;[H,L]=POINTER AT THE "USING" STRING'S
	LD	A,(HL)		;DATA
	INC	HL
	LD	H,(HL)
	LD	L,A
	JR	PRCCHR		;GO INTO THE LOOP TO SCAN
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
	JR	NZ,NOSTRF	;IF NOT, ITS NOT A STRING FIELD
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
	JR	Z,NUMNUM	;GO SCAN IT
	CP	'&'		;SEE IF ITS A VARIABLE LENGTH STRING FIELD
	JP	Z,VARSTR	;GO PRINT ENTIRE STRING
	DEC	B		;ALL THE OTHER POSSIBILITIES
				;REQUIRE AT LEAST 2 CHARACTERS
	JP	Z,REUSIN	;IF THE VALUE LIST IS NOT EXHAUSTED
				;GO REUSE "USING" STRING
	CP	'+'		;A LEADING "+" ?
	LD	A,08H		;SETUP [D] WITH THE PLUS-FLAG ON IN
	JR	Z,PLSFIN	;CASE A NUMERIC FIELD STARTS
	DEC	HL		;POINTER HAS ALREADY BEEN INCREMENTED
	LD	A,(HL)		;GET BACK THE CURRENT CHARACTER
	INC	HL		;REINCREMENT THE POINTER
	CP	'.'		;NUMERIC FIELD WITH TRAILING DIGITS
	JR	Z,DOTNUM	;IF SO GO SCAN WITH [E]=
				;NUMBER OF DIGITS BEFORE THE "."=0
	CP	'_'		;CHECK FOR LITERAL CHARACTER DECLARATION
	JP	Z,LITCHR
	CP	'\'		;CHECK FOR A BIG STRING FIELD STARTER
	JR	Z,BGSTRF	;GO SEE IF IT REALLY IS A STRING FIELD
	CP	(HL)		;SEE IF THE NEXT CHARACTER MATCHES THE
				;CURRENT ONE
	JR	NZ,NEWUCH	;IF NOT, CAN'T HAVE $$ OR ** SO ALL THE
				;POSSIBILITIES ARE EXHAUSTED
	CP	'$'		;IS IT $$ ?
	JR	Z,DOLRNM	;GO SET UP THE FLAG BIT
	CP	'*'		;IS IT ** ?
	JR	NZ,NEWUCH	;IF NOT, ITS NOT PART
				;OF A FIELD SINCE ALL THE POSSIBILITIES
				;HAVE BEEN TRIED
	INC	HL		;CHECK FOR $
	LD	A,B		;SEE IF THE "USING" STRING IS LONG
	CP	02H		; ENOUGH FOR THE SPECIAL CASE OF
	JR	C,NOTSPC	; **$
	LD	A,(HL)
	CP	'$'		;IS THE NEXT CHARACTER $ ?
NOTSPC	LD	A,' '		;SET THE ASTERISK BIT
	JR	NZ,SPCNUM	;IF IT NOT THE SPECIAL CASE, DON'T
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
	JR	Z,ENDNUS	;IF NOT, WE ARE DONE SCANNING THIS
				;NUMERIC FIELD
	LD	A,(HL)		;GET THE NEW CHARACTER
	INC	HL		;ADVANCE THE POINTER AT THE "USING" STRING DATA
	CP	'.'		;DO WE HAVE TRAILING DIGITS?
	JR	Z,AFTDOT	;IF SO, USE SPECIAL SCAN LOOP
	CP	'#'		;MORE LEADING DIGITS ?
	JR	Z,NUMNUM	;INCREMENT THE COUNT AND KEEP SCANNING
	CP	','		;DOES HE WANT A COMMA
				;EVERY THREE DIGITS?
	JR	NZ,FINNUM	;NO MORE LEADING DIGITS, CHECK FOR ^^^
	LD	A,D		;TURN ON THE COMMA BIT
	OR	40H
	LD	D,A
	JR	NUMNUM		;GO SCAN SOME MORE

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
	JR	Z,ENDNUS	;CHARACTERS, AND IF NOT, STOP SCANNING
	LD	A,(HL)		;GET THE NEXT CHARACTER
	INC	HL
	CP	'#'		;MORE DIGITS AFTER THE DECIMAL POINT?
	JR	Z,AFTDOT	;IF SO, INCREMENT THE COUNT AND KEEP
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
	JR	NZ,ENDNUM	;ALL DONE WITH THE FIELD IF SO
				;IF THERE IS A LEADING PLUS
	DEC	E		;NO LEADING PLUS SO DON'T INCREMENT THE
				;NUMBER OF DIGITS BEFORE THE DECIMAL POINT
	LD	A,B
	OR	A		;SEE IF THERE ARE MORE CHARACTERS
	JR	Z,ENDNUM	;IF NOT, STOP SCANNING
	LD	A,(HL)		;GET THE CURRENT CHARACTER
	SUB	'-'		;TRAIL MINUS?
	JR	Z,SGNTRL	;SET THE TRAILING SIGN FLAG
	CP	'+' - '-'	;A TRAILING PLUS?
	JR	NZ,ENDNUM	;IF NOT, WE ARE DONE SCANNING
	LD	A,08H		;TURN ON THE POSITIVE="+" FLAG
SGNTRL	ADD	A,04H		;TURN ON THE TRAILING SIGN FLAG
	ADD	A,D		;INCLUDE WITH OLD FLAGS
	LD	D,A
	DEC	B		;DECREMENT THE "USING" STRING CHARACTER
				;COUNT TO ACCOUNT FOR THE TRAILING SIGN
ENDNUM	POP	HL		;[H,L]=THE OLD TEXT POINTER
	POP	AF		;POP OFF FLAG THAT SAYS WHETHER THERE
				;ARE MORE VALUES IN THE VALUE LIST
	JR	Z,FLDFIN	;IF NOT, WE ARE DONE WITH THE "PRINT"
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
	JR	Z,CRDNUS	;IF IT WAS A END-OF-STATEMENT
				;FLAG THAT THE VALUE LIST ENDED
				;AND THAT  CRLF SHOULD BE PRINTED
	LD	(SARYFL),A	;FLAG THAT VALUE HAS BEEN PRINTED.
				;DOESNT MATTER IF ZERO SET, [A]
				;MUST BE NON-ZERO OTHERWISE
	CP	';'		;A SEMI-COLON?
	JR	Z,SEMUSN	;A LEGAL DELIMITER
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
	JR	FINUSI		;SEE IF THERE ARE MORE VALUES

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
	JR	CHKUSI		;GO SEE IF USING STRING ENDED

;
;	HERE TO HANDLE VARIABLE LENGTH STRING FIELD SPECIFIED WITH "&"
;
VARSTR	LD	C,00H		;SET LENGTH TO 0 TO FLAG VARIABLE LENGTH
	JR	ISSTR1

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
	JR	Z,FLDFIN	;IF THERE ARE NO MORE VALUES
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
	JR	UPRTSP		;AND LOOP PRINTING THEM

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
	IF	GFX
	LD	A,(GPRFLG)	;===>	PRINT#-3 FLAG
	OR	A		;	IS IT SET?
	JP	NZ,GPRINT	;<===	YES, DO PRINT#-3 ON GRAPHICS DISPLAY
	ENDIF
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
	JR	NZ,OUTDO2	;NO
	LD	A,(LPTPOS)	;GET LPT POS
	SUB	01H		;SUBTRACT ONE FROM PRINTER POSIT
	JR	C,OUTDO1
	LD	(LPTPOS),A
OUTDO1	POP	AF		;GET BACK BACKSPACE
	JR	OUTDO9		;SEND CHAR
OUTDO2	CP	09H		;TAB
	JR	NZ,OUTDO5	;NO
OUTDO3	LD	A,' '
	CALL	OUTDO
	LD	A,(LPTPOS)	;GET CURRENT PRINT POSIT
	CP	0FFH		;--->
	JR	Z,OUTDO4	;<---
	AND	07H		;AT TAB STOP?
	JR	NZ,OUTDO3	;GO BACK IF MORE TO PRINT
OUTDO4	POP	AF		;POP OFF CHAR
	RET			;RETURN

OUTDO5	POP	AF		;GET CHAR BACK
	PUSH	AF		;SAVE AGAIN
	SUB	0DH		;IF FUNNY CONTROL CHAR, (LF) DO NOTHING
	JR	Z,OUTDO7
	JR	C,OUTDO8	;JUST PRINT CHAR
	LD	A,(LPTSIZ)	;GET SIZE OF PRINTER
	INC	A		;IS IT INFINITE?
	LD	A,(LPTPOS)	;GET POSIT
	JR	Z,OUTDO6	;THEN DONT FOLD
				;If 'WIDTH' is 255, the line width
				; is "infinite" (no CRLF)
	PUSH	HL		;SAVE [H,L]
	LD	HL,LPTSIZ	;Value for 'WIDTH' on printer output.
	CP	(HL)		;MAX size reached ?
	POP	HL
	CALL	Z,OUTDOB	;THEN DO CRLF
OUTDO6	CP	0FFH		;MAX LENGTH?
	JR	Z,OUTDO8	;THEN JUST PRINT
	INC	A		;INCREMENT POSIT
OUTDO7	LD	(LPTPOS),A	;SAVE POSIT
;	Output character to printer
OUTDO8	POP	AF		;GET CHAR BACK
OUTDO9	JP	LPTCHR		;<-->

;	aka FINLPT
OUTDOA	XOR	A		;	RESET PRINT FLAG SO
	LD	(PRTFLG),A	;	OUTPUT GOES TO TERMINAL
	RET			;<--> RETURN

OUTDOB	LD	A,0DH		;PUT OUT CRLF
	CALL	OUTDO9
	LD	A,0AH
	CALL	OUTDO9
	XOR	A		;ZERO LPTPOS
	LD	(LPTPOS),A	;DONE
	RET


;-----------------------------------------------------------------------------
;	TRS-80: output to console
OUTCON	LD	A,(CNTOFL)
	OR	A
	JP	NZ,PPSWRT	;NO, DO OUTPUT
	POP	AF		;GET THE CHARACTER
	PUSH	BC
	PUSH	AF		;AND SAVE IT AGAIN
	CP	09H		;--->	OUTPUTTING TAB?
	JR	NZ,OCNTAB	;	NO.
	LD	A,(DOTABP?)	;	(?) TAB position for console
	OR	A
	JR	NZ,OCTAB	;	Do TAB using spaces
	LD	A,09H
	JR	OCNTAB		;<---

;	Do TAB using spaces
OCTAB	LD	A,' '		;GET SPACE CHAR
	CALL	OUTDO		;CALL OUTCHR RECURSIVELY (!)
	LD	A,(TTYPOS)	;GET CURRENT PRINT POS.
	CP	0FFH		;--->
	JR	Z,OCXTAB	;<---	Done TAB
	AND	07H		;AT TAB STOP YET??
	JR	NZ,OCTAB	;NO, KEEP SPACING
;	Done TAB
OCXTAB	POP	AF		;RESTORE CURRENT CHAR (TAB)
	POP	BC		;GET [B,C] BACK
	RET			;ALL DONE

OCNTAB	CP	' '		;IS THIS A MEANINGFUL CHARACTER?
	JR	C,OCEOL		;IF IT'S A NON-PRINTING CHARACTER
	LD	A,(TTYSIZ)	;[B]=LINE LENGTH  (DON'T INCLUDE IT IN TTYPOS)
	LD	B,A		;DON'T INCLUDE IT IN TTYPOS
	LD	A,(TTYPOS)	;SEE IF PRINT HEAD IS AT THE END OF THE LINE
	INC	B		;IS WIDTH 255?
	JR	Z,OCNEOL	;YES, JUST INC TTYPOS
	DEC	B		;CORRECT [B]
	CP	B
	CALL	Z,CRDO		;TYPE CRLF AND SET TTYPOS AND [A]=0 IF SO
;	Jump if no EOL
OCNEOL	CP	0FFH		;HAVE WE HIT MAX #?
	JR	Z,OCEOL		;THEN LEAVE IT THERE
	INC	A		;INCREMENT TTYPOS SINCE WE'RE GOING TO PRINT A CHARACTER.
	LD	(TTYPOS),A	;STORE NEW PRINT HEAD POSITION
;	Jump if got EOL
OCEOL	POP	AF		;GET CHAR OFF STACK
	POP	BC		;RESTORE [B,C]
	PUSH	AF		;SAVE PSW BACK
	POP	AF
	CALL	SVCDSP		;--->
	PUSH	AF
	PUSH	BC
	LD	B,A
	LD	A,(DOTABP?)	;	(?) TAB position for console
	OR	B
	JP	NZ,OCNDCB	;	Jump to skip DEC B (tab position)
	DEC	B
OCNDCB	LD	A,B
	LD	(DOTABP?),A	;<---	(?) TAB position for console
	POP	BC
	POP	AF		;RESTORE CHAR
	RET			;RETURN FROM OUTCHR

LDREOF	PUSH	BC		;<-->	SAVE ALL REGISTERS
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
	JR	Z,NORUNC
	LD	HL,NFILES	;--->	Number of files
	LD	A,(HL)
	LD	(NFILSSV),A	;	temp save NFILES
	LD	(HL),00H
	CALL	RUNC		;	RUN IT
	LD	A,(NFILSSV)	;	temp save NFILES
	LD	(NFILES),A	;	Number of files
	CALL	UPPROF		;	Update PROFLG protection flag
	JP	NEWSTT		;<---

; 	a.k.a. INCPRMT
NORUNC	PUSH	HL		;PRESERVE REGISTERS
	PUSH	BC
	PUSH	DE
	LD	HL,REDDY	;PRINT PROMPT "Ready"
	CALL	STROUT
	POP	DE
	POP	BC
	XOR	A
	POP	HL
	CALL	UPPROF		;<-->	Update PROFLG protection flag
	RET


;-----------------------------------------------------------------------------
; ## GIO86.ASM:1012 ##
;	CRDONZ - output carriage return if file PTRFIL is not at left margin
CRDONZ	LD	A,(TTYPOS)	;GET CURRENT TTYPOS
	OR	A		;SET CC'S
	RET	Z		;IF ALREADY ZERO, RETURN
	JR	CRDO		;DO CR

FININL	LD	(HL),00H	;PUT A ZERO AT THE END OF BUF
	LD	HL,BUFMIN	;SETUP POINTER

;-----------------------------------------------------------------------------
;	CRDO - output ASCII carriage return to current file
;	and return A=0
CRDO	LD	A,0DH
	CALL	OUTDO		;output Carriage Return
; ## GIO86.ASM:1007 ##
;	DON'T PUT CR OUT TO LOAD FILE
CRFIN	CALL	ISFLIO		;SEE IF OUTPUTTING TO DISK
	JR	Z,BEGLIN	;NOT DISK FILE, CONTINUE
	XOR	A		;CRFIN MUST ALWAYS RETURN WITH A=0
	RET			;AND CARRY=0.

;	Set BOL *DO or *PR
;TODO:	BEGLIN vs. CRCONT
BEGLIN	LD	A,(PRTFLG)	;GOING TO PRINTER?
	OR	A		;TEST
	JR	Z,BEGLDO	;NO
	XOR	A		;DONE, RETURN
	LD	(LPTPOS),A	;ZERO POSITION
	RET

;-----------------------------------------------------------------------------
;	Set BOL *DO
;TODO: BEGLDO vs. NTPRTR
BEGLDO	XOR	A		;SET TTYPOS=0
	LD	(TTYPOS),A
	XOR	A		;SOME ROUTINES DEPEND ON CRDO
	RET			;AND CRFIN RETURNING [A]=0 AND Z TRUE

;-----------------------------------------------------------------------------
;	TRS-80: output character to current file/device
;	Special handling of char LF (0AH): output a CR (0DH) instead
OUTCHR	CP	0AH		;IS IT A LF?
	JP	NZ,OUTDO	;NO, OUTPUT THE CHAR
	LD	A,(PRTFLG)	;IF TO PRINTER, OUTPUT A CR INSTEAD
	OR	A
	JR	NZ,OUTCR
	PUSH	HL
	LD	HL,(PTRFIL)	;IF TO FILE, OUTPUT A CR INSTEAD
	LD	A,H
	OR	L
	POP	HL
	JR	Z,OUTCR
	LD	A,0AH		;ELSE OUTPUT A LF
	CALL	OUTDO
	RET

;-----------------------------------------------------------------------------
;	TRS-80: output New Line to current file/device
;	and return A=LF (0AH)
;	(only used by OUTCHR)
OUTCR	LD	A,0DH		;DO CR
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
	JR	BLTUC1		;LOOP UNTIL ALL BYTES MOVED


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
	JR	C,OMERR		;IN CASE [H,L] WAS TOO BIG(MBM 3/18**)
				;NOW SEE IF SP IS LARGER
	ADD	HL,SP		;IF SO, CARRY WILL BE SET
	POP	HL		;GET BACK ORIGINAL [H,L]
	RET	C		;WAS OK?
;	OMERR fixes program links (starting at TXTTAB), resets SAVSTK to TOPMEM-2
;	 and issues an Out-of-Memory error message
OMERR	CALL	LINKER		;<-->
	LD	HL,(FILTAB)	;ELIMINATE ALL STACK CONTEXT TO FREE
	DEC	HL		; UP SOME MEMORY SPACE
	DEC	HL		;MAKE SURE THE FNDFOR STOPPER IS SAVED
	LD	(SAVSTK),HL	;PLACE STACK IS RESTORED FROM
	LD	DE,ERROM	;"OUT OF MEMORY"
	JP	ERROR

REASON	CALL	REASON1		;ENOUGH SPACE BETWEEN STRING & STACK
	RET	NC		;YES
	LD	A,(CHNFLG)	;This extra check was not present
	OR	A		;on CP/M 5.20, nor on MSX 5.22
	JR	NZ,OMERR	;Can't garbage collect if CHAINing
	PUSH	BC		;SAVE ALL REGS
	PUSH	DE
	PUSH	HL
	CALL	GARBA2		;DO A GARBAGE COLLECTION
	POP	HL		;RESTORE ALL REGS
	POP	DE
	POP	BC
	CALL	REASON1		;ENOUGH SPACE THIS TIME?
	RET	NC		;YES
	JR	OMERR		;NO, GIVE "OUT OF MEMORY"

REASON1	PUSH	DE		;SAVE [D,E]
	EX	DE,HL		;SAVE [H,L] IN [D,E]
	LD	HL,(FRETOP)	;GET WHERE STRINGS ARE
	CALL	COMPAR		;IS TOP OF VARS LESS THAN STRINGS?
	EX	DE,HL		;BACK TO [D,E]
	POP	DE		;RESTORE [D,E]
	RET			;DONE

;-----------------------------------------------------------------------------
;	TRS-80: Clear Files Table
;
;	THE CODE BELOW SETS THE FILE MODE TO 0 (CLOSED) FOR ALL FCB'S
ZRDSKS	LD	A,(NFILES)	;GET LARGEST FILE #
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
	RET

;	Clear Files Table, close all files, NEW
NODSKS	CALL	ZRDSKS
	CALL	CLSALL		;Close all files (?)
	XOR	A
	JR	SCRTCH


;-----------------------------------------------------------------------------
;	ERROR HANDLER, READY, COMPACTIFICATION, NEW, CLEAR, MAIN
; ## BIMISC.ASM ##
;
;
;	The "NEW" command clears the program text as well
;	as variable space
;
SCRATH	RET	NZ		;MAKE SURE THERE IS A TERMINATOR
	CALL	CLS		;<-->
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
	JR	NZ,CLEARC1	;LEAVE DEFAULT TABLE ALONE
	XOR	A
	LD	(OPTFLG),A	;INDICATE NO "OPTION" HAS BEEN SEEN
	LD	(OPTVAL),A	;DEFAULT TO "OPTION BASE 0"
	LD	B,1AH		;INITIALIZE THE DEFAULT VALTYPE TABLE
	LD	HL,DEFTBL	;POINT AT THE FIRST ENTRY
LDSNGAZ	LD	(HL),04H	;LOOP 26 TIMES STORING A DEFAULT VALTYP
	INC	HL		;FOR SINGLE PRECISION
	DEC	B		;COUNT OFF THE LETTERS
	JP	NZ,LDSNGAZ	;LOOP BACK, AND SETUP THE REST OF THE TABLE
CLEARC1	XOR	A
	LD	(ONEFLG),A	;RESET ON ERROR FLAG FOR RUNS
	LD	L,A		;RESET ERROR LINE NUMBER
	LD	H,A		;BY SETTING ONELIN=0.
	LD	(ONELIN),HL	;CLEAR ERROR LINE NUMBER
	LD	(OLDTXT),HL	;MAKE CONTINUING IMPOSSIBLE
	LD	HL,(MEMSIZ)
	LD	A,(CHNFLG)	;ARE WE CHAINING?
	OR	A		;TEST
	JR	NZ,CLEARC2	;FRETOP IS GOOD, LEAVE IT ALONE
	LD	(FRETOP),HL	;FREE UP STRING SPACE
CLEARC2	XOR	A		;MAKE SURE [A] IS ZERO, CC'S SET
	CALL	RESTOR		;RESTORE DATA
	LD	HL,(VARTAB)	;GET START OF VARIABLE SPACE
	LD	(ARYTAB),HL	;SAVE IN START OF ARRAY SPACE
	LD	(STREND),HL	;AND END OF VARIABLE STORAGE
	XOR	A		;--->
	LD	(DOSERR),A	;<---
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
	JR	NZ,JSNERR	;GIVE ERROR IF CHARS DONT MATCH
	INC	HL		;DUPLICATION OF CHRGET RST FOR SPEED
	EX	(SP),HL		;SEE CHRGET RST FOR EXPLANATION
	INC	HL
	LD	A,(HL)		;GET IT
	CP	':'		;IS IT END OF STATEMENT OR BIGGER
	RET	NC		;DONE IF YES
	JP	CHRCON		;REST OF CHRGET

JSNERR	JP	SNERR		;IFE CYB


;-----------------------------------------------------------------------------
;	RESTORE, STOP, END, LINGET, CHRCON
; ## BIMISC.ASM:553 ##
;
RESTOR	EX	DE,HL		;SAVE [H,L] IN [D,E]
	LD	HL,(TXTTAB)
	JR	Z,RESTOR1	;RESTORE DATA POINTER TO BEGINNING OF PROGRAM
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
	JR	STPEND

ENDST	RET	NZ		;MAKE SURE "END" STATEMENTS HAVE A TERMINATOR
	XOR	A		;--->	clear ONEFLG to indicate that we aren't
	LD	(ONEFLG),A	;<---	within an error-trapping routine
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
	JR	Z,DIRIS		;IF NOT SET UP FOR CONTINUE
	LD	(OLDLIN),HL	;SAVE OLD LINE #
	LD	HL,(SAVTXT)	;GET POINTER TO START OF STATEMENT
	LD	(OLDTXT),HL	;SAVE IT
	IF	GFX
	CALL	TOTEXT		;<==>	Set text mode
	ENDIF
DIRIS	XOR	A
	LD	(CNTOFL),A	;FORCE OUTPUT
	CALL	OUTDOA
	CALL	CRDONZ		;PRINT CR IF TTYPOS .NE. 0
	POP	AF		;GET BACK ^C FLAG
	LD	HL,BRKTXT	;"BREAK"
	JP	NZ,ERRFIN	;CALL STROUT AND FALL INTO READY
	JP	STPRDY		;POP OFF LINE NUMBER & FALL INTO READY

;	Unused... leftover from MBASIC
CTROPT	LD	A,0FH		;PRINT AN ^O.
KILIN	PUSH	AF		;SAVE CURRENT CHAR
	SUB	03H		;CONTROL-C?
	JR	NZ,NTCTCT	;NO
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
	LD	DE,(OLDLIN)
	LD	(CURLIN),DE	;SET UP OLD LINE # AS CURRENT LINE #
	RET

;	no 'NULL' BASIC command

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
	JR	NZ,GFCERR	;IF ITS CHANGED, ERROR
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
	JR	NZ,GFCERR	;PTRGET DID NOT FIND VARIABLE!
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
	JR	NZ,ERASE12	;MOVE THE REST
	DEC	BC
	LD	H,B		;SETUP THE NEW STORAGE END POINTER
	LD	L,C
	LD	(STREND),HL
	POP	HL		;GET BACK THE TEXT POINTER
	LD	A,(HL)		;SEE IF MORE ERASURES NEEDED
	CP	','		;ADDITIONAL VARIABLES DELIMITED BY COMMA
	RET	NZ		;ALL DONE IF NOT
	CALL	CHRGTR
	JR	ERASE

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
CLEAR	PUSH	AF		;--->	Since FILTAB is reset below,
	CALL	CLSALL		;	close all files - even file 0,
	POP	AF		;<---	because of DYNAMIC FDB's
	JR	Z,GCLERC	;IF NO FORMULA JUST CLEAR
	CP	','		;ALLOW NO STRING SPACE
	JR	Z,CSKPCM
	CALL	INTID2		;Get Dummy Integer Parameter into [D,E]
	DEC	HL
	CALL	CHRGTR		;SEE IF ITS THE END
	JR	Z,GCLERC
CSKPCM	CALL	SYNCHR
	DB	','
	JR	Z,GCLERC
	LD	DE,(FILTAB)	;Use current top of memory as default
	CP	','		;SHOULD FINISH THERE
	JR	Z,CLEARS
	CALL	GETMPM		;GET MEMORY SIZE PARAMETER
	PUSH	HL		;--->
	LD	HL,(MAXMEM)	;	[HL]=highest byte available to BASIC
	CALL	COMPAR
	JP	C,FCERR		;	branch if user tried to get more
	POP	HL		;<---
CLEARS	DEC	HL		;BACK UP
	CALL	CHRGTR		;GET CHAR
	PUSH	DE		;SAVE NEW HIGH MEM
	JR	Z,CDFSTK	;USE SAME STACK SIZE
	CALL	SYNCHR
	DB	','
	JR	Z,CDFSTK
	CALL	GETMPM		;GET STACK SIZE PARAMETER
	DEC	HL
	CALL	CHRGTR
	JP	NZ,SNERR
CLEART	EX	(SP),HL		;SAVE TEXT POINTER
	PUSH	HL		;SAVE CANDIDATE FOR TOPMEM
				;(2*NUMLEV)+20 = 004EH
	LD	HL,NUMLEV*2+20	;CHECK STACK SIZE IS REASONABLE
	CALL	COMPAR
	JR	NC,GOMERR
	POP	HL		;[HL]=candidate for TOPMEM
	CALL	CKDOSB		;--->
	JR	NC,GOMERR	;<---
	CALL	SUBDE		;DE=HL-DE=High Ram - Stack Size=new stack bottom
	JR	C,GOMERR	;WANTED MORE THAN TOTAL!
	PUSH	HL		;SAVE STACK BOTTOM
	LD	HL,(VARTAB)	;TOP LOCATION IN USE
	LD	BC,0014H	;LEAVE BREATHING ROOM
	ADD	HL,BC
	CALL	COMPAR		;ROOM?
	JR	NC,GOMERR	;NO, DON'T EVEN CLEAR
	EX	DE,HL		;NEW STACK BASE IN [H,L]
	DEC	HL		;<-->
	LD	(MEMSIZ),HL	;SET UP NEW STACK BOTTOM
	POP	HL		;HL=Highest Ram available to BASIC
	LD	(FILTAB),HL	;SAVE IT, IT MUST BE OK
	CALL	INIFDB		;--->
	POP	DE
	POP	BC
	LD	HL,(FILTAB)	;points to 1st FDB (=STKLOW if no FDB's)
	LD	SP,HL
	PUSH	BC
	PUSH	DE
	CALL	ZRDSKS		;<---
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
	INC	HL		;<-->
	LD	A,E		;SUB DX,STKLOW
	SUB	L
	LD	E,A
	LD	A,D
	SBC	A,H
	LD	D,A
	POP	HL
	JR	CLEART

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
	JR	NZ,NXTDO	;NO, CONTINUE "NEXT"
	LD	HL,FVALSV	;FETCH THE INITIAL VALUE INTO THE FAC
	CALL	$MOVFM
	XOR	A		;CONTINUE THE "NEXT" WITH INITIAL VALUE
NXTDO	JP	Z,NXTDOA	;(Why not a CALL NZ,...?)
	CALL	$FADDS
NXTDOA	POP	HL		;POP PTR TO LOOP VARIABLE
	CALL	$MOVMF		;WILL MOVE FAC TO THERE
	POP	HL		;GET ENTRY PTR.
	CALL	$MOVRM		;GET THE FINAL INTO THE REGISTERS
	PUSH	HL		;SAVE ENTRY PTR.
	CALL	FCOMP		;COMPARE THE NOS. RETURNING
				; 377 IF FAC LESS THAN REGS,0 IF =
				; AND 1 IF GREATER
	JR	FINNXT		;SKIP INTEGER CODE

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
	JR	NZ,INXTDO	;NO, JUST CONTINUE
	LD	HL,(FVALSV)	;GET THE INITIAL VALUE
	JR	IFORIN		;CONTINUE FIRST ITERATION CHECK

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
	JR	Z,LOOPDN	;JUMP IF DONE
	EX	DE,HL
	LD	(CURLIN),HL	;STORE LINE NO.
	LD	L,C		;SETUP THE TEXT PTR
	LD	H,B
	JP	NXTCON

LOOPDN	LD	SP,HL		;ELIMINATE FOR ENTRY & SAVE UPDATED
	LD	(SAVSTK),HL	; STACK
	EX	DE,HL		;<-->
	LD	HL,(TEMP)	;RESTORE TEXT PTR
	LD	A,(HL)
	CP	','		;COMMA?
	JP	NZ,NEWSTT	;"NEXT
	LD	(NXTFLG),A	;<--> Set NXTFLG non-zero since NEXT couldn't
				; have been called by FOR in this case
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
	JR	Z,CSLOOP	;IF BOTH THE SAME, MUST BE MORE TO STRINGS
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
	JR	STRC		;JUMP INTO STR$ CODE

;	HEX$ (STRH$) same as OCT$ (STRO$) except uses hex instead of octal
HEX$	CALL	FOUTH1		;PUT HEX NUMBER IN FBUFFR
	JR	STRC		;JUMP INTO STR$ CODE

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
	JR	Z,STRFIN	;TEST
	CP	D
	JR	Z,STRFIN
	CP	B		;CLOSING QUOTE
	JR	NZ,STRGET	;NO, GO BACK FOR MORE
STRFIN	CP	'"'		;IF QUOTE TERMINATES THE STRING
	CALL	Z,CHRGTR	;SKIP OVER THE QUOTE
	PUSH	HL		;SAVE POINTER AT END OF STRING
	LD	A,B		;WERE WE SCANNING AN UNQUOTED STRING?
	CP	','
	JR	NZ,NTTRLS	;IF NOT, DON'T SUPPRESS TRAILING SPACES
	INC	C		;FIX [C] WHICH IS THE CHARACTER COUNT
LPTRLS	DEC	C		;DECREMENT UNTIL WE FIND A NON-SPACE CHARACTER
	JR	Z,NTTRLS	;DON'T GO PAST START (ALL SPACES)
	DEC	HL		;LOOK AT PREVIOUS CHARACTER
	LD	A,(HL)
	CP	' '
	JR	Z,LPTRLS	;IF SO CONTINUE LOOKING
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
	JR	STRPR2		;AND PRINT IT...


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
	JR	C,GARBAG	;NOT ENOUGH ROOM FOR STRING, OFFAL TIME
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
TVAR	LD	DE,(TEMPPT)	;SEE IF DONE
	CALL	COMPAR		;TEST
	LD	BC,TVAR		;FORCE JUMP TO TVAR
	JP	NZ,DVAR2	;DO TEMP VAR GARBAGE COLLECT
	LD	HL,PRMPRV	;SETUP ITERATION FOR PARAMETER BLOCKS
	LD	(TEMP9),HL
	LD	HL,(ARYTAB)	;GET STOPPING POINT IN [H,L]
	LD	(ARYTA2),HL	;STORE IN STOP LOCATION
	LD	HL,(VARTAB)	;GET STARTING POINT IN [H,L]
SVAR	LD	DE,(ARYTA2)	;GET STOPPING LOCATION
	CALL	COMPAR		;SEE IF AT END OF SIMPS
	JR	Z,ARYVAR
	LD	A,(HL)		;GET VALTYP
	INC	HL		;BUMP POINTER TWICE
	INC	HL		;
	INC	HL		;POINT AT THE VALUE
	PUSH	AF		;SAVE VALTYP
	CALL	IADAHL		;AND SKIP OVER EXTRA CHARACTERS AND COUNT
	POP	AF
	CP	03H		;SEE IF ITS A STRING
	JR	NZ,SKPVAR	;IF NOT, JUST SKIP AROUND IT
	CALL	DVARS		;COLLECT IT
	XOR	A		;AND DON'T SKIP ANYTHING MORE
SKPVAR	LD	E,A
	LD	D,00H		;[D,E]=AMOUNT TO SKIP
	ADD	HL,DE
	JR	SVAR		;GET NEXT ONE

ARYVAR	LD	HL,(TEMP9)	;GET LINK IN PARAMETER BLOCK CHAIN
	LD	E,(HL)		;GO BACK ONE LEVEL
	INC	HL
	LD	D,(HL)
	LD	A,D
	OR	E		;WAS THAT THE END?
	LD	HL,(ARYTAB)	;SETUP TO START ARRAYS
	JR	Z,ARYVAR?	;OTHERWISE GARBAGE COLLECT ARRAYS
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
	JR	SVAR

ARYVA2	POP	BC		;GET RID OF STACK GARBAGE
;	TODO: Duplicate ARYVAR ??
ARYVAR?	LD	DE,(STREND)	;GET END OF ARRAYS
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
	JR	NZ,ARYVA2	;IF NOT JUST SKIP IT
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
	JR	Z,ARYVAR?	;END OF ARRAY, TRY NEXT ARRAY
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
	JR	MOVLP		;KEEP DOING IT


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
	JR	NZ,NOTLST	;NO SO DON'T ADD
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
	LD	HL,(DSCTMPI)	;GET ADDR OF STR
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
	JR	Z,STRSTR	;WAS A STRING
	CALL	CONINT		;GET ASCII VALUE OF CHAR
	JR	CALSPA		;NOW CALL SPACE CODE

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
	JR	Z,FINBCK	;YES, ALL DONE
	LD	HL,(DSCTMPI)	;GET DESC. POINTER
SPLP$	LD	(HL),A		;SAVE CHAR
	INC	HL		;BUMP PTR
				;DECR COUNT
	DEC	B
	JP	NZ,SPLP$	;KEEP STORING CHAR
	JR	FINBCK		;PUT TEMP DESC WHEN DONE


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
	JR	C,ALLSTR	;IF #CHARS ASKED FOR.GE.LENGTH,YES
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
	JR	LEFT3		;CONTINUE WITH LEFT CODE

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
	JR	Z,WUZSTR	;WAS A STRING
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
	JR	NZ,OHWELL	;NO
	INC	DE		;BUMP COMPARE POINTER
	DEC	C		;END OF SEARCH STRING?
	JR	Z,GOTSTR	;WE FOUND IT!
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
	JR	RETZR1		;END OF STRING, RETURN 0


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
	JR	C,NCPMID	;IF ALREADY IN STRING SPACE
				;DONT COPY.
	LD	HL,(TXTTAB)
	CALL	COMPAR		;Is this a fielded string?
	JR	NC,NCPMID	;Yes, Don't copy!!
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
	JR	C,BIGLEN	;IF SO, DONT TRUNCATE
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
	JR	Z,MID2		;[E] SAYS USE ALL CHARS
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
MEM	XOR	A		;--->
	PUSH	HL
	LD	(VALTYP),A
	CALL	FRE
	POP	HL
	CALL	CHRGTR
	RET			;<---

FRE	CALL	GETYPR
	JP	NZ,CLCDIF
	CALL	FREFAC		;FREE UP ARGUMENT AND SETUP
				;TO GIVE FREE STRING SPACE
	CALL	GARBA2		;DO GARBAGE COLLECTION
CLCDIF	LD	DE,(STREND)
	LD	HL,(FRETOP)	;TOP OF FREE AREA
	JP	GIVDBL		;RETURN [H,L]-[D,E]


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
	JP	NZ,DERBFM	;IF NOT, "BAD FILE MODE"
	CALL	SYNCHR
	DB	','		;GO PAST THE COMMA
FILSET	LD	D,B		;SETUP PTRFIL
	LD	E,C
	LD	(PTRFIL),DE
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
	IF	GFX
FACFPT	CALL	INTFR2		;===>	HANDLE PRINT #-3. GET FILE #
	JR	Z,FDBPTE	;	IF .GE. 0, GO TO FDBPTR WITH FILE # IN E
	CP	0FFH		;	IS BETWEEN -1 AND -256?
	JP	NZ,DERBFN	;	IF NOT, "BAD FILE NUMBER" !
	LD	A,E		;	CHECK LSB
	CP	0FDH		;	IS IT -3 ?
	JP	NZ,DERBFN	;	IF NOT, "BAD FILE NUMBER" !
	LD	A,0FFH		;	SET PRINT#-3 FLAG
	LD	(GPRFLG),A	;	
	LD	A,02H		;	ASSUME SEQUENTIAL OUTPUT MODE
	RET			;<===
	ELSE
FACFPT	CALL	CONINT		;[AL] = file number
				;fall into FDBPTR
	ENDIF

;	FDBPTR - Transform file number into File-Data-Block pointer
;	Entry - [AL] = file number (0..n)
;	Exit  - if File-Data-Block is allocated,
;	           SI points to 1st byte of File-Data-Block
;	        else FLAGS.Z is true
;	        All other registers are preserved
;
FDBPTR	LD	E,A
;	FDBPTR with file number in E
; TODO: FDBPTE vs. FILID2
FDBPTE	LD	A,(NFILES)	;Number of files
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
GETPTR	CALL	FDBPTE		;FDBPTR with file number in E
	LD	HL,0034H	;Return pointer to MODE
	ADD	HL,BC
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
	JR	NZ,INPDOR	;IF NUMERIC, GO READ THE FILE
	LD	E,D		;MAKE BOTH TERMINATORS COMMA
	JR	INPDOR		;GO READ THE FILE

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
NOTNWT	CALL	INCHR		;Read a character from file PTRFIL
	JP	C,DERRPE	;Input past end
	CP	' '		;SKIP LEADING SPACES
	JR	NZ,NOTSPC1	;EXCEPT FOR LINE INPUT
	INC	D		;CHECK FOR LINEINPUT
	DEC	D
	JR	NZ,NOTNWT	;SKIP ANY NUMBER
NOTSPC1	CP	'"'		;QUOTED STRING COMING IN?
	JR	NZ,NOTQTE
	LD	A,E		;MUST BE INPUT OF A STRING
	CP	','		;WHICH HAS [E]=44 (",")
	LD	A,'"'		;--->	QUOTE BACK INTO [A]
	JR	NZ,NOTQTE
	LD	D,A		;	TERMINATORS ARE QUOTES ONLY
	LD	E,A
	CALL	INCHR		;	READ PAST QUOTATION
	JR	C,QUITSI	;<--- 	IF EOF, ALL DONE
NOTQTE	LD	HL,BUF		;BUFFER FOR DATA
	LD	B,0FFH		;MAXIMUM NUMBER OF CHARACTERS (255)
LOPCRS	LD	C,A		;SAVE CHARACTER IN [C]
	LD	A,D		;CHECK FOR QUOTED STRING
	CP	'"'
	LD	A,C		;RESTORE CHARACTER
	JR	Z,NOTQTL	;DON'T IGNORE CR OR STOP ON LF
	CP	0DH		;CR?
	PUSH	HL		;SAVE DEST PTR. ON STACK
	JR	Z,ICASLF	;EAT LINE FEED IF ONE
	POP	HL		;RESTORE DEST. PTR.
	CP	0AH		;LF?
	JR	NZ,NOTQTL	;NO, TEST OTHER TERMINATORS
LPISLF				;** 5/14/82 BUG FIX(MULTIPLE LF FOR UNQUOTED STRING)
	LD	C,A		;SAVE CURRENT CHAR
	LD	A,E		;GET TERMINATOR 2
	CP	','		;CHECK FOR COMMA (UNQUOTED STRING)
	LD	A,C		;RESTORE CHARACTER
	CALL	NZ,STRCHR	;IF NOT, STORE LF (?)
	CALL	INCHR		;GET NEXT CHAR
	JR	C,QUITSI	;IF EOF, ALL DONE.
	CP	0AH		;** 5/14/82 BUG FIX(MULTIPLE LF FOR UNQUOTED STRING)
	JR	Z,LPISLF	;** 5/14/82 BUG FIX(MULTIPLE LF FOR UNQUOTED STRING)
	CP	0DH		;IS IT A CR?
	JR	NZ,NOTQTL	;IF NOT SEE IF STORE NORMALLY
	LD	A,E		;GET TERMINATOR
	CP	' '		;IS IT NUMERIC INPUT?
	JR	Z,LPCRGT	;IF SO, IGNORE CR, DONT PUT IN BUFFER
	CP	','		;IS IT NON-QUOTED STRING (TERM=,)
	LD	A,0DH		;GET BACK CR.
	JR	Z,LPCRGT	;IF SO, IGNORE CR.
NOTQTL	OR	A		;IS CHAR ZERO
	JR	Z,LPCRGT	;ALWAYS IGNORE, AS IT IS TERMINATOR
				;FOR STRLIT (SEE QUIT2B)
	CP	D		;TERMINATOR ONE?
	JR	Z,QUITSI	;STOP THEN
	CP	E		;TERMINATOR TWO?
	JR	Z,QUITSI
	CALL	STRCHR		;SAVE THE CHAR
LPCRGT	CALL	INCHR		;READ ANOTHER CHARACTER
	JR	NC,LOPCRS	;IF NOT, CHECK AS TERMINATOR
QUITSI	PUSH	HL		;SAVE PLACE TO STUFF ZERO
	CP	'"'		;STOPPED ON QUOTE?
	JR	Z,MORSPC	;DON'T SKIP SPACES THEN
				;BUT DO SKIP FOLLOWING COMMA OR
				;CRLF THOUGH
	CP	' '		;STOPPED ON SPACE?
	JR	NZ,NOSKCR	;NO, DON'T SKIP SPACES
				;OR ANY FOLLOWING COMMAS OR CRLFS EITHER
MORSPC	CALL	INCHR		;READ SPACES
	JR	C,NOSKCR	;EOF, ALL DONE.
	CP	' '
	JR	Z,MORSPC
	CP	','		;COMMA?
	JP	Z,NOSKCR	;OK, SKIP IT
	CP	0DH		;CARRIAGE RETURN?
	JR	NZ,BAKUPT	;BACK UP PAST THIS CHARACTER
ICASLF	CALL	INCHR		;READ ANOTHER
	JR	C,NOSKCR	;EOF, ALL DONE.
	CP	0AH		;LINE FEED?
	JR	Z,NOSKCR	;OK, SKIP IT TOO
BAKUPT	LD	HL,(PTRFIL)	;GO TO NUMBER OF CHARACTERS
	LD	BC,0007H	;--->
	ADD	HL,BC
	CALL	BAKCHR		;	backup file PTRFIL
	JR	NOSKCR

;	BAKCHR - backup sequential input file
;	Entry - [AL] = char to be backed up
;	        [PTRFIL] points to FDB of file to be backed up
; ## GIO86.ASM:1069 ##
;	Note: the implementation is different...
BAKCHR	DEC	(HL)		;TODO: comments
	LD	A,(HL)
	OR	A
	JR	NZ,BAKCHR1
	LD	BC,0FFFCH
	ADD	HL,BC
	LD	A,(HL)
	OR	20H
	LD	(HL),A
	LD	BC,0009H
	ADD	HL,BC
	LD	C,(HL)
	INC	HL
	LD	B,(HL)
	DEC	BC
	LD	(HL),B
	DEC	HL
	LD	(HL),C
	RET

BAKCHR1	CP	0FFH
	RET	NZ
	PUSH	HL
	PUSH	DE
	LD	BC,0FFFBH
	ADD	HL,BC
	EX	DE,HL
	$SVC	@BKSP		;	backspace one log. rec.
	JP	NZ,FERROR	;	Get Error Code for function
	POP	DE
	POP	HL
	LD	(HL),0FFH
	RET			;<---

NOSKCR	POP	HL		;GET BACK PLACE TO STORE TERMINATOR
QUIT2B	LD	(HL),00H	;STORE THE TERMINATOR
	LD	HL,BUFMIN	;ITEM IS NOW STORED AT THIS POINT +1
	LD	A,E		;WAS IT A NUMERIC INPUT?
	SUB	20H		;IF SO, [E]=" "
	JR	Z,NUMIMK	;USE FIN TO SCAN IT
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
	JR	QUIT2B		;SPECIAL QUIT


;-----------------------------------------------------------------------------
;	LOAD AND RUN ROUTINES
;
PRGFLI	LD	A,0FFH		;--->	MD.SQI: SEQUENTIAL INPUT MODE
	LD	(XEQMOD),A	;<---	INTERNAL FILE NUMBER IS ALWAYS ZERO
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
	JR	Z,NOTRNL	;NO, JUST LOAD
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
	JR	NZ,NOTINI	;NO
	LD	(CURLIN),HL	;SAVE DIRECT LINE NUMBER
NOTINI	CALL	INCHR		;READ THE FIRST CHARACTER
	JP	C,MAIN		;ALL DONE IF NOTHING IN FILE
	CP	0FEH		;IS THIS A PROTECTED FILE?
	JR	NZ,NTPROL	;NO
	LD	(PROFLG),A	;SET PROTECTED FILE
	JR	BINLOD		;DO BINARY LOAD

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
;	a Fast Load code. A similar routine is implemented
;	in the LSI Enhancements...
BINLOD	LD	HL,(TXTTAB)	;GET PLACE TO START STORING INTO
BINLLP	EX	DE,HL		;SEE IF THERE IS ROOM TO SPARE
	LD	HL,(FRETOP)
	LD	BC,0FFAAH
	ADD	HL,BC
	CALL	COMPAR
	EX	DE,HL
	JR	C,OUTLOD	;ERROR AND WIPE OUT PARTIAL GARBAGE
				;UNLINKED!! NO ZEROES AT THE END!!
	CALL	INCHR		;READ THE A DATA BYTE
				;THIS IS SEMI-WEAK SINCE MEMORY
				;IS LEFT IN A BAD BAD STATE
				;IF AN I/O ERROR OCCURS
	LD	(HL),A		;STORE BYTE
	INC	HL		;INCREMENT POINTER
	JR	NC,BINLLP	;READ THE NEXT CHAR
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
	CALL	UPPROF		;Update PROFLG protection flag
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
	JR	Z,OKGETM	;READ THE FILE
	CALL	PRGFIN		;CLOSE OUT TIME
	JP	SNERR		;AND "SYNTAX ERROR"

OKGETM	XOR	A		;NO RUN OPTION WITH "MERGE"
	LD	(RUNFLG),A	;SET UP THE RUN FLAG
	CALL	INCHR		;READ FROM [PTRFIL] FILE
	JP	C,MAIN		;GO BACK IF EOF
	INC	A		;IS IT A BINARY FILE??
	JP	Z,DERBFM	;BINARY IS WRONG FILE MODE
	INC	A		;OR PROTECTED BINARY FILE??
	JP	Z,DERBFM	;ALSO GIVE BAD FILE MODE
MAINGO	LD	HL,(PTRFIL)	;GET FILE POINTER
	LD	BC,0007H	; (NMLOFC) POINT TO NUMBER OF CHARS IN BUFFER
	ADD	HL,BC		;BY ADDING OFFSET
	CALL	BAKCHR		;BACK UP FILE
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
SAVE	PUSH	HL		;---> save text pointer for rescan of filename
	CALL	FRMEVL		;skip filename
	PUSH	HL
	CALL	FRESTR		;release string temporary
	POP	HL
	DEC	HL
	CALL	CHRGTR		;END OF STATEMENT?
	JP	Z,ENDOFS	;BINARY SAVE!!
	CALL	SYNCHR
	DB	','		;eat comma
	CP	'A'		;ASCII SAVE?
	JP	Z,GOODSV	;branch if got "A"
	CP	'P'		;PROTECTED SAVE?
	JP	NZ,SNERR	;branch if not "A" or "P"
GOODSV	PUSH	HL		;save text pointer
	PUSH	AF		;save "A" or "P"
	CALL	CHRGTR		;check for end-of-statement
	JP	NZ,SNERR	;error if not end-of-statement
	POP	AF
	POP	HL		;leave text pointer pointing at "A" or "P"
ENDOFS	EX	(SP),HL		;save End-Of-Statement text pointer
				;HL = text pointer to filename
	PUSH	AF		;save "A", "P" or null option
	CP	'P'
	JP	Z,NOTPRO
	CALL	PROCHK		;Check protection flag
NOTPRO	LD	D,02H		;(MD.SQO) ELIMINATE EARLIER VERSION
				; AND CREATE EMPTY FILE
	CALL	PRGFL2		;READ FILE NAME AND DISK NUMBER
				; AND LOOK IT UP
	POP	AF		;[A]="A", "P" or null option
	POP	HL		;restore End-Of-Statement text pointer
	CP	'P'		;PROTECTED SAVE?
	JP	Z,PROSAV	;DO IT
	CP	'A'		;ASCII save?
	JP	NZ,BINSAV	;if not, must be Binary Save
	CALL	CHRGTR		;skip "A"
				;set File Code to ASCII (overridding user setting)
	JP	LISTS		;USE THE LIST CODE TO DO THE OUTPUT
				;CONTROL-CS ARE NOT ALLOWED
				;AND AT THE END PTRFIL IS ZEROED
				;<---

;-----------------------------------------------------------------------------
;	BINSAV - Binary SAVE support.
; ## GIO86.ASM:1088 ##
;	Note: in DSKCOM.ASM, BINSAV is implemented using
;	a fast block save code. A similar routine is implemented
;	in the LSI Enhancements...
BINSAV	CALL	SCCPTR		;GET RID OF POINTERS BEFORE SAVING
	LD	A,0FFH		;ALWAYS START WITH 255
BINPSV	CALL	FILOU3		;SEND TO FILE
	LD	DE,(VARTAB)	;GET STOP POINT
	LD	HL,(TXTTAB)	;GET START POINT
BNPSLP	CALL	COMPAR		;REACHED THE END?
	JP	Z,PRGFIN	;REGET TEXT POINTER AND CLOSE FILE 0
	LD	A,(HL)		;GET LINE DATA
	INC	HL		;POINT AT NEXT DATA
	PUSH	DE		;SAVE LIMIT
	CALL	FILOU3		;SEND CHAR TO FILE
	POP	DE		;RESTORE LIMIT
	JR	BNPSLP		;CONTINUE WITH LINE DATA


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
	JR	NZ,CLOSE1	;NOT END OF STATEMENT, SO SCAN ARGUMENTS
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
	JP	Z,DERBFN	;Bad File Number
	SUB	03H		;MAKE SURE ITS A RANDOM FILE
	JP	NZ,DERBFM	;IF NOT, "BAD FILE MODE"
	EX	DE,HL		;SAVE TEXT POINTER
	LD	HL,000BH	;POINT TO RECORD SIZE
	ADD	HL,BC
	LD	A,(HL)		;GET IT
	LD	(TEMPB),A	;--->	STORE MAX ALLOWED
	XOR	A		;<---	MAKE [A]=0
	EX	DE,HL		;GET BACK TEXT POINTER
	LD	DE,0034H	;POINT TO 5.0 FIELD BUFFER
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
	ADD	A,B		;--->	Add in current field size
	JR	NC,FIELD11	;	IN RANGE?
	JP	NZ,DERFOV	;	NO, GIVE ERROR
FIELD11	LD	B,A
	LD	A,(TEMPB)	;	DUP TEMPA ?
	CP	B
	JR	NC,FIELD12
	OR	A
	JP	NZ,DERFOV	;	FIELD overflow
FIELD12	LD	A,B
	OR	A		;<---
	EX	DE,HL		;[H,L] POINT AT STRING DESCRIPTOR
	LD	(HL),C		;STORE THE LENGTH
	INC	HL
	LD	(HL),E		;STORE THE POINTER INTO THE DATA BLOCK
	INC	HL
	LD	(HL),D
	LD	B,00H		;<-->
	POP	HL		;GET BACK THE TEXT POINTER
	JR	NZ,LOPFLD	;--->	CONTINUE SCANNING "AS" CLAUSES IF MORE
	LD	A,C
	OR	A
	RET	NZ
	JR	LOPFLD		;<---	CONTINUE SCANNING "AS" CLAUSES IF MORE


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
	JR	Z,RETCUR	;Yes, don't change
	LD	HL,(TXTTAB)
	CALL	COMPAR		;Stringvar in disk buffer?
	JR	NC,OLDSTR	;Yes, use it
	LD	HL,(VARTAB)
	CALL	COMPAR		;stringvar in program(literal)?
	JR	C,OLDSTR	;No, in string space so use it
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
	JR	C,MAKDSC	;Yes, better have stringexp temp.
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
	JR	NC,FILDOK	;Jump if field large enough for result
	LD	B,A		;Save # of bytes to copy
FILDOK	SUB	B
	LD	C,A		;[C] = # blanks to fill
	POP	AF		;Get LSET/RSET flag
	CALL	NC,BLKFIL	;Fill leading if RSET
	INC	B		;In case zero
COPLOP	DEC	B		;Decr. # to copy
	JR	Z,LRSTDN	;Done when all copied
	LD	A,(HL)		;Get byte from stringexp
	LD	(DE),A		;Copy to stringvar
	INC	HL
	INC	DE
	JR	COPLOP

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
	JR	BLKFL1

;	If stringexp was a temporary, create a new temporary to point to
;	stringexp since old one was freed. This must be done since GETSPA
;	will be called and garbage collection might occur. If stringexp is
;	not a temporary, return.
MAKDSC	POP	AF		;Get temp flag
	POP	HL
	POP	BC
	EX	(SP),HL		;Dig down to stringexp descr.
	EX	DE,HL		;FRETMS wants [D,E]
	JR	NZ,MAKDS1	;Not a temp, don't reallocate
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
	PUSH	HL		;--->	Preserve PTRFIL across INPUT$ so
	LD	HL,(PTRFIL)	;	cases like PRINT #2,INPUT$(3,#1)
	LD	DE,0000H	;	will work properly.
	LD	(PTRFIL),DE	;	(Clear PTRFIL in case no file number
	EX	(SP),HL		;<---	is specified.)
	CALL	GETBYT		;Get # of bytes to read
	PUSH	DE		;Save # of bytes to read
	LD	A,(HL)
	CP	2CH		;Read from disk file?
	JR	NZ,REDTTY	;No, from user's terminal
	CALL	CHRGTR
	CALL	FILSCN		;Set up file #
	CP	01H		;NOT SEQUENTIAL INPUT FILE?
	JP	NZ,DERBFM	;THEN BAD FILE MODE
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
	JR	Z,DSKCHR	;Read from disk file
	CALL	SVCKBD		;Read a char from terminal
;	Note: will check flag on interrupt ^c
	CP	03H		;<Break>?
	JR	Z,FICTLC	;Yes, stop
PUTCHR	LD	(HL),A		;Put char into string
	INC	HL
	DEC	C		;Read enough yet?
	JR	NZ,FIXLOP	;No, read more
	POP	AF		;Get flag off stack
	POP	BC		;--->	B:=text pointer.
	POP	HL		;	Restore PTRFIL.
	LD	(PTRFIL),HL
	PUSH	BC		;<---	Restack the text pointer.
	JP	PUTNEW		;Return string as result

;	Got <Break>
;	aka INTCTC
FICTLC	LD	HL,(SAVSTK)	;GET SAVED STACK POINTER
	LD	SP,HL		;SET [H,L] TO IT
	JP	ENDCON		;STOP PROGRAM

DSKCHR	CALL	INCHR		;Get char from file
	JP	C,DERRPE	;If carry, read past EOF
	JR	PUTCHR		;Put char in string

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
	CP	02H		;IS IT A SEQUENTIAL OUTPUT FILE?
	JP	Z,DERBFM	;THEN GIVE BAD FILE MODE
	LD	HL,000FH	; (0+ORNOFS)
				;SEE IF ANY BYTES ARRIVED IN THIS BUFFER
	ADD	HL,BC
	LD	A,(HL)
	DEC	HL
	LD	E,(HL)
	DEC	HL
	CP	(HL)
	JR	NZ,EOF11
	DEC	HL
	LD	A,E
	CP	(HL)
	JR	NZ,EOF11
	DEC	HL
	DEC	HL
	LD	A,(HL)
	DEC	HL
	DEC	HL
	DEC	HL
	SUB	(HL)
EOF11	SUB	01H
	SBC	A,A
	JP	CONIA		 ;CONVERT TO AN INTEGER AND RETURN


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
	RET	Z		;RETURN IF NOT OPEN
	CP	01H		;SEQENTIAL INPUT?
	JR	Z,NTOPNC	;NO NEED TO FORCE PARTIAL OUTPUT BUFFER
	PUSH	BC
	XOR	A
	LD	(BC),A
	INC	BC
	INC	BC
	LD	D,B
	LD	E,C
;	CLOSE FILE
;
;	(DE) points to FCB
	$SVC	@CLOSE		;CALL DOS @CLOSE SVC
	JP	NZ,FERROR	;Get Error Code for function
	POP	BC		;RESTORE FILE POINTER
NTOPNC	LD	D,34H		; (DATOFS) NUMBER OF BYTES TO ZERO
	XOR	A
MORCZR	LD	(BC),A
	INC	BC
	DEC	D
	JR	NZ,MORCZR
	RET

;	LOC(n) Function
;	Entry - [FAC] = file number
;	Exit  - [FAC] = current record number
LOC	LD	A,@LOC		; DOS SVC: TODO
	DB	01H
;	LOF(n) Function
;	Entry - [FAC] = file number
;	Exit  - [FAC] = length of file in bytes
LOF	LD	A,@LOF		; DOS SVC: TODO
	PUSH	AF
	CALL	FACFPT		;CONVERT ARGUMENT AND INDEX
	JP	Z,DERBFN	;"BAD FILE NUMBER" IF NOT OPEN
	INC	BC
	INC	BC
	LD	D,B
	LD	E,C
	POP	AF
	RST	28H		;Invoke DOS SVC
	JP	NZ,FERROR	;DOS Error?
	LD	H,B		;GET RC
	LD	L,C
	JP	MAKINT		;FLOAT IT

;	FILOUT -- PUT A CHARACTER IN AN OUTPUT BUFFER AND OUTPUT IF NECESSARY
;
;	CALL AT FILOUT WITH [H,L] TO BE SAVED ON THE STACK
;	AND THE CHARACTER IN THE HIGH ORDER BYTE BELOW THE [H,L]
;	ON THE STACK. THE CURRENT DATA IS OUTPUT IF THERE ARE 256
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
	POP	AF		;TAKE THE CHARACTER OFF
	PUSH	DE
	PUSH	BC
	INC	HL		;POINT AT PRINT POSITION
	LD	B,00H
	PUSH	AF		;RESAVE FOR OUTPUT
	LD	D,(HL)		;[D]=CURRENT POSITION
	CP	0DH		;BACK TO ZERO POSITION WITH RETURN?
	LD	(HL),B		;ASSUME RESET TO ZERO SINCE [B]=0
	JR	Z,ISCRDS	;ALL DONE UPDATING POSITION
	ADD	A,0E0H		;SET CARRY FOR SPACES AND HIGHER
	LD	A,D		;[A]=CURRENT POSITION
	ADC	A,B		;ADD ON CARRY SINCE [B]=0
	LD	(HL),A		;UPDATE THE POSITION IN THE DATA BLOCK
ISCRDS	INC	HL
	EX	DE,HL
	POP	AF		;GET THE CHARACTER
	PUSH	AF
	LD	C,A
	PUSH	DE
	$SVC	@PUT		;write byte to device
	POP	DE
	JP	NZ,FILOU33
FILOU32	POP	AF
	POP	BC
	POP	DE
	POP	HL
	RET

FILOU33	LD	C,A
	LD	A,(DE)
	OR	A
	LD	A,C
	JP	M,FERROR	;Get Error Code for function
	JP	FILOU32

;	Syntax - GET fn [,recnum]   (if no recnum next relative record assumed)
;	         PUT fn [,recnum]
;	 Entry - [BX] = text pointer
;	         [CX] = 0 for GET, 1 for PUT
PUTST	DB	0F6H		;Set PUT flag

GETST	XOR	A		;Set PUT flag
	LD	(PGTFLG),A
	XOR	A		;Set GET flag
	LD	(GTPUTC5),A	;PUT/GET Flag
	CALL	FILSCN		;[A]=file mode
	CP	03H		;Not random - bad file mode
	JP	NZ,DERBFM	;Bad file mode
	PUSH	BC		;Save pointer at file data block
	PUSH	HL		;Save text pointer
	LD	HL,000CH	;(FD.LOG) Fetch current logical posit
	ADD	HL,BC
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	INC	DE		;Compensate for "DCX D" when call INTIDX
	POP	HL		;Save data block pointer and get text pointer
	LD	A,(HL)
	CP	','		;Is there a record number
	JR	Z,GTPUTC1	;TODO
	LD	A,0FFH
	LD	(GTPUTC5),A
	;TODO
GTPUTC1	JR	NZ,GTPUTC2
	CALL	ADRGET		;Get 16-bit unsigned int
	JP	C,DERBRN	;Bad record number
GTPUTC2	DEC	HL		;Make sure statement ends
	CALL	CHRGTR
	JP	NZ,SNERR
	EX	(SP),HL		;Save text pointer, get data block pointer
	PUSH	HL
	LD	A,E		;Get record #
	OR	D		;Make sure its not zero
	JP	Z,DERBRN	;If so, "Bad record number"
	DEC	DE
	LD	B,D
	LD	C,E
	EX	DE,HL
	INC	DE
	INC	DE
	LD	HL,0032H
	ADD	HL,DE
	LD	A,00H
GTPUTC5	EQU	$-1
	OR	A
	JR	NZ,GTPUTC8
	PUSH	HL
	$SVC	@POSN		;position file to LRN
	POP	HL
	JR	Z,GTPUTC8
	CP	1CH
	JR	Z,GTPUTC6
	CP	1DH
	JP	NZ,FERROR	;Get Error Code for function
GTPUTC6	LD	A,(PGTFLG)	;PUT or GET
	OR	A		;Set cc's
	JR	Z,GTPUTCA	;Was Read
GTPUTC7	PUSH	HL
	PUSH	BC
	LD	HL,0006H
	ADD	HL,DE
	LD	A,(HL)
	AND	07H
	LD	C,A
	LD	B,00H
	LD	HL,DRVFLG
	ADD	HL,BC
	LD	A,(HL)
	AND	01H
	JP	NZ,DERDWP	;Disk write protected
	POP	BC
	POP	HL
	$SVC	@WRITE		;write record to file
	JP	GTPUTC9

GTPUTC8	LD	A,(PGTFLG)
	OR	A
	JR	NZ,GTPUTC7
	$SVC	@READ		;read record from file
GTPUTC9	JP	NZ,FERROR	;Get Error Code for function
	POP	BC
	POP	HL
	RET

GTPUTCA	POP	HL
	LD	DE,0034H
	ADD	HL,DE
;	ZERO OUT THE BUFFER IN CASE NOTHING READ
	LD	B,00H		;NUMBER OF BYTES/BUFFER
	XOR	A
GTPUTCB	LD	(HL),A		;ZERO IT
	INC	HL		;INCREMENT BUFFER POINTER
	DEC	B		;DECREMENT COUNT
	JP	NZ,GTPUTCB	;KEEP ZEROING
	POP	HL
	RET


;-----------------------------------------------------------------------------
;	INCHR - get next byte from file PTRFIL
;	 Exit  - [AL]=byte, [FLAGS], [AH] destroyed.
;	         All other regs preserved
;	         if END-OF-FILE then
;	            if program load was in progress, file 0 closed etc.
;	            else Read-Past-End error is generated
; ## GIO86.ASM:903 ##
INCHR	PUSH	BC		;SAVE CHAR COUNTER
	PUSH	HL		;SAVE [H,L]
	LD	HL,(PTRFIL)	;GET DATA BLOCK POINTER
	PUSH	DE
	EX	DE,HL
	INC	DE
	INC	DE
	;TODO
INCHR1	$SVC	@GET		;get byte from device
	JR	Z,INCHR4
	OR	A
	JP	NZ,INCHR2
	CALL	ISCNTC
	JP	INCHR1		;TODO

INCHR2	CP	1CH
	JR	Z,INCHR3
	CP	1DH
	JP	NZ,FERROR	;Get Error Code for function
INCHR3	SCF
	DB	06H
INCHR4	OR	A
	POP	DE
	POP	HL
	POP	BC
	RET

;	NAMFIL - Scan a file name for NAME, KILL, or FILES command
;	 Entry - [HL] = text pointer
;	 Exit  - [HL] = text pointer
;	         [DE] points to 1st byte of filename
;	         [??] = number of bytes in filename string
;	 Uses  - [A]
;	Get file name, etc..
;	a.k.a. FNAME -- SCAN A FILE NAME AND NAME COMMAND
NAMFIL	PUSH	BC
	CALL	FRMEVL		;EVALUATE STRING
	PUSH	HL		;SAVE TEXT POINTER
	CALL	FRESTR		;FREE UP THE TEMP
	LD	A,(HL)		;GET LENGTH OF STRING
	OR	A		;NULL STRING?
	JP	Z,DERIFN	;YES, ERROR
	CP	21H
	JP	NC,DERIFN
	INC	HL		;PICK UP POINTER TO STRING
	LD	C,(HL)		;BY GETTING ADDRESS
	INC	HL		;OUT OF DESCRIPTOR
	LD	B,(HL)		;[B,C] POINTS TO STRING
	LD	HL,DOSFCB	;DOS File Control Block
	LD	E,A		;SAVE LENGTH
NAMFIL1	LD	A,(BC)		;TO UPPER CASE
	CP	'a'
	JR	C,NAMFIL2
	CP	'z'+1
	JR	NC,NAMFIL2
	XOR	20H
NAMFIL2	LD	(HL),A
	INC	HL
	INC	BC
	DEC	E
	JR	NZ,NAMFIL1	;TODO
	LD	(HL),E
	POP	HL
	POP	DE
	LD	B,D
	LD	C,E
	PUSH	BC
	PUSH	HL
	LD	HL,DOSFCB	;DOS File Control Block
	INC	DE
	INC	DE
	$SVC	@FSPEC		;parse filename
	JP	NZ,DERIFN	;Bad file name
	OR	A
	JP	NZ,DERIFN	;Bad file name
	POP	HL
	POP	BC
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
	JR	Z,HAVMOD	;[D] HAS CORRECT MODE
	LD	D,01H		;ASSUME SEQUENTIAL (1=MD.SQI)
	CP	'I'		;IS IT?
	JR	Z,HAVMOD	;[D] SAYS SEQUENTIAL INPUT
	LD	D,04H		;ASSUME IT'S "E" (4=MD.???)
	CP	'E'		;IS IT?
	JR	Z,HAVMOD	;[D] SAYS SEQUENTIAL EXTEND
	LD	D,03H		;MUST BE RANDOM (3=MD.RND)
	CP	'D'		;IS IT?
	JR	Z,HAVMOD	;[D] SAYS DIRECT/RANDOM
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
	PUSH	BC		;SAVE POINTER AT FILE DATA BLOCK
	CALL	NAMFIL		;READ THE NAME
	LD	B,00H
	POP	DE
	POP	AF
	PUSH	AF
	PUSH	DE
	CP	03H
	JR	NZ,PRGFIL1	;TODO
	DEC	HL
	CALL	CHRGTR
	JR	Z,PRGFIL1	;TODO
	CALL	SYNCHR
	DB	','
	CALL	GETBYT		;Get byte value in E and A
	LD	B,E
	;TODO
PRGFIL1	LD	(TEMP),HL
	LD	HL,(SFLAGS)	;Ptr to DOS SFLAGS
	LD	A,(HL)
	POP	HL
	AND	0F9H
	LD	C,A
	LD	A,(XEQMOD)	;Allow the loading of EXEC-only files
	OR	A
	LD	A,C
	JR	Z,PRGFIL2
	OR	04H
PRGFIL2	PUSH	HL
	LD	HL,(SFLAGS)	;Ptr to DOS SFLAGS
	LD	(HL),A
	POP	HL
	POP	AF
	PUSH	AF
	PUSH	HL
	LD	(PTRFIL),HL
	EX	DE,HL
	INC	DE
	INC	DE
	LD	HL,0032H
	ADD	HL,DE
	LD	C,A
	LD	A,B
	OR	A
	JR	Z,PRGFIL3
	INC	H
PRGFIL3	LD	A,C		;Get mode flag
	CP	04H		;Is Sequential Append mode?
	JR	NZ,PRGFIL4	;If not
;	Open for APPEND
	$SVC	@INIT		;open new or existing file
	CALL	PRGFILA		;Accept OK or Error 2AH
	JP	NZ,FERROR	;Get Error Code for function
	POP	HL
	POP	AF
	LD	A,02H		;Set mode to Output
	PUSH	AF
	PUSH	HL
	$SVC	@PEOF		;position to EOF
	CALL	PRGFILB		;Accept OK or Error 1CH
	JP	PRGFILF

PRGFIL4	CP	01H		;Sequential Input mode?
	JR	Z,PRGFILD
	CP	03H		;Random mode?
	JR	NZ,PRGFILC
;	Open for RANDOM
	$SVC	@OPEN		;open existing file
	CALL	PRGFIL9		;Accept OK, Error 29H or Error 2AH
	JR	Z,PRGFIL6
	CP	18H
	JR	NZ,PRGFIL5
	$SVC	@INIT		;open new or existing file
	CALL	PRGFIL9		;Accept OK, Error 29H or Error 2AH
	JR	Z,PRGFIL6
;	Got DOS Error
PRGFIL5	JP	FERROR		;Get Error Code for function

PRGFIL6	PUSH	HL
	PUSH	BC
	LD	HL,0006H
	ADD	HL,DE
	LD	A,(HL)
	AND	07H
	LD	C,A
	LD	B,00H
	PUSH	BC
	$SVC	@CKDRV		;check drive availability
	POP	BC
	PUSH	AF
	LD	HL,DRVFLG
	ADD	HL,BC
	POP	AF
	JR	NC,PRGFIL7
	LD	A,(HL)
	OR	01H
	LD	(HL),A
	JR	PRGFIL8

PRGFIL7	LD	A,(HL)
	AND	0FEH
	LD	(HL),A
PRGFIL8	POP	BC
	POP	HL
	XOR	A
	JP	PRGFILF

;	Accept OK, Error 29H or Error 2AH
PRGFIL9	RET	Z
	CP	29H
;	Accept OK or Error 2AH
PRGFILA	RET	Z
	CP	2AH
	RET

;	Accept OK or Error 1CH
PRGFILB	RET	Z
	CP	1CH
	RET

;	Open for OUTPUT
PRGFILC	$SVC	@INIT		;open new or existing file
	CALL	PRGFILA		;Accept OK or Error 2AH
	JR	Z,PRGFILF
	JR	PRGFIL5

;	Open for INPUT
PRGFILD	PUSH	HL
	LD	HL,(SFLAGS)	;Ptr to DOS SFLAGS
	LD	A,(HL)
	OR	01H
	LD	(HL),A
	POP	HL
	$SVC	@OPEN		;open existing file
	CALL	PRGFIL9		;Accept OK, Error 29H or Error 2AH
	PUSH	AF
	LD	A,(XEQMOD)	;Allow the loading of EXEC-only files
	OR	A
	JR	Z,PRGFILE
	XOR	A
	LD	(XEQMOD),A	;Allow the loading of EXEC-only files
	PUSH	HL
	LD	HL,(SFLAGS)	;Ptr to DOS SFLAGS
	LD	A,(HL)
	POP	HL
	RRA
	RRA
	SBC	A,A
	LD	(NEWPRO),A	;New Protection Flag
PRGFILE	POP	AF
PRGFILF	JP	NZ,FERROR	;Get Error Code for function
	POP	HL
	POP	AF
	LD	(HL),A
	INC	HL
	LD	(HL),00H
	LD	HL,(TEMP)
	RET

TEMPB	DB	00H
	;Allow the loading of EXEC-only files
XEQMOD	DB	00H
	;New Protection Flag
NEWPRO	DB	00H



;	KILL filename
;	 Entry - [BX] = text pointer
;	 Exit -  [BX] = text pointer
;
; ## GIODSK.ASM:1018 ##
KILL	XOR	A
	CALL	FDBPTR
	CALL	NAMFIL		;SCAN FILE NAME
	PUSH	HL		;SAVE TEXT POINTER
	LD	HL,(FDBTAB)	;Table of up to 16 FDB's
	EX	DE,HL
	INC	DE
	INC	DE
	LD	B,00H
	$SVC	@OPEN		;DOS: open existing file
	CALL	PRGFILA
	JP	NZ,FERROR	;Get Error Code for function
	PUSH	HL
	PUSH	DE
	PUSH	BC
	LD	HL,0006H
	ADD	HL,DE
	LD	A,(HL)
	LD	(KILL12),A
	INC	HL
	LD	A,(HL)
	LD	(KILL13),A
	LD	A,(NFILES)	;Number of files
	OR	A
	JR	Z,KILL15
	;TODO
KILL11	PUSH	AF
	CALL	FDBPTR
	JR	Z,KILL14
	LD	HL,0008H
	ADD	HL,BC
	LD	A,00H
KILL12	EQU	$-1
	CP	(HL)
	JR	NZ,KILL14
	LD	A,00H
KILL13	EQU	$-1
	INC	HL
	CP	(HL)
	JR	NZ,KILL14
	POP	AF
	POP	BC
	POP	DE
	POP	HL
	$SVC	@CLOSE		;close file
	LD	A,29H		;'File Already Open'
	JP	FERROR		;Get Error Code for function

KILL14	POP	AF
	DEC	A
	JR	NZ,KILL11	;TODO
KILL15	POP	BC
	POP	DE
	POP	HL
	$SVC	@REMOV		;remove file or device
	JP	NZ,FERROR	;Get Error Code for function
	LD	HL,(FDBTAB)	;Table of up to 16 FDB's
	LD	(HL),00H
	POP	HL		;GET BACK TEXT POINTER
	RET

;	Handle DOS error
FERROR	LD	B,00H
	LD	HL,ERRCODS	;Error codes for function
	LD	(DOSERR),A	;Save for ERRS$
	CP	0FFH		;Command aborted?
	JR	NZ,FERROR1	;If not, translate
	LD	E,ERRCMA	;'Command Aborted'
	JP	ERROR		;Raise error

;	Translate DOS error code to BASIC error code
FERROR1	CP	44		;Can the error code be translated?
	JR	C,FERROR2	;Yes, translate
	LD	A,01H		;Otherwise set DOS error code 1
FERROR2	LD	C,A		;Translate error code
	ADD	HL,BC		;
	LD	E,(HL)		;
	JP	ERROR		;Raise error

ERRCODS	EQU	$-1
	DB	ERRDIO		;01 Parity error during header read
	DB	ERRDIO		;02 Seek error during read
	DB	ERRDIO		;03 Lost data during read
	DB	ERRDIO		;04 Parity error during read
	DB	ERRDIO		;05 Data record not found during read
	DB	ERRDIO		;06 Attempted to read system data record
	DB	ERRDIO		;07 Attempted to read locked/deleted data record
	DB	ERRDIO		;08 Device not available
	DB	ERRDIO		;09 Parity error during header write
	DB	ERRDIO		;10 Seek error during write
	DB	ERRDIO		;11 Lost data during write
	DB	ERRDIO		;12 Parity error during write
	DB	ERRDIO		;13 Data record not found during write
	DB	ERRDIO		;14 Write fault on disk drive
	DB	ERRDWP		;15 Write protected disk
	DB	ERRIER		;16 Illegal logical file number
	DB	ERRDIO		;17 Directory read error
	DB	ERRDIO		;18 Directory write error
	DB	ERRIFN		;19 Illegal file name
	DB	ERRDIO		;20 GAT read error
	DB	ERRDIO		;21 GAT write error
	DB	ERRDIO		;22 HIT read error
	DB	ERRDIO		;23 HIT write error
	DB	ERRFNF		;24 File not in directory
	DB	ERRFAD		;25 File access denied
	DB	ERRDWP		;26 No directory space available
	DB	ERRDFL		;27 Disk space full
	DB	ERRRPE		;28 End of file encountered
	DB	ERRBRN		;29 Record number out of range
	DB	ERRDFL		;30 Directory full - can't extend file
	DB	ERRFNF		;31 Program not found
	DB	ERRIFN		;32 Illegal drive number
	DB	ERRDFL		;33 No device space available
	DB	ERRIER		;34 Load file format error
	DB	ERRIER		;35 (undefined)
	DB	ERRIER		;36 (undefined)
	DB	ERRFAD		;37 Illegal access attempted to protected file
	DB	ERRIER		;38 File not open
	DB	ERRIER		;39 Device in use
	DB	ERRIER		;40 Protected system device
	DB	ERRFAO		;41 File already open
	DB	ERRFC		;42 Logical record length open fault
	DB	ERRIER		;43 SVC parameter error
				;44 Parameter error


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
	JR	NZ,WNOTOL	;IF NO MATCH NO NEED TO TRUNCATE THE STACK
	ADD	HL,BC		;ELIMINATE EVERYTHING UP TO AND INCLUDING
				;THE MATCHING WHILE ENTRY
	LD	SP,HL
	LD	(SAVSTK),HL
WNOTOL	LD	HL,(CURLIN)	;MAKE THE STACK ENTRY
	PUSH	HL
	LD	HL,(ENDFOR)	;GET TEXT POINTER FOR WHILE BACK
	PUSH	HL
	PUSH	DE		;SAVE THE WEND TEXT POINTER
	JR	FNWEND		;FINISH USING WEND CODE

WEND	JP	NZ,SNERR	;STATEMENT HAS NO ARGUMENTS
	EX	DE,HL		;FIND MATCHING WHILE ENTRY ON STACK
	CALL	FNDWND
	JR	NZ,WEERR	;MUST MATCH OR ELSE ERROR
	LD	SP,HL		;TRUNCATE STACK AT MATCH POINT
	LD	(SAVSTK),HL	;[H,L] POINTING INTO STACK ENTRY
	LD	DE,(CURLIN)	;REMEMBER WEND LINE #
	LD	(NXTLIN),DE	;IN NXTLIN
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
	JR	Z,FLSWHL	;GO BACK AT WEND IF FALSE
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
	JR	NZ,FNDWN3	;TODO: OO12H vs. 0010H next line
	LD	BC,0012H	;Yes, so skip over it.  Note that
	ADD	HL,BC		;the pointer has already been
	JR	FNDWN2		;incremented once.

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
	JR	FNDWN2

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
CALLST	CALL	PRODIR		;<--> Check if direct PEEK/POKE/CALL allowed
	LD	A,80H		;Flag PTRGET not to allow arrays
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
	JR	Z,CALLST3	;If end of line, GO!   (jp if no parameters)
	CALL	SYNCHR		;Eat left paren
	DB	'('
CALLST1	PUSH	BC		;Save count
	PUSH	DE		;Save pointer into stack
	PUSH	HL		;--->
	LD	HL,(ARYTAB)
	EX	(SP),HL		;<---
	CALL	PTRGET		;Evaluate param address
	EX	(SP),HL		;--->
	PUSH	DE
	EX	DE,HL
	LD	HL,(ARYTAB)
	CALL	COMPAR
	JP	NZ,FCERR	;Illegal function call
	POP	DE
	POP	HL		;<---
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
	JR	NZ,CALLST2	;Test
	DEC	C		;Decrement count of params
	CALL	CHRGTR		;Get next char
	JR	CALLST1		;Back for more

CALLST2	CALL	SYNCHR		;check for terminating right paren
	DB	')'		; )
	LD	(TEMP),HL	;save text pointer
	LD	A,MAXPRM+1	;Calc # of params
	SUB	C
	POP	HL		;At least one, get its address in [H,L]
	DEC	A		;Was it one?
	JR	Z,CALLST3	;Yes
	POP	DE		;Next address in [D,E]
	DEC	A		;Two?
	JR	Z,CALLST3	;Yes
	POP	BC		;Final in [B,C]
	DEC	A		;Three?
	JR	Z,CALLST3	;Yes
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
	LD	A,(OPTFLG)	;--->
	LD	(TOPTFG),A	;	SAVE OPTION BASE VALUE
	LD	A,(OPTVAL)	;	SAVE OPTION VALUE FOR ARRAY BASE
	LD	(TOPTVL),A	;<---
	LD	A,(HL)		;Get current char
	LD	DE,MERGETK	;Is it MERGE?
	CP	E		;Test
	JR	NZ,NTCHNM	;NO
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
	JR	Z,NTCHAL	;No line number etc.
	CALL	SYNCHR
	DB	','		;Must be comma
	CP	','		;Omit line # (Use ALL for instance)
	JR	Z,NTLINF	;YES
	CALL	FRMEVL		;Evaluate line # formula
	PUSH	HL		;Save text poiner
	CALL	FRQINT		;Force to int in [H,L]
	LD	(CHNLIN),HL	;Save it for later
	POP	HL		;Restore text poiner
	DEC	HL		;Rescan last char
	CALL	CHRGTR
	JR	Z,NTCHAL	;No ALL i.e. preserve all vars across CHAIN
NTLINF	CALL	SYNCHR
	DB	','		;Should be comma here
	LD	DE,DELETETK	;Test for DELETE option
	CP	E		;Is it?
	JR	Z,CHMWDL	;Yes
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
	JR	NC,FCERRG	;Must have exact match on end of range
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
	LD	(CURLIN),DE	;<-->	Save current line # in CURLIN for errors
CSTSCN	CALL	CHRGTR		;Get statement type
AFTCOM	OR	A
	JR	Z,CLPSC1	;EOL Scan next one
	CP	':'		;Are we looking at colon
	JR	Z,CSTSCN	;Yes, get next statement
	LD	DE,COMMONTK	;Test for COMMON, avoid byte externals
	CP	E		;Is it a COMMON?
	JR	Z,DOCOMM	;Yes, handle it
	CALL	CHRGTR		;Get first char of statement
	CALL	DATA		;Skip over statement
	DEC	HL		;Back up to rescan terminator
	JR	CSTSCN		;Scan next one

DOCOMM	CALL	CHRGTR		;Get thing after COMMON
	JR	Z,AFTCOM	;Get next thing
NXTCOM	PUSH	HL		;Save text pointer
	LD	A,01H		;Call PTRGET to search for array
	LD	(SUBFLG),A
	CALL	PTRGTN		;This subroutine in F3 scans variables
	JR	Z,FNDAAY	;Found array
	LD	A,B		;Try finding array with COMMON bit set
	OR	80H
	LD	B,A
	XOR	A		;Set zero CC
	CALL	ERSFIN		;Search array table
	LD	A,00H		;Clear SUBFLG in all cases
	LD	(SUBFLG),A
	JR	NZ,NTFN2T	;Not found, try simple
	LD	A,(HL)		;Get terminator, should be "("
	CP	'('		;Test
	JR	NZ,SCNSMP	;Must be simple then
	POP	AF		;Get rid of saved text pointer
	JR	COMADY		;Already was COMMON, ignore it

NTFN2T	LD	A,(HL)		;Get terminator
	CP	'('		;Array specifier?
	POP	DE		;--->	(DE:=saved text pointer.)
	JR	Z,SKPCOM	;	Yes, undefined array - just skip it.
	PUSH	DE		;<---	No, resave pointer to start of variable
SCNSMP	POP	HL		;Rescan variable name for start
	CALL	PTRGTN		;Evaluate as simple
	LD	A,D		;If var not found, [D,E]=0
	OR	E
	JR	NZ,COMFNS	;Found it
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
	JR	Z,SKPCOM	;<--> No, just skip over this variable
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
	JR	NZ,SCNSMP	;No, scan as simp
	EX	(SP),HL		;Save text pointer, get rid of saved text pointer
	DEC	BC		;Point at last char of name extension
	DEC	BC
	CALL	CBAKBL		;Back up before variable and mark as COMMON
BCKUCM	POP	HL		;Restore text pointer
SKPCOM	DEC	HL		;Rescan terminator
	CALL	CHRGTR
	JP	Z,AFTCOM	;End of COMMON statement
	CP	'('		;End of COMMON array spec?
	JR	NZ,CHKCST	;No, should be comma
COMADY	PUSH	HL		;--->
	CALL	CHRGTR		;	Fetch char after paren
	CP	')'		;
	JR	Z,COMRPN	;	Only right paren follows
	POP	HL
	CALL	EVAL		;	Possible number of dimensions(compiler compatible)
	CALL	GETYPR
	JP	Z,FCERR		;	Dimensions argument cannot be string
	JR	COMRP1

COMRPN	POP	DE		;<---
COMRP1	CALL	SYNCHR
	DB	')'		;Right paren should follow
	JP	Z,AFTCOM
CHKCST	CALL	SYNCHR
	DB	','		;Force comma to appear here
	JP	NXTCOM		;Get next COMMON variable

; 	Step 3 - Squeeze..
CLPFIN	POP	HL		;Restore previous CURLIN
	LD	(CURLIN),HL
	LD	DE,(ARYTAB)	;End of simple var squeeze to [D,E]
	LD	HL,(VARTAB)	;Start of simps
CLPSLP	CALL	COMPAR		;Are we done?
	JR	Z,DNCMDS	;Yes done, with simps
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
	JR	CLPSLP

VARDLS	EX	DE,HL		;Point to where var ends
	LD	HL,(STREND)	;One beyond last byte to move
DLSVLP	CALL	COMPAR		;Done?
	LD	A,(DE)		;Grab byte
	LD	(BC),A		;Move down
	INC	DE		;Increment pointers
	INC	BC
	JR	NZ,DLSVLP
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

DNCMDS	LD	DE,(STREND)	;Limit of array search
CLPAKP	CALL	COMPAR		;Done?
	JR	Z,DNCMDA	;Yes
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
	JR	CLPAKP		;Check next array

;	Step 4 - Copy literals into string space
;	This code is very similar to the string garbage collect code
;	If BIGSTR is on, we also have to fix up the string back pointers.
DNCMDA	LD	HL,(VARTAB)	;Look at simple strings
CSVAR	LD	DE,(ARYTAB)	;Limit of search to [D,E]
	CALL	COMPAR		;Done?
	JR	Z,CAYVAR	;Yes
	CALL	SKPNAM		;<-->	Skip name, returns Z if was a string
	JR	NZ,CSKPVA	;Skip this var, not string
	CALL	CDVARS		;Copy this guy into string space if nesc
	XOR	A		;CDVARS has already incremented [H,L]
CSKPVA	LD	E,A
	LD	D,00H		;Add length of VALTYP
	ADD	HL,DE
	JR	CSVAR

CAYVA2	POP	BC		;Adjust stack
CAYVAR	LD	DE,(STREND)	;New limit of search
	CALL	COMPAR		;Done?
	JR	Z,DNCCLS	;--->	Yes
	CALL	SKPNAM		;<---	Skip over name
	PUSH	AF
	LD	C,(HL)		;Get length of array
	INC	HL
	LD	B,(HL)		;Into [B,C]
	INC	HL
	POP	AF		;Get back VALTYP
	PUSH	HL		;Save pointer to array element
	ADD	HL,BC		;Point after array
	CP	03H		;String array?
	JR	NZ,CAYVA2	;No, look at next one
	LD	(TEMP3),HL	;Save pointer to end of array
	POP	HL		;Get back pointer to array start
	LD	C,(HL)		;Pick up number of DIMs
	LD	B,00H		;Make double with high zero
	ADD	HL,BC		;Go past DIMS
	ADD	HL,BC
	INC	HL		;One more to account for # of DIMs
CAYSTR	LD	DE,(TEMP3)	;Get end of array
	CALL	COMPAR		;See if at end of array
	JR	Z,CAYVAR	;Get next array
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
	LD	DE,(VARTAB)	;Start of simps into [D,E]
	LD	HL,(ARYTAB)
	OR	A		;--->
	SBC	HL,DE		;<--- 	Get length of simps in [H,L]
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
	JR	Z,NTMDLT	;No
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
CHNRET	CALL	UPPROF		;---> 	Update PROFLG protection flag
	LD	A,(TOPTVL)	;	RESTORE OPTION BASE VALUE
	LD	(OPTVAL),A	;
	LD	A,(TOPTFG)	;	RESTORE OPTION FLG
	LD	(OPTFLG),A	;<---
	XOR	A		;Clear CHAIN, MERGE flags
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
	JR	NZ,MVBKVR
	DEC	BC		;Point to last var byte
	LD	H,B		;[H,L]=last var byte
	LD	L,C
	LD	(STREND),HL	;This is new end
	XOR	A		;--->	allow all files to be closed
	CALL	RESTOR		;<---	Make sure DATA is valid by doing RESTORE
	LD	DE,(CHNLIN)	;Get CHAIN line # in [D,E]
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

;				;--->
;
;	Convenience routine to skip a variable's name pointed to by HL.
;	Returns VALTYP in A with the zero flag set if it is a string.
;
SKPNAM	LD	A,(HL)		;	Get VALTYP
	INC	HL		;	Point to length of long var name
	INC	HL
	INC	HL
	PUSH	AF		;	Save VALTYP
	CALL	IADAHL		;	Move past long variable name
	POP	AF		;	Get back VALTYP
	CP	03H		;	String?
	RET			;<---

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
	JR	Z,WRTFIN	;Done with WRITE
WRTMLP	CALL	FRMEVL		;Evaluate formula
	PUSH	HL		;Save the text pointer
	CALL	GETYPR		;See if we have a string
	JR	Z,WRTSTR	;We do
	CALL	FOUT		;Convert to a string
	CALL	STRLIT		;Literalize string
	LD	HL,(FACLO)	;Get pointer to string
	INC	HL		;Point to address field
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	LD	A,(DE)		;Is number positive?
	CP	' '		;Test
	JR	NZ,WRTNEG	;No, must be negative
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
	JR	Z,WRTFIN	;end
	CP	';'		;Semicolon?
	JR	Z,WASEMI	;Was one
	CALL	SYNCHR
	DB	','		;Only possib left is comma
	DEC	HL		;to compensate for later CHRGET
WASEMI	CALL	CHRGTR		;Fetch next char
	LD	A,','		;put out comma
	CALL	OUTDO
	JR	WRTMLP		;Back for more

WRTSTR	LD	A,'"'		;put out double quote
	CALL	OUTDO		;Send it
	CALL	STRPRT		;print the string
	LD	A,'"'		;Put out another double quote
	CALL	OUTDO		;Send it
	JR	NXTWRV		;Get next value

WRTFIN	CALL	CRDO		;<-->	Do crlf
	JP	FINPRT


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
	JR	NZ,CNTZER	;Still non-Zero
	LD	C,N1		;Re-initialize counter 1
CNTZER	DEC	B		;dedecrement counter-2
	JR	NZ,ENCDBL	;Still non-zero, go for more
	LD	B,N2		;Re-initialize counter 2
	JR	ENCDBL		;Keep going until done

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
	JR	NZ,CNTZR2	;Still non-Zero
	LD	C,N1		;Re-initialize counter 1
CNTZR2	DEC	B
	JP	NZ,DECDBL	;Decrement counter-2, Still non-zero, go for more
	LD	B,N2		;Re-initialize counter 2
	JR	DECDBL		;Keep going until done

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

;	Signature 2 ? (DB 0F4H,29H,53H,2AH)
SIG2	DB	0F4H,29H,53H,2AH



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
	JR	NZ,STKSRC	;No - check "FOR" as well
	LD	BC,0006H	;WHLSIZ
	ADD	HL,BC
	JR	LOOPER

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
	JR	Z,POPGOF	;POINTING TO THE VARIABLE
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
	JR	LOOPER		;Keep on looking


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
	JR	Z,ENDCNJ	;IF DIRECT DONE, ALLOW FOR DEBUGGING PURPOSES
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
	DERMAK	FAE		;File already exists
	DERMAK	DWP,0		;Disk write protected
	JR	ERROR		;Error handler, E = Error code

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
	JR	Z,ERRESM
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
	JR	Z,NTMDCN	;IF DIRECT, DONT MODIFY OLDTXT & OLDLIN
	LD	(OLDLIN),HL	;SET OLDLIN=ERRLIN.
	EX	DE,HL		;GET BACK SAVTXT
	LD	(OLDTXT),HL	;SAVE IN OLDTXT.
NTMDCN	LD	HL,(ONELIN)	;SEE IF WE ARE TRAPPING ERRORS.
	LD	A,H		;BY CHECKING FOR LINE ZERO.
	OR	L
	EX	DE,HL		;PUT LINE TO GO TO IN [D,E]
	LD	HL,ONEFLG	;POINT TO ERROR FLAG
	JR	Z,NOTRAP	;SORRY, NO TRAPPING...
	AND	(HL)		;A IS NON-ZERO, SETZERO IF ONEFLG ZERO
	JR	NZ,NOTRAP	;IF FLAG ALREADY SET, FORCE ERROR
	DEC	(HL)		;IF ALREADY IN ERROR ROUTINE, FORCE ERROR
	EX	DE,HL		;GET LINE POINTER IN [H,L]
	JP	GONE4		;GO DIRECTLY TO NEWSTT CODE

NOTRAP	XOR	A		;A MUST BE ZERO FOR CONTRO
	LD	(HL),A		;RESET ONEFLG
	LD	E,C		;GET BACK ERROR CODE
	LD	(CNTOFL),A	;FORCE OUTPUT
	IF	GFX
	CALL	TOTEXT		;<==>	Set text mode
	ENDIF
	CALL	CRDONZ		;CRLF
	LD	HL,ERRTAB	;GET START OF ERROR TABLE
	LD	A,E		;GET ERROR CODE
	CP	LSTERR		;IS IT PAST LAST ERROR?
	JR	NC,UPERR	;YES, TOO BIG TO PRINT
	CP	DSKERR		;DISK ERROR?
	JR	NC,NTDER2	;YES
	CP	NONDSK		;IS IT BETWEEN LAST NORMAL & FIRST DISK?
	JR	C,NTDERR	;YES, OK TO PRINT IT
	;Unprintable error?
UPERR	LD	A,ERRUE1	;PRINT "UNPRINTABLE ERROR"
	;Disk error: error message # = (E) - 13H (=19=50-31)
NTDER2	SUB	DSKER1		;FIX OFFSET INTO TABLE OF MESSAGES
	LD	E,A		;SAVE BACK ERROR CODE
	;Skip to end of message (using REM), get (E)th msg
NTDERR	CALL	REM
	INC	HL		;SKIP OVER THIS ERROR MESSAGE
	DEC	E		;DECREMENT ERROR COUNT
	JR	NZ,NTDERR	;Skip to end of message (using REM),
				; get (E)th msg
	PUSH	HL		;SAVE TEXT POINTER
	LD	HL,(ERRLIN)	;GET ERROR LINE NUMBER
	EX	(SP),HL		;GET BACK ERROR TEXT POINTER
ERRFIN	LD	A,(HL)		;GET 1ST CHAR OF ERROR
	CP	'?'		;PADDED ERROR?
	JR	NZ,ERRFN1	;NO,PRINT
	POP	HL		;GET LINE # OFF STACK
	LD	HL,ERRTAB
	JR	UPERR		;MAKE UNPRINTABLE ERROR

;	Print error message
ERRFN1	CALL	STROUT		;PRINT MESSAGE
	POP	HL		;RESTORE LINE NUMBER
	LD	DE,0FFFEH	;IS INIT EXECUTING?
	CALL	COMPAR
	CALL	Z,CRDO		;DO CRLF
	JP	Z,SYSTEM	;SYSTEM error exit
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
	CALL	INIT		;gets STROUT during INIT
REPINI	EQU	$-2
	LD	A,(ERRFLG)	;SEE IF IT WAS A "SYNTAX ERROR"
	SUB	02H
	CALL	Z,ERREDT	;"EDIT" THE BAD LINE
MAIN	LD	HL,0FFFFH
	LD	(CURLIN),HL	;SETUP CURLIN FOR DIRECT MODE
	LD	HL,ENDPRG	;--->
	LD	(SAVTXT),HL	;<---	SAVTXT POINTS AT FAKE END OF PROGRAM
	LD	A,(AUTFLG)	;IN AN AUTO COMMAND?
	OR	A		;SET CC'S
	JR	Z,NTAUTO	;NO, REGULAR MODE
	LD	HL,(AUTLIN)	;GET CURRENT AUTO LINE
	PUSH	HL		;SAVE AWAY FOR LATER USE
	CALL	LINPRT		;PRINT THE LINE #
	POP	DE		;GET IT BACK
	PUSH	DE		;SAVE BACK AGAIN
	CALL	FNDLIN		;SEE IF IT EXISTS
	LD	A,'*'		;CHAR TO PRINT IF LINE ALREADY EXISTS
	JR	C,AUTELN	;DOESN'T EXIST
	LD	A,' '		;PRINT SPACE
AUTELN	CALL	OUTDO		;PRINT CHAR
	CALL	PINLIN		;GET PROGRAM LINE INPUT
	POP	DE
	JR	NC,NTSTOP
	XOR	A
	LD	(AUTFLG),A	;IN CASE OF AUTO, CLEAR IT
	JR	READY

;	TODO: label
NTSTOP0	XOR	A
	LD	(AUTFLG),A	;IN CASE OF AUTO, CLEAR IT
	JR	NTSTOP1

;	TODO: label
NTSTOP	LD	HL,(AUTINC)	;GET INCREMENT
	ADD	HL,DE		;ADD INCREMENT TO THIS LINE
	JR	C,NTSTOP0	;CHECK FOR PATHETIC CASE
	PUSH	DE		;SAVE LINE NUMBER #
	LD	DE,0FFFAH	;CHECK FOR LINE # TOO BIG (0FFF9 in MBASIC)
	CALL	COMPAR
	POP	DE		;GET BACK LINE #
	JR	NC,NTSTOP0	;IF TOO BIG, QUIT
	LD	(AUTLIN),HL	;SAVE IN NEXT LINE
;	SET NON-ZERO CONDITION CODES (SEE EDIT)
NTSTOP1	LD	A,(BUF)		;GET CHAR FROM BUFFER
	OR	A		;IS IT NULL LINE?
	JR	Z,MAIN		;YES, LEAVE LINE ALONE
	JP	EDTSTL		;JUMP INTO EDIT CODE

NTAUTO	CALL	PINLIN		;GET PROGRAM LINE INPUT
	JR	C,MAIN
	PUSH	AF		;--->
	PUSH	BC
	PUSH	DE
	PUSH	HL
	LD	C,0F4H
	CALL	RSKFLG		;	Reset KFLAG$ status with mask in C
	POP	HL
	POP	DE
	POP	BC
	POP	AF		;<---
	CALL	CHRGTR		;GET THE FIRST
	INC	A		;SEE IF 0 SAVING THE CARRY FLAG
	DEC	A
	JR	Z,MAIN		;IF SO, A BLANK LINE WAS INPUT
	PUSH	AF		;SAVE STATUS INDICATOR FOR 1ST CHARACTER
	CALL	LINGET		;READ IN A LINE #
	JR	NC,MAINBX	;--->	ANOTHER DIGIT AFTER LINE#?
	CALL	ISFLIO		;	 AND COMING FROM TERMINAL?
	JP	Z,SNERR		;<---	IF SO, BAD INPUT
MAINBX	CALL	BAKSP		;BACK UP THE POINTER
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
	LD	(DOT),DE	;SAVE THIS LINE # IN DOT
	CALL	FNDLIN		;GET A POINTER TO THE LINE
	JR	C,INONLY	;LINE EXISTS, DELETE IT
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
	CALL	C,DEL		;Delete the line
	POP	DE		;POP POINTER AT PLACE TO INSERT
	POP	AF		;SEE IF THIS LINE HAD
				;ANYTHING ON IT
	PUSH	DE		;SAVE PLACE TO START FIXING LINKS
	JR	Z,FINI		;IF NOT DON'T INSERT
	POP	DE		;GET RID OF START OF LINK FIX
	LD	A,(CHNFLG)	;ONLY CHANGET FRETOP IF NOT CHAINING
	OR	A
	JR	NZ,LEVFRE	;LEAVE FRETOP ALONE
	LD	HL,(MEMSIZ)	;DELETE ALL STRINGS
	LD	(FRETOP),HL	;SO REASON DOESNT USE THEM
LEVFRE	LD	HL,0000H	;--->	Clear error trap before inserting
	LD	(ONELIN),HL	;<---	 to prevent OM error from being trapped
	LD	HL,(VARTAB)	;CURRENT END
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
	JR	NZ,MLOOPR
FINI	POP	DE		;GET START OF LINK FIXING AREA
	CALL	CHEAD		;FIX LINKS
	LD	HL,DIRTMP	;DON'T ALLOW ZERO TO BE CLOSED
	LD	(HL),00H	;NOT SEQUENTIAL OUTPUT
	LD	(FDBTAB),HL
	LD	HL,(PTRFIL)	;GET FILE POINTER, COULD BE ZERO
	LD	(TEMP2),HL	;SAVE IT
	LD	A,H		;--->
	OR	L		;
	JR	Z,FINI11	;	TODO
	LD	A,(NFILES)	;	Number of files
	LD	(NFILSSV),A	;	temp save NFILES
	XOR	A
	LD	(NFILES),A	;<---	Number of files
FINI11	CALL	RUNC		;DO CLEAR & SET UP STACK
	LD	HL,(FILPT1)	;RESET [FILPTR]
	LD	(FDBTAB),HL
	LD	HL,(TEMP2)	;RESET [PTRFIL]
	LD	(PTRFIL),HL
	LD	A,H		;--->	TODO
	OR	L
	JR	Z,FINI12
	LD	A,(NFILSSV)	;	temp save NFILES
	LD	(NFILES),A	;<--- 	Number of files
FINI12	JP	MAIN		;GO TO MAIN CODE


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
	JR	Z,CZLIN		;END OF LINE, DONE.
	CP	DBLCON+1	;EMBEDDED CONSTANT?
	JR	NC,CZLOOP	;NO, GET NEXT
	CP	0BH		;IS IT LINEFEED OR BELOW?
	JR	C,CZLOOP	;THEN SKIP PAST
	CALL	CHRGT2		;GET CONSTANT
	CALL	CHRGTR		;GET OVER IT
	JR	CZLOO2		;GO BACK FOR MORE
CZLIN	INC	HL		;MAKE [H,L] POINT AFTER TEXT
	EX	DE,HL		;SWITCH TEMP
	LD	(HL),E		;STORE FIXUP
	INC	HL
	LD	(HL),D
	JR	CHEAD		;KEEP CHAINING TIL DONE

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
	JR	Z,ALLLST	;IF FINISHED, LIST IT ALL
	POP	DE		;WE ARE GOING TO GRAB A #
	CALL	LINSPC		;GET A LINE #. IF NONE, RETURNS ZERO
	PUSH	DE		;SAVE FIRST
	JR	Z,SNGLIN	;IF ONLY # THEN DONE.
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
	JR	LOOP		;KEEP LOOPING

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
	JR	NZ,NCRDON	;NO, CONTINUE
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
	JR	Z,STUFFH	;JUST STUFF AWAY
	LD	A,(DORES)	;IN DATA STATEMENT AND NO CRUNCH?
	OR	A
	LD	A,(HL)		;GET THE CHARACTER AGAIN
	JR	Z,NTDATA	;IF NO CRUNCHING JUST STORE
				;THE CHARACTER
STUFFH	INC	HL		;ENTRY TO BUMP [H,L]
	PUSH	AF		;SAVE CHAR AS KRNSAV CLOBBERS
	CALL	KRNSAV		;SAVE CHAR IN KRUNCH BUFFER
	POP	AF		;RESTORE CHAR
	SUB	':'		;SEE IF IT IS A COLON
	JR	Z,COLIS		;IF SO ALLOW CRUNCHING AGAIN
	CP	DATATK-':'
	JR	NZ,NODATT	;SEE IF IT IS A DATA TOKEN
	LD	A,01H		;SET LINE NUMBER ALLOWED FLAG
				;KLUDGE AS HAS TO BE NON-ZERO.
COLIS	LD	(DORES),A	;SETUP FLAG
	LD	(DONUM),A	;SET NUMBER ALLOWED FLAG
NODATT	SUB	REMTK-':'
	JR	NZ,KLOOP	;KEEP LOOPING
	PUSH	AF		;SAVE TERMINATOR ON STACK
STR1	LD	A,(HL)		;GET A CHAR
	OR	A		;SET CONDITION CODES
	EX	(SP),HL		;GET AL BACK WITHOUT AFFECTING PSW
	LD	A,H
	POP	HL
	JR	Z,CRDONE	;IF END OF LINE THEN DONE
	CP	(HL)		;COMPARE CHAR WITH THIS TERMINATOR
	JR	Z,STUFFH	;IF YES, DONE WITH STRING
STRNG	PUSH	AF		;SAVE TERMINATOR
	LD	A,(HL)		;GET BACK LINE CHAR
STRNG2	INC	HL		;INCREMENT TEXT POINTER
	CALL	KRNSAV		;SAVE CHAR IN KRUNCH BUFFER
	JR	STR1		;KEEP LOOPING

;	Now check hghbit chars, ? for PRINT
NTDATA	INC	HL		;--->	IF SO SKIP IT
	OR	A		;	IS THIS A KANA CHARACTER IN A BAD PLACE?
	JP	M,KLOOP		;	MOVE TO THE NEXT CHARACTER
	DEC	HL		;<---	RESTORE THE TEXT POINTER
	CP	'?'		;A QMARK?
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
	JR	NZ,NOTGOS	;NOPE
	CALL	CHRGTR		;SKIP ANY NUMBER OF SPACES
	LD	DE,TOTEX	;IS IT TO?
	CALL	CHKRES		;CHECK
	LD	A,GOTOTK	;ASSUME SO
	JP	Z,GPUTRS	;USE IT
	LD	DE,SUBTEX	;"GO SUB"
	CALL	CHKRES
	JR	NZ,NOTGOS	;NO
	LD	A,GOSUBTK
GPUTRS	POP	BC		;POP OFF THE OLD TEXT POINTER
	JP	NOTFNT		;STORE THE RESERVED WORD

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
	JR	CHKRES		;LOOP TILL DONE

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
	JR	NZ,LOPSKP	;IF NO MATCH, SEARCH FOR NEXT RESWRD
	LD	A,(DE)		;FETCH FROM CODE SEGMENT
	INC	DE		;BUMP RESLST POINTER
	OR	A		;SET CC'S
	JP	P,LOPPSI	;SEE IF REST OF CHARS MATCH
	LD	A,C		;GET LAST CHAR OF RESWRD
	CP	28H		;IF TAB( OR SPC(, SPACE NEED NOT FOLLOW
	JR	Z,ISRESW	;IS A RESWORD
	LD	A,(DE)		;FETCH FROM CODE SEGMENT
	CP	FNTK		;FUNCTION?
	JR	Z,ISRESW	;THEN NO SPACE NEED AFTERWARD
	CP	USRTK		;OR USR DEFINITION?
	JR	Z,ISRESW
	CALL	MAKUPL		;GET NEXT CHAR IN LINE (MC 6/22/80)
	CP	'.'		;IS IT A DOT
	JR	Z,ISVARS	;YES
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
	JR	Z,NOTFN4	;YES, DONE
	INC	DE		;POINT TO NEXT RESWORD
	CP	C		;SAME AS ONE WERE LOOKING AT?
	JR	NZ,NOTFN3	;KEEP LOOKING
	JR	NOTRS2		;DOESNT HAVE LINE # ARG

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
	JR	NZ,CKSNGQ	;Brif not
	CALL	KRNSAV		;Emit WHILE Token, then Plus (+)
	LD	A,PLUSTK	;PLUS SIGN IS OK AND AVOIDS CONSTANT
CKSNGQ	CP	SNGQTK		;SINGLE QUOATATION MARK?
	JP	NZ,NTSNGT
	PUSH	AF		;SAVE SNGQTK
	CALL	KRNSVC		;SAVE ":" IN CRUNCH BUFFER
	LD	A,REMTK		;STORE ":$REM" IN FRONT FOR EXECUTION
	CALL	KRNSAV		;SAVE IT
	POP	AF		;GET SNGQTK BACK
	PUSH	HL		;--->	save text pointer
	LD	HL,0000H	;
	EX	(SP),HL		;<---	Save terminator (0), restore Txt Ptr
	JP	STRNG2		;STUFF THE REST OF THE LINE WITHOUT CRUNCHING

TSTNUM	LD	A,(HL)		;GET CHAR
	CP	'.'		;TEST FOR START OF FLOATING #
	JR	Z,NUMTRY	;TRY INPUTTING IT AS CONSTANT
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
	JR	Z,FLTGET	;IF DONUM=0 THEN FLOATING #'S ALLOWED
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
	JR	NZ,NTINTG	;NO
	LD	HL,(FACLO)	;GET IT
	LD	A,H		;GET HIGH PART
	OR	A		;IS IT ZERO?
	LD	A,02H		;RESTORE INT VALTYP
	JR	NZ,NTINTG	;THEN ISNT SINGLE BYTE INT
	LD	A,L		;GET LOW BYTE
	LD	H,L		;GET LOW BYTE IN HIGH BYTE TO STORE
	LD	L,0FH		;GET CONSTANT FOR 1 BYTE INTS
	CP	0AH		;IS IT TOO BIG FOR A SINGLE BYTE CONSTANT?
	JR	NC,SAVI		;TOO BIG, USE SINGLE BYTE INT
	ADD	A,ONECON	;MAKE SINGLE BYTE CONSTANT
	JR	POPSTF		;POP H & STUFF AWAY CHAR

NTINTG	PUSH	AF		;SAVE FOR LATER
	RRCA			;DIVIDE BY TWO
	ADD	A,1BH		;ADD OFFSET TO GET TOKEN
	CALL	KRNSAV		;SAVE THE TOKEN
	LD	HL,FACLO	;GET START POINTER
	CALL	GETYPR		;SET CC'S ON VALTYPE
	JR	C,NTDBL		;IF NOT DOUBLE, START MOVING AT FACLO
	LD	HL,DFACLO	;DOUBLE, START MOVING AT DFACLO
NTDBL	POP	AF
MOVCON	PUSH	AF		;SAVE BYTE MOVE COUNT
	LD	A,(HL)		;GET A BYTE
	CALL	KRNSAV		;SAVE IT IN KRUNCH BUFFER
	POP	AF		;GET BACK COUNT
	INC	HL		;BUMP POINTER INTO FAC
	DEC	A		;MOVE IT DOWN
	JR	NZ,MOVCON	;KEEP MOVING IT
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
	JR	NZ,SRCSP2	;IF NO MATCH, KEEP LOOKING
	JP	NOTRS1		;FOUND, SAVE AWAY AND SET DONUM=1.

NTSNGT	CP	'&'		;OCTAL CONSTANT, '&POINT' OR '&VIEW'?
	JP	NZ,STUFFH	;JUST STUFF IT AWAY
	IF	GFX
	PUSH	BC		;===>	HERE TO CHECK FOR '&POINT' AND '&VIEW'
	PUSH	DE		;	SAVE REGISTERS
	PUSH	HL		;	SAVE TEXT POINTER
	LD	DE,POINT$	;	CHECK FOR 'POINT'
	INC	HL		;	SKIP '&'
	CALL	CHKRES		;	CHECK
	JR	NZ,NTFPNT	;	NOPE, GO CHECK FOR 'VIEW'
	LD	A,FPOINTTK	;	USE &POINT TOKEN
	JR	GOTTOK		;	GO STORE TOKEN

FPOINT$	DB	'&'		;	PREFIX '&' FOR BUFLIN
POINT$	DB	'POINT',00H	;	'POINT' KEYWORD

NTFPNT	POP	HL		;	GET BACK TEXT POINTER
	PUSH	HL		;	SAVE IT BACK
	LD	DE,VIEW$	;	CHECK FOR 'VIEW'
	INC	HL		;	SKIP '&'
	CALL	CHKRES		;	CHECK
	JR	NZ,NTFVEW	;	NOPE, GO CHECK FOR &O AND &H
	LD	A,FVIEWTK	;	USE &VIEW TOKEN
GOTTOK	POP	DE		;	DISCARD SAVED TEXT POINTER
	POP	DE		;	RESTORE REGISTERS
	POP	BC		;
	CALL	KRNSAV		;	SAVE TOKEN
	JP	KLOOP		;	KEEP KRUNCHING

FVIEW$	DB	'&'		;	PREFIX '&' FOR BUFLIN
VIEW$	DB	'VIEW',00H	;	'VIEW' KEYWORD

NTFVEW	POP	HL		;	CHECK FOR &O AND &H
	POP	DE		;	RESTORE TEXT POINTER
	POP	BC		;<===	 AND REGISTERS
	ENDIF
	PUSH	HL		;SAVE TEXT POINTER
	CALL	CHRGTR		;GET NEXT CHAR
	POP	HL		;RESTORE TEXT POINTER
	CALL	MAKUPS		;MAKE CHAR UPPER CASE
	CP	'H'		;HEX CONSTANT?
	LD	A,OCTCON	;ASSUME OCTAL CONSTANT
	JR	NZ,WUZOCT	;YES, IT WAS
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
	JR	NC,KRNVAR	;YES, EAT
	CP	':'		;DIGIT?
	JR	NC,JKLOOP	;NO, TOO LARGE
	CP	'0'
	JR	NC,KRNVAR	;YES, EAT
	CP	'.'		;IS IT DOT
	JR	Z,KRNVAR	;YES, DOTS OK IN VAR NAMES
JKLOOP	JP	KLOOP		;DONE LOOKING AT VARIABLE NAME

NOTRS5	LD	A,(HL)		;GET CHAR FROM LINE
	CP	' '		;SPACE OR HIGHER ?
	JR	NC,NOTRS1	;YES = SAVE IT
	CP	09H		;TAB ?
	JR	Z,NOTRS1	;YES = THAT'S OK
	CP	0AH		;ALSO ALLOW...
	JR	Z,NOTRS1	;...LINE FEEDS
	LD	A,' '		;FORCE REST TO SPACES
NOTRS1	PUSH	AF		;SAVE THIS CHAR
	LD	A,(DONUM)	;GET NUMBER OK FLAG
	INC	A		;SEE IF IN A VARIABLE NAME.
	JR	Z,NOTRS11	;IF SO & SPECIAL CHAR SEEN, RESET DONUM
	DEC	A		;OTHERWISE LEAVE DONUM UNCHANGED.
NOTRS11	JP	NOTRS6

; 	Routine to back up pointer after # eaten
BAKSP	DEC	HL		;POINT TO PREVIOUS CHAR
	LD	A,(HL)		;GET THE CHAR
	CP	' '		;A SPACE?
	JR	Z,BAKSP		;YES, KEEP BACKING UP
	CP	09H		;TAB?
	JR	Z,BAKSP		;YES, BACK UP
	CP	0AH		;LF?
	JR	Z,BAKSP
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
	LD	(TEMP),DE	;AND IN TEMP
				; FOR USE LATER ON
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
	JR	NZ,NOTOL	;IF NO MATCHING ENTRY, DON'T
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
	JR	NZ,LPFORM	;KEEP SEARCHING IF NO MATCH
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
	JR	STPSGN		;FINISH UP THE ENTRY
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
	JR	NZ,ONEON	;PUSH SOME CONSTANTS ON IF NOT
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
	OR	A		;Is the sign of the step zero?
	JR	NZ,NT0STP	;No, then the step is not zero.
	LD	A,02H		;Yes, force an infinite loop by making
				;the sign of step a value such that
				;the loop termination test can never
				;be met.
NT0STP	LD	C,A		;[C]=SIGN OF STEP
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
NEWSTT	CALL	ISCNTC		;<-->	DOS "GET CONSOLE STATUS"
	LD	(SAVTXT),HL	;Save code address for break
				;USED BY CONTINUE AND INPUT AND
				; CLEAR AND PRINT USING
	LD	(SAVSTK),SP	;--->	SAVE STACK POINTER TO
				;<--- 	REMEMBER HOW TO RESTART
	LD	A,(HL)		;GET CURRENT CHARACTER
				; WHICH TERMINATED THE LAST STATEMENT
	CP	':'		;IS IT A COLON?
	JR	Z,GONE		;Yes - Execute Multi statement line
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
	JR	Z,NOTTRC	;SKIP THIS PRINTING
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
	CP	SOUNDTK+1-ENDTK	;END to SOUND ?
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
	JR	Z,CHRGTR	;GET ANOTHER CHARACTER
	JR	NC,NOTLFT	;SOME FUNNY THING
	OR	A		;NULL AT EOL?
	RET	Z		;YES, ALL DONE
	CP	OCTCON		;IS IT INLINE CONSTANT?
	JR	C,NOTCON	;NO, SHOULD BE TAB OR LF
	CP	CONCON		;ARE WE TRYING TO RE-SCAN A CONSTANT?
	JR	NZ,NTRSCC	;NO.
	LD	A,(CONSAV)	;GET THE SAVED CONSTANT TOKEN
	OR	A		;SET NON-ZERO, NON CARRY CC'S
	RET			;ALL DONE

NTRSCC	CP	CONCN2		;GOING TO SCAN PAST EMBEDDED CONSTANT?
	JR	Z,CONSCN	;YES SCAN AND GO ON
	PUSH	AF		;SAVE TOKEN TO RETURN
	INC	HL		;POINT TO NUMBER
	LD	(CONSAV),A	;SAVE CURRENT TOKEN
	SUB	INTCON		;IS IT LESS THAN INTEGER CONSTANT?
	JR	NC,MAKTKN	;NO, NOT LINE NUMBER CONSTANT
	SUB	ONECON-INTCON	;<ONECON-INTCON>&^O377
				;LESS THAN EMBEDDED 1 BYTER
	JR	NC,ONEI		;WAS ONE BYTER
	CP	IN2CON-ONECON	;IS IT TWO BYTER?
	JR	NZ,FRCINC	;NOPE, NORMAL INT
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
	JR	ONEI2		;FINISH SCANNING

CONFAC	CALL	CONFC1		;SCAN FLOATING CONSTANT
CONSCN	LD	HL,(CONTXT)	;GET SAVED TEXT POINTER
	JR	CHRGT2		;AND SCAN THING AFTER CONSTANT

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
	JR	NC,NTLINE	;NO
	CP	PTRCON		;LINE POINTER CONSTANT?
	JR	C,NTLINE	;NO
	LD	HL,(CONLO)	;GET VALUE
	JR	NZ,FLTLIN	;MUST BE LINE NUMBER, NOT POINTER
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
	JR	Z,CONFDB	;YES
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
	JR	NZ,NOTRNG	;IF NOT, JUST ONE LETTER
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
	JR	NZ,LPDCHG
	POP	HL		;GET BACK THE TEXT POINTER
	LD	A,(HL)		;GET LAST CHARACTER
	CP	','		;IS IT A COMMA?
	RET	NZ		;IF NOT STATEMENT SHOULD HAVE ENDED
	CALL	CHRGTR		;OTHERWISE SET UP TO SCAN NEW RANGE
	JR	DEFCON

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
	LD	DE,(DOT)	;GET CURRENT LINE #
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
	JR	Z,LINGT3	;YES, RETURN DOUBLE BYTE VALUE
	CP	PTRCON		;ALSO CHECK FOR POINTER
LINGT3	LD	DE,(CONLO)	;GET EMBEDDED LINE #
	JP	Z,CHRGTR	;EAT FOLLOWING CHAR
	XOR	A		;SET FLAG THAT NO CONSTANT WAS SEEN SO
	LD	(CONSAV),A	;GOTO2 DOESN'T CHANGE LINCON TO PTRCON
	LD	DE,0000H	;ZERO ACCUMULATED LINE #
	DEC	HL		;BACK UP POINTER
MORLIN	CALL	CHRGTR
	RET	NC
	PUSH	HL
	PUSH	AF
	LD	HL,6552		;SEE IF THE LINE # IS TOO BIG
	CALL	COMPAR
	JR	C,POPHSR	;YES, DON'T SCAN ANY MORE DIGITS IF SO
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
	JR	MORLIN

POPHSR	POP	AF		;GET OFF TERMINATING DIGIT
	POP	HL		;GET BACK OLD TEXT POINTER
	RET


;-----------------------------------------------------------------------------
;	RUN, GOTO, GOSUB, RETURN, DATA, REM
; ## GWMAIN.ASM:2016 ##
;
RUN	JP	Z,RUNC		;NO LINE # ARGUMENT
	CP	LINCON		;LINE NUMBER CONSTANT?
	JR	Z,CONRUN	;YES
	CP	PTRCON		;LINE POINTER (RATHER UNLIKELY)
	JP	NZ,LRUN
CONRUN:				;CLEAN UP,SET [H,L]=[TXTTAB]-1 AND
				;RETURN TO NEWSTT
	CALL	CLEARC		;CLEAN UP -- RESET THE STACK
				;DATPTR,VARIABLES ...
				;[H,L] IS THE ONLY THING PRESERVED
	LD	BC,NEWSTT
	JR	RUNC2		;PUT "NEWSTT" ON AND FALL INTO "GOTO"

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
	JR	GOTO2		;HAVE NOW GRAB LINE # PROPERLY

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
	JR	NC,USERR	;LINE NOT FOUND, DEATH
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
	CP	GOSUBTK		;---> see if matching GOSUB found
	JR	Z,RTRNOK	;branch if so
	DEC	HL		;<--- else error, reset stack pointer
RTRNOK	LD	SP,HL		;UPDATE THE STACK
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
	JR	Z,EXCHQT	;IF SO TIME TO TRADE
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
	JR	Z,REMER1	;THEN IGNORE FOLLOWING FN NUMBER
	SUB	IFTK+1		;IS IT AN "IF"
	JR	NZ,REMER	;IF NOT, CONTINUE ON
	CP	B		;SINCE "REM" CAN'T SMASH
				;[D,E] WE HAVE TO BE CAREFUL
				;SO ONLY IF B DOESN'T EQUAL
				;ZERO WE INCREMENT D. (THE "IF" COUNT)
	ADC	A,D		;CARRY ON IF [B] NOT ZERO
	LD	D,A		;UPDATE [D]
	JR	REMER


;-----------------------------------------------------------------------------
;	"LET"
; ## GWMAIN.ASM:2267 ##
;
;	LETCON is LET entry point with VALTYP-3 in [A]
;	because GETYPR has been called
LETCON	POP	AF		;GET VALTYPE OFF STACK
	ADD	A,03H		;MAKE VALTYPE CORRECT
	JR	LETCN2		;CONTINUE

LET	CALL	PTRGET		;GET THE POINTER TO THE VARIABLE
				;NAMED IN TEXT AND PUT
				;IT INTO [D,E]
	CALL	SYNCHR
	DB	EQULTK		;CHECK FOR "="
				;MUST SET UP TEMP FOR "FOR"
	LD	(TEMP),DE	; UP HERE SO WHEN USER-FUNCTIONS
				; CALL REDINP, TEMP DOESN'T GET CHANGED
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
	JR	Z,LETCN5	;VALTYPE ALREADY SET UP, GO!
	CALL	DOCNVF		;FORCE VALTPES TO BE [A]'S
LETCN4	LD	A,(VALTYP)	;GET VALTYPE
LETCN5	LD	DE,FACLO	;ASSUME THIS IS WHERE TO START MOVEING
	CP	05H		;IS IT?
	JR	C,LETCN6	;YES
	LD	DE,DFACLO	;NO, USE D.P. FAC
LETCN6	PUSH	HL		;SAVE THE POINTER AT THE VALUE POSITION
	CP	03H		;STRING?
	JR	NZ,COPNUM	;NUMERIC, SO FORCE IT AND COPY
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
	JR	NC,INBUFC	;GO COPY, IF DATA REALLY IS IN BUF
	LD	HL,(STREND)	;SEE IF IT POINTS INTO STRING SPACE
	CALL	COMPAR		;IF NOT DON'T COPY
	POP	DE		;GET BACK THE POINTER AT THE DESCRIPTOR
	JR	NC,DNTCPY	;DON'T COPY LITERALS
	LD	HL,DSCTMP
	CALL	COMPAR		;IS THE DESCRIPTOR A TEMP?
				;NO, MUST POINT TO VARIABLE(COPY IT!)
	JR	NC,DNTCPY	;YES, DON'T COPY
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
	JR	NZ,NTOERR	;NO.
	CALL	CHRGTR		;GET NEXT THING
	CALL	SYNCHR
	DB	GOTOTK		;MUST HAVE ...GOTO
	CALL	LINGET		;GET FOLLOWING LINE #
	LD	A,D		;IS LINE NUMBER ZERO?
	OR	E
	JR	Z,RESTRP	;IF ON ERROR GOTO 0, RESET TRAP
	CALL	FNDLN1		;SEE IF LINE EXISTS (SAVE [H,L] ON STACK)
	LD	D,B		;GET POINTER TO LINE IN [D,E]
	LD	E,C		;(LINK FIELD OF LINE)
	POP	HL		;RESTORE [H,L]
	JP	NC,USERR	;ERROR IF LINE NOT FOUND
RESTRP				;SAVE POINTER TO LINE OR ZERO IF 0.
	LD	(ONELIN),DE
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
	JR	Z,ISGOSU	;YES, SOME FEATURE USE
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
	JR	LOOPON		;CONTINUE GOBBLING LINE #S


;-----------------------------------------------------------------------------
;	RESUME, ERROR STATEMENT CODE
; ## GWMAIN.ASM:2470 ##
;
RESUME	LD	A,(ONEFLG)	;Get flag
	OR	A		;Were we called by an ON ERROR?
	JR	NZ,??L057	;Not in an ON ERROR routine
	LD	(ONELIN),A	;---> Clear ON ERROR GOTO line
	LD	(ONELIN+1),A	;
	JP	REERR		;RESUME without error
				;<---
??L057	INC	A
	LD	(ERRFLG),A	;CLEAR ERROR FLAG SO ^C DOESN'T GIVE ERROR
	LD	(DOSERR),A	;<-->
	LD	A,(HL)		;GET CURRENT CHAR BACK
	CP	NEXTTK		;RESUME NEXT?
	JR	Z,RESNXT	;YUP.
	CALL	LINGET		;GET FOLLOWING LINE #
	RET	NZ		;SHOULD TERMINATE
	LD	A,D		;IS LINE NUMBER ZERO?
	OR	E		;Yep, go set non-zero CC's
	JR	Z,RES0		;---> Yep, go set non-zero CC's
	CALL	GOTO2		;Go find line
	XOR	A		;Now zero flag (otherwise RESUME non-
				; existent line number would be trapped
				; by the ON ERROR routine and we would
				; loop infinitely).
	LD	(ONEFLG),A
	JR	RET618F		;<--- (why not just a RET?)

RESNXT	CALL	CHRGTR		;MUST TERMINATE
	RET	NZ		;BLOW HIM UP
	JR	RESTXT		;--->

RES0	XOR	A
	LD	(ONEFLG),A	;	clear RESUME-TRAP flag
	INC	A		;<---	Set nonzero CC's
RESTXT	LD	HL,(ERRTXT)	;GET POINTER INTO LINE.
	EX	DE,HL		;SAVE ERRTXT IN [D,E]
	LD	HL,(ERRLIN)	;GET LINE #
	LD	(CURLIN),HL	;SAVE IN CURRENT LINE #
	EX	DE,HL
	JR	NZ,RET618F	;GO TO NEWSTT IF JUST "RESUME"
				; (why not just a RET NZ?)
	LD	A,(HL)		;GET ":" OR LINE HEADER
	OR	A		;SET CC
	JR	NZ,NOTBGL	;#0 MEANS MUST BE ":"
	INC	HL		;SKIP HEADER
	INC	HL
	INC	HL
	INC	HL
NOTBGL	INC	HL		;POINT TO START OF THIS STATEMENT
	XOR	A		;--->	Now zero flag (otherwise RESUME non-
				;	 existent line number would be trapped
				;	 by the ON ERROR routine and we would
				;	 loop infinitely).
	LD	(ONEFLG),A
	CALL	DATA		;	GET NEXT STMT
RET618F	RET			;<--- 	(why not just a JP DATA?)

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
	JR	Z,SNGAUT	;IF END OF COMMAND USE 10,10
	CALL	LINSPC		;GET LINE #, ALLOW USE OF . FOR CURRENT LINE
	EX	DE,HL		;GET TXT PTR IN [D,E]
	EX	(SP),HL		;PUT INIT ON STACK, GET 10 IN [H,L]
	JR	Z,SNGAU1	;IF TERMINATOR, USE INC OF 10
	EX	DE,HL		;GET TEXT PTR BACK IN [H,L]
	CALL	SYNCHR
	DB	','		;COMMA MUST FOLLOW
	LD	DE,(AUTINC)	;GET PREVIOUS INC
	JR	Z,SNGAUT	;USE PREVIOUS INC IF TERMINATOR
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
	JR	Z,OKGOTO
	CALL	SYNCHR
	DB	THENTK		;MUST HAVE A THEN
	DEC	HL
OKGOTO	PUSH	HL		;SAVE THE TEXT POINTER
	CALL	VSIGN
	POP	HL		;GET BACK THE TEXT POINTER
	JR	Z,FALSIF	;HANDLE POSSIBLE "ELSE"
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
	JR	NZ,SKPMRF	;NO, STILL IN THE "THEN" CLAUSE
	DEC	D		;DECREMENT THE NUMBER OF "ELSE"S THAT
				;MUST BE SEEN
	JR	NZ,SKPMRF	;SKIP MORE IF HAVEN'T SEEN
				;ENOUGH
	JR	DOCOND		;FOUND THE RIGHT "ELSE" -- GO EXECUTE




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
	CP	'@'
	CALL	Z,PRINT@
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
	JR	Z,COMPRT	;Print Comma
	CP	';'		;is it a ";"
	JP	Z,NOTABR
	POP	BC		;get rid of old text pointer
	CALL	FRMEVL		;evaluate the formula
	PUSH	HL		;save text pointer
	CALL	GETYPR		;see if we have a string
	JR	Z,STRDON	;if so, print special
	CALL	FOUT		;make a number into a string
	CALL	STRLIT		;make it a string
	LD	(HL),' '	;put a space at the end
	LD	HL,(FACLO)	;AND INCREASE SIZE BY 1
	INC	(HL)		;SIZE BYTE IS FIRST IN DESCRIPTOR
;	Output string contents (a.k.a. PRNTST)
;	USE FOLDING FOR STRINGS AND #S
STRDON	CALL	ISFLIO		;DISK OUTPUT?  IF SO, DON'T EVER FORCE A CRLF
	JR	NZ,LINCH2
	LD	HL,(FACLO)	;GET THE POINTER
	LD	A,(PRTFLG)
	OR	A
	JR	Z,ISTTY		;LPT OR TTY?
	LD	A,(LPTSIZ)	;GET WIDTH OF PRINTER
	LD	B,A		;SAVE IN [B]
	INC	A		;IS IT INFINITE? (255="infinite")
	JR	Z,LINCH2	;THEN JUST PRINT
	LD	A,(LPTPOS)	;Get cursor position
	OR	A		;DON'T DO A CRLF IF STRING LONGER THAN LINE
	JR	Z,LINCH2	;LENGTH IF POSITION IS 0
	ADD	A,(HL)		;Add length of string
	CCF			;SET NC IF OVERFLOW ON CHECK
	JR	NC,LINCHK	;START ON A NEW LINE
	DEC	A		;Adjust it
	CP	B		;Will output fit on this line?
	JR	LINCHK

ISTTY	LD	A,(TTYSIZ)	;Get width of line
	LD	B,A		;To B
	INC	A		;NO OVERFLOW LINE WIDTH?
	JR	Z,LINCH2	;YES
	LD	A,(TTYPOS)	;SEE WHERE WE ARE
	OR	A		;don't CR if string longer than line
	JR	Z,LINCH2	;  length if position is 0
	ADD	A,(HL)		;[AL]=column + string size
	CCF			;set nc if overflow on check
	JR	NC,LINCHK	;start on a new line if overflow
	DEC	A
	CP	B		;check for overlap
				;branch if still on current line
LINCHK	CALL	NC,CRDO		;else output CR
LINCH2	CALL	STRPRT		;PRINT THE string/number
	POP	HL		;restore text pointer
	JP	NEWCHR		;print some more

;	PRINT comma (text pointer stacked)
;
COMPRT	LD	BC,0007H	;(NMLO.C) if file output, SPECIAL PRINT
				; POSITION SHOULD BE FETCHED FROM FILE DATA
	LD	HL,(PTRFIL)
	ADD	HL,BC		;[H,L] POINT AT POSITION
	CALL	ISFLIO		;OUTPUTING INTO A FILE?
	LD	A,(HL)		;IF FILE IS ACTIVE
	JR	NZ,COMPRT3
	LD	A,(PRTFLG)	;OUTPUT TO THE LINE PRINTER?
	OR	A		;NON-ZERO MEANS YES
	JR	Z,COMPRT1	;NO, DO TELETYPE COMMA
	LD	A,(NLPPOS)	;Get comma width
	LD	B,A		;Save in B
	INC	A		;TEST
	LD	A,(LPTPOS)	;GET LINE PRINTER POSITION
	JR	Z,COMPRT3	;ALWAYS DO MODULUS IF WIDTH=255
	CP	B		;CHECK IF NO MORE COMMA FIELDS
	JP	COMPRT2		;USE TELETYPE CHECK

COMPRT1	LD	A,(NTTPOS)	;POSITION BEYOND WHICH
	LD	B,A		; THERE ARE NO MORE COMMA FIELDS
	LD	A,(TTYPOS)	;[AL]=file's current column position
	CP	0FFH		;infinite width?
	JR	Z,COMPRT3	;do modulus
	CP	B		;compare current with last comma column
				;branch if not beyond last comma col
COMPRT2	CALL	NC,CRDO		;start new line
	JP	NC,NOTABR	;AND QUIT IF BEYOND LAST COMMA FIELD
COMPRT3	SUB	CLMWID		;[AL]=MODULUS CLMWID
	JR	NC,COMPRT3
	CPL			;fill the print position out
				;to an even CLMWID, so
				;we print CLMWID-[AL] MOD CLMWID spaces
	JR	ASPA2		;go print [AL]+1 spaces

;	PRINT TAB(N) & SPC(N)
;
TABER	PUSH	AF		;remember IF [A]=SPCTK or TABTK
	CALL	CHRGTR
	CALL	GETIN2		;EVALUATE THE ARGUMENT
	POP	AF		;SEE IF ITS SPC OR TAB
	PUSH	AF
	CP	SPCTK		;IF SPACE LEAVE ALONE
	JR	Z,SPCNDC
	DEC	DE		;offset TAB by 1
SPCNDC	LD	A,D
	OR	A		;MAKE SURE ITS NOT NEGATIVE
	JP	P,TBNONG
	LD	DE,0000H
TBNONG	PUSH	HL		;SAVE THE TEXT POINTER
	CALL	ISFLIO		;SEE IF GOING TO DISK FILE
	JR	NZ,LNOMOD	;DONT MOD
	LD	A,(PRTFLG)	;GOING TO PRINTER?
	OR	A		;SET FLAGS
	LD	A,(LPTSIZ)	;GET SIZE
	JR	NZ,TABER12	;WAS LPT, MOD BY ITS SIZE
	LD	A,(TTYSIZ)	;GET THE LINE LENGTH
TABER12	LD	L,A		;[L]=file width
	INC	A		;test for width of 255 (no folding)
	JR	Z,LNOMOD	;if so, don't mod
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
	JR	Z,DOSIZT	;value in [AL]
	LD	BC,0007H	;(NMLO.C) if file output, SPECIAL PRINT
				; POSITION SHOULD BE FETCHED FROM FILE DATA
	LD	HL,(PTRFIL)
	ADD	HL,BC		;[H,L] POINT AT POSITION
	CALL	ISFLIO		;OUTPUTING INTO A FILE?
				;(IF SO, [PTRFIL] .NE. 0)
	LD	A,(HL)		;IF FILE IS ACTIVE
	JR	NZ,DOSIZT	;DO TAB CALCULATION NOW
	LD	A,(PRTFLG)	;LINE PRINTER OR TTY?
	OR	A		;NON-ZERO MEANS LPT
	JP	Z,TABTTY
	LD	A,(LPTPOS)	;GET LINE PRINTER POSITION
	JR	DOSIZT		;value in [AL]

;	TAB on TTY
TABTTY	LD	A,(TTYPOS)	;[AL]=file position
DOSIZT	CPL			;print [E]-[A] spaces
	ADD	A,E
	JR	C,ASPA2		;print if past current
	INC	A
	JR	Z,NOTABR	;do nothing if at current
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
	IF	GFX
	LD	(GPRFLG),A	;<==>	PRINT#-3 flag
	ENDIF
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
	IF	GFX
	CP	INPUTTK		;===>	IS IT FOLLOWED BY 'INPUT'?
	JP	NZ,GLINE	;<===	YES, GO DRAW A LINE
	ENDIF
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
	CALL	PINLIN		;READ A LINE OF INPUT
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

TRYAGN	DB	'?Redo from start',0DH,00H

;
;	Here when passing over string literal in subscript of variable in INPUT list
;	on the first pass of INPUT checking for type match and number
;
SCNSTR	INC	HL		;LOOK AT THE NEXT CHARACTER
	LD	A,(HL)		;FETCH IT
	OR	A		;END OF LINE?
	JP	Z,SNERR		;ENDING IN STRING IN SUBSCRIPT IS BAD SYNTAX
	CP	'"'		;ONLY OTHER TERMINATOR IS QUOTE
	JR	NZ,SCNSTR	;CONTINUE UNTIL QUOTE OR 0 IS FOUND
	JP	SCNCON		;CONTINUE MATCHING PARENS SINCE STRING ENDED

INPBAK	POP	HL		;GET RID OF PASS1 DATA POINTER
	POP	HL		;GET RID OF PASS2 DATA POINTER
	JR	RDOIN2		;GET RID OF PASS2 VARLST POINTER AND RETRY

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
	JR	Z,FILSTI
	IF	GFX
	CALL	TOTEXT		;<==>	Set text mode
	ENDIF
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
	JR	NZ,NTICMA	;NO
	XOR	A		;FLAG NOT TO DO IT
	LD	(TEMPA1),A	;=TEMPA+1 (in GW-BASIC)
	CALL	CHRGTR		;FETCH NEXT CHAR
	JR	INPCMA		;CONTINUE

NTICMA	CALL	SYNCHR
	DB	';'		;MUST END WITH SEMI-COLON
INPCMA	PUSH	HL		;REMEMBER WHERE IT ENDED
	CALL	STRPRT		;PRINT IT OUT
	POP	HL		;GET BACK SAVED TEXT PTR
	RET			;ALL DONE
NOTQTI	PUSH	HL
	LD	A,(TEMPA1)	;DO "? "
	OR	A
	JR	Z,SUPPRS	;THEN SUPPRESS "?"
	LD	A,'?'		;TYPE "? " AND INPUT A LINE OF TEXT
	CALL	OUTDO
	LD	A,' '
	CALL	OUTDO
SUPPRS	CALL	PINLIN
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
	JR	Z,SCNBKT
	CP	'('		;ARRAY OR NOT?
	JR	NZ,ENDSCN	;IF NOT, VARIABLE NAME IS DONE
SCNBKT	INC	HL		;NOW SCAN THE SUBSCRIPT EXPRESSION
	LD	B,00H		;INITIALIZE THE PAREN COUNT
SCNOPN	INC	B		;UP THE COUNT FOR EVERY "("
SCNCON	CALL	CHRGTR		;GET THE NEXT CHARACTER
	JP	Z,SNERR		;SHOULDN'T END STATEMENT IN EXPRESSION
	CP	'"'		;IS THERE A QUOTED STRING CONSTANT
	JP	Z,SCNSTR	;GO SCAN THE ENDTIRE CONSTANT (MAY CONTAIN PARENS)
	CP	'('		;ANOTHER LEVEL OF NESTING?
	JR	Z,SCNOPN	;INCREMENT COUTN AND KEEP SCANNING
	CP	'['		;left bracket?
	JR	Z,SCNCON	;yes, ok
	CP	']'		;left bracket?
	JR	Z,LEFPRN	;yes
	CP	')'		;ONE LESS LEVEL OF PARENS?
	JR	NZ,SCNCON	;NO, KEEP SCANNING
				;DECREMENT PAREN COUNT. OUT OF SUBSCRIPT?
LEFPRN	DEC	B
	JP	NZ,SCNCON	;IF NOT AT ZERO LEVEL, KEEP SCANNING
ENDSCN	CALL	CHRGTR		;GET TERMINATING CHARACTER
	JR	Z,OKVLST	;LAST VARIABLE IN INPUT LIST
	CP	','		;OTHERWISE IT MUST BE A COMMA
	JP	NZ,SNERR	;BADLY FORMED INPUT -- SYNTAX ERROR
OKVLST	EX	(SP),HL		;SAVE THE VARLST POINTER
				;GET THE DATA POINTER INTO [H,L]
	LD	A,(HL)		;DATA SHOULD ALWAYS HAVE A LEADING COMMA
	CP	','		;IS IT PROPERLY FORMED?
	JP	NZ,INPBAK	;NO, ASK FOR COMPLETE REINPUT
	LD	A,01H		;SET OVCSTR=1
	LD	(OVCSTR),A
	LD	(FLGSCN1),A	;<-->	Flag to SCNVAL that call is from INPUT
	CALL	SCNVAL		;GO INTO PASS2 CODE AND SCAN A VALUE
	LD	A,(FLGSCN1)	;--->
	DEC	A		;	Restore flag
	LD	(FLGSCN1),A	;<---
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
	JR	INPCON

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
	JR	LOPDAT
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
	JR	Z,DATBK		;A COMMA SO A VALUE MUST FOLLOW
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
	JR	NZ,NUMINS	;IF NUMERIC, USE FIN TO GET IT
				;ONLY THE VARIABLE TYPE IS
				;CHECKED SO AN UNQUOTED STRING
				;CAN BE ALL DIGITS
	CALL	CHRGTR
	LD	D,A		;ASSUME QUOTED STRING
	LD	B,A		;SETUP TERMINATORS
	CP	'"'		;QUOTE ?
	JR	Z,NOWGET	;TERMINATORS OK
	LD	A,(SARYFL)	;INPUT SHOULDN'T TERMINATE ON ":"
	OR	A		;SEE IF READ OR INPUT
	LD	D,A		;SET D TO ZERO FOR INPUT
	JR	Z,NCOLST
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
	JR	Z,TRMOK		;End of line - More needed?
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
	JR	NZ,NOWLIN	;No - See if DATA statement
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
	LD	(DATLIN),DE	;SAVE DATA LINE NUMBER
NOWLIN	CALL	CHRGTR		;GET THE STATEMENT TYPE
	CP	DATATK		;IS IS "DATA"?
	JR	NZ,DATLOP	;NOT DATA SO LOOK SOME MORE
	JP	DATBK		;CONTINUE READING

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
	JR	Z,RESSN		;IF JUST 'RESEQ' RESEQ 10 BY 10
	CP	','		;COMMA
	JR	Z,EATCOM	;DONT USE STARTING # OF ZERO
	PUSH	DE		;SAVE [D,E]
	CALL	LINSPC		;GET NEW NN
	LD	B,D		;GET IN IN [B,C] WHERE IT BELONGS
	LD	C,E
	POP	DE		;GET BACK [D,E]
	JR	Z,RESSN		;IF EOS, DONE
EATCOM	CALL	SYNCHR
	DB	','		;EXPECT COMMA
	CALL	LINSPC		;GET NEW MM
	JR	Z,RESSN		;IF EOS, DONE
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
	JR	NXTRSL

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
	JR	Z,RESSD1	;ZERO, DONE
	LD	A,(HL)		;GET FIRST BYTE OF LINK
	INC	HL		;INC POINTER
	OR	(HL)		;SET CC'S
	DEC	HL		;MOVE POINTER BACK
	EX	DE,HL		;BACK IN [D,E]
	JR	NZ,NXTRSC	;INC COUNT

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
	JR	Z,SCCALL	;STOP RESEQING WHEN SEE END OF PGM
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
	JR	RESNX1		;KEEP RESEQING

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
	JR	Z,SCNPLN	;SCAN NEXT LINE
	LD	C,A		;SAVE [A]
	LD	A,(PTRFLG)	;CHANGE LINE TOKENS WHICH WAY?
	OR	A		;SET CC'S
	LD	A,C		;GET BACK CURRENT CHAR
	JR	Z,SCNPT2	;CHANGING POINTERS TO #'S
	CP	ERRORTK		;IS IT ERROR TOKEN?
	JR	NZ,NTERRG	;NO.
	CALL	CHRGTR		;SCAN NEXT CHAR
	CP	GOTOTK		;ERROR GOTO?
	JR	NZ,SCNEX2	;GET NEXT ONE
	CALL	CHRGTR		;GET NEXT CHAR
	CP	LINCON		;LINE # CONSTANT?
	JR	NZ,SCNEX2	;NO, IGNORE.
	PUSH	DE		;SAVE [D,E]
	CALL	LINGT3		;GET IT
	LD	A,D		;IS IT LINE # ZERO?
	OR	E
	JR	NZ,CHGPTR	;CHANGE IT TO A POINTER
	JR	SCNEX3		;YES, DONT CHANGE IT

NTERRG	CP	LINCON		;LINE # CONSTANT?
	JR	NZ,SCNEXT	;NOT, KEEP SCANNING
	PUSH	DE		;SAVE CURRENT LINE # FOR POSSIBLE ERROR MSG
	CALL	LINGT3		;GET LINE # OF LINE CONSTANT INTO [D,E]
CHGPTR	PUSH	HL		;SAVE TEXT POINTER JUST AT END OF LINCON 3 BYTES
	CALL	FNDLIN		;TRY TO FIND LINE IN PGM.
	DEC	BC		;POINT TO ZERO AT END OF PREVIOUS LINE
	LD	A,PTRCON	;CHANGE LINE # TO PTR
	JR	C,MAKPTR	;IF LINE FOUND CHANGE # TO PTR
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
JSCNXT	JR	SCNEXT		;KEEP SCANNING

LINM	DB	'Undefined line ',00H

SCNPT2	CP	PTRCON		;POINTER
	JR	NZ,JSCNXT	;NO, KEEP SCANNING
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
				;---> 	ADDON: skip if unchanged
	LD	A,(HL)		;	GET THE BASE NUMBER
	SUB	'0'
	JP	C,SNERR
	CP	'2'-'0'
	JP	NC,SNERR	;	ONLY 0 AND 1 ARE LEGAL
	LD	B,A
	LD	A,(OPTVAL)	;	Option base
	CP	B
	JR	Z,OLDOPT	;<---	BRIF OLD OPTION BASE VALUE
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
	LD	A,B		;<-->
	LD	(OPTVAL),A	;SAVE IF FOR DIM AND PTRGET
OLDOPT	INC	A		;MAKE SURE [A] IS NON ZERO
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
; ## GWMAIN.ASM:3414 ##
;	This code scans ahead to find the "NEXT" that matches a "FOR"
;	in order to 1) handle empty loops and
;	            2) make sure loops match up properly.
;
WNDSCN	LD	C,ERRWH		;SCAN FOR MATCHING WEND THIS IS ERROR IF FAIL
	JR	SCNCNT

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
	JR	Z,FORTRM	;HAVE STATEMENT AFTER "THEN"
	CP	'"'		;--->	CHECK FOR QUOTE WHICH MAY HAVE
	JR	NZ,NTQTSC	;	EIGHT BIT LITERAL DATA SAME AS $THEN
				;	OR $WHILE OR $WEND OR $FOR OR $NEXT
QTLOPF	CALL	CHRGTR		;	IN IT. SCAN THROUGH STRING.
	OR	A
	JR	Z,FORTRM	;	TERMINATE ONLY ON ZERO NOT COLON
	CP	'"'
	JR	NZ,QTLOPF	;<---
NTQTSC	CP	ELSETK		;ELSE STATEMENT
	JR	Z,FNNWST	;THEN ALLOW NEXT OR WEND AFTER IT
	CP	THENTK		;SO SCAN USING CHRGET WAITING FOR END
	JR	NZ,SCANWF	;OF STATEMENT OR $THEN
FORTRM	OR	A		;SEE HOW IT ENDED
	JR	NZ,FNNWST	;JUST NEW STATEMENT -- EXAMINE IT
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
	LD	(NXTLIN),DE	;SAVE AS "NEXT" LINE NUMBER
FNNWST	CALL	CHRGTR		;GET THE TYPE OF THE NEXT STATEMENT
	CP	REMTK		;--->	REM STATEMENTS SINCE THEY HAVE COLONS
	JR	NZ,NTREMR	;	IN THE MIDDLE WHICH MAY BE VIEWED AS
	PUSH	BC
	CALL	REM		;	STATEMENT TERMINATORS AND THE NEXT CHAR
	POP	BC
	JR	FORTRM		;	MIGHT BE AN EIGTH BIT LITERAL THAT

NTREMR	CP	DATATK		;	ACCIDENTALLY MATCHES THE SEARCH
	JR	NZ,NTDATR	;	SO WE CALL REM AND DATA TO DO THE SKIPPING
	PUSH	BC
	CALL	DATA
	POP	BC
	JR	FORTRM		;<---

NTDATR	LD	A,C		;GET THE ERROR NUMBER TO SEE WHAT WE ARE
	CP	ERRFN		;SCANNING FOR
	LD	A,(HL)		;GET BACK THE CHARACTER
	JR	Z,NXTLOK	;FOR/NEXT SEARCHING
	CP	WHILETK		;ANOTHER WHILE/WEND NEST?
	JR	Z,FORINC
	CP	WENDTK
	JR	NZ,FNLOP
	DEC	B
	JP	NZ,FNLOP
	RET

NXTLOK	CP	FORTK		;ANOTHER "FOR"?
	JR	Z,FORINC	;INCREMENT THE FOR COUNT
	CP	NEXTTK		;END WITH NEXT?
	JR	NZ,FNLOP	;SKIP OVER THIS STATEMENT
DECNXT	DEC	B		;DECREMENT THE LOOP COUNT
	RET	Z		;RETURN WITH [H,L] ABOUT TO GET
				; FIRST CHARACTER OF "NEXT" VARIABLE
;
;	Scan  the variables listed in a "NEXT" statement
;
	CALL	CHRGTR		;SEE IF THERE IS A NAME
	JR	Z,FORTRM	;ONLY ONE SO SCAN MORE STATEMENTS
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
	JR	Z,TRMNXT	;END OF "NEXT"
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


;=============================================================================
;	IBMRES - IBM COMPATIBLE RESERVED WORDS / MLC
; ## IBMRES.ASM ##
;

;-----------------------------------------------------------------------------

;	The following tables are the alphabetic dispatch table
;	followed by the reserved word table itself

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
	DEFTOK	WAIT,FNWAIT
	DEFTOK	DEF,DEFST
	DEFTOK	POKE
	DEFTOK	CONT
	SKIPTOK	SNERR		;BASIC-80: DRAW
	SKIPTOK	SNERR		;BASIC-80: CIRCLE
	DEFTOK	OUT,FNOUT
	DEFTOK	LPRINT
	DEFTOK	LLIST
	DEFTOK	CLS
	SKIPTOK	0		;CLG in some BASIC's ?
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
	SKIPTOK	0		;BASIC-80: PSET
	SKIPTOK	0		;BASIC-80: PRESET
	DEFTOK	WHILE
	DEFTOK	WEND
	DEFTOK	CALL,CALLST
	DEFTOK	WRITE
	DEFTOK	COMMON,DATA
	DEFTOK	CHAIN
	DEFTOK	OPTION
	DEFTOK	RANDOM
	SKIPTOK	0		;BASIC-80: COLOR
	DEFTOK	SYSTEM
	SKIPTOK	0		;BASIC-80: LOCATE
	DEFTOK	OPEN
	DEFTOK	FIELD
	IF	GFX
	DEFTOK	GET,XGET	;===>
	DEFTOK	PUT,XPUT	;<===
	ELSE
	DEFTOK	GET,GETST
	DEFTOK	PUT,PUTST
	ENDIF
	DEFTOK	CLOSE
	DEFTOK	LOAD
	DEFTOK	MERGE
	SKIPTOK	0		;BASIC-80: FILES
	DEFTOK	NAME
	DEFTOK	KILL
	DEFTOK	LSET
	DEFTOK	RSET
	DEFTOK	SAVE
	SKIPTOK	0		;BASIC-80: RESET
	DEFTOK	SOUND
	SKIPTOK			;BASIC-80: VPOKE
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
	DEFTOK	DATE$,0
	DEFTOK	ERRS$,0
	DEFTOK	INKEY$,0
	DEFTOK	MEM,0		;not standard in BASIC-80
	DEFTOK	TIME$,0		;not standard in BASIC-80
	IF	GFX
	DEFTOK	FPOINT,0	;===>
	DEFTOK	FVIEW,0
	DEFTOK	CIRCLE,0
	DEFTOK	CLR,0
	DEFTOK	GLOCATE,0
	DEFTOK	PAINT,0
	DEFTOK	PRESET,0
	DEFTOK	PSET,0
	DEFTOK	SCREEN,0
	DEFTOK	VIEW,0		;<===
	ENDIF

;	Operators:
QQ	DEFL	0F0H
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
	SKIPTOK	0		;BASIC-80: VPEEK
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
	DEFTOK	EOF
	DEFTOK	LOC
	DEFTOK	LOF
	DEFTOK	MKI$
	DEFTOK	MKS$
	DEFTOK	MKD$
	DEFTOK	ROW,ROWFN

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
	IF	GFX
	TOKEN	CIRCLE		;<==>	CIRCLE statement
	ENDIF
	TOKEN	CLS
	IF	GFX
	TOKEN	CLR		;<==>	CLR statement
	ENDIF
	DB	00H
DTAB	TOKEN	DELETE
	TOKEN	DATA
	TOKEN	DIM
	TOKEN	DEFSTR
	TOKEN	DEFINT
	TOKEN	DEFSNG
	TOKEN	DEFDBL
	TOKEN	DEF
	TOKEN	DATE$
	DB	00H
ETAB	TOKEN	ELSE
	TOKEN	END
	TOKEN	ERASE
	TOKEN	EDIT
	TOKEN	ERRS$
	TOKEN	ERROR
	TOKEN	ERL
	TOKEN	ERR
	TOKEN	EXP
	TOKEN	EOF
	TOKEN	EQV
	DB	00H
FTAB	TOKEN	FOR
	TOKEN	FIELD
	TOKEN	FN
	TOKEN	FRE
	TOKEN	FIX
	DB	00H
GTAB	TOKEN	GOTO
	DB	'O T','O'+80H,GOTOTK	;'GO TO'
	TOKEN	GOSUB
	TOKEN	GET
	IF	GFX
	TOKEN	GLOCATE		;<==>	GLOCATE statement
	ENDIF
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
	TOKEN	MEM
	DB	00H
NTAB	TOKEN	NEXT
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
	IF	GFX
	TOKEN	PAINT		;<==>	PAINT statement
	TOKEN	PRESET		;<==>	PRESET statement
	TOKEN	PSET		;<==>	PSET statement
	ENDIF
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
	TOKEN	RANDOM
	TOKEN	ROW
	DB	00H
STAB:
	IF	GFX
	TOKEN	SCREEN		;<==>	SCREEN statement
	ENDIF
	TOKEN	STOP
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
	TOKEN	SOUND
	DB	00H
TTAB	TOKEN	THEN
	TOKEN	TRON
	TOKEN	TROFF
	TOKEN	TAB(,TAB
	TOKEN	TO
	TOKEN	TIME$
	TOKEN	TAN
	DB	00H
UTAB	TOKEN	USING
	TOKEN	USR
	DB	00H
VTAB	TOKEN	VAL
	IF	GFX
	TOKEN	VIEW		;<==>	VIEW statement
	ENDIF
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
	DCL	DIO,'Device I/O error'
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
	DCL	DWP,'Disk write protected'
	DCL	FAD,'File access DENIED'
	DCL	CMA,'Command Aborted'
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
NULLS	DB	01H		;MBASIC leftover
	;TRS-80: (?) TAB position for console
DOTABP?	DB	0FFH
	;TRS-80: RND temp value (3 bytes)
RNDTMP	DB	40H,0E6H,4DH
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
;	Fake end of program for RESUME NEXT
ENDPRG	DB	3AH,00H,00H,00H,00H
;	TODO: clarify
FLGSCN1	DB	00H
;	(no comment)
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
	;temp area for LOAD/SAVE
DIRTMP	DB	00H
	;=KBUF-1, initialized with a ":"
KBUF1	DB	0		;gets a colon for restarting input
;	This is the KRUNCH buffer
KBUF	DC	KBFLEN,0
	;Old name buffer for NAME ... AS ...
FILNAM	EQU	$-1
;	=BUF-1
BUFMIN	DB	00H		;gets a comma used, e.g. in "FILSTI"
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
FILNA2	EQU	BUF+30
DOSBUF	EQU	BUF+32
;	Store terminal position here
TTYPOS	DB	00H
;	TRS-80: RND mantissa low byte
RNDVAL	DB	00H
;	TRS-80: RND mantissa mid byte (loaded with R)
RNDVAM	DB	00H
;	TRS-80: RND mantissa high byte
RNDVAH	DB	00H,00H
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
;	Maximum size of BASIC's Data Segment
MAXMEM	DB	00H,00H
;	Highest location in memory
MEMSIZ	DB	00H,00H
;	Pointer at first free Temp Descriptor
;	Initialized to point to TEMPST
TEMPPT	DB	00H,00H
;	Storage for NUMTMP Temp Descriptors
TEMPST	DB	00H,00H,00H,00H,00H,00H,00H,00H
	DB	00H,00H,00H,00H,00H,00H,00H
	;=DEFTBL-'A' (DEFINT...) (TODO:replace with expr)
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
OPTFLG	DB	00H
;	temp- holds OPTVAL during Chain
TOPTVL	DB	00H
;	temp- holds OPTFLG during Chain
TOPTFG	DC	31,0
;	Misc temp used by CALL and LIST
TEMPA	DB	00H
;	??? -- FLAG TO DO "? "
TEMPA1	DB	00H
;	FRETOP saved here by CHAIN
SAVFRE	DB	00H,00H
;	=MAXREC (unused?)
?RECSIZ	DB	00H,00H
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
;	Exponent adjustment factor
;	used during E format to adjust exp
;	if field overflow occurs
EXPAF	DB	00H
;	Temp FAC save area used while
;	testing FAC for field overflow
;	of E formated output
EXPTMP	DB	00H,00H,00H,00H,00H,00H,00H,00H
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
;TODO	The last 3 locations are temp for ROM fmult
FMLTT1	DB	00H,00H,00H,00H,00H,00H
;TODO 	=FMLTT1+6=FBUFFR+21H: END OF FBUFFR
FMLTT16	DB	00H
;TODO 	=FMLTT1+7=FBUFFR+22H
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
	IF	GMESOFT
REDDY	DB	'OK',0DH,00H
	ELSE
REDDY	DB	'Ready',0DH,00H
	ENDIF
BRKTXT	DB	'Break',00H


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
	JR	C,DORELS	;YES, DO IT
	SUB	PLUSTK		;SUBTRACT OFFSET FOR FIRST ARITHMETIC
	LD	E,A		;MUST MULTIPLY BY 3 SINCE
				;OPTAB ENTRIES ARE 3 LONG
	JR	NZ,NTPLUS	;NOT ADDITION OP
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
	JR	Z,EXPSTK	;IF SO, "FRCSNG" AND MAKE A SPECIAL STACK ENTRY
	CP	51H		;SEE IF THE OPERATOR IS "AND" OR "OR"
	JR	C,ANDORD	;AND IF SO "FRCINT" AND
				;MAKE A SPECIAL STACK ENTRY
	AND	0FEH		;MAKE 123 AND 122 BOTH MAP TO 122
	CP	7AH		;MAKE A SPECIAL CHECK FOR "MOD" AND "IDIV"
	JR	Z,ANDORD	;IF SO, COERCE ARGUMENTS TO INTEGER
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
	JR	C,FINREL	;RELATIONS ALL THROUGH
	CP	LESSTK-GREATK+1	;IS IT REALLY RELATIONAL?
	JR	NC,FINREL	;NO JUST BIG
	CP	01H		;SET UP BITS BY MAPPING
	RLA			;0 TO 1 1 TO 2 AND 2 TO 4
	XOR	D		;BRING IN THE OLD BITS
	CP	D		;MAKE SURE RESULT IS BIGGER
	LD	D,A		;SAVE THE MASK
	JP	C,SNERR		;DON'T ALLOW TWO OF THE SAME
	LD	(TEMP3),HL	;SAVE CHARACTER POINTER
	CALL	CHRGTR		;GET THE NEXT CANDIDATE
	JR	LOPREL

;	For exponentiation we want to force the current value in the FAC
;	to be single precision. When application time comes we force
;	the right hand operand to single precision as well
EXPSTK	CALL	CSNG		;COERCE LEFT HAND OPERAND
	CALL	PUSHF		;PUT IT ON THE STACK
	LD	BC,FPWRQ	;PLACE TO COERCE RIGHT HAND
				;OPERAND AND DO EXPONENTIATION
	LD	D,7FH		;RESTORE THE PRECEDENCE
	JR	FINTMP		;FINISH ENTRY AND EVALUATE MORE FORMULA

;	For "AND" and "OR" we want to force the current value in the
;	FAC to be an integer, and at application time force the right
;	hand operand to be an integer
ANDORD	PUSH	DE		;SAVE THE PRECEDENCE
	CALL	CINT
	POP	DE		;[D]=PRECEDENCE
	PUSH	HL		;PUSH THE LEFT HAND OPERAND
	LD	BC,DANDOR	;"AND" AND "OR" DOER
	JR	FINTMP		;PUSH ON THIS ADDRESS,PRECEDENCE
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
	LD	E,OPCNT		;[D]=PRECEDENCE=100
	LD	D,64H		;[E]=DISPATCH OFFSET FOR
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
	JR	FINTMP		;PUSH THE ADDRESS, REGET THE TEXT POINTER
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
	JR	NZ,VALNSM	;NO
	CP	02H		;INTEGER?
	JR	Z,INTDPC	;YES, DISPATCH!!
	CP	04H		;SINGLE?
	JP	Z,SNGDPC	;YES, DISPATCH!!
	JR	NC,DBLDPC	;MUST BE DOUBLE, DISPATCH!!
VALNSM	LD	D,A		;SAVE IN [D]
	LD	A,B		;CHECK FOR DOUBLE
	CP	08H		;PRECISION ENTRY ON THE STACK
	JR	Z,STKDBL	;FORCE FAC TO DOUBLE
	LD	A,D		;GET VALTYPE OF FAC
	CP	08H		;AND IF SO, CONVERT THE STACK OPERAND
	JR	Z,FACDBL	;TO DOUBLE PRECISION
	LD	A,B		;SEE IF THE STACK ENTRY IS SINGLE
	CP	04H		;PRECISION AND IF SO, CONVERT
	JR	Z,STKSNG	;THE FAC TO SINGLE PRECISION
	LD	A,D		;SEE IF THE FAC IS SINGLE PRECISION
	CP	03H		;AND IF SO CONVERT THE STACK TO SINGLE
	JP	Z,TMERR		;BLOW UP ON RIGHT HAND STRING OPERAND
	JR	NC,FACSNG	;AND IF SO CONVERT THE STACK TO SINGLE
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
	JR	Z,SNGDBL	;IT'S SINGLE PRECISION
				;SO DO A POPR / CALL MOVFR
	POP	HL		;POP OFF THE INTEGER VALUE
	LD	(FACLO),HL	;SAVE IT FOR CONVERSION
	JR	SETDB1		;SET IT UP

;
;	This is the case where the stack is single precision
;	and the FAC is either single precision or integer
;
STKSNG	CALL	CSNG		;CONVERT THE FAC IF NECESSARY
SNGDPC	POP	BC		;PUT THE LEFT HAND OPERAND IN THE REGISTERS
	POP	DE
SNGDO	LD	HL,SNGDSP	;SETUP THE DISPATCH ADDRESS
				;FOR THE SINGLE PRECISION OPERATOR ROUTINES
	JR	DODSP		;DISPATCH

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
	JR	SNGDO		;PERFORM THE OPERATION

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
	JR	Z,EVAL
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
	JR	NZ,NTERC	;NO, TRY OTHER POSSIBILITIES
	CALL	CHRGTR		;GRAB FOLLOWING CHAR
	LD	A,(ERRFLG)	;GET THE ERROR CODE.
	PUSH	HL		;SAVE TEXT POINTER
	CALL	SNGFLT		;RETURN THE VALUE
	POP	HL		;RESTORE TEXT POINTER
	RET			;ALL DONE.

NTERC	CP	ERLTK		;ERROR LINE NUMBER VARIABLE.
	JR	NZ,NTERL	;NO, TRY MORE THINGS.
;	'ERL' BASIC function
	CALL	CHRGTR		;GET FOLLOWING CHARACTER
	PUSH	HL		;SAVE TEXT POINTER
	LD	HL,(ERRLIN)	;GET THE OFFENDING LINE #
	CALL	MAKSNG		;FLOAT 2 BYTE UNSINGED INT
	POP	HL		;RESTORE TEXT POINTER
	RET			;RETURN

NTERL	CP	VARPTRTK	;VARPTR CALL?
	JR	NZ,NTVARP	;NO
	CALL	CHRGTR		;EAT CHAR AFTER
	CALL	SYNCHR
	DB	'('		;EAT LEFT PAREN
	CP	'#'		;WANT POINTER TO FILE?
	JR	NZ,NVRFIL	;NO, MUST BE VARIABLE
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
	CP	DATE$TK		;--->
	JP	Z,DATE$
	CP	ERRS$TK
	JP	Z,ERRS$
	CP	MEMTK
	JP	Z,MEM
	CP	TIME$TK
	JP	Z,TIME$		;<---
	IF	GFX
	CP	FPOINTTK
	JP	Z,FPOINT	;<==>	&POINT function
	CP	FVIEWTK
	JP	Z,FVIEW		;<==>	&VIEW function
	ENDIF
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
	JR	Z,LOPOCT	;IF SO, DO IT
	CP	'H'		;HEX?
	JR	NZ,LOPOC2	;THEN DO IT
	LD	B,05H		;INIT DIGIT COUNT
LOPHEX	INC	HL		;BUMP POINTER
	LD	A,(HL)		;GET CHAR
	CALL	MAKUPS		;MAKE UPPER CASE
	CALL	ISLET2		;FETCH CHAR, SEE IF ALPHA
	EX	DE,HL		;SAVE [H,L]
	JR	NC,ALPTST	;YES, MAKE SURE LEGAL HEC
	CP	'9'+1		;IS IT BIGGER THAN LARGEST DIGIT?
	JR	NC,HOCFIN	;YES, BE FORGIVING & RETURN
	SUB	'0'		;CONVERT DIGIT, MAKE BINARY
	JR	C,HOCFIN	;BE FORGIVING IF NOT HEX DIGIT
	JR	NXTHEX		;ADD IN OFFSET

ALPTST	CP	'F'+1		;IS IT LEGAL HEX?
	JR	NC,HOCFIN	;YES, TERMINATE
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
;	If not INPUT statement goto OVFLW error from here, else pass back error
CKOVER	LD	A,(FLGSCN1)	;--->	Dup of FLGSCN
	OR	A
	JP	Z,OVERR		;	IF NOT INPUT STATEMENT, THIS IS ERROR
	PUSH	HL
	LD	HL,$OVMSG
	CALL	STRPRN		;	PRINT OVERFLOW MESSAGE
	CALL	CRDO
	POP	HL
CONER2	LD	A,(FLGOVC)
	INC	A
	LD	(FLGOVC),A	;	TELL INPUT CODE THAT ERROR OCCURED
	RET

CONERR	LD	A,(FLGSCN1)
	OR	A
	JP	NZ,CONER2	;	IF INPUT CODE ERROR, RETURN ERROR CODE
	JP	SNERR		;<---	ELSE GOTO ERROR FROM HERE

LOPOC2	DEC	HL		;REGET LAST CHARACTER
LOPOCT	CALL	CHRGTR		;READ A DIGIT
	EX	DE,HL		;RESULT INTO [H,L]
	JR	NC,HOCFIN	;OUT OF DIGITS MEANS DONE
	CP	38H		;IS THIS AN OCTAL DIGIT
	JP	NC,CONERR	;NO, TOO BAD YOU WILL LOSE
	LD	BC,CKOVER	;WHERE TO GO ON OVERFLOW ERROR
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
	JR	LOPOCT		;SCAN MORE DIGITS

HOCFIN	CALL	MAKINT		;SAVE AS AN INTEGER
	EX	DE,HL		;[H,L]-TEXT POINTER
	RET

ISFUN	INC	HL		;BUMP SOURCE TEXT POINTER
	LD	A,(HL)		;GET THE ACTUAL TOKEN FOR FN
	SUB	80H+ONEFUN	;MAKE INTO OFFSET
	LD	B,00H
	RLCA			;MULTIPLY BY 2
	LD	C,A
	PUSH	BC		;SAVE THE FUNCTION # ON THE STACK
	CALL	CHRGTR
	LD	A,C		;LOOK AT FUNCTION #
	CP  (MID$TK-ONEFUN)*2+1	;IS IT PAST LASNUM?
	JR	NC,OKNORM	;NO, MUST BE A NORMAL FUNCTION
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
	JR	FINGO		;DISPATCH TO FUNCTION

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
	JR	C,NOTFRF	;DON'T FORCE THE ARGUMENT
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
	JR	RETAPG		;RETURN FROM OPERATOR APPLICATION

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
	JR	NC,CGETYP	;SPLIT OFF NO CARRY CASE
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
	JR	NZ,NOTOR
;	'OR' boolean expression
	LD	A,E
	OR	L		;OR L,E
	LD	L,A
	LD	A,H
	OR	D		;OR H,D
	RET			;RETURN THE INTEGER [A,L]

NOTOR	CP	50H		;AND?
	JR	NZ,NOTAND
;	'AND' boolean expression
	LD	A,E
	AND	L		;AND L,E
	LD	L,A
	LD	A,H
	AND	D		;AND H,D
	RET			;RETURN THE INTEGER [A,L]

NOTAND	CP	3CH		;XOR?
	JR	NZ,NOTXOR	;NO
;	'XOR' boolean expression
	LD	A,E
	XOR	L		;XOR L,E
	LD	L,A
	LD	A,H
	XOR	D		;XOR H,D
	RET			;RETURN THE INTEGER [A,L]

;	For "EQV" use A EQV B = NOT(A XOR B)
NOTXOR	CP	32H		;EQV?
	JR	NZ,NOTEQV	;NO
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
GIVDBL	OR	A		;--->
	SBC	HL,DE		;<---	[H,L]=[H,L]-[D,E]
	JP	MAKSNG		;FLOAT 2 BYTE UNSIGNED INT

;	'LPOS' BASIC command
;	Return printer position in line
LPOS	LD	A,(LPTPOS)	;MAKE [A] AN UNSIGNED INTEGER
	JR	POSC

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
USRFN	CALL	PRODIR		;---> Don't allow in direct mode in
				;<--- protected environment
	CALL	SCNUSR		;SCAN THE USR#
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
	JR	NC,NOARGU	;NO, MUST BE DEFAULTING TO USR0
	CP	ONECON		;IS IT SMALLER THAN ONECON?
	JR	C,NOARGU	;YES, ASSUME TRYING TO DEFAULT TO USR0
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
	JR	Z,DEFUSR	;YES, DO IT
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
	JR	SCNLIS

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
	JR	Z,POPASG	;IS THE ARGUMENT LIST ENDING?
	CALL	SYNCHR		;MAKE SURE THE ARGUMENT LIST ALSO ENDED
	DB	','
	PUSH	HL		;SKIP OVER ARGUMENT COMMA
	LD	HL,(TEMP3)	;SAVE THE ARGUMENT LIST TEXT POINTER
				;GET THE TEXT POINTER INTO THE DEFINTION'S
	CALL	SYNCHR		;PARAMETER LIST
	DB	','
	JR	ASGMOR		;SKIP OVER THE PARAMETER LIST COMMA
				;AND BIND THE REST OF THE PARAMETERS
POPAS2	POP	AF		;IF ASSIGNMENT IS SUCESSFUL UPDATE PRMLN2
	LD	(PRMLN2),A	;INDICATE NEW VARIABLE IS IN PLACE
POPASG	POP	AF		;GET THE VALUE TYPE
	OR	A
	JR	Z,FINASG	;ZERO MEANS NO MORE LEFT TO POP AND ASSIGN
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
	JR	NZ,NOCPRS	;WHOSE DESCRIPTOR IS ABOUT TO BE WIPED OUT
				;BECAUSE IT IS SITTING IN PARM1 (THIS
				; HAPPENS IT THE FUNCTION IS A PROJECTION
				; FUNCTION ON A STRING ARGUMENT)
	LD	DE,DSCTMP	;DSCTMP IS PAST ALL THE TEMP AREA
	LD	HL,(FACLO)	;GET THE ADDRESS OF THE DESCRIPTOR
	CALL	COMPAR
	JR	C,NOCPRS	;RESULT IS A TEMP - NO COPY NESC
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
	JR	NZ,BCTRAL
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
	IF	GFX
	JP	NZ,GDISPA	;<==>	Graphics dispatcher
	ELSE
	JP	NZ,SNERR	;NO, ERROR.
	ENDIF
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
FNINP	CALL	FRQINT		;---> MAKE ARGUMENT AN INTEGER CHANNEL NUMBER
	LD	B,H
	LD	C,L
	IN	A,(C)		;READ BYTE INTO A FROM C
	JP	SNGFLT

	;Set I/O port for INP(...) and OUT ...,...
SETIO	CALL	FRMQNT		;READ A 16-BIT PORT FOR Z80
	PUSH	DE		;SAVE FOR USE BY WAIT AND FNOUT
	CALL	SYNCHR
	DB	','
	CALL	GETBYT		;READ THE DATA BYTE TO [A] AND [E]
	POP	BC		;RETURN PORT IN [D,E]=DX FOR 8086
	RET

FNOUT	CALL	SETIO		;GET PORT IN [B,C] AND DATA IN [A] & [E]
	OUT	(C),A		;<--- OUTPUT TO PORT DX=[D,E] FROM [A]
	RET

;	'WAIT' BASIC command
;
;	The WAIT CHANNEL#,MASK,MASK2 waits until the status
;	returned by CHANNEL# is non zero when XORed with MASK2
;	and then ANDed with MASK. If MASK2 is not present it is assumed
;	to be zero.
;
FNWAIT	CALL	SETIO		;SET UP FOR WAIT
	PUSH	BC		;<--> SAVE THE I/O PORT
	PUSH	AF		;SAVE THE MASK
	LD	E,00H		;DEFAULT MASK2 TO ZERO
	DEC	HL
	CALL	CHRGTR		;SEE IF THE STATEMENT ENDED
	JR	Z,NOTTHR	;IF NO THIRD ARGUMENT SKIP THIS
	CALL	SYNCHR
	DB	','		;MAKE SURE THERE IS A ","
	CALL	GETBYT
NOTTHR	POP	AF		;REGET THE "AND" MASK
	LD	D,A		;KEEP AND MASK IN [D]
	POP	BC		;<-->	GET BACK THE PORT NUMBER
				;	GET READY TO READ PORT
LOPINP	IN	A,(C)		;<--> READ BYTE INTO [A]
	XOR	E		;XOR WITH MASK2
	AND	D		;AND WITH MASK
	JR	Z,LOPINP	;LOOP UNTIL RESULT IS NON-ZERO
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
	JR	NZ,NOTLPR	;NO
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
	JR	NC,LSTCOM
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
	JR	Z,LIST41	;THEN DONT PRINT SPACE
	LD	A,' '
	CALL	OUTDO		;PRINT A SPACE AFTER THE LINE #
LIST41	CALL	BUFLIN		;UNPACK THE LINE INTO BUF
	LD	HL,BUF		;POINT AT THE START OF THE UNPACKED CHARACTERS
	CALL	OUTSTRZ		;PRINT THE LINE
	CALL	CRDO		;PRINT CRLF
	JR	LIST4		;GO BACK FOR NEXT LINE

;	Output null-terminated string
OUTSTRZ	LD	A,(HL)
	OR	A
	RET	Z		;IF =0 THEN END OF LINE
	CALL	OUTCHR		;OUTPUT CHAR AND CHECK FOR LF
	INC	HL		;INCR POINTER
	JR	OUTSTRZ		;PRINT NEXT CHAR

;	Unpack the line into buf
BUFLIN	LD	BC,BUF		;GET START OF TEXT BUFFER
	LD	D,0FFH		;GET ITS LENGTH INTO [D]
	XOR	A		;SET MODE OF DECRUNCH
	LD	(DORES),A	;<---	BIT0 IS QUOTE, BIT1 IS DATA, BIT2 IS REM
	XOR	A		;--->	SET ON SPECIAL CHAR FOR SPACE INSERTION
	LD	(TEMPA),A
	CALL	PROCHK		;ONLY PROCEED IF OK
	JR	PLOOP2		;START HERE

PLOOP	INC	BC		;INCREMENT DEPOSIT PTR.
	INC	HL		;ADVANCE TEXT PTR
	DEC	D		;BUMP DOWN COUNT
	RET	Z		;IF BUFFER FULL, RETURN
PLOOP2	LD	A,(HL)		;GET CHAR FROM BUF
	OR	A		;SET CC'S
	LD	(BC),A		;SAVE THIS CHAR
	RET	Z		;IF END OF SOURCE BUFFER, ALL DONE.
	CP	OCTCON		;IS IT SMALLER THAN SMALLEST EMBEDDED CONSTANT?
	JR	C,NTEMBL	;YES, DONT TREAT AS ONE
	CP	DBLCON+1	;IS IT EMBEDDED CONSTANT?
	LD	E,A		;SAVE CHAR IN [E]
	JR	C,PRTVAR	;PRINT LEADING SPACE IF NESC.
	CP	'"'		;--->	IS IT A QUOTATION
	JR	NZ,BFCHKC	;	IF NOT CHECK FOR COLON
	LD	A,(DORES)	;	 COMPLEMENT THE QUOTE BIT
	XOR	01H		;
	LD	(DORES),A	;
	LD	A,'"'		;	RESTORE THE CHARACTER
BFCHKC	CP	':'		;	IS IT A COLON ENDING DATA?
	JR	NZ,NTEMBL	;
	LD	A,(DORES)	;	DON'T END IF IN QUOTE
	RRA			;
	JR	C,QTCOLN	;
	RLA			;
	AND	0FDH		;	TURN OFF BIT1 (DATA BIT)
	LD	(DORES),A	;
QTCOLN	LD	A,':'		;<---
NTEMBL	OR	A		;SET CC'S
	JP	M,PLOOPR	;RESERVED WORD OF SOME KIND
	LD	E,A		;SAVE CHAR IN [E]
	CP	'.'		;DOT IS PART OF VAR NAME
	JR	Z,PRTVAR
	CALL	TSTANM		;IS CHAR ALPHANUMERIC
	JR	NC,PRTVAR	;ALPHANUMERIC
	XOR	A		;MAKE SPECIAL
	JR	PLOOPH

PRTVAR	LD	A,(TEMPA)	;WHAT DID WE DO LAST?
	OR	A		;SET CONDITION CODES
	JR	Z,PLOOPG	;SPECIAL, NEVER INSERT SPACE
	INC	A		;IN RESERVED WORD?
	JR	NZ,PLOOPG	;NO
	LD	A,' '		;PUT OUT SPACE BEFORE RESWORD
	LD	(BC),A		;STORE IN BUFFER
	INC	BC		;INCREMENT POINTER INTO BUFFER
	DEC	D		;SPACE LEFT?
	RET	Z		;NO, DONE
PLOOPG	LD	A,01H		;STORE FLAG SAYING IN VAR
PLOOPH	LD	(TEMPA),A
	LD	A,E		;GET BACK CHAR WE HAD
	CP	OCTCON		;IS IT SMALLER THAN SMALLEST EMBEDDED CONSTANT?
	JR	C,PLOOPZ	;YES, DONT TREAT AS ONE
	CP	DBLCON+1	;IS IT EMBEDED CONSTANT?
	JP	C,NUMLIN	;YES, UNPACK IT
PLOOPZ	LD	(BC),A		;MAKE SURE BYTE STORED AFTER SPACE
	JP	PLOOP		;STORE IN BUFFER

PLOOPR	LD	A,(DORES)	;--->	SEEWHAT OUR UNCRUNCH MODE IS
	RRA			;	THE LSB IS THE QUOTE BIT
	JR	C,GPLOOP	;
	RRA			;	GET THE REM BIT
	RRA			;	AND SEE IF SET
	JR	NC,CHKDRS	;<---	IF NOT JUST CHECK DATA BIT
	LD	A,(HL)		;GET CHAR; MUST SEE IF ITS SNGQTK
	CP	SNGQTK		;AND PRECEDED BY ":REM"
	PUSH	HL		;--->
	PUSH	BC		;	SAVE BUFFER POINTER
	LD	HL,NOSGNQ	;	PLACE TO RETURN ON FAILURE
	PUSH	HL
	RET	NZ
	DEC	BC
	LD	A,(BC)
	CP	'M'
	RET	NZ
	DEC	BC
	LD	A,(BC)
	CP	'E'
	RET	NZ
	DEC	BC
	LD	A,(BC)
	CP	'R'
	RET	NZ
	DEC	BC
	LD	A,(BC)
	CP	':'
	RET	NZ
	POP	AF		;	GET RID OF RETURN ON FAIL ADDRESS
	POP	AF		;	GET RID OF BAD BUFFER POINTER
	POP	HL		;	GET BACK POINTER INTO LINE
	INC	D		;	UPDATE CHAR COUNT
	INC	D
	INC	D
	INC	D
	JR	RESEXP

NOSGNQ	POP	BC		;	GET BACK THE BUFFERPOINTER
	POP	HL		;	GET BACK SOURCE LINE POINTER
	LD	A,(HL)		;	GET BACK THE CHARACTER
GPLOOP	JP	PLOOP

;	Set DATA flag
DATSET	LD	A,(DORES)	;	BIT INDICATING INSIDE DATA
	OR	02H		;	IS BIT1
SETDRS	LD	(DORES),A
	XOR	A
	RET

;	Set REM flag
REMSET	LD	A,(DORES)
	OR	04H
	JR	SETDRS

CHKDRS	RLA			;	GET DATA BIT INTO CARRY
	JR	C,GPLOOP
	LD	A,(HL)		;	GET BACK THE CHARACTER
	CP	DATATK		;	NEED TO SET A BIT
	CALL	Z,DATSET	;	Set DATA flag
	CP	REMTK
	CALL	Z,REMSET	;	Set REM flag
RESEXP	LD	A,(HL)
	INC	A		;	SET ZERO IF FN TOKEN
	LD	A,(HL)		;	GET CHAR BACK
	JR	NZ,NTFNTK	;	NOT FUNCTION JUST TREAT NORMALLY
	INC	HL		;	BUMP POINTER
	LD	A,(HL)		;	GET CHAR
	AND	7FH		;	TURN OFF HIGH BIT
;	No 0FFH (preceding a function token + 80H)
NTFNTK	INC	HL		;	ADVANCE TO POINT AFTER
	CP	ELSETK		;	ELSE?
	JR	NZ,NOTELS
	DEC	BC
	INC	D		;<---
;	No ELSE (skip deleting preceding ':')
NOTELS	CP	WHILETK		;MIGHT HAVE AN EXTRA "+" IN WHILE FORMULA
	JR	NZ,BFNTWH	;SO SKIP OVER IT IF IT'S THERE
	LD	A,(HL)		;GET CHARACTER TO SEE IF ITS PLUSTK
	INC	HL		;ASSUME IS PLUSTK
	CP	PLUSTK		;MIGHT NOT BE PLUS IF BINARY SAVED IN
	LD	A,WHILETK	;RESTORE TOKEN VALUE
	JR	Z,BFNTWH	;VERSION OF BASIC BEFORE CRUNCH CHANGED
	DEC	HL		;MOVE POINTER BACK
;	No WHILE (skip ungetting following char except '+')
BFNTWH:
	IF	GFX
	CP	FPOINTTK	;===>	IS &POINT ?
	JR	NZ,BFNTFP	;	IF NOT
	PUSH	HL		;	SAVE CODE POINTER
	LD	HL,FPOINT$	;	'&POINT'
	JR	BFCPRS		;	COPY CHARS

;	No &POINT
BFNTFP	CP	FVIEWTK		;	IS &VIEW ?
	JR	NZ,BFNTFV	;	IF NOT
	PUSH	HL		;	SAVE CODE POINTER
	LD	HL,FVIEW$	;	'&VIEW'
BFCPRS	LD	A,(HL)		;	GET CHAR
	OR	A		;	END OF STRING?
	JP	Z,NTSPCT	;	DONE TOKEN
	LD	(BC),A		;	PUT CHAR
	INC	BC		;	BUMP POINTERS
	INC	HL		;
	DEC	D		;	DECR CHAR COUNT
	JP	Z,PPSWRT	;	IF BUFFER FULL, POP HL & RET
	JR	BFCPRS		;<===

;	No &VIEW
BFNTFV:
	ENDIF
	PUSH	HL		;SAVE TEXT PTR.
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
	JR	Z,RESSR3	;IF END OF THIS CHARS TABLE, GO BACK & BUMP C
	INC	HL		;BUMP SOURCE PTR
	JP	P,RESSR2	;IF NOT END OF THIS RESWRD, THEN KEEP LOOKING
	LD	A,(HL)		;GET PTR TO RESERVED WORD VALUE
	CP	B		;SAME AS THE ONE WE SEARCH FOR?
	JR	NZ,RESSR1	;NO, KEEP LOOKING.
	EX	DE,HL		;SAVE FOUND PTR IN [H,L]
	CP	USRTK		;USR FUNCTION TOKEN?
	JR	Z,NOISPA	;DONT INSERT SPACE
	CP	FNTK		;IS IT FUNCTION TOKEN?
NOISPA	LD	A,C		;GET LEADING CHAR
	POP	DE		;RESTORE LINE CHAR COUNT
	POP	BC		;RESTORE DEPOSIT PTR
	LD	E,A		;SAVE LEADING CHAR
	JR	NZ,NTFNEX	;NOT "FN" EXPANSION
	LD	A,(TEMPA)	;SET CC'S ON TEMPA
	OR	A
	LD	A,00H		;CLEAR RESWRD FLAG - MARK AS SPECIAL
	LD	(TEMPA),A	;SET FLAG
	JR	MORLNZ		;DO EXPANSION

;	No USR or FN, don't strip following ' '
NTFNEX	CP	'['		;WAS IT A SPECIAL CHAR?
	JR	NZ,NTSPCH	;NON-SPECIAL CHAR
	XOR	A		;SET NON-SPECIAL
	LD	(TEMPA),A
	JR	MORPUR		;PRINT IT

;	No '[', don't strip following ' '
NTSPCH	LD	A,(TEMPA)	;WHAT DID WE DO LAST?
	OR	A		;SPECIAL?
	LD	A,0FFH		;FLAG IN RESERVED WORD
	LD	(TEMPA),A	;CLEAR FLAG
;	Check to append ' '
MORLNZ	JR	Z,MORLN0	;GET CHAR AND PROCEED
	LD	A,' '		;PUT SPACE IN BUFFER
	LD	(BC),A
	INC	BC
	DEC	D		;ANY SPACE LEFT IN BUFFER
	JP	Z,PPSWRT	;NO, RETURN
MORLN0	LD	A,E
	JR	MORLN1		;CONTINUE
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
	JR	NZ,NTSPCT	;NO
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
	JR	Z,SAVBAS	;YES, PRINT IT
	CP	HEXCON		;HEX CONSTANT?
	LD	E,'H'		;ASSUME SO.
	JR	NZ,NUMSLN	;NOT BASE CONSTANT
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
	JR	C,TYPSET
	LD	E,'!'		;ASSUME SINGLE PREC.
	JR	Z,TYPSET	;IS CONTYP=4, WAS SINGLE
	LD	E,'#'		;DOUBLE PREC INDICATOR
TYPSET	LD	A,(HL)		;GET LEADING CHAR
	CP	' '		;LEADING SPACE
	CALL	Z,INXHRT	;GO BY IT
NUMSL2	LD	A,(HL)		;GET CHAR FROM NUMBER BUFFER
	INC	HL		;BUMP POINTER
	OR	A		;SET CC'S
	JR	Z,NUMDN		;IF ZERO, ALL DONE.
	LD	(BC),A		;SAVE CHAR IN BUF.
	INC	BC		;BUMP PTR
	DEC	D		;SEE IF END OF BUFFER
	RET	Z		;IF END OF BUFFER, RETURN
	LD	A,(CONTYP)	;GET TYPE OF CONSTANT TO BE PRINTED
	CP	04H		;TEST FOR SINGLE OR DOUBLE PRECISION
	JR	C,NUMSL2	;NO, WAS INTEGER
	DEC	BC		;PICK UP SAVED CHAR
	LD	A,(BC)		;EASIER THAN PUSHING ON STACK
	INC	BC		;RESTORE TO POINT WHERE IT SHOULD
	JR	NZ,DBLSCN	;IF DOUBLE, DONT TEST FOR EMBEDED "."
	CP	'.'		;TEST FOR FRACTION
	JR	Z,ZERE		;IF SINGLE & EMBEDED ., THEN DONT PRINT !
;	Double Precision specifier (exponential syntax, e.g. -1.09432D-06)
DBLSCN	CP	'D'		;DOUBLE PREC. EXPONENT?
	JR	Z,ZERE		;YES, MARK NO VALUE TYPE INDICATOR NESC.
;	Exponential format specifier (e.g. -1.09E-06)
	CP	'E'		;SINGLE PREC. EXPONENT?
	JR	NZ,NUMSL2	;NO, PROCEED
ZERE	LD	E,00H		;MARK NO PRINTING OF TYPE INDICATOR
	JR	NUMSL2		;KEEP MOVING NUMBER CHARS INTO BUF

NUMDN	LD	A,E		;GET FLAG TO INDICATE WHETHER TO INSERT
	OR	A		;A "D" AFTER DOUBLE PREC. #
	JR	Z,NOD		;NO, DONT INSERT IT
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
	JR	NC,FCERRG1	;MUST HAVE A MATCH ON THE UPPER BOUND
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
	JR	NZ,MLOOP	;NO
	LD	H,B
	LD	L,C
;--->	Clear SCALARS and ARRAYS incase we are replacing line which may force
;<---	 garbage collection in BLTU
	LD	(VARTAB),HL
	LD	(ARYTAB),HL	;--->
	LD	(STREND),HL	;<---
	RET

	IF	GFX
;	===>	BASICG ADDITIONS BEGIN
;=============================================================================
;	GENGRP  GENERALIZED GRAPHICS    /WHG
; ## GENGRP.ASM ##
;
;	These are the general graphics routines that assume the "Machine
;	Independent" Graphics Interface. They deal within a  16-bit
;	graphics coordinate system for both X and Y. Attributes range from
;	0 TO 255.
;
;	HLFDE,SCAND,ATRSCN,SCAN1,DOGRPH,XCHGX,XCHGY,XDELT,YDELT
;
;	These are the entry points into the Machine Dependent Graphics
;	Routines. They follow standard conventions to avoid modifications
;	to code in this package for specific implementations
;
;	SCALXY,MAPXYC,UPC,DOWNC,LEFTC,RIGHTC
;	READC,SETATR,NSETCX
;	FETCHC,STOREC,SETC
;
;-----------------------------------------------------------------------------
;	SCAN A COORDINATE - SCAN1 AND SCAND
; ## GENGRP.ASM:86 ##
;
;	Allow a coordinate of the form (X,Y) or STEP(X,Y)
;	The latter is relative to the graphics AC.
;	The graphics AC is updated with the new value
;	result is returned with [B,C]=X and [D,E]=Y
;	Call SCAN1 to get first in a set of two pairs since it allows
;	a null argument to imply the current AC value and
;	it will skip a "@" if one is present
SCAN1	LD	A,(HL)		;GET THE CURRENT CHARACTER
	CP	'@'		;ALLOW MEANINGLESS "@"
	CALL	Z,CHRGTR	;BY SKIPPING OVER IT
	LD	BC,0000H	;ASSUME NO COODINATES AT ALL (-SECOND)
	LD	D,B
	LD	E,C
	CP	MINUTK		; "-", SEE IF ITS SAME AS PREVIOUS
	JR	Z,SCANN		;USE GRAPHICS ACCUMULATOR
;
;	The standard entry point
;
SCAND	LD	A,(HL)		;GET THE CURRENT CHARACTER
	CP	STEPTK		;IS IT RELATIVE?    
				; If STEP is used, coordinates are interpreted 
				; relative to the current cursor position.
				; In this case the values can also be negative.
	PUSH	AF		;REMEMBER
	CALL	Z,CHRGTR	;SKIP OVER $STEP TOKEN
	CALL	SYNCHR		
	DB	'('		;SKIP OVER OPEN PAREN
	CALL	GETIN2		;SCAN X INTO [D,E]
	POP	AF		;RECALL IF RELATIVE OR NOT
	PUSH	AF
	JR	Z,L7D0C		;Yes, skip
	PUSH	HL		;No, adjust with viewport
	LD	HL,(VIEWX1)	;Viewport upper left X-coordinate
	ADD	HL,DE
	EX	DE,HL
	POP	HL
L7D0C	PUSH	DE		;SAVE WHILE SCANNING Y
	CALL	SYNCHR		
	DB	','		;SCAN COMMA
	CALL	GETIN2		;GET Y INTO [D,E]
	CALL	SYNCHR		
	DB	')'
	POP	BC		;GET BACK X INTO [B,C]
	POP	AF		;RECALL IF RELATIVE OR NOT
	JR	Z,SCANN		;YES, SKIP
	PUSH	HL		;No, adjust with viewport
	LD	HL,(VIEWY1)	
	ADD	HL,DE
	EX	DE,HL
	POP	HL
SCANN	PUSH	HL		;SAVE TEXT POINTER
	LD	HL,(GRPACX)	;GET OLD POSITION
	JR	Z,SCXREL	;IF ZERO,RELATIVE SO USE OLD BASE
	LD	HL,0000H	;IN ABSOLUTE CASE, JUST Y USE ARGEUMENT

SCXREL	ADD	HL,BC		;ADD NEW VALUE
	LD	(GRPACX),HL	;UPDATE GRAPHICS ACCUMLATOR
	LD	(GXPOS),HL	;STORE SECOND COORDINTE FOR CALLER
	LD	B,H		;RETURN X IN BC
	LD	C,L
	LD	HL,(GRPACY)	;GET OLDY POSITION
	JR	Z,SCYREL	;IF ZERO, RELATIVE SO USE OLD BASE
	LD	HL,0000H	;ABSOLUTE SO OFFSET BY 0

SCYREL	ADD	HL,DE
	LD	(GRPACY),HL	;UPDATE Y PART OF ACCUMULATOR
	LD	(GYPOS),HL	;STORE Y FOR CALLER
	EX	DE,HL		;RETURN Y IN [D,E]
	POP	HL		;GET BACK THE TEXT POINTER
	RET


;-----------------------------------------------------------------------------
;	PSET,PRESET,POINT
; ## GENGRP.ASM:86 ##
;
;	These are the statement and function entry points defined by this
;	package. The appropriate entries must be selected in the
;	reserved word tables to get BASIC to dispatch to these routines
;
;	PUBLIC	PSET,PRESET,POINT
;
;	PRESET (x,y)[,attribute] default attibute to BAKCLR
PRESET	LD	A,(BAKCLR)	;Background color (=0)
	JR	PSETC		;Set or reset pixel

;	PSET (x,y)[,attribute] default attribute to FORCLR
PSET	LD	A,(FORCLR)	;Get default color (PSET=foreground)
	;Set or reset pixel
PSETC	PUSH	AF		;SAVE DEFAULT ATTRIBUTE
	CALL	SCAND		;SCAN A SINGLE COORDINATE
	POP	AF		;GET BACK DEFAULT ATTRIBUTE
	CALL	ATRENT		;SCAN POSSIBLE ATTRIBUTE
	PUSH	HL		;SAVE TEXT POINTER
	CALL	SCALXY		;SCALE INTO BOUNDS
	JR	NC,PSTNOT	;NO PSET IF NOT IN BOUNDS
	CALL	MAPXYC		;MAP INTO A "C"           
				; Find position in VRAM. 
				; CLOC=memory address, CMASK=color pixelmask
	CALL	SETC		;ACTUALLY DO THE SET
PSTNOT	POP	HL		;RESTORE TEXT POINTER
	RET

;-----------------------------------------------------------------------------
;	POINT (X,Y) returns the attribute stored at that point
;	It returns -1 if the point is out of bounds
FPOINT	CALL	CHRGTR		;POINT IS RECOGNIZED IN EVAL
				;SO NEED TO SKIP ONE MORE CHAR
	PUSH	HL		;Save the text pointer.
	CALL	FETCHC		;Preserve the graphics cursor, GXPOS,
	POP	DE		;and GYPOS across the POINT function
	PUSH	HL		;so cases like
	PUSH	AF		;LINE (x1,y1)-(x2,y2),POINT(x3,y3) will
	LD	HL,(GYPOS)	;work correctly.
	PUSH	HL
	LD	HL,(GXPOS)	
	PUSH	HL
	LD	HL,(GRPACY)	
	PUSH	HL
	LD	HL,(GRPACX)	
	PUSH	HL
	EX	DE,HL		;Put the text pointer back in HL.
	CALL	SCAND		;READ THE SPECIFICATION OF THE POINT
	PUSH	HL		;SAVE THE TEXT POINTER
	CALL	SCALXY		;SCALE THE POINT
	LD	HL,0FFFFH	;ASSUME ILLEGAL POINT
	JR	NC,PNTNOT	;NOT LEGAL - RETURN -1
	CALL	MAPXYC		
	CALL	READC		;READ OUT THE ATTRIBUTE
	LD	L,A
	LD	H,00H
PNTNOT	CALL	MAKINT		
	POP	DE		;Restore text pointer
	POP	HL		
	LD	(GRPACX),HL	
	POP	HL
	LD	(GRPACY),HL	
	POP	HL		;Restore GXPOS, GYPOS, and the graphics
	LD	(GXPOS),HL	;cursor.
	POP	HL
	LD	(GYPOS),HL	
	POP	AF
	POP	HL
	PUSH	DE
	CALL	STOREC		
	POP	HL		;Retrieve the text pointer and return.
	RET

;-----------------------------------------------------------------------------
;	ATTRIBUTE SCAN
;	Look at the current position and if there is an argument read it as
;	the 8-bit attribute value to send to SETATR. If statement has ended
;	or there is a null argument, send FORCLR  to SETATR
ATRSCN	LD	A,(FORCLR)	;Get forground/background colors
ATRENT	PUSH	BC		;SAVE THE CURRENT POINT
	PUSH	DE
	LD	E,A		;SAVE DEFAULT ATTRIBUTE IN [E]
	DEC	HL		;SEE IF STATEMENT ENDED
	CALL	CHRGTR
	JR	Z,ATRFIN	;USE DEFAULT
	CALL	SYNCHR		
	DB	','		;INSIST ON COMMA
	CP	','		;ANOTHER COMMA FOLLOWS?
	JR	Z,ATRFIN	;IF SO, NULL ARGUMENT SO USE DEFAULT
	CALL	GETBYT		;GET THE BYTE
ATRFIN	LD	A,E		;GET ATTRIBUTE INTO [A]
	PUSH	HL		;SAVE THE TEXT POINTER
	CALL	STOATR		;SET THE ATTRIBUTE AS THE CURRENT ONE
	JP	C,FCERR		;ILLEGAL ATTRIBUTES GIVE FUNCTION CALL
	POP	HL
	POP	DE		;GET BACK CURRENT POINT
	POP	BC
	JP	CHRGT2


;-----------------------------------------------------------------------------
;	UTILITY ROUTINES FOR LINE CODE
; ## GENGRP.ASM:272 ##
;
;	XDELT sets [H,L]=ABS(GXPOS-[B,C]) and sets Carry if [B,C].GT.GXPOS
;	All registers except [H,L] and [A,PSW] are preserved
;	Note: [H,L] will be a Delta between GXPOS and [B,C] - add 1 for an X "COUNT"
;
XDELT	LD	HL,(GXPOS)	;GET ACCUMULATOR POSITION
	LD	A,L
	SUB	C
	LD	L,A		;DO SUBTRACT INTO [H,L]
	LD	A,H
	SBC	A,B
	LD	H,A
CNEGHL	RET	NC		;IF NO CARRY, NO NEED TO NEGATE COUNT

;-----------------------------------------------------------------------------
;	Negate HL
;	Entry:	HL	Value to negate
;	Exit:	DE	= -HL
NEGHL	XOR	A		;STANDARD [H,L] NEGATE
	SUB	L
	LD	L,A
	SBC	A,H
	SUB	L
	LD	H,A
	SCF			;FLAG THAT NEGATE TOOK PLACE
	RET

;-----------------------------------------------------------------------------
;	YDELT sets [H,L]=ABS(GYPOS-[D,E]) and sets Carry if [D,E].GT.GYPOS
;	All registers except [H,L] and [A,PSW] are preserved
;
YDELT	LD	HL,(GYPOS)	;GET ACCUMULATOR POSITION
	LD	A,L
	SUB	E
	LD	L,A		;DO SUBTRACT INTO [H,L]
	LD	A,H
	SBC	A,D
	LD	H,A
	JR	CNEGHL

;-----------------------------------------------------------------------------
;	XCHGX exchanges [B,C] with GXPOS
;	XCHGY exchanges [D,E] with GYPOS
;	XCHGAC performs both of the above
;	None of the other registers is affected
;
;	Exchange [D,E] with GYPOS
XCHGY	PUSH	HL
	LD	HL,(GYPOS)	;Y Position of Second Coordinate
	EX	DE,HL
	LD	(GYPOS),HL	;Y Position of Second Coordinate
	POP	HL
	RET

;	Exchange ([B,C],[D,E]) with (GXPOS,GYPOS)
XCHGAC	CALL	XCHGY		;exchanges [D,E] with GYPOS

;	Exchange [B,C] with GXPOS
XCHGX	PUSH	HL
	PUSH	BC
	LD	HL,(GXPOS)	;X Position of Second Coordinate
	EX	(SP),HL
	LD	(GXPOS),HL	;X Position of Second Coordinate
	POP	BC
	POP	HL
	RET


;-----------------------------------------------------------------------------
;	LINE COMMAND
; ## GENGRP.ASM:333 ##
;
;	LINE [(x1,y1)]-(x2,y2) [,attribute[,B[F]]]
;	Draw a line from (X1,Y1) to (X2,Y2) either
;	1. Standard form -- just a line connecting the 2 points
;	2. ,B=BOXLINE -- rectangle treating (X1,Y1) and (X2,Y2) as opposite corners
;	3. ,BF=BOXFILL -- filled rectangle with (X1,Y1) and (X2,Y2) as opposite corners
;
; TRS-80:
;	LINE [(x1,y1)]-[STEP](x2,y2) [,attribute[,B[F]]] [,style]
;	STEP allows for relative coordinates (undocumented!).
;	style is a 16-bit integer.
;
GLINE	CALL	TOGRPH		;Set graphics mode if enabled with SCREEN 0
	CALL	SCAN1		;SCAN THE FIRST COORDINATE
	PUSH	BC		;SAVE THE POINT
	PUSH	DE
	CALL	SYNCHR
	DB	MINUTK		;MAKE SURE ITS PROPERLY SEPERATED
	CALL	SCAND		;SCAN THE SECOND SET
	CALL	ATRSCN		;SCAN THE ATTRIBUTE
	PUSH	HL
	LD	HL,0FFFFH
L7E22	LD	(GSTYLE),HL	;Line style
	POP	HL
	POP	DE		;GET BACK THE FIRST POINT
	POP	BC
	JR	Z,DOLINE	;IF STATEMENT ENDED ITS A NORMAL LINE
	CALL	SYNCHR		
	DB	','		;OTHERWISE MUST HAVE A COMMA
	CP	','		;HAVE ANOTHER COMMA?
	JP	Z,DOLNST	;YES: SCAN STYLE FOR LINE
	CALL	SYNCHR
	DB	'B'		;A "B"?
	JP	Z,BOXLIN	;IF JUST "B" THE NON-FILLED BOX
	CP	','		;HAVE ANOTHER COMMA?
	JP	Z,BOXLI0	;YES: SCAN STYLE FOR NON-FILLED BOX
	CALL	SYNCHR
	DB	'F'		;MUST BE FILLED BOX
DOBOXF	PUSH	HL		;SAVE THE TEXT POINTER (unneeded...)
	CALL	INIBOX		;TRS-80: Initialize BOX coordinates (Cy=1 if OK)
	POP	HL		;RESTORE THE TEXT POINTER
	RET	NC		;RETURN IF NOT OK
	PUSH	HL		;SAVE THE TEXT POINTER
	CALL	SCALXY		;SCALE FIRST POINT
	CALL	XCHGAC		;SWITCH POINTS
	CALL	SCALXY		;SCALE SECOND POINT
	CALL	YDELT		;SEE HOW MANY LINES AND SET CARRY
	CALL	C,XCHGY		;MAKE [D,E] THE SMALLEST Y
	INC	HL		;MAKE [H,L] INTO A COUNT
	PUSH	HL		;SAVE COUNT OF LINES
	CALL	XDELT		;GET WIDTH AND SMALLEST X
	CALL	C,XCHGX		;MAKE [B,C] THE SMALLEST X
	INC	HL		;MAKE [H,L] INTO A WIDTH COUNT
	PUSH	HL		;SAVE WIDTH COUNT
	CALL	MAPXYC		;MAP INTO A "C"
	POP	DE		;GET WIDTH COUNT
	POP	BC		;GET LINE COUNT
BOXLOP	PUSH	DE		;SAVE WIDTH
	PUSH	BC		;SAVE NUMBER OF LINES
	CALL	FETCHC		;LOOK AT CURRENT C
	PUSH	AF		;SAVE BIT MASK OF CURRENT "C"
	PUSH	HL		;SAVE ADDRESS
	EX	DE,HL		;SET UP FOR NSETCX WITH COUNT
	CALL	NSETCX		; IN [H,L] OF POINTS TO SETC
	POP	HL		;GET BACK STARTING C
	POP	AF		;ADDRESS AND BIT MASK
	CALL	STOREC		;SET UP AS CURRENT "C"
	CALL	DOWNC		;MOVE TO NEXT LINE DOWN IN Y
	POP	BC		;GET BACK NUMBER OF LINES
	POP	DE		;GET BACK WIDTH
	DEC	BC		;COUNT DOWN LINES
	LD	A,B
	OR	C		;SEE IF ANY LEFT
	JR	NZ,BOXLOP	;KEEP DRAWING MORE LINES
	POP	HL
	RET

;	DO LINE WITH STYLE
DOLNST	CALL	SCSTYL		;SCAN STYLE

;-----------------------------------------------------------------------------
;	DO LINE
;	Draw a line from (BC,DE) to (GXPOS,GYPOS)
;	Entry:	BC	X1
;		DE	Y1
;		GXPOS	X2
;		GYPOS	Y2
;	Saves registers, GXPOS and GYPOS
DOLINE	PUSH	BC		;SAVE COORDINATES
	PUSH	DE
	PUSH	HL		;SAVE TEXT POINTER
	CALL	DOGRPH		;draw a line from (BC,DE) to (GXPOS,GYPOS)
	LD	HL,(GRPACX)	;RESTORE ORIGINAL SECOND COORDINATE
	LD	(GXPOS),HL	
	LD	HL,(GRPACY)	;FOR BOXLIN CODE
	LD	(GYPOS),HL	
	POP	HL		;RESTORE TEXT POINTER
	POP	DE
	POP	BC
	RET


;	DO BOX WITH STYLE
BOXLI0	CALL	SCSTYL		;SCAN STYLE

;-----------------------------------------------------------------------------
;	DO BOX
;	Entry:	BC	X1
;		DE	Y1
;		GXPOS	X2
;		GYPOS	Y2
BOXLIN	PUSH	HL		;SAVE TEXT POINTER
	LD	HL,(GYPOS)	;get Y2
	PUSH	HL		;SAVE Y2
	PUSH	DE		;SAVE Y1
	EX	DE,HL		;MOVE Y2 TO Y1
	CALL	DOLINE		;DO BOTTOM LINE
	POP	HL		;MOVE Y1 TO Y2
	LD	(GYPOS),HL	
	EX	DE,HL		;RESTORE Y1 TO [D,E]
	CALL	DOLINE		;DO TOP LINE
	POP	HL		;GET BACK Y2
	LD	(GYPOS),HL	;AND RESTORE
	LD	HL,(GXPOS)	;GET X2
	PUSH	BC		;SAVE X1
	LD	B,H		;SET X1=X2
	LD	C,L		;DO RIGHT LINE
	CALL	DOLINE
	POP	HL		
	LD	(GXPOS),HL	;SET X2=X1
	LD	B,H		;RESTORE X1 TO [B,C]
	LD	C,L		
	CALL	DOLINE		;DO LEFT LINE
	POP	HL		;RESTORE THE TEXT POINTER
	RET

;-----------------------------------------------------------------------------
;	SCAN STYLE (TRS-80)
SCSTYL	PUSH	DE		;Save regs
	PUSH	BC
	CALL	SYNCHR		;Must have a comma
	DB	','
	CALL	GETIN2		;Get integer value
	LD	(GSTYLE),DE	;Set as Line style
	POP	BC		;Restore regs
	POP	DE
	RET

;-----------------------------------------------------------------------------
;
;	DOGRPH draws a line from ([B,C],[D,E]) to (GXPOS,GYPOS)
;
DOGRPH	CALL	ADJCVP		;Conv viewport rel to abs coords
	RET	NZ
	CALL	XCHGAC		;exchange ([B,C],[D,E]) with (GXPOS,GYPOS)
	CALL	YDELT		;GET COUNT DIFFERENCE IN [H,L]
	CALL	C,XCHGAC	;IF CURRENT Y IS SMALLER NO EXCHANGE
	PUSH	DE		;SAVE Y1 COORDINATE
	PUSH	HL		;SAVE DELTA Y
	CALL	XDELT
	EX	DE,HL		;PUT DELTA X INTO [D,E]
	LD	HL,RIGHTC	;ASSUME X WILL GO RIGHT
	JR	NC,LINCN2
	LD	HL,LEFTC	
LINCN2	EX	(SP),HL		;PUT ROUTINE ADDRESS ON STACK AND GET DELTA Y
	CALL	COMPAR		;SEE WHICH DELTA IS BIGGER
	JR	NC,YDLTBG	;YDELTA IS BIGGER OR EQUAL
	LD	(MINDEL),HL	;SAVE MINOR AXIS DELTA (Y)
	POP	HL		;GET X ACTION ROUTINE
	LD	(MAXUPD1),HL	;SAVE IN MAJOR ACTION ADDRESS
	LD	HL,DOWNC	;ALWAYS INCREMENT Y
	LD	(MINUPD1),HL	;WHICH IS THE MINOR AXIS
	EX	DE,HL		;[H,L]=DELTA X=MAJOR DELTA
	JR	LINCN3		;MERGE WITH YDLTBG CASE AND DO DRAW

YDLTBG	EX	(SP),HL		;ACTION ROUTINE FOR X INTO [H,L]
				;SAVE DELTA Y ON THE STACK
	LD	(MINUPD1),HL	;SAVE ADDRESS OF MINOR AXIS UPDATE
	LD	HL,DOWNC	;Y IS ALWAYS INCREMENT MODE
	LD	(MAXUPD1),HL	;SAVE AS MAJOR AXIS UPDATE
	EX	DE,HL		;[H,L]=DELTA X
	LD	(MINDEL),HL	;SAVE MINOR DELTA
	POP	HL		;[H,L]=DELTA Y=MAJOR DELTA
LINCN3	POP	DE		;GET BACK Y1
;	Major axis is one with the largest Delta
;	Minor is the other
;	Ready to draw now
;	MINUPD+1=ADDRESS to go to update minor axis coordinate
;	MAXUPD+1=ADDRESS to go to update major axis coordinate
;	[H,L]=Major Axis Delta=# of points-1
;	MINDEL=Delta on Minor Axis
;
;	Idea is
;	 Set SUM=Major Delta/2
;	 [B,C]=# of points
;	 MAXDEL=-Major Delta (convenient for adding)
;	LINE loop (LINLP3):
;	      Draw at current position
;	      Update Major Axis
;	      SUM=SUM+Minor Delta
;	      If SUM.GT.Major Delta then update Minor and SUM=SUM-Major Delta
;	      Decrement [B,C] and test for 0 -- Loop if not
;	END LOOP
	PUSH	HL		;SAVE FOR SETTING UP COUNT
	CALL	NEGHL		;SAVE MAJOR DELTA FOR SUMMING
	LD	(MAXDEL),HL	;GET POSITION INTO BITMSK AND [H,L]
	CALL	MAPXYC
	POP	DE		;START SUM AT MAXDEL/2
	PUSH	DE
	CALL	HLFDE		;GET COUNT IN [B,C]
	POP	BC		;NUMBER OF POINTS IS DELTA PLUS ONE
	INC	BC
	JR	LINLP3


;=============================================================================
;	GWSTS - GW-BASIC Common Statement Support
;-----------------------------------------------------------------------------
;	Graphics Support Specific to the 8086
; ## GWSTS.ASM:376 ##
;
;-----------------------------------------------------------------------------
;	LINLP3: Inner loop of LINE code.
LINLOP	POP	HL
	LD	A,B
	OR	C
	RET	Z		;CONTINUE UNTIL COUNT EXHAUSTED
LINLP2	CALL	MAXUPD		;UPDATE MAJOR AXIS
LINLP3	CALL	SETCST		;SETC WITH STYLE
	DEC	BC		;DECREMENT COUNT
	PUSH	HL
	LD	HL,(MINDEL)	;GET SMALL DELTA
	ADD	HL,DE		;ADD SMALL DELTA TO SUM
	EX	DE,HL
	LD	HL,(MAXDEL)	;UPDATE SUM FOR NEXT POINT
	ADD	HL,DE		;TIME TO UPDATE MINOR?
	JR	NC,LINLOP	;NO, UPDATE MAJOR AND CONTINUE
	EX	DE,HL
	POP	HL
	LD	A,B
	OR	C
	RET	Z		;CONTINUE UNTIL COUNT EXHAUSTED
	CALL	MINUPD		;ADVANCE MINOR AXIS
	JR	LINLP2		;CONTINUE

;-----------------------------------------------------------------------------
;	Half DE
;	Shift DE right one
HLFDE	LD	A,D
	OR	A		;CLEAR CARRY
	RRA			;SCALE MEANS SHIFTING RIGHT ONE
	LD	D,A
	LD	A,E
	RRA
	LD	E,A
	RET

;-----------------------------------------------------------------------------
;	SETC using Style
SETCST	PUSH	HL		;SAVE TEXT POINTER
	LD	HL,(GSTYLE)	;GET LINE STYLE POINTER
	ADD	HL,HL		;IS ACTIVE (HIGH BIT NOT SET)?
	JR	NC,L7F5F	;NO, SKIP
	INC	HL		;INCREMENT LINE STYLE POINTER
L7F5F	LD	(GSTYLE),HL	;SAVE STYLE POINTER
	POP	HL		;RESTORE TEXT POINTER
	CALL	C,SETC		;DO THE SET
	RET

;-----------------------------------------------------------------------------
;	TRS-80: Initialize BOX coordinates (Cy=1 if OK)
;	Entry:	BC	1st X-coord
;		DE	1st Y-coord
;		GXPOS	2nd X-coord
;		GYPOS	2nd Y-coord
;	Exit:	BOXLFT	min(BC,GXPOS)
;		BOXRGT	max(BC,GXPOS)
;		Carry	1 if the box is inside the viewport
INIBOX	PUSH	HL		;Save ptr
	PUSH	DE		;Save Y-coord
	PUSH	BC		;Save X-coord
	LD	D,B		;X-coord to DE
	LD	E,C		;
	LD	HL,(GXPOS)	;X Position of Second Coordinate
	CALL	ICOMP		;compare
	OR	A		;set flags
	JP	P,L7F77		;if GXPOS < X-coord
	EX	DE,HL		;  swap them
L7F77	LD	(BOXLFT),HL	;Set left edge of box
	LD	(BOXRGT),DE	;Set right edge of box
	POP	BC		;restore X-coord
	POP	DE		;restore Y-coord
	PUSH	DE		;Save Y-coord
	PUSH	BC		;Save X-coord
	LD	HL,(GYPOS)	;Y Position of Second Coordinate
	CALL	ICOMP		;compare
	OR	A		;set flags
	JP	P,L7F8D		;if GYPOS < Y-coord
	EX	DE,HL		;  swap them
L7F8D	LD	(BOXTOP),HL	;Set top edge of box (LINE ...,BF)
	LD	(BOXBOT),DE	;Set bottom edge of box (LINE ...,BF)
	CALL	CHKBOX		;Check that box is in viewport (Cy=1 if OK)
	POP	BC		;Restore X-coord
	POP	DE		;Restore Y-coord
	POP	HL		;Restore pointer
	RET

;-----------------------------------------------------------------------------
;	TRS-80: Check that box is in viewport (Cy=1 if OK)
CHKBOX	LD	HL,(VIEWX1)	;Viewport upper left X-coordinate
	LD	DE,(BOXLFT)	;Left edge of box (LINE ...,BF)
	EX	DE,HL		;Swap them
	CALL	ICOMP		;Left edge of box .LT. viewport left bound?
	OR	A		;set flags, clear Carry
	RET	M		;Yes: return
	LD	HL,(VIEWX2)	;Viewport lower right X-coordinate
	LD	DE,(BOXRGT)	;Right edge of box (LINE ...,BF)
	CALL	ICOMP		;Right side of box .GT. viewport right bound?
	OR	A		;set flags, clear Carry
	RET	M		;Yes: return
	LD	HL,(VIEWY1)	;Viewport upper left Y-coordinate
	LD	DE,(BOXTOP)	;Top edge of box (LINE ...,BF)
	EX	DE,HL		;Swap them
	CALL	ICOMP		;Top edge of box .LT. viewport upper bound?
	OR	A		;set flags, clear Carry
	RET	M		;Yes: return
	LD	HL,(VIEWY2)	;Viewport lower right Y-coordinate
	LD	DE,(BOXBOT)	;Bottom edge of box (LINE ...,BF)
	CALL	ICOMP		;Right side of box .GT. viewport right bound?
	OR	A		;set flags, clear Carry
	RET	M		;Yes: return
	SCF			;Set carry meaning that the box is fully
	RET			; enclosed inside the viewport


;=============================================================================
;	ADVGRP - ADVANCED GENERALIZED GRAPHICS STUFF
;-----------------------------------------------------------------------------
;	PAINT - FILL AN AREA WITH COLOR
; ## ADVGRP.ASM:108 ##
;
;-----------------------------------------------------------------------------
;       PAINT - Fill an area with color
;
; 	Syntax: PAINT (x,y) [, [fillc] [,bordc] ]
;
;	TRS-80:	PAINT (x,y) [, [tiling] [, [bordc] [,background ] ] ]
;		tiling = paint style as a string (pattern)
;			 or a color as a numeric value (1,default -or- 0)
;		bordc = 1,default -or- 0
;		background = chr$(255),default -or- chr$(0)
;
PAINT:				;GET (X,Y) OF START
	CALL	INTQUE		;INIT QUE
	CALL	SCAN1
	PUSH	BC		;SAVE COORDS OF START
	PUSH	DE
	XOR	A
	LD	(TLEQFL),A	;Equal tiles flag
	LD	(TILROW),A	;Current tile row
	CALL	TILSCN		;<-->	Scan the tiling or color parameter
	LD	A,(TMPATR)	;DEFAULT BORDER COLOR IS SAME AS FILL
	LD	E,A		;DEFAULT ATTRIBUTE TO [E] LIKE GETBYT
	DEC	HL
	CALL	CHRGTR
	JR	Z,GOTBRD	;NOTHING THERE - USE DEFAULT
	CALL	SYNCHR		
	DB	','		;MAKE SURE OF COMMA
	CP	','
	LD	A,(TMPATR)	;Default border attrib
	LD	E,A		
	JR	Z,GOTBRD	;NOTHING THERE - USE DEFAULT
	CALL	GETBYT
GOTBRD	LD	A,E		;BORDER ATTRIBUTE TO A
	CALL	PNTINI		;INIT PAINT STUFF & CHECK BORDER ATTRIB
	JP	C,FCERR		;if not ok, FC Error
	CALL	BKGSCN		;<-->	Scan background string chr$(0) or chr$(255)
	POP	DE		;--->
	PUSH	DE		;
	PUSH	HL
	CALL	TILMOD		;	(TLROWS?) := DE % (TILCNT?+1)
	CALL	CPYTIL		;	Copy tiles?
	CALL	CMPTIL		;	Compare tiles ??
	POP	HL		;<---
	POP	DE		;GET BACK START COORDS
	POP	BC		;
	PUSH	HL		;SAVE TXTPTR UNTIL DONE
	CALL	CHKRNG		;MAKE SURE POINT IS ON SCREEN
	CALL	MAPXYC		
	LD	DE,0001H	;ENTRY COUNT IS ONE (SKIP NO BORDER)
	CALL	DNOTHG		;<-->	?? does nothing (unknown OEM API)
	CALL	SCANR1		;SCAN RIGHT FROM INITIAL POSITION
	JR	Z,POPTRT	;STARTED ON BORDER - GET TXTPTR & QUIT
	PUSH	HL		;SAVE NO. OF POINTED PAINTED TO RIGHT
	CALL	SCANL1		;NOW SCAN LEFT FROM INITIAL POS.
	POP	DE		;GET RIGHT SCAN COUNT.
	ADD	HL,DE		;ADD TO LEFT SCAN COUNT
	EX	DE,HL		;COUNT TO [DE]
	LD	A,40H		;MAKE ENTRY FOR GOING DOWN
	CALL	ENTST1
	LD	A,(TILROW)	
	OR	0C0H		;CAUSE PAINTING UP
	LD	B,A		
	JR	STPAIN		;START PAINTING UPWARD

POPTRT	POP	HL		;GET BACK TEXTPTR
	RET

;
;	Main PAINT loop
;
PNTLOP	CALL	GETQ		;GET ONE ENTRY FROM QUEUE
	LD	A,C		;NOW GO SET UP CURRENT LOCATION
	CALL	STOREC		
STPAIN	LD	A,B		;GET DIRECTION
	LD	(PDIREC),A
	AND	3FH
	LD	(TILROW),A	;Current tile row
	LD	A,B		;GET DIRECTION
	ADD	A,A		;SEE WHETHER TO GO UP, DOWN, OR QUIT
	JR	Z,POPTRT	;IF ZERO, ALL DONE.
	PUSH	DE		;SAVE SKPCNT IN CASE TUP&TDOWN DON'T
	JR	NC,PDOWN	;IF POSITIVE, GO DOWN FIRST
	CALL	TUPC		;MOVE UP BEFORE SCANNING
	JR	PDOWN2

PDOWN	CALL	TDOWNC		;SEE IF AT BOTTOM & MOVE DOWN IF NOT
PDOWN2	POP	DE		;GET SKPCNT BACK
	JR	C,PNTLOP	;OFF SCREEN - GET NEXT ENTRY
				;SCAN RIGHT & SKIP UP TO SKPCNT BORDER
	LD	A,(TILROW)	
	LD	C,A
	LD	HL,PDIREC	;PAINT direction 40=down, C0=up, 00=stop
	LD	A,(HL)
	AND	0C0H
	OR	C
	LD	(HL),A
	CALL	DNOTHG		;?? does nothing (unknown OEM API)
	CALL	SCANR1		
	JP	Z,PNTLOP	;IF NO POINTS PAINTED, GET NEXT ENTRY
	CALL	SCANL1		;NOW SCAN LEFT FROM START POINT
	LD	E,L		;[DE] = LEFT MOVCNT
	LD	D,H
	OR	A		;SEE IF LINE WAS ALREADY PAINTED
	JR	Z,PNTLP3	;IT WAS - DON'T MAKE OVERHANG ENTRY
	DEC	HL		;IF LMVCNT.GT.1, NEED TO MAKE ENTRY
	DEC	HL		;IN OPPOSITE DIRECTION FOR OVERHANG.
	LD	A,H
	ADD	A,A		;SEE IF [HL] WAS .GT. 1
	JR	C,PNTLP3
	LD	A,(PDIREC)
	XOR	80H
	LD	C,A		; PUSH PSW
	LD	A,(TILROW)	;Current tile row
	OR	C
	PUSH	AF
	CALL	FETCHC		;GET CURRENT POINT ADDRESS
	LD	C,A		;C=CMASK
	POP	AF		; POP PSW
	LD	B,A
	CALL	PUTQ
PNTLP3	LD	HL,(MOVCNT)	;GET COUNT PAINTED DURING RIGHT SCAN
	ADD	HL,DE		;ADD TO LEFT MOVCNT
	EX	DE,HL		;ENTRY COUNT TO [DE]
	CALL	ENTSLR		;GO MAKE ENTRY.
	LD	HL,(CSAVEA)	;SET CURRENT LOCATION BACK TO END
	LD	A,(CSAVEM)	;OF RIGHT SCAN.
	CALL	STOREC		
PNTLP4	LD	HL,(SKPCNT)	;CALC SKPCNT - MOVCNT TO SEE IF
	LD	DE,(MOVCNT)	;ANY MORE BORDER TO SKIP
	OR	A
	SBC	HL,DE		
	JR	Z,GOPLOP	;NO MORE - END OF THIS SCAN
	JR	C,PNTLP6	;RIGHT OVERHANG - SEE IF ENTRY NEEDED
	EX	DE,HL		;SKIP COUNT TO [DE] FOR SCANR
	CALL	SCANR1		;HERE IF NEED TO CONTINUE RIGHT SCAN
	JR	Z,GOPLOP	;NO MORE POINTS.
	OR	A		;SEE IF LINE ALREADY PAINTED
	JR	Z,PNTLP4	;YES, DON'T ENTER ANYTHING
	EX	DE,HL		;ENTRY COUNT TO [DE]
	LD	HL,(CSAVEA)	;MAKE ENTRY AT LOCATION SAVED BY SCANR
	LD	A,(CSAVEM)	;SO WE CAN ENTER A POSITIVE SKPCNT
	LD	C,A
	LD	A,(PDIREC)	
	LD	B,A
	CALL	ENTSTK		;MAKE ENTRY
	JR	PNTLP4		;CONTINUE UNTIL SKPCNT .LE. 0

PNTLP6	CALL	NEGHL		;MAKE NEW SKPCNT POSITIVE
	DEC	HL		;IF SKPCNT-MOVCNT .LT. -1
	DEC	HL		;THEN RIGHT OVERHANG ENTRY IS NEEDED.
	LD	A,H		;SEE IF POSITIVE.
	ADD	A,A
	JR	C,GOPLOP	;OVERHANG TOO SMALL FOR NEW ENTRY
	INC	HL		;NOW MOVE LEFT TO BEGINNING OF SCAN
	PUSH	HL		;SO WE CAN ENTER A POSITIVE SKPCNT
RTOVH0	CALL	LEFTC		;START IS -(SKPCNT-MOVCNT)-1 TO LEFT
	DEC	HL		
	LD	A,H
	OR	L
	JR	NZ,RTOVH0

	POP	DE		;GET BACK ENTRY SKPCNT INTO [DE]
	LD	A,(PDIREC)	;MAKE ENTRY IN OPPOSITE DIRECTION
	XOR	80H
	CALL	ENTST1		;MAKE ENTRY
GOPLOP	JP	PNTLOP		;GO PROCESS NEXT ENTRY


;-----------------------------------------------------------------------------
;	Make entry to PAINT queue
ENTSLR	LD	A,(LFPROG)	;DON'T STACK IF SCANNED LINE
	LD	C,A		;WAS ALREADY PAINTED
	LD	A,(RTPROG)	
	OR	C
	LD	C,A
	LD	A,(TLEQFL)	;Equal tiles flag
	OR	C
	RET	Z
	LD	A,(PDIREC)	;PAINT direction 40=down, C0=up, 00=stop
ENTST1	LD	B,A		;DIRECTION IN [B]
	CALL	FETCHC		;LOAD REGS WITH CURRENT "C"
	LD	C,A		;BIT MASK IN [C]
ENTSTK	LD	A,(TILROW)	;Current tile row
	OR	B
	LD	B,A
	JP	PUTQ

;-----------------------------------------------------------------------------
;	High level right scan routine
SCANR1	CALL	SCANR		;PERFORM LOW LEVEL RIGHT SCAN
	LD	(SKPCNT),DE	;SAVE UPDATED SKPCNT
	LD	(MOVCNT),HL	;SAVE MOVCNT
	LD	A,H		;SET CC'S ON MOVCNT
	OR	L		
	LD	A,C		;GET ALREADY-PAINTED FLAG FROM [C]
	LD	(RTPROG),A	
	RET

;-----------------------------------------------------------------------------
;	High level left scan routine
SCANL1	CALL	FETCHC		;GET CURRENT LOCATION
	PUSH	HL		;AND SWAP WITH CSV
	PUSH	AF
	LD	HL,(CSAVEA)	
	LD	A,(CSAVEM)
	CALL	STOREC		;REPOS AT BEGINNING OF SCAN
	POP	AF		;REGET PLACE WHERE RT SCN STOPPED
	POP	HL
	LD	(CSAVEA),HL	;AND SAVE IT IN TEMP LOCATION
	LD	(CSAVEM),A
	CALL	SCANL		;NOW DO LOW LEVEL LEFT SCAN
	LD	A,C		;GET ALREADY-PAINTED FLAG FROM [C]
	LD	(LFPROG),A	;WHETHER IT WAS ALREADY PAINTED
L8138	RET

;-----------------------------------------------------------------------------
;	Negate DE
;	Entry:	DE	Value to negate
;	Exit:	DE	= -DE
NEGDE	EX	DE,HL		;Swap DE and HL
	CALL	NEGHL		;Negate HL
	EX	DE,HL		;Swap DE and HL back
	RET


;-----------------------------------------------------------------------------
;	TRS-80 specific code to handle tiles in PAINT
;
;	Scan the tiling parameter
TILSCN	XOR	A
	LD	(TLSTFL),A	;Clear "Tiling as string" flag
	LD	A,(FORCLR)	;Foreground color (=1)
	LD	E,A		;to E
	DEC	HL		;unget char
	CALL	CHRGTR		;re-get char
	JP	Z,NTILNG	;null, No tiling parameter
	CALL	SYNCHR		;eat comma
	DB	','
	CP	','		;is it followed by another comma?
	JR	Z,NTILNG	;if yes, skip tiling parameter
	CALL	FRMEVL		;Formula evaluator
	CALL	GETYPR		;Check value type
	PUSH	HL		;save text ptr
	JR	Z,TILSTR	;handle tiling defined as string
	CALL	CONINT		;Convert FAC to integer into DE, as a color
	POP	HL		;restore text ptr
;	No tiling parameter
NTILNG	LD	A,E		;get color (0 or 1)
	CALL	STOATR		;Check and store -attr = 00 or FF
	JP	C,FCERR		;Illegal function call
	JP	CHRGT2		;Check next char and return

;	Tiling defined as string
TILSTR	LD	A,01H		;
	LD	(TLSTFL),A	;Set "Tiling as string" flag
	LD	HL,(FACLO)	;copy string descriptor ptr to GARG
	LD	(GARG),HL
	LD	C,(HL)		;get length to C
	INC	HL
	LD	E,(HL)		;get string addr to DE
	INC	HL
	LD	D,(HL)
	EX	DE,HL		;move to HL
	LD	(TILNG$),HL	;store to Tiling string ptr
	CALL	GETSIZ		;get minimum string size in B (=1!)
	LD	A,C		;get string length
	CP	B		;check against minimum string size (=1)
	JP	C,FCERR		;if too short, FC Error
	LD	E,0FFH		;divide length by minimum size, result in E
L818B	INC	E		;Increment quotient
	SUB	B		;subtract size from length and check
	JP	NC,L818B	;loop until length is negative
	DEC	E		;incremented one time too much
	ADD	A,B		;add size to length to get the remainder
	LD	B,A		;remainder to B
	LD	A,C		;calc size - remainder to get the 
	SUB	B		; highest multiple of the size
	LD	C,A		; into C
	LD	B,00H		;add the highest multiple of the size to the
	ADD	HL,BC		; tile string pointer (addr+len-len/siz)
	LD	(TILEND),HL	;store to tile string end pointer
	LD	A,E		;get quotient length/size
	LD	(TILCNT),A	;store to tiles count = len/siz (with siz==1)
	CP	64		;must be less than 64
	JP	NC,FCERR	;if not, FC Error
	POP	HL		;restore text ptr
	RET

;-----------------------------------------------------------------------------
;	Compare tiles ??
;	Called PAINT after GOTBRD: and CALL CPYTIL
CMPTIL	LD	A,(TLSTFL)	;Tiling as string flag
	OR	A
	RET	Z
	LD	HL,(TILNG$)	;Tiling string ptr
	EX	DE,HL
	LD	HL,(BKGSTR)	;Background string
	CALL	GETSIZ		;get minimum string size in B
L81B6	LD	C,03H
	PUSH	DE
L81B9	PUSH	BC
	PUSH	HL
	CALL	MEMCMP		;Compare B bytes between (HL) and (DE)
	JP	NZ,L81CC
	CALL	RESTTL		;Reset tile pointer if end reached
	POP	HL
	POP	BC
	DEC	C
	JR	NZ,L81B9
	JP	FCERR		;Illegal function call

L81CC	POP	HL
	POP	BC
	EX	(SP),HL
	LD	C,B
	LD	B,00H
	ADD	HL,BC
	LD	B,C
	EX	DE,HL
	CALL	RESTTL		;Reset tile pointer if end reached
	POP	HL
	JR	NZ,L81B6
	RET

;-----------------------------------------------------------------------------
;	Reset tile pointer if end reached
;	Entry:	DE	Tile pointer
;	Exit:	DE	Tiline string ptr if tile pointer reached TILEND
RESTTL	PUSH	HL
	LD	HL,(TILEND)	;addr + len - len/siz
	CALL	COMPAR
	POP	HL
	RET	NZ
	LD	DE,(TILNG$)	;Tiling string ptr
	RET

;-----------------------------------------------------------------------------
;	Lexicographically compare B bytes between (HL) and (DE)
;	Entry	DE	Pointer 1
;		HL	Pointer 2
;		B	Blocks length
;	Exit	PSW	(A and F)
;			Z if blocks are equal
;			Cy if (DE) .LT. (HL)
MEMCMP	LD	A,(DE)		;Get byte @DE
	CP	(HL)		;Compare with byte @HL
	RET	NZ		;Return with flags if different
	INC	DE		;bump pointers
	INC	HL		;
	DEC	B		;decrement bytes count
	JP	NZ,MEMCMP	;and loop again until zero
	RET			;return with Z=Cy=0

;-----------------------------------------------------------------------------
;	A := A * B, by successive additions...
;	OK because B is low (and here = 1)
MULAB	DEC	B
	RET	Z
	LD	C,A
L81F7	ADD	A,C
	DEC	B
	JP	NZ,L81F7
	RET

;-----------------------------------------------------------------------------
;	Compute TILROW the tile row from DE
;
;	Entry:	DE	pixel row
;	Exit:	A	=TILROW, the tile row
;	
;	(TILROW) := DE % (TILCNT+1) by successive subtractions
;
;	Called by PAINT after GOTBRD: and before CALL CPYTIL
TILMOD	LD	A,(TLSTFL)	;get "Tiling as string" flag
	OR	A		;is set?
	RET	Z		;no, return
	LD	A,(TILCNT)	;Tiles count = len/siz with siz==1
	INC	A		;TILCNT+1
	EX	DE,HL		;DE to HL
	LD	E,A		;TILCNT+1 to DE
	LD	D,00H
L820A	OR	A		;Subtract TILCNT from HL
	SBC	HL,DE
	JP	NC,L820A	;until HL < 0
	ADD	HL,DE		;Add TILCNT to HL, we have the modulus
	LD	A,L		;Modulus to A
	LD	(TILROW),A	;Save modulus to TILROW
	RET

;-----------------------------------------------------------------------------
;	Copy tiles?
;	Called by PAINT after GOTBRD: and CALL TILMOD, TUPC and TDOWNC
CPYTIL	LD	A,(TLSTFL)	;get "Tiling as string" flag
	OR	A		;Is set?
	RET	Z		;no, return
	LD	HL,(TILNG$)	;get Tiling string ptr
	CALL	GETSIZ		;get minimum string size in B (=1!!)
	LD	A,(TILROW)	;Current tile row
	PUSH	BC		;save size
	CALL	MULAB		;A := A * B (=A!!)
	LD	E,A		;to DE
	LD	D,00H		;
	ADD	HL,DE		;add to tiling string pointer
	PUSH	HL		;save the pointer
	CALL	P3ATRB		;Copy 3 bytes from (HL) to ATRBYT
	XOR	A		;clear the "tiles equal" flag
	LD	(TLEQFL),A	;
	POP	DE		;restore the pointer to DE
	LD	HL,(BKGSTR)	;Background string
	POP	BC		;restore the size
	CALL	MEMCMP		;Compare B (=1) bytes between (HL) and (DE)
	RET	NZ		;return if no match
	LD	A,01H		;set the "tiles equal" flag
	LD	(TLEQFL),A	;
	RET

;-----------------------------------------------------------------------------
;	Decrement tile row?
;	Called by TUPC
DECTLR	LD	A,(TILROW)	;Current tile row
	DEC	A
	LD	(TILROW),A	;Current tile row
	RET	P
	LD	A,(TILCNT)	;Tiles count = len/siz with siz==1
	LD	(TILROW),A	;Current tile row
	RET

;-----------------------------------------------------------------------------
;	Increment tile row?
;	Called by TDOWNC
INCTLR	LD	A,(TILROW)	;Current tile row
	INC	A
	LD	(TILROW),A	;Current tile row
	LD	C,A
	LD	A,(TILCNT)	;Tiles count = len/siz with siz==1
	INC	A
	CP	C
	RET	NZ
	XOR	A
	LD	(TILROW),A	;Current tile row
	RET

;-----------------------------------------------------------------------------
;	Scan background string chr$(0) or chr$(255)
BKGSCN	LD	A,(TLSTFL)	;Tiling as string flag
	OR	A
	RET	Z
	DEC	HL
	CALL	CHRGTR
	JR	Z,L82A0
	CALL	SYNCHR
	DB	','
	CALL	FRMEVL		;Formula evaluator
	CALL	GETYPR
	JP	NZ,FCERR	;Illegal function call
	PUSH	HL
	CALL	FREFAC
	LD	HL,(FACLO)
	LD	C,(HL)
	INC	HL
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	EX	DE,HL
	LD	(BKGSTR),HL	;Background string
	CALL	GETSIZ		;get minimum string size in B
	LD	A,C
	CP	B
	JP	C,FCERR		;Illegal function call
	LD	HL,(GARG)	;FP argument for graphics
	LD	(FACLO),HL
	CALL	FREFAC
	POP	HL
	RET

L82A0	PUSH	HL
	LD	HL,BKGBUF	;Default background string buffer
	LD	(BKGSTR),HL	;Background string
	CALL	GETSIZ		;get minimum string size in B
	CALL	SUB8AAE		;Copy B bytes from ATRBYT to (HL)
	LD	HL,(GARG)	;FP argument for graphics
	LD	(FACLO),HL
	CALL	FREFAC
	POP	HL
	RET


;-----------------------------------------------------------------------------
;	PAINT - FILL AN AREA WITH COLOR (Cont'd)
; ## ADVGRP.ASM:303 ##
;
;	Routine for initialising queue params
;	QUELEN:contains queue length
;	PSNLEN:contains present queue length
;	QUEINP:        QUEINPUT pointer
;	QUEOUT:        QUEOUTPUT pointer
INTQUE	PUSH	HL		
	LD	HL,(STREND)	;GET BEGINNING OF FREE SPACE
	EX	DE,HL		;
	PUSH	DE
	LD	HL,(FRETOP)	;GET END OF FREE SPACE
	OR	A
	SBC	HL,DE		;HL=FREE SPACE
	LD	DE,1000		
	CALL	COMPAR		;IS IT LESS THAN 1000 BYTES
	JP	NC,INTQU2
	CALL	GARBA2		;CALL GARBAGE COLLECTION
INTQU2	POP	DE		;GET BACK FREE START
	LD	HL,(FRETOP)
	OR	A
	SBC	HL,DE
	LD	DE,12
	CALL	COMPAR
	JP	C,OMERR		;IF LESS THAN 12 BYTES GIVE MEMORY OVERFLOW
	LD	(QUELEN),HL	;SET LENGTH OF QUEUE
	LD	HL,0000H
	LD	(PSNLEN),HL	;SET PRESENT LENGTH
	LD	HL,(STREND)	;HL=BEGINNING OF FREE SPACE
	LD	(QUEINP),HL	;
	LD	(QUEOUT),HL	;INIT QUEUE POINTERS
	POP	HL		;GET BACK CALLERS HL
	RET

;-----------------------------------------------------------------------------
;	Put HL, DE, BC to queue
;	for PAINT:
;		HL = physical coordinates
;		C  = physical bit mask
;		B  = PDIREC
;		for TRS-80: ( PDIREC & 0C0H ) .OR. ( (tiles?)count & 3FH )
;		DE = move count
PUTQ	PUSH	DE		
	PUSH	HL
	LD	HL,(PSNLEN)	;GET QUEUE      PRESENT LENGTH
	LD	DE,0006H	;IS ENOUGH SPACE LEFT OUT
	ADD	HL,DE
	LD	(PSNLEN),HL	;UPDATE PRESENT LENGTH
	LD	DE,(QUELEN)	;
	CALL	COMPAR		;
	JP	NC,OMERR	;IF THE PRESENT LENGTH IS EQUAL TO MAX OR GREATER THEN GIVE ERROR
	LD	HL,(QUEINP)	
	CALL	WRAP		;CHECK FOR WRAP AROUND CASE
	POP	DE
	LD	(HL),E
	INC	HL
	LD	(HL),D
	INC	HL
	LD	(HL),C
	INC	HL
	LD	(HL),B
	INC	HL
	POP	DE
	LD	(HL),E
	INC	HL
	LD	(HL),D
	INC	HL
	LD	(QUEINP),HL	;Queue input pointer
	RET

;-----------------------------------------------------------------------------
;	Get HL, DE, BC from queue
;	for PAINT:
;		HL = physical coordinates
;		C  = physical bit mask
;		B  = PDIREC
;		for TRS-80: ( PDIREC & 0C0H ) .OR. ( (tiles?)count & 3FH )
;		DE = move count
GETQ	LD	HL,(PSNLEN)
	LD	A,H
	OR	L		;ANY ENTRYS ON STACK
	LD	B,00H		;FOR NO ENTRIES SET B TO ZERO
	RET	Z
	LD	DE,0006H
	OR	A		;DECREMENT QUEUE LENGTH BY 6
	SBC	HL,DE
	LD	(PSNLEN),HL	
	LD	HL,(QUEOUT)	;HL=DEQUE POINTER
	CALL	WRAP		;CHECK FOR WRAP AROUND
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	INC	HL
	PUSH	DE
	LD	C,(HL)
	INC	HL
	LD	B,(HL)
	INC	HL
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	INC	HL
	LD	(QUEOUT),HL
	POP	HL
	RET

;-----------------------------------------------------------------------------
;	CHECK FOR WRAP AROUND
WRAP	PUSH	HL
	LD	DE,0006H
	ADD	HL,DE
	LD	DE,(FRETOP)
	CALL	COMPAR		;ARE WE GOING OUT OF QUEUE END
	POP	HL
	RET	C		;NO
	LD	HL,(STREND)	;SET TO BEGINNING OF QUEUE
	RET


;-----------------------------------------------------------------------------
;	CIRCLE - DRAW A CIRCLE
; ## ADVGRP.ASM:445 ##
;
;       CIRCLE - Draw a circle
;
;	Syntax: CIRCLE @(x,y),radius[,atrb[,+/-startang[,+/-endang[,aspect]]]]
;
;	TRS-80: CIRCLE @(x,y),radius[,c[,+/-startang[,+/-endang[,aspect]]]]
;		where  c  is 1 (white, default) or 0 (black)
;
CIRCLE	CALL	SCAN1		;GET (X,Y) OF CENTER INTO GRPACX,Y
	CALL	SYNCHR		
	DB	','		;EAT COMMA
	CALL	INTID2		;GET THE RADIUS
	PUSH	HL		;SAVE TXTPTR
	EX	DE,HL
	LD	(GXPOS),HL	;SAVE HERE TILL START OF MAIN LOOP
	CALL	MAKINT		;PUT INTEGER INTO FAC
	CALL	CSNG		;CONVERT TO SINGLE PRECISION
	LD	BC,8035H	;LOAD REGS WITH SQR(2)/2
	LD	DE,04F3H
	CALL	FMULT		;DO FLOATING PT MULTIPLY
	CALL	CINT		;CONVERT TO INTEGER & GET INTO [HL]
	LD	(CNPNTS),HL	;CNPNTS=RADIUS*SQR(2)/2=# PTS TO PLOT
	XOR	A		;ZERO OUT CLINEF - NO LINES TO CENTER
	LD	(CLINEF),A	
	LD	(CSCLXY),A	;INITIALLY SCALING Y
	POP	HL		;REGET TXTPTR
	CALL	ATRSCN		;SCAN POSSIBLE ATTRIBUTE
	LD	C,01H		;SET LO BIT IN CLINEF FOR LINE TO CNTR
	LD	DE,0000H	;DEFAULT START COUNT = 0
	CALL	CGTCNT
	PUSH	DE		;SAVE COUNT FOR LATER COMPARISON
	LD	C,80H		;SET HI BIT IN CLINEF FOR LINE TO CNTR
	LD	DE,0FFFFH	;DEFAULT END COUNT = INFINITY
	CALL	CGTCNT
	EX	(SP),HL		;GET START COUNT, PUSH TXTPTR TILL DONE
	XOR	A
	EX	DE,HL		;REVERSE REGS TO TEST FOR .LT.
	CALL	COMPAR		;SEE IF END .GE. START
	LD	A,00H
	JR	NC,CSTPLT	;YES, PLOT POINTS BETWEEN STRT & END
	DEC	A		;PLOT POINTS ABOVE & BELOW
	EX	DE,HL		;SWAP START AND END SO START .LT. END
	PUSH	AF		;Swap sense of center line flags
	LD	A,(CLINEF)
	LD	C,A
	RLCA
	RLCA
	OR	C
	RRCA
	LD	(CLINEF),A	;Store swapped flags
	POP	AF
CSTPLT	LD	(CPLOTF),A	;SET UP PLOT POLARITY FLAG
	LD	(CSTCNT),DE	;STORE START COUNT
	LD	(CENCNT),HL	;AND END COUNT
	POP	HL		;GET TXTPTR
	DEC	HL		;NOW SEE IF LAST CHAR WAS A COMMA
	CALL	CHRGTR
	JR	NZ,CIRC1	;SOMETHING THERE
	PUSH	HL		;SAVE TXTPTR
	CALL	GTASPC		;GET DEFAULT ASPECT RATIO INTO [HL]
	LD	A,H
	OR	A		;IS ASPECT RATIO GREATER THAN ONE?
	JR	Z,CIRC2		;BRIF GOOD ASPECT RATIO
	LD	A,01H
	LD	(CSCLXY),A
	EX	DE,HL		;ASPECT RATIO IS GREATER THAN ONE, USE INVERSE
	JR	CIRC2		;NOW GO CONVERT TO FRACTION OF 256

CIRC1	CALL	SYNCHR
	DB	','		;EAT COMMA
	CALL	FRMEVL		
	PUSH	HL		;SAVE TXTPTR
	CALL	CSNG		;MAKE IT FLOATING POINT
	CALL	CMPONE		;SEE IF GREATER THAN ONE
	JR	NZ,CIRC11	;LESS THAN ONE - SCALING Y
	INC	A		;MAKE [A] NZ
	LD	(CSCLXY),A	;FLAG SCALING X
	CALL	FDIV		;RATIO = 1/RATIO
				;MAKE NUMBER FRACTION OF 256
CIRC11	LD	HL,FAC		;BY MULTIPLYING BY 2^8 (256)
	LD	A,(HL)
	ADD	A,08H		;ADD 8 TO EXPONENT
	LD	(HL),A
	CALL	CINT		;MAKE IT AN INTEGER IN [HL]
CIRC2	LD	(ASPECT),HL	;STORE ASPECT RATIO
;
;       CIRCLE algorithm
;
;       [HL] = x = radius * 2 (One bit fraction for rounding)
;       [DE] = y = 0
;       SUM = 0
; LOOP: If y is even then
;             Reflect( (x+1)/2, (y+1)/2 ) (i.e., plot points)
;             If x .LT. y then exit
;       SUM = SUM + 2 * y + 1
;       y = y + 1
;       If SUM .GE. 0   /* "IF SUM.GGWGRP.RNO" in original comment... */
;             SUM = SUM - 2 * x + 1
;             X = x - 1
;       EndIf
;       GoTo LOOP
;
	LD	DE,0000H	;INIT Y = 0
	LD	(CRCSUM),DE	;SUM = 0
	LD	HL,(GXPOS)	;X = RADIUS*2
	ADD	HL,HL
CIRCLP	LD	A,E		;TEST EVENNESS OF Y
	RRA			;TO SEE IF WE NEED TO PLOT
	JR	C,CRCLP2	;Y IS ODD - DON'T TEST OR PLOT
	PUSH	DE		;SAVE Y AND X
	PUSH	HL
	INC	HL		;ACTUAL COORDS ARE (X+1)/2,(Y+1)/2
	EX	DE,HL
	CALL	HLFDE		;(PLUS ONE BEFORE DIVIDE TO ROUND UP)
	EX	DE,HL
	INC	DE
	CALL	HLFDE
	CALL	CPLOT8
	POP	DE		;RESTORE X AND Y
	POP	HL		;INTO [DE] AND [HL] (BACKWARDS FOR CMP)
	CALL	COMPAR		;QUIT IF Y .GE. X
	JP	NC,POPTRT	;GO POP TXTPTR AND QUIT
	EX	DE,HL		;GET OFFSETS INTO PROPER REGISTERS
CRCLP2	LD	B,H		;[BC]=X
	LD	C,L
	LD	HL,(CRCSUM)	
	INC	HL		;SUM = SUM+2*Y+1
	ADD	HL,DE
	ADD	HL,DE		
	LD	A,H		;NOW CHECK SIGN OF RESULT
	ADD	A,A		
	JR	C,CNODEX	;DON'T ADJUST X IF WAS NEGATIVE
	PUSH	DE		;SAVE Y
	EX	DE,HL		;[DE]=SUM
	LD	H,B		;[HL]=X
	LD	L,C		
	ADD	HL,HL		;[HL]=2*X-1s
	DEC	HL		
	EX	DE,HL		;PREPARE TO SUBTRACT
	OR	A
	SBC	HL,DE		;CALC SUM-2*X+1
	DEC	BC		;X=X-1
	POP	DE		;GET Y BACK
CNODEX	LD	(CRCSUM),HL	;UPDATE CIRCLE SUM
	LD	H,B		;GET X BACK TO [HL]
	LD	L,C
	INC	DE		;Y=Y+1
	JR	CIRCLP

;-----------------------------------------------------------------------------
CPLSCX	PUSH	DE		
	CALL	SCALEY
	POP	HL		;GET UNSCALED INTO [HL]
	LD	A,(CSCLXY)	;SEE WHETHER ASPECT WAS .GT. 1
	OR	A
	RET	Z		;DON'T SWAP IF ZERO
	EX	DE,HL
	RET

;-----------------------------------------------------------------------------
;
;	CIRCLE: Reflect the points around center
;	[HL]=x offset from center, [DE]=y offset from center
;
CPLOT8	LD	(CPCNT),DE	;POINT COUNT IS ALWAYS = Y
	PUSH	HL		;SAVE X
	LD	HL,0000H	;START CPCNT8 OUT AT 0
	LD	(CPCNT8),HL	
	CALL	CPLSCX		;SCALE Y AS APPROPRIATE
	LD	(CXOFF),HL	;SAVE CXOFF
	POP	HL		;GET BACK X
	EX	DE,HL
	PUSH	HL		;SAVE INITIAL [DE]
	CALL	CPLSCX		;SCALE X AS APPROPRIATE
	LD	(CYOFF),DE	
	POP	DE		;GET BACK INITIAL [DE]
	CALL	NEGDE		;START: [DE]=-Y,[HL]=X,CXOFF=Y,CY=X
	CALL	CPLOT4		;PLOT +X,-SY -Y,-SX -X,+SY +Y,-SX
	PUSH	HL
	PUSH	DE
	LD	HL,(CNPNTS)	;GET # PNTS PER OCTANT
	LD	(CPCNT8),HL	;AND SET FOR DOING ODD OCTANTS
	LD	DE,(CPCNT)	;GET POINT COUNT
	OR	A		
	SBC	HL,DE		;ODD OCTANTS ARE BACKWARDS SO
	LD	(CPCNT),HL	;PNTCNT = PNTS/OCT - PNTCNT
	LD	HL,(CXOFF)	;NEED TO NEGATE CXOFF TO START OUT RIGHT
	CALL	NEGHL
	LD	(CXOFF),HL	
	POP	DE
	POP	HL
	CALL	NEGDE		;ALSO NEED TO MAKE [DE]=-SX=-[DE]
				;PLOT +Y,-SX -X,-SY -Y,+SX +X,+SY
				;(FALL THRU TO CPLOT4)
CPLOT4	LD	A,04H		;LOOP FOUR TIMES
CPLOT	PUSH	AF		;SAVE LOOP COUNT
	PUSH	HL		;SAVE BOTH X & Y OFFSETS
	PUSH	DE
	PUSH	HL		;SAVE TWICE
	PUSH	DE
	LD	DE,(CPCNT8)	;GET NP*OCTANT*8
	LD	HL,(CNPNTS)	;ADD SQR(2)*RADIUS FOR NEXT OCTANT
	ADD	HL,HL
	ADD	HL,DE
	LD	(CPCNT8),HL	;UPDATE FOR NEXT TIME
	LD	HL,(CPCNT)	;CALC THIS POINT'S POINT COUNT
	ADD	HL,DE		;ADD IN PNTCNT*OCTANT*NP
	EX	DE,HL		;SAVE THIS POINT'S COUNT IN [DE]
	LD	HL,(CSTCNT)	;GET START COUNT
	CALL	COMPAR
	JR	Z,CLINSC	;SEE IF LINE TO CENTER REQUIRED
	JR	NC,CNBTWN	;IF SC .GT. PC, THEN NOT BETWEEN
	LD	HL,(CENCNT)	;GET END COUNT
	CALL	COMPAR
	JR	Z,CLINEC	;GO SEE IF LINE FROM CENTER NEEDED
	JR	NC,CBTWEN	;IF EC .GT. PC, THEN BETWEEN
CNBTWN	LD	A,(CPLOTF)	;SEE WHETHER TO PLOT OR NOT
	OR	A		;IF NZ, PLOT POINTS NOT IN BETWEEN
	JR	NZ,CPLTIT	;NEED TO PLOT NOT-BETWEEN POINTS
	JR	GCPLFN		;DON'T PLOT - FIX UP STACK & RETURN

CLINEC	LD	A,(CLINEF)	;GET CENTER LINE FLAG BYTE
	ADD	A,A		;BIT 7=1 MEANS DRAW LINE FROM CENTER
	JR	NC,CPLTIT	;NO LINE REQUIRED - JUST PLOT POINT
	JR	CLINE		;LINE REQUIRED.

CLINSC	LD	A,(CLINEF)	;GET CENTER LINE FLAG BYTE
	RRA			;BIT 0=1 MEANS LINE FROM CENTER NEEDED.
	JR	NC,CPLTIT	;NO LINE REQUIRED - JUST PLOT POINT
CLINE	POP	DE		;GET X & Y OFFSETS
	POP	HL
	CALL	GTABSC		;GO CALC TRUE COORDINATE OF POINT
	CALL	CLINE2		;DRAW LINE FROM [BC],[DE] TO CENTER
	JR	CPLFIN

CBTWEN	LD	A,(CPLOTF)	;SEE WHETHER PLOTTING BETWEENS OR NOT
	OR	A
	JR	Z,CPLTIT	;IF Z, THEN DOING BETWEENS
GCPLFN	POP	DE		;CLEAN UP STACK
	POP	HL
	JR	CPLFIN

CPLTIT	POP	DE		;GET X & Y OFFSETS
	POP	HL
	CALL	GTABSC		;CALC TRUE COORDINATE OF POINT
	CALL	SCALXY		;SEE IF POINT OFF SCREEN
	JR	NC,CPLFIN	;NC IF POINT OFF SCREEN - NO PLOT
	CALL	MAPXYC		
	CALL	SETC		;PLOT THE POINT
CPLFIN	POP	DE		;GET BACK OFFSETS
	POP	HL
	POP	AF		;GET BACK LOOP COUNT
	DEC	A
	RET	Z		;QUIT IF DONE.
	PUSH	AF
	PUSH	DE		;SAVE X OFFSET
	LD	DE,(CXOFF)	;SWAP [HL] AND CXOFF
	CALL	NEGDE		;NEGATE NEW [HL]
	LD	(CXOFF),HL	
	EX	DE,HL
	POP	DE
	PUSH	HL
	LD	HL,(CYOFF)	;SWAP [DE] AND CYOFF
	EX	DE,HL		;NEGATE NEW [DE]
	LD	(CYOFF),HL	
	CALL	NEGDE
	POP	HL
	POP	AF		; POP PSW
	JP	CPLOT		;PLOT NEXT POINT

;-----------------------------------------------------------------------------
CLINE2	LD	HL,(GRPACX)	;DRAW LINE FROM [BC],[DE]
	LD	(GXPOS),HL	;TO GRPACX,Y
	LD	HL,(GRPACY)	
	LD	(GYPOS),HL	
	LD	HL,0FFFFH
	LD	(GSTYLE),HL	;Clear line style
	JP	DOGRPH		;GO DRAW THE LINE

;-----------------------------------------------------------------------------
;	GTABSC - Get absolute coords
;	([BC],[DE])=(GRPACX+[HL],GRPACY+[DE])
GTABSC	PUSH	DE		;SAVE Y OFFSET FROM CENTER
	LD	DE,(GRPACX)	;GET CENTER POS
	ADD	HL,DE		;ADD TO DX
	LD	B,H		;[BC]=X CENTER + [HL]
	LD	C,L
	POP	DE		
	LD	HL,(GRPACY)	;GET CENTER Y
	ADD	HL,DE		
	EX	DE,HL		;[DE]=Y CENTER + [DE]
	RET

;-----------------------------------------------------------------------------
SCALEY	LD	HL,(ASPECT)	;SCALE [DE] BY ASPECT RATIO
	LD	A,L		;CHECK FOR *0 AND *1 CASES
	OR	A		;ENTRY TO DO [A]*[DE] ([A] NON-Z)
	JR	NZ,SCAL2	;NON-ZERO
	OR	H		;TEST HI BYTE
	RET	NZ		;IF NZ, THEN WAS *1 CASE
	EX	DE,HL		;WAS *0 CASE - PUT 0 IN [DE]
	RET

SCAL2	LD	C,D
	LD	D,00H
	PUSH	AF
	CALL	GMULT		;Fast multiply routine for graphics (HL):=(A)*(DE)
	LD	E,80H		;Round up
	ADD	HL,DE
	LD	E,C
	LD	C,H
	POP	AF
	CALL	GMULT
	LD	E,C
	ADD	HL,DE
	EX	DE,HL
	RET

;-----------------------------------------------------------------------------
;	Fast multiply routine for graphics (HL):=(A)*(DE)
GMULT	LD	B,08H
	LD	HL,0000H
L8561	ADD	HL,HL
	ADD	A,A
	JR	NC,L8566
	ADD	HL,DE
L8566	DEC	B
	JP	NZ,L8561
	RET

;-----------------------------------------------------------------------------
;	Parse the begin and end angles
;	 Setting appropriate bits in CLINEF if neg.
CGTCNT	DEC	HL		
	CALL	CHRGTR		;GET CURRENT CHAR
	RET	Z		;IF NOTHING, RETURN DFLT IN [DE]
	CALL	SYNCHR		
	DB	','		;EAT THE COMMA
	CP	','		;USE DEFAULT IF NO ARGUMENT.
	RET	Z
	PUSH	BC		;SAVE FLAG BYTE IN [C]
	CALL	FRMEVL		;EVALUATE THE THING
	EX	(SP),HL		;POP FLAG BYTE, PUSH TXTPTR
	PUSH	HL		;RESAVE FLAG BYTE
	CALL	CSNG		;MAKE IT A SINGLE PRECISION VALUE
	POP	BC		;GET BACK FLAG BYTE
	LD	HL,FAC		;NOW SEE WHETHER POSITIVE OR NOT
	LD	A,(HL)		;GET EXPONENT BYTE
	OR	A
	JR	Z,CGTC2
	DEC	HL		;SET TO HIGH MANTISSA BYTE
	LD	A,(HL)
	OR	A
	JP	P,CGTC2
	AND	7FH		;MAKE IT POSITIVE
	LD	(HL),A
	LD	HL,CLINEF	;SET BIT IN [C] IN CLINEF
	LD	A,(HL)
	OR	C
	LD	(HL),A
CGTC2	LD	BC,07E22H
	LD	DE,0F983H	;LOAD REGS WITH 1/2*PI
	CALL	FMULT		;MULTIPLY BY 1/(2*PI) TO GET FRACTION
	CALL	CMPONE		;SEE IF RESULT IS GREATER THAN ONE
	JP	Z,FCERR		;FC ERROR IF SO
	CALL	PUSHF		;SAVE FAC ON STAC
	LD	HL,(CNPNTS)	;GET NO. OF POINTS PER OCTANT
	ADD	HL,HL		;TIMES 8 FOR TRUE CIRCUMFERENCE
	ADD	HL,HL
	ADD	HL,HL
	CALL	MAKINT		;STICK IT IN FAC
	CALL	CSNG		;AND MAKE IT SINGLE PRECISION
	POP	BC		;GET BACK ANG/2*PI IN REGS
	POP	DE
	CALL	FMULT		;DO THE MULTIPLY
	CALL	CINT		;CONVERT TO INTEGER IN [HL]
	POP	DE		;GET BACK TXTPTR
	EX	DE,HL
	RET

;-----------------------------------------------------------------------------
;	Test if FAC is less than one
CMPONE	LD	BC,8100H
	LD	DE,0000H
	CALL	FCOMP
	DEC	A
	RET

;-----------------------------------------------------------------------------
;	GET AND PUT - READ AND WRITE GRAPHICS BIT ARRAY
; ## ADVGRP.ASM:844 ##
;
;       GET & PUT - Read & Write graphics bit array
;
; Syntax:
;       GET @(x,y),ArrayVar
;       PUT @(x,y),ArrayVar[,Function]
;
;       Function = OR/AND/PRESET/PSET/XOR
;
; TRS-80:
;	GET (x1,y1)-(x2,y2),ArrayVar
;	PUT (x,y),ArrayVar[,Function]
;
GPUTG	LD	(PUTFLG),A	;Whether doing PUT() or GET()
	CALL	TOGRPH		;Set graphics mode if enabled with SCREEN 0
	PUSH	AF		;SAVE THIS FLAG A SEC
	CALL	SCAN1		;GET FIRST COORD
	CALL	CHKRNG		;Check X-Y coordinates, FC Error if bad
	POP	AF		;REGET PUT FLAG
	OR	A
	JP	NZ,PUT1
	CALL	SYNCHR
	DB	MINUTK		;EAT "-"
	PUSH	BC		;SAVE X1
	PUSH	DE		;SAVE Y1
	CALL	SCAND		;GET SECOND COORD FOR 'GET' ONLY
	CALL	CHKRNG		
	POP	DE		;GET Y1 BACK
	POP	BC		;AND X1
	PUSH	HL		;SAVE TXTPTR
	CALL	YDELT		;CALC DELTA Y
	CALL	C,XCHGY		;MAKE DE=MIN(GXPOS,DE)
	INC	HL		;MAKE DELTA A COUNT
	LD	(MINDEL),HL	;SAVE DELTA Y IN MIDEL
	CALL	XDELT
	CALL	C,XCHGX		;BC = MIN(GXPOS,DE)
	INC	HL		;MAKE DELTA A COUNT
	LD	(MAXDEL),HL	;SAVE DX IN MAXDEL
	CALL	MAPXYC		
	POP	HL		;GET BACK TXTPTR
	CALL	GTARRY		;SCAN ARRAY NAME
	PUSH	HL		;SAVE TXTPTR UNTIL DONE
	PUSH	DE		;SAVE BEG. OF ARRAY DATA PTR
	PUSH	BC		;SAVE END OF ARRAY DATA PTR
	PUSH	DE		;SAVE BEG. PTR AGAIN FOR COMPAR
	LD	DE,(MAXDEL)	
	EX	DE,HL
	LD	B,H		;CALC DX * BITS/PIX
	LD	C,L
	LD	DE,0007H
	ADD	HL,DE
	LD	A,L
	AND	0F8H
	LD	L,A
	EX	DE,HL
	CALL	PIXSIZ		;Get # bits per pixel into [A] (=1)
	LD	HL,0000H
MULBLP	ADD	HL,DE		;CALC DX * BITS/PIX
	DEC	A
	JR	NZ,MULBLP	;KEEP LOOPING
	LD	DE,0007H	;Round to byte boundry.
	ADD	HL,DE
	EX	DE,HL		;RESULT TO [DE]
	CALL	HLFDE		;NOW CALC NO. OF BYTES PER SCAN LINE
	CALL	HLFDE
	CALL	HLFDE
	LD	HL,(MINDEL)	;GET DELTA Y
	PUSH	BC		;SAVE DX*BITS/PIX
	LD	B,H		;INTO [BC] FOR UMULT
	LD	C,L		
	CALL	UMULT		;[DE]=DX*DY*BITS/PIX
	POP	BC		;GET BACK DX*BITS/PIX
	LD	HL,0004H	;ADD 4 BYTES FOR DX,DY STORAGE
	ADD	HL,DE		;[HL] HAS NO. OF BYTES TO BE USED
	POP	DE		;BEG OF ARRAY DATA TO [HL]
	ADD	HL,DE		;ADD NO. OF BYTES TO BE USED
	EX	DE,HL		;[DE] = CALCULATED END OF DATA
	POP	HL		;END OF ARRAY DATA TO [HL]
	CALL	COMPAR
	JP	C,FCERR		;ERROR IF TOO BIG
				;BEG OF DATA PTR IS ON STK HERE
	POP	HL		;GET POINTER TO ARRAY DATA
	CALL	COMPAR
	JP	NC,FCERR	;ARRAY START+LENGTH .GT. 64K
	LD	(HL),C		;SAVE DX*BITS/PIX IN 1ST 2 BYTES OF ARY
	INC	HL
	LD	(HL),B		;PASS NO. OF BITS DESIRED IN [BC]
	INC	HL
	LD	DE,(MINDEL)	;GET LINE (Y) COUNT
	LD	(HL),E
	INC	HL
	LD	(HL),D		
	INC	HL		;SAVE DY IN 2ND 2 BYTES
	OR	A		;CLEAR CARRY FOR GET INIT.
	JP	GOPGIN		;GIVE LOW LEVEL ADDR OF ARRAY & GO

PUT1	PUSH	HL		;SAVE TXTPTR
	CALL	MAPXYC		;MAP THE POINT
	POP	HL
	CALL	GTARRY		;SCAN ARRAY NAME & GET PTR TO IT
	PUSH	DE		;SAVE PTR TO DELTAS IN ARRAY
	DEC	HL		;NOW SCAN POSSIBLE PUT OPTION
	CALL	CHRGTR
	LD	B,05H		;DEFAULT OPTION IS XOR
	JR	Z,PUT2		;IF NO CHAR, USE DEFAULT
	CALL	SYNCHR
	DB	','		;MUST BE A COMMA
	EX	DE,HL		;PUT TXTPTR IN [DE]
	LD	HL,GFUNTB+4	;TABLE OF POSSIBLE OPTIONS
PFUNLP	CP	(HL)		;IS THIS AN OPTION?
	JR	Z,PUT20		;YES, HAND IT TO PGINIT.
	DEC	HL		;POINT TO NEXT
	DEC	B
	JP	NZ,PFUNLP
	EX	DE,HL		;GET TXTPTR BACK TO [HL]
	POP	DE		;CLEAN UP STACK
	RET			;LET NEWSTT GIVE SYNTAX ERROR
				
PUT20	EX	DE,HL		;GET TXTPTR BACK TO [HL]
	CALL	CHRGTR		;EAT THE TOKEN
PUT2	DEC	B		;1..5 -TO 0..4
	LD	A,B		;INTO [A] FOR PGINIT
	EX	(SP),HL		;POP ARRAY PTR, PUSH TXTPTR
	PUSH	AF		;SAVE PUT ACTION MODE
	LD	E,(HL)		;[DE]=NO. OF BITS IN X
	INC	HL
	LD	D,(HL)
	INC	HL
	PUSH	DE		;SAVE BIT COUNT
	PUSH	HL		;SAVE ARRAY POINTER
	EX	DE,HL		;INTO [HL] FOR IDIV
	DEC	HL		;DECREMENT DX SINCE IT'S A COUNTER
	LD	DE,(GXPOS)	;NOW CALC TRUE X
	ADD	HL,DE
	JR	C,PRNGER	;ERROR IF CARRY
	LD	B,H		;TO [BC] FOR SCALXY
	LD	C,L
	POP	HL		;GET BACK ARRAY POINTER
	LD	E,(HL)		;[DE] = DELTA Y ([HL] POINTS TO DATA)
	INC	HL		
	LD	D,(HL)
	INC	HL
	PUSH	DE		;SAVE DELTA Y ON STACK
	PUSH	HL		;SAVE PTR ON STACK AGAIN
	LD	HL,(GYPOS)	
	DEC	DE		;DECREMENT DY SINCE IT'S A COUNTER
	ADD	HL,DE
PRNGER	JP	C,FCERR		;ERROR IF CARRY
	EX	DE,HL		;[DE]=Y + DELTA Y
	POP	HL		;GET BACK ARRAY POINTER
	CALL	CHKRNG		;MAKE SURE [BC],[HL] ARE ON THE SCREEN
	POP	DE		;POP DY
	POP	BC		;POP DX*BITS/PIX
	POP	AF		;GET BACK ACTION MODE
	SCF			;SET CARRY TO FLAG PUT INIT
GOPGIN	PUSH	DE		;RESAVE DY
	CALL	PGINIT		;[BC]=BIT COUNT,[HL]=ARRAY ADDR
	POP	DE		;GET Y COUNT
PGLOOP	PUSH	DE		;SAVE LINE COUNT
	CALL	FETCHC		
	PUSH	HL		;SAVE LINE COUNT
	PUSH	AF
	LD	A,(PUTFLG)	;SEE IF PUTTING OR GETTING
	OR	A
	JR	NZ,PGLOOP2
	CALL	NREAD		
	JR	PGLOOP3

PGLOOP2	CALL	NWRITE		
PGLOOP3	POP	AF
	POP	HL
	CALL	STOREC		
	CALL	DOWNC		;NOW MOVE DOWN A LINE
	POP	DE
	DEC	DE
	LD	A,D
	OR	E
	JR	NZ,PGLOOP	;CONTINUE IF NOT ZERO
	POP	HL		;GET BACK TXTPTR
	RET			;AND RETURN

;-----------------------------------------------------------------------------
GTARRY	CALL	SYNCHR
	DB	','		;EAT COMMA
	LD	A,01H		;SEARCH ARRAYS ONLY
	LD	(SUBFLG),A
	CALL	PTRGET		;GET PTR TO ARRAY
	JP	NZ,FCERR	;NOT THERE - ERROR
	LD	(SUBFLG),A	;CLEAR THIS
	PUSH	HL		;SAVE TXTPTR
	LD	H,B		;HL = PTR TO ARRAY
	LD	L,C		;HL = LENGTH
	EX	DE,HL		;HL = LAST BYTE OF ARRAY
	ADD	HL,DE		;SAVE
	PUSH	HL
	LD	A,(BC)		;GET NO. OF DIMS
	ADD	A,A		;DOUBLE SINCE 2 BYTE ENTRIES
	LD	L,A
	LD	H,00H
	INC	BC		;SKIP NO. OF DIMS
	ADD	HL,BC
	EX	DE,HL		;DE = PTR TO FIRST BYTE OF DATA
	POP	BC		;BC = PTR TO LAST BYTE OF DATA
	POP	HL		;GET TXTPTR
	RET

;-----------------------------------------------------------------------------
;	List of valid PUT options
;	XOR, PSET, PRESET, AND, OR (reversed)
GFUNTB	DB	ORTK		;LIST OF VALID PUT OPTIONS
	DB	ANDTK
	DB	PRESETTK
	DB	PSETTK
	DB	XORTK

;-----------------------------------------------------------------------------
;	TRS-80: Check X-Y coordinates, FC Error if bad
CHKRNG	PUSH	HL
	CALL	SCALXY		;Scale into bounds
	JP	NC,FCERR	;Illegal function call
	POP	HL
	RET


;=============================================================================
;	Page Independent Uninitialized RAM Location Definitions
; ## GWRAM.SRC ##
;
;	TRS-80: Inhibit GCLEAR on INIT
NOCLR	DB	00H
;	FP argument for graphics
GARG	DB	00H,00H,00H,00H
;	  Left edge of box (LINE ...,BF)
BOXLFT	DB	00H,00H
;	 Right edge of box (LINE ...,BF)
BOXRGT	DB	00H,00H
;	Bottom edge of box (LINE ...,BF)
BOXBOT	DB	00H,00H
;	   Top edge of box (LINE ...,BF)
BOXTOP	DB	00H,00H
;	TRS-80: Port 83H image (Graphics Mode)
OUT83H	DB	00H
;	X Position of Second Coordinate
GXPOS	DB	00H,00H
;	Y Position of Second Coordinate
GYPOS	DB	00H,00H
;	Foreground color (=1)
FORCLR	DB	01H
;	Background color (=0)
BAKCLR	DB	00H
;	Largest Delta for Line
MAXDEL	DB	00H,00H
;	Smaller of 2 Deltas for Line
MINDEL	DB	00H,00H
;	Address of Minor Axis Move Update
MINUPD	DB	0C3H
;	=MINUPD+1
MINUPD1	DW	0000H
;	Address of Major Axis Move Update
MAXUPD	DB	0C3H
;	=MAXUPD+1
MAXUPD1	DW	0000H
;	Line style
GSTYLE	DB	00H,00H
;	X part of graphics accumulator
GRPACX	DB	00H,00H
;	Y part of graphics accumulator
GRPACY	DB	00H,00H
;	Viewport upper left X-coordinate
VIEWX1	DB	00H,00H
;	Viewport lower right X-coordinate
VIEWX2	DB	00H,00H
;	Viewport upper left Y-coordinate
VIEWY1	DB	00H,00H
;	Viewport lower right Y-coordinate
VIEWY2	DB	00H,00H
;	Pixel addr (word or byte=col)
PIXADR	DB	00H
;	Pixel row addr (byte=row)
PIXROW	DB	00H
;	Pixel bit mask (1 bit set)
PIXBIT	DB	00H
;	Attributes byte
ATRBYT	DB	00H
;	Array address for GET/PUT
ARYADR	DB	00H,00H
;	PUT/GET counter 1
PGCTR1	DB	00H
;	PUT/GET counter 2
PGCTR2	DB	00H
;	PUT/GET pixel offset
PGPXOF	DB	00H
;	PUT/GET left mask
PGLMSK	DB	00H
;	PUT/GET right mask
PGRMSK	DB	00H
;	Address of PUT operator routine
PUTTER	DB	00H,00H
;	Default border attrib - Modified anywhere?
TMPATR	DB	00H
;	01 if border, 00 if not
BRDATR	DB	00H
;	FF if border, 00 if not
BRDMSK	DB	00H
;	Scan flags TBD
SCNFLG	DB	00H
;	ADVGRP C save area
CSAVEA	DB	00H,00H
;	ADVGRP C save area
CSAVEM	DB	00H
;	Default background string buffer
BKGBUF	DB	00H,00H
;	addr+len-len/siz
TILEND	DB	00H,00H
;	Tiling string ptr
TILNG$	DB	00H,00H
;	Tiling as string flag
TLSTFL	DB	00H
;	Tiles equal flag
TLEQFL	DB	00H
;	Tiles count = len/siz (with siz==1)
TILCNT	DB	00H
;	Current tile row
TILROW	DB	00H
;	Background string
BKGSTR	DB	00H,00H,00H,00H
;	Present queue length
PSNLEN	DB	00H,00H
;	Maximum queue length
QUELEN	DB	00H,00H
;	Queue output pointer
QUEOUT	DB	00H,00H
;	Queue input pointer
QUEINP	DB	00H,00H
;	PAINT direction 40=down, C0=up, 00=stop
PDIREC	DB	00H
;	Move count
MOVCNT	DB	00H,00H
;	Skip count
SKPCNT	DB	00H,00H
;	PAINT: scan line already painted flags
LFPROG	DB	00H
;	
RTPROG	DB	00H
;	Whether doing PUT() or GET()
PUTFLG	DB	00H
;	DATA AREA FOR CIRCLE STATEMENT
;	Aspect ratio
ASPECT	DB	00H,00H
;	Start count
CSTCNT	DB	00H,00H
;	End CIRCLE point count
CENCNT	DB	00H,00H
;	CIRCLE sum
CRCSUM	DB	00H,00H
;	Plot polarity flag
CPLOTF	DB	00H
;	Line-To-Center flag
CLINEF	DB	00H
;	CNPNTS=RADIUS*SQR(2)/2=# Pts to plot
CNPNTS	DB	00H,00H
;	No. of pts in CIRCLE
CPCNT8	DB	00H,00H
;	X offset from center save loc
CXOFF	DB	00H,00H
;	Y offset save location
CYOFF	DB	00H,00H
;	Flag whether ASPECT was .GT. 1
CSCLXY	DB	00H
;	1/8 no. of pts in CIRCLE
CPCNT	DB	00H,00H
;	Viewport left bound addr
VWLADR	DB	00H
;	Viewport right bound addr
VWRADR	DB	00H
;	Viewport left bound bit mask
VWLMSK	DB	00H
;	Viewport right bound bit mask
VWRMSK	DB	00H
;	8 byte masks with right-justified white pixels
WHITER	DB	11111111B
	DB	01111111B
	DB	00111111B
	DB	00011111B
	DB	00001111B
	DB	00000111B
	DB	00000011B
	DB	00000001B
;	8 byte masks with left-justified white pixels
WHITEL	DB	10000000B
	DB	11000000B
	DB	11100000B
	DB	11110000B
	DB	11111000B
	DB	11111100B
	DB	11111110B


;=============================================================================
;	OEM Graphics routines (TRS-80)
;
;	Gets screen relations
;	Get default aspect ratio into [HL]
GTASPC	LD	HL,0080H
	LD	DE,0200H
	RET

;-----------------------------------------------------------------------------
;	DGET or GGET
XGET	CP	'@'		;Eat optional '@'
	CALL	Z,CHRGTR
	CP	'('		;Open parenthesis?
	JP	NZ,GETST	;If not, perform standard GET statement
	XOR	A		;Otherwise perform graphics GET statement
	JP	GPUTG		;GGET (A=0) or GPUT (A=80H)

;-----------------------------------------------------------------------------
;	DPUT or GPUT
XPUT	CP	'@'		;Eat optional '@'
	CALL	Z,CHRGTR
	CP	'('		;Open parenthesis?
	JP	NZ,PUTST	;If not, perform standard PUT statement
	LD	A,80H		;Otherwise perform graphics PUT statement
	JP	GPUTG		;GGET (A=0) or GPUT (A=80H)

;-----------------------------------------------------------------------------
;	GET/PUT initialization
;	[BC]=bit count,[HL]=array addr,[A]=function
PGINIT	LD	(ARYADR),HL	;Array address for GET/PUT
	PUSH	AF
	XOR	A
	LD	(PGCTR1),A	;PUT/GET counter 1
	LD	(PGCTR2),A	;PUT/GET counter 2
	LD	A,(PIXBIT)	;Pixel bit mask (1 bit set)
;	Compute bit offset to H
	LD	H,00H
PGINI1	RLCA
	JP	C,PGINI2
	INC	H
	JR	PGINI1

PGINI2	LD	A,H		;Bit offset
	LD	(PGPXOF),A	;PUT/GET pixel offset
;	Compute left mask from bit offset
	LD	HL,WHITER	;8 byte masks with right-justified white pixels
	ADD	A,L
	LD	L,A
	LD	A,00H
	ADC	A,H
	LD	H,A
	LD	A,(HL)
	LD	(PGLMSK),A	;PUT/GET left mask
;	Compute remaining width
	LD	A,(PGPXOF)	;PUT/GET pixel offset
	LD	H,A		;Calc H = 8 - pixel offset
	LD	A,08H
	SUB	H
	LD	H,A
	LD	A,C		;Calc width BC = BC - H
	SUB	H
	LD	C,A
	LD	A,B
	SBC	A,00H
	LD	B,A
	JP	NC,PGINI3
;	Update left mask if remaining width is negative
	LD	A,C
	CPL
	INC	A
	LD	H,A
	LD	A,08H
	SUB	H
	LD	HL,WHITER	;8 byte masks with right-justified white pixels
	ADD	A,L
	LD	L,A
	LD	A,00H
	ADC	A,H
	LD	H,A
	LD	A,(HL)
	CPL
	LD	C,A
	LD	A,(PGLMSK)	;PUT/GET left mask
	AND	C
	LD	(PGLMSK),A	;PUT/GET left mask
	JR	PGINI4

;	Compute right mask from bit offset
PGINI3	LD	A,07H
	AND	C
	LD	(PGCTR2),A	;PUT/GET counter 2 (right bits)
	DEC	A
	LD	HL,WHITEL	;8 byte masks with left-justified white pixels
	ADD	A,L
	LD	L,A
	LD	A,00H
	ADC	A,H
	LD	H,A
	LD	A,(HL)
	LD	(PGRMSK),A	;PUT/GET right mask
;	Compute PGCTR1 = remaining width / 8
	LD	A,B		
	RRCA
	RRCA
	RRCA
	LD	B,A
	LD	A,C
	AND	0F8H
	RRCA
	RRCA
	RRCA
	OR	B
	LD	(PGCTR1),A	;PUT/GET counter 1
;	Initialize putter routine address from operator
;	OR, AND, RESET, SET or XOR
PGINI4	POP	AF
	RET	NC
	ADD	A,A
	LD	HL,PUTDSP	;PUT mode dispatch table
	ADD	A,L
	LD	L,A
	LD	A,00H
	ADC	A,H
	LD	H,A
	LD	A,(HL)
	INC	HL
	LD	H,(HL)
	LD	L,A
	LD	(PUTTER),HL	;Address of PUT operator routine
	RET

;	PUT mode dispatch table
PUTDSP	DW	PUTOR,PUTAND,PUTRST,PUTSET,PUTXOR

;-----------------------------------------------------------------------------
;	Read a row of pixels from the graphics screen (GET)
NREAD	LD	HL,(ARYADR)	;Array address for GET/PUT
	EX	DE,HL		;Array pointer to DE
	LD	HL,(PIXADR)	;Pixel address
	LD	A,(PGPXOF)	;PUT/GET pixel offset
	LD	B,A		;to B
	PUSH	HL
	CALL	NRDGET		;Read and shift a byte from graphics
	LD	C,L
	LD	A,(PGCTR1)	;PUT/GET counter 1
	OR	A
	JP	Z,NREAD2
NREAD1	LD	C,L
	POP	HL
	INC	L
	PUSH	HL
	PUSH	AF
	CALL	NRDGET		;Read and shift a byte from graphics
	LD	A,C
	OR	H
	LD	C,L
	LD	(DE),A
	INC	DE
	POP	AF
	DEC	A
	JR	NZ,NREAD1
NREAD2	LD	A,(PGCTR2)	;PUT/GET counter 2
	OR	A
	JR	Z,NREAD3
	POP	HL
	INC	L
	PUSH	HL
	CALL	NRDGET		;Read and shift a byte from graphics
NREAD3	LD	A,C
	OR	H
	LD	(DE),A
	INC	DE
	LD	A,(PGCTR2)	;PUT/GET counter 2
	LD	B,A
	LD	A,(PGPXOF)	;PUT/GET pixel offset
	SUB	B		;pix offset - counter 2
	JP	NC,NREAD4	;skip if .GE. 0
	LD	A,L
	LD	(DE),A
	INC	DE
NREAD4	POP	HL
	LD	(ARYADR),DE	;Array address for GET/PUT
	RET

;-----------------------------------------------------------------------------
;	sub for NREAD (GET)
;	Read and shift a byte from graphics
;	Entry:	HL	Pixel address (ROW,COL)
;		B	Pixel offset
;	Exit:	HL	Byte from graphics left-shifted by (B) positions
NRDGET	PUSH	BC		;Save reg
	LD	A,L		;COL address
	OUT	(80H),A		;to graphics port
	LD	A,H		;ROW address
	OUT	(81H),A		;to graphics port
	IN	A,(82H)		;Read byte from graphics
	LD	L,A		;to HL
	LD	H,00H		;
	LD	A,B		;Prepare to left-shift
	OR	A		;No shift
	JR	Z,NRDSB2	;If so, skip
NRDSB1	ADD	HL,HL		;Left-shift B times
	DEC	B		;
	JP	NZ,NRDSB1	;until done
NRDSB2	POP	BC		;Restore reg
	RET

;-----------------------------------------------------------------------------
;	Write a row of pixels to the graphics screen (PUT)
NWRITE	LD	HL,(PIXADR)	;Pixel addr (word or byte=col)
	EX	DE,HL
	LD	HL,(ARYADR)	;Array address for GET/PUT
	LD	A,(PGPXOF)	;PUT/GET pixel offset
	LD	B,A
	LD	A,08H
	SUB	B		;8 - pixel offset
	PUSH	HL
	LD	B,A		;to B
	CALL	NWRGET		;Get shifted byte from array in HL
	LD	C,L		;Save L to C
	PUSH	DE
	LD	A,(PGLMSK)	;PUT/GET left mask
	CALL	NWRPUT		;PUT using function dispatcher
	POP	DE
	INC	DE
	POP	HL
	INC	HL
	LD	A,(PGCTR1)	;PUT/GET counter 1
	OR	A
	JP	Z,NWRIT2
NWRIT1	PUSH	AF
	PUSH	HL
	CALL	NWRGET		;Get shifted byte from array in HL
	LD	A,C		;Saved (L) from previous call
	OR	H		;ORed with shifted byte's high bits
	LD	H,A		;update shifted byte's high bits
	LD	C,L		;Save L to C
	LD	A,0FFH
	PUSH	DE
	CALL	NWRPUT		;PUT using function dispatcher
	POP	DE
	INC	E
	POP	HL
	INC	HL
	POP	AF
	DEC	A
	JR	NZ,NWRIT1
NWRIT2	LD	A,(PGCTR2)	;PUT/GET counter 2
	OR	A
	JP	Z,NWRIT4
	PUSH	BC
	LD	B,A
	LD	A,(PGPXOF)	;PUT/GET pixel offset
	SUB	B
	POP	BC
	JP	NC,NWRIT3
	PUSH	HL
	CALL	NWRGET		;Get shifted byte from array in HL
	LD	A,C		;Saved K from last NWRPUT call
	OR	H		;ORed with shifted byte's high bits
	LD	C,A		;save to C
	POP	HL
	INC	HL
NWRIT3	PUSH	HL
	LD	H,C		;H = New bit values
	LD	A,(PGRMSK)	;PUT/GET right mask
	PUSH	DE
	CALL	NWRPUT		;PUT using function dispatcher
	POP	DE
	POP	HL
NWRIT4	LD	(ARYADR),HL	;Array address for GET/PUT
	RET

;-----------------------------------------------------------------------------
;	Sub for NWRITE (PUT)
;	Get shifted byte from array in HL
;	Entry:	HL	Memory address
;		B	Pixel offset
;	Exit:	HL	Byte from memory left-shifted by (B) positions
;		BC & DE left unaltered
NWRGET	PUSH	BC
	LD	L,(HL)
	LD	H,00H
	LD	A,B
	OR	A
	JR	Z,NWRSB2
NWRSB1	ADD	HL,HL
	DEC	B
	JP	NZ,NWRSB1
NWRSB2	POP	BC
	RET

;-----------------------------------------------------------------------------
;	PUT using function dispatcher
;	Entry:	DE	Pixel address
;		H	Bit mask, new bit values
;		A	Bit mask, zeros are bits to keep unmodified
NWRPUT	PUSH	BC
	PUSH	DE
	LD	B,A		;Protect bit mask
	LD	A,E
	OUT	(80H),A
	LD	A,D
	OUT	(81H),A
	LD	E,H
	LD	HL,(PUTTER)	;Address of PUT operator routine
	JP	(HL)

;	PUT with PRESET
;	Entry:	E	New bit values
;		B	Bit mask, ones are bits to alter,
;			zeros are bits to keep
;		
PUTRST	LD	A,E
	CPL
	LD	E,A

;	PUT with PSET
;	Eg:	B	00001111 - bit mask
;		A	00110011 - old byte
;		E	01010101 - new bits, set ????0101
PUTSET	IN	A,(82H)
	LD	D,A		;00110011
	XOR	E		;01100110
	AND	B		;00000110
	XOR	D		;00110101
	JR	PUTFIN		;Finalize PUT

;	PUT with AND
PUTAND	LD	A,B		;00001111
	CPL			;11110000
	LD	B,A
	OR	E		;11110101
	LD	E,A
	IN	A,(82H)		;00110011
	AND	E		;00110001
	JR	PUTFIN		;Finalize PUT

;	PUT with OR
PUTOR	LD	A,E		;01010101
	AND	B		;00000101
	LD	B,A
	IN	A,(82H)		;00110011
	OR	B		;00110111
	JR	PUTFIN		;Finalize PUT

;	PUT with XOR
PUTXOR	LD	A,E		;01010101
	AND	B		;00000101
	LD	B,A
	IN	A,(82H)		;00110011
	XOR	B		;00110110

;	Finalize PUT
PUTFIN	OUT	(82H),A
	POP	DE
	POP	BC
	RET

;-----------------------------------------------------------------------------
;	Get # bits per pixel into [A]
PIXSIZ	LD	A,01H
	RET

;-----------------------------------------------------------------------------
;	Draw 1 pix to left, cy if bound hit
DRAWL	LD	A,E
	RLCA
	LD	E,A
	JP	NC,L8973
	CALL	PNTPIX
	DEC	L
	LD	A,L
	OUT	(80H),A
	CALL	SUB89E4		;XORPIX (ATRBYT) and (BORDFF)
L8973	CALL	CHKVWL		;Check left view bound, Cy if hit
	RET

;-----------------------------------------------------------------------------
;	Draw 1 pix to right, cy if bound hit
DRAWR	LD	A,E
	RRCA
	LD	E,A
	JR	NC,L8986
	CALL	PNTPIX
	INC	L
	LD	A,L
	OUT	(80H),A
	CALL	SUB89E4		;XORPIX (ATRBYT) and (BORDFF)
L8986	CALL	CHKVWR		;Check right view bound, Cy if hit
	RET

;-----------------------------------------------------------------------------
;	Low-level scan left routine
;	Scans screenpixels to the left
SCANL	CALL	SCNINI		;Init SCANL, SCANR
	CALL	DRAWL		;Draw 1 pix to left, cy if bound hit
	PUSH	HL
	LD	HL,0000H
	EX	(SP),HL
	JP	C,SCANLJ1
SCANLL1	LD	A,E
	AND	C
	JP	NZ,SCANLJ1
	EX	(SP),HL
	INC	HL
	EX	(SP),HL
	LD	A,E
	AND	B
	OR	D
	LD	D,A
	CALL	DRAWL		;Draw 1 pix to left, cy if bound hit
	JP	C,SCANLJ1
	JR	SCANLL1

SCANLJ1	CALL	DRAWR		;Draw 1 pix to right, cy if bound hit
	LD	A,E
	LD	(PIXADR),HL	;Pixel addr (word or byte=col)
	LD	(PIXBIT),A	;Pixel bit mask (1 bit set)
	CALL	PNTPIX
	POP	HL
	LD	A,(SCNFLG)	;Scan flags TBD
	LD	C,A
	LD	A,H
	OR	L
	RET	Z
	LD	A,(TLEQFL)	;Equal tiles flag
	OR	C
	LD	C,A
	RET

;-----------------------------------------------------------------------------
;	Paint pixel (L874D).OR.(D) at addr (HL)
;	Used by DRAWL, DRAWR, SCANL, SCANR
PNTPIX	PUSH	DE
	LD	B,D
	LD	A,(SCNFLG)	;Scan flags TBD
	OR	B
	LD	(SCNFLG),A	;Scan flags TBD
	LD	A,L
	OUT	(80H),A
	LD	A,H
	OUT	(81H),A
	LD	A,(ATRBYT)	;Attributes byte
	LD	E,A
	IN	A,(82H)
	LD	D,A
	XOR	E
	AND	B
	XOR	D
	OUT	(82H),A
	POP	DE
	RET

;-----------------------------------------------------------------------------
;	Do XORPIX with (ATRBYT) to B
;		  with (BRDMSK) CPLed to C
;	In:	HL = data addr
;	Out:	B = (ATRBYT) XOR data(H,L)
;		C = (BRDMSK) XOR NOT data(H,L)
;	Used by DRAWL, DRAWR, SCNINI
SUB89E4	PUSH	DE
	LD	A,(ATRBYT)	;Attributes byte
	CALL	XORPIX		;A := A .XOR. data(H,L)
	PUSH	AF
	LD	A,(BRDMSK)	;FF if border, 00 if not
	CALL	XORPIX		;A := A .XOR. data(H,L)
	CPL
	LD	C,A
	POP	AF
	LD	B,A
	POP	DE
	LD	D,00H
	RET

;-----------------------------------------------------------------------------
;	XOR with graphics data byte at (H,L)
;	In:	HL = data addr
;		A = bitmask to XOR
;	Out:	A = bitmask XOR data(H,L)
XORPIX	LD	D,A
	LD	A,L
	OUT	(80H),A
	LD	A,H
	OUT	(81H),A
	IN	A,(82H)
	XOR	D
	RET

;-----------------------------------------------------------------------------
;	Low-level scan right routine
;	Scans screenpixels to the right
SCANR	PUSH	DE
	CALL	SCNINI		;Init SCANL, SCANR
SCANRL1	LD	A,C
	AND	E
	JR	Z,SCANRJ2
	EX	(SP),HL
	DEC	HL
	LD	A,H
	OR	L
	EX	(SP),HL
	JR	Z,SCANRJ1
	CALL	DRAWR		;Draw 1 pix to right, cy if bound hit
	JP	NC,SCANRL1
SCANRJ1	LD	(PIXADR),HL	;Pixel addr (word or byte=col)
	LD	A,E
	LD	(PIXBIT),A	;Pixel bit mask (1 bit set)
	POP	HL
	LD	HL,0000H
	LD	DE,0000H
	LD	C,00H
	RET

SCANRJ2	PUSH	HL
	LD	(CSAVEA),HL	;ADVGRP C save area
	LD	A,E
	LD	(CSAVEM),A	;ADVGRP C save area
	LD	HL,0000H
	EX	(SP),HL
	JR	SCANRJ3

SCANRL2	LD	A,C
	AND	E
	JR	NZ,SCANRJ4
SCANRJ3	LD	A,B
	AND	E
	OR	D
	LD	D,A
	EX	(SP),HL
	INC	HL
	EX	(SP),HL
	CALL	DRAWR		;Draw 1 pix to right, cy if bound hit
	JP	NC,SCANRL2
	CALL	DRAWL		;Draw 1 pix to left, cy if bound hit
SCANRJ4	CALL	PNTPIX
	LD	(PIXADR),HL	;Pixel addr (word or byte=col)
	LD	A,E
	LD	(PIXBIT),A	;Pixel bit mask (1 bit set)
	POP	HL
	POP	DE
	LD	A,(SCNFLG)	;Scan flags TBD
	LD	C,A
	LD	A,(TLEQFL)	;Equal tiles flag
	OR	C
	LD	C,A
	RET

;-----------------------------------------------------------------------------
;	Some initialization routine
;	
;	Clear SCNFLG;
;	get 	PIXADR to HL,
;		PIXBIT to E,
;		data(H,L) XOR (ATRBYT) to B,
;		data(H,L) XOR NOT (BRDMSK) to C.
;	
;	Used by SCANL, SCANR
;
SCNINI	LD	HL,(PIXADR)	;Pixel addr (word or byte=col)
	XOR	A
	LD	(SCNFLG),A	;Scan flags TBD
	LD	A,(PIXBIT)	;Pixel bit mask (1 bit set)
	LD	E,A
	CALL	SUB89E4		;XORPIX (ATRBYT) and (BORDFF)
	RET

;-----------------------------------------------------------------------------
;	Init PAINT stuff & check border attrib
PNTINI	CALL	CHKATR		;Assert attr A<2, Cy if not
	RET	C
	LD	(BRDATR),A	;01 if border, 00 if not
	CALL	NEGA		;A := -A (0 or FF)
	LD	(BRDMSK),A	;FF if border, 00 if not
	OR	A
	RET

;-----------------------------------------------------------------------------
;	Check left view bound, Cy if hit
;	Used by DRAWL
CHKVWL	LD	A,L
	OR	A
	SCF
	RET	M
	LD	A,(VWLADR)	;Viewport left bound addr
	SUB	L
	CCF
	RET	NZ
	LD	A,(VWLMSK)	;Viewport left bound bit mask
	CP	E
	RET

;-----------------------------------------------------------------------------
;	Check right view bound, Cy if hit
;	Used by DRAWR
CHKVWR	LD	A,(VWRADR)	;Viewport right bound addr
	SUB	L
	RET	NZ
	LD	A,(VWRMSK)	;Viewport right bound bit mask
	CP	E
	RET	Z
	CCF
	RET

;-----------------------------------------------------------------------------
;	Does nothing 
;	Implements unknown OEM API
;	Used by PAINT before calling SCANR1
DNOTHG	RET

;-----------------------------------------------------------------------------
;	Get minimum string (or tile?) size in B
GETSIZ	LD	B,01H
	RET

;-----------------------------------------------------------------------------
;	Copy 3 bytes from (HL) to ATRBYT
;	Used by CPYTIL
P3ATRB	LD	DE,ATRBYT	;Attributes byte
	LD	B,03H
L8AA5	LD	A,(HL)
	LD	(DE),A
	INC	DE
	INC	HL
	DEC	B
	JP	NZ,L8AA5
	RET

;-----------------------------------------------------------------------------
;	Copy B bytes from ATRBYT to (HL) after saving TMPATR. 
;	Store TMPATR to ATRBYT when done.
;
;	Called by BKGSCN when using default background string.
;	Does a copy of ATRBYT to BKGATR ...
;
SUB8AAE	LD	A,(TMPATR)	;Default border attrib - Modified anywhere?
	PUSH	AF
	LD	A,(BAKCLR)	;Background color (=0)
	CALL	STOATR		;Check and store -attr = 00 or FF
	LD	DE,ATRBYT	;Attributes byte
L8ABB	LD	A,(DE)
	LD	(HL),A
	INC	DE
	INC	HL
	DEC	B
	JP	NZ,L8ABB
	POP	AF
	LD	(TMPATR),A	;Default border attrib - Modified anywhere?
	CALL	STOATR		;Check and store -attr = 00 or FF
	RET

;-----------------------------------------------------------------------------
;	Tests whether UPC is possible, if possible, execute UPC
TUPC	LD	A,(PIXROW)	;Pixel row addr (byte=row)
	LD	E,A
	LD	A,(VIEWY1)	;Viewport upper left Y-coordinate
	SUB	E
	CCF
	RET	C
	CALL	UPC		;Dec row and output address
	LD	A,(TLSTFL)	;Tiling as string flag
	OR	A
	PUSH	AF
	CALL	NZ,DECTLR	;Decrement tile row?
	POP	AF
	CALL	NZ,CPYTIL	;Copy tiles?
	OR	A
	RET

;-----------------------------------------------------------------------------
;	Tests whether DOWNC is possible, if possible, execute DOWNC
TDOWNC	LD	A,(VIEWY2)	;Viewport lower right Y-coordinate
	LD	E,A
	LD	A,(PIXROW)	;Pixel row addr (byte=row)
	SUB	E
	CCF
	RET	C
	CALL	DOWNC		;Move to next line down in Y
	LD	A,(TLSTFL)	;Tiling as string flag
	OR	A
	PUSH	AF
	CALL	NZ,INCTLR	;Increment tile row?
	POP	AF
	CALL	NZ,CPYTIL	;Copy tiles?
	OR	A
	RET

;-----------------------------------------------------------------------------
;	Scales X and Y coordinates
;	Scale into viewport bounds
;	Entry:	BC = X-coords
;		DE = Y-coords
;	Exit:	BC = X-coords scaled to bounds
;		DE = Y-coords scaled to bounds
;		Carry set if coords were not altered
;		HL preserved
SCALXY	PUSH	HL		;Save HL
	PUSH	DE		;Save coords
	LD	D,B		;X-coords to DE
	LD	E,C
	LD	B,01H		;Set OK flag
	LD	HL,VIEWX1	;Viewport upper left X-coordinate
	CALL	FRCVEW		;Scale X-coords into bounds
	EX	DE,HL		;DE to HL
	EX	(SP),HL		;Save X-coords, get Y-coords
	EX	DE,HL		;Y-coords to DE
	LD	HL,VIEWY1	;Viewport upper left Y-coordinate
	CALL	FRCVEW		;Scale Y-coords into bounds
	LD	A,B		;get OK flag
	RRCA			;to Carry
	POP	BC		;restore X-coords
	POP	HL		;restore HL
	RET

;-----------------------------------------------------------------------------
;	Force coords inside viewport
;	Entry:	DE = coords to check
;		HL = pointer to viewport (VP) lower and upper bounds
;		B  = old OK flag
;	Exit:	DE = initial coords if in limits, 
;		     VP lower or upper bound otherwise
;		B  = 0 if out of bounds, unchanged otherwise
;		C  preserved
FRCVEW	PUSH	BC		;Save BC
	LD	C,(HL)		;get VP coords
	INC	HL
	LD	B,(HL)
	INC	HL
	PUSH	BC		;save VP coords
	EX	(SP),HL		;VP coords in HL, pointer on stack
	PUSH	HL		;save VP coords
	CALL	ICOMP		;is (DE) .LT. VP coords (lower bound)?
	OR	A
	JP	M,OKLBND	;OK if greater
	JR	Z,OKLBND	;OK if equal
				;we are below lower bound
	POP	DE		;VP coords to DE
	POP	HL		;restore pointer
	POP	BC		;restore BC
	LD	B,00H		;clear B
	RET

;	Not below lower bound
OKLBND	POP	BC		;VP coords to BC (discard)
	POP	HL		;restore pointer
	LD	C,(HL)		;get next coords
	INC	HL
	LD	B,(HL)
	INC	HL		;useless
	PUSH	BC		;save VP coords
	POP	HL		;VP coords to HL, pointer discarded
	PUSH	HL		;save VP coords
	CALL	ICOMP		;is (DE) .GE. VP coords (lower bound)?
	OR	A
	JP	P,OKUBND	;OK if less
				;we are above upper bound
	POP	DE		;VP coords to DE
	POP	BC		;restore BC
	LD	B,00H		;clear B
	RET

;	Not above upper bound
OKUBND	POP	HL		;restore VP coords
	POP	BC		;restore BC
	RET

;-----------------------------------------------------------------------------
;	Places cursor at current cursor address
;	Map into a "C"
;	aka MAPXY (msx)
MAPXYC	PUSH	BC
	PUSH	DE
	LD	A,07H
	AND	C
	LD	HL,GBITS	;Pixel bit array
	ADD	A,L
	LD	L,A
	LD	A,00H
	ADC	A,H
	LD	H,A
	LD	A,(HL)
	LD	(PIXBIT),A	;Pixel bit mask (1 bit set)
	LD	A,B
	RRCA
	RRCA
	RRCA
	LD	B,A
	LD	A,C
	AND	0F8H
	RRCA
	RRCA
	RRCA
	OR	B
	LD	H,E
	LD	L,A
	LD	(PIXADR),HL	;Pixel addr (word or byte=col)
	CALL	OUTADR		;Send pixel address to port
	POP	DE
	POP	BC
	RET

;	Pixel bit array
GBITS	DB	10000000B
	DB	01000000B
	DB	00100000B
	DB	00010000B
	DB	00001000B
	DB	00000100B
	DB	00000010B
	DB	00000001B

;-----------------------------------------------------------------------------
;	Shifts screenpixel to the right
;	Inc col and output address
RIGHTC	LD	A,(PIXBIT)	;Pixel bit mask (1 bit set)
	RRCA
	LD	(PIXBIT),A	;Pixel bit mask (1 bit set)
	RET	NC
	LD	A,(PIXADR)	;Pixel addr (word or byte=col)
	INC	A
	OUT	(80H),A
	LD	(PIXADR),A	;Pixel addr (word or byte=col)
	RET

;-----------------------------------------------------------------------------
;	Shifts screenpixel to the left
;	Dec col and output address
LEFTC	LD	A,(PIXBIT)	;Pixel bit mask (1 bit set)
	RLCA
	LD	(PIXBIT),A	;Pixel bit mask (1 bit set)
	RET	NC
	LD	A,(PIXADR)	;Pixel addr (word or byte=col)
	DEC	A
	LD	(PIXADR),A	;Pixel addr (word or byte=col)
	OUT	(80H),A
	RET

;-----------------------------------------------------------------------------
;	Shifts screenpixel up
;	Dec row and output address
UPC	LD	A,(PIXROW)	;Pixel row addr (byte=row)
	DEC	A
;	Set row and output address
SETROW	LD	(PIXROW),A	;Pixel row addr (byte=row)
	CALL	OUTADR		;Send pixel address to port
	RET

;-----------------------------------------------------------------------------
;	Shifts screenpixel down
;	Move to next line down in Y
DOWNC	LD	A,(PIXROW)	;Pixel row addr (byte=row)
	INC	A
	JR	SETROW		;Set row and output address

;-----------------------------------------------------------------------------
;	Send pixel address to port
OUTADR	LD	A,(PIXADR)	;Pixel addr (word or byte=col)
	OUT	(80H),A
	LD	A,(PIXROW)	;Pixel row addr (byte=row)
	OUT	(81H),A
	RET

;-----------------------------------------------------------------------------
;	Get pixel address and bit pos
FETCHC	LD	HL,(PIXADR)	;Pixel addr (word or byte=col)
	LD	A,(PIXBIT)	;Pixel bit mask (1 bit set)
	RET

;-----------------------------------------------------------------------------
;	Put pixel address and bit pos
STOREC	LD	(PIXADR),HL	;Pixel addr (word or byte=col)
	LD	(PIXBIT),A	;Pixel bit mask (1 bit set)
	CALL	OUTADR		;Send pixel address to port
	RET

;-----------------------------------------------------------------------------
;	Reads attribute byte of current screenpixel
READC	CALL	OUTADR		;Send pixel address to port
	PUSH	HL
	IN	A,(82H)
	LD	L,A
	LD	A,(PIXBIT)	;Pixel bit mask (1 bit set)
	AND	L
	POP	HL
	RET	Z
	LD	A,01H
	RET

;-----------------------------------------------------------------------------
; 	Set horizontal screenpixels: 
;	draws an horizontal line - used for filled polygons
;	Entry:	HL	Pixel count
;		PIXBIT	Pixel bit mask
;		PIXADR	Pixel address
;		ATRBYT	Tile row
;	Preserved: DE
NSETCX	PUSH	DE		;save register DE
	EX	DE,HL		;DE = pixel count
NSETCL1	LD	A,(PIXBIT)	;Pixel bit mask (1 bit set)
	RLCA			;shift out leftmost bit
	JR	C,NSETCLN	;If set, try & draw line
NSETCL2	CALL	SETC		;Set the pixel
	CALL	RIGHTC		;Inc col and output address
	DEC	DE		;decrement pixel count
	LD	A,D		;0 reached?
	OR	E
	JR	NZ,NSETCL1	;nope, loop again
	JR	POPDRT		;restore DE and return

NSETCLN	LD	HL,(PIXADR)	;Get pixel address
	PUSH	DE		;Save pixel count (<640D = 280H)
	LD	A,E		;eeee eeee - Divide DE by 8
	AND	0F8H		;eeee e000
	RRCA			;0eee ee00
	RRCA			;00ee eee0
	RRCA			;000e eeee
	LD	E,A		
	LD	A,D		;0000 00dd
	AND	0FH		;0000 00dd - probably unneeded
	RRCA			;d000 000d
	RRCA			;dd00 0000
	RRCA			;0dd0 0000
	OR	E		;0dde eeee - <80D = 50H
	CALL	NZ,DRAWH	;draw horiz line of A bytes
	POP	DE		;restore pixel count
	LD	A,E		;get pixel count low byte
	AND	07H		;get pixel offset in byte
	LD	E,A		;update pixel count low byte
	JR	Z,POPDRT	;is null? yes, return
	LD	A,80H		;
	LD	(PIXBIT),A	;address leftmost pixel of byte
	LD	(PIXADR),HL	;update pixel addr
	LD	D,00H		;clear pixel count high byte
	JR	NSETCL2		;complete the line

;	Restore register DE and return
POPDRT	POP	DE		;restore DE
	RET

;-----------------------------------------------------------------------------
;	Draw Horizontal line
;	Entry:	A	number of bytes to write
;		HL	= PIXADR, pixel address
;		OUT83H	graphics control byte
;		ATRBYT	byte to write to graphics (tile row)
;	Exit:	HL	= pixel address + number of bytes written
;		Preserved: H, DE
DRAWH	LD	B,A		;Bytes count
	ADD	A,L		;Horiz address
	LD	L,A		;Add bytes count to it
	LD	A,(OUT83H)	;get Port 83H image (Graphics Mode)
	AND	0BBH		;Set X auto-increment
	OUT	(83H),A		;Activate mode
	LD	A,(ATRBYT)	;Attributes byte
DRHLOP	OUT	(82H),A		;Write byte with auto-increment
	DEC	B		;DJNZ loop
	JP	NZ,DRHLOP
	LD	A,(OUT83H)	;Restore graphics mode
	OUT	(83H),A		;
	RET

;-----------------------------------------------------------------------------
;	Check and store -(attr) = 00 or FF
;	Entry:	A	attr, 0 or 1
;	Exit:	A	-attr, 0 of -1 (0FFH)
STOATR	CALL	CHKATR		;Assert attr A<2, Cy if not
	RET	C		;return with Cy set if not
	CALL	NEGA		;A := -A (0 or FF)
	LD	(ATRBYT),A	;Attributes byte
	OR	A		;Set flags, clear carry
	RET

;-----------------------------------------------------------------------------
;	A := -A (0 or FF)
NEGA	RRCA
	SBC	A,A
	RET

;-----------------------------------------------------------------------------
;	Assert attr A<2, Cy if not
CHKATR	CP	02H
	CCF
	RET

;-----------------------------------------------------------------------------
;	Update current screenpixel of specified attribute byte
;	TRS-80 Do the SET
SETC	PUSH	DE
	PUSH	BC
	CALL	OUTADR		;Send pixel address to port
	LD	A,(PIXBIT)	;Pixel bit mask (1 bit set)
	LD	B,A
	LD	A,(ATRBYT)	;Attributes byte
	LD	C,A
	IN	A,(82H)
	LD	E,A
	XOR	C
	AND	B
	XOR	E
	OUT	(82H),A
	POP	BC
	POP	DE
	RET

;-----------------------------------------------------------------------------
;	TRS-80 Init graphics
GINIT	CALL	GCLEAR		;clear graphics
	CALL	GRSTVW		;reset viewport
	CALL	GINIVW		;Init viewport
	RET

;-----------------------------------------------------------------------------
;	TRS-80 Initialize CRTC
INICRT	PUSH	AF
	PUSH	BC
	PUSH	HL
	LD	B,10H
	LD	C,00H
	LD	HL,CRTINI
L8C73	LD	A,C
	OUT	(88H),A
	INC	C
	LD	A,(HL)
	INC	HL
	OUT	(89H),A
	DEC	B
	JP	NZ,L8C73
	POP	HL
	POP	BC
	POP	AF
	RET

;-----------------------------------------------------------------------------
;	TRS-80 clear graphics
GCLEAR	CALL	INICRT
	LD	A,(NOCLR)	;TRS-80: Inhibit GCLEAR on INIT
	OR	A
	JR	NZ,L8CA8
	LD	A,0A0H
	OUT	(83H),A
	LD	C,0F0H
	LD	DE,0000H
L8C95	LD	A,E
	OUT	(80H),A
	LD	A,D
	OUT	(81H),A
	XOR	A
	LD	B,50H
L8C9E	OUT	(82H),A
	DEC	B
	JP	NZ,L8C9E
	INC	D
	DEC	C
	JR	NZ,L8C95
L8CA8	LD	A,0FEH
	OUT	(83H),A
	LD	(OUT83H),A	;TRS-80: Port 83H image (Graphics Mode)
	RET

;	TRS-80 CRTC initialization data
CRTINI	DB	62H,50H,55H,08H,19H,04H,18H,18H
	DB	00H,09H,00H,00H,00H,00H,00H,00H

;-----------------------------------------------------------------------------
;	TRS-80 reset viewport
GRSTVW	LD	HL,00EFH
	LD	(VIEWY2),HL	;Viewport lower right Y-coordinate
	LD	HL,027FH
	LD	(VIEWX2),HL	;Viewport lower right X-coordinate
	LD	HL,0000H
	LD	(VIEWX1),HL	;Viewport upper left X-coordinate
	LD	(VIEWY1),HL	;Viewport upper left Y-coordinate
	LD	(GRPACX),HL	;X part of graphics accumulator
	LD	(GRPACY),HL	;Y part of graphics accumulator
	EX	DE,HL
	LD	B,D
	LD	C,E
	CALL	MAPXYC		;Map into a "C"
	RET

;-----------------------------------------------------------------------------
;	TRS-80 Init viewport
GINIVW	LD	HL,(VIEWX1)	;Viewport upper left X-coordinate
	LD	B,H
	LD	C,L
	LD	DE,0000H
	CALL	MAPXYC		;Map into a "C"
	LD	A,(PIXADR)	;Pixel addr (word or byte=col)
	LD	(VWLADR),A	;Viewport left bound addr
	LD	A,(PIXBIT)	;Pixel bit mask (1 bit set)
	LD	(VWLMSK),A	;Viewport left bound bit mask
	LD	HL,(VIEWX2)	;Viewport lower right X-coordinate
	LD	B,H
	LD	C,L
	LD	DE,0000H
	CALL	MAPXYC		;Map into a "C"
	LD	A,(PIXADR)	;Pixel addr (word or byte=col)
	LD	(VWRADR),A	;Viewport right bound addr
	LD	A,(PIXBIT)	;Pixel bit mask (1 bit set)
	LD	(VWRMSK),A	;Viewport right bound bit mask
	LD	HL,(VIEWX1)	;Viewport upper left X-coordinate
	LD	B,H
	LD	C,L
	LD	HL,(VIEWY1)	;Viewport upper left Y-coordinate
	EX	DE,HL
	CALL	MAPXYC		;Map into a "C"
	RET


;=============================================================================
;	Specific statements for the TRS-80 (not in GW-BASIC)
;
;	VIEW command
;
;	Syntax: VIEW (x1,y1)-(x2,y2) [ , [ c ] [ ,b ] ]
;		c = color of the interior (1 -or- 0,default)
;		b = border (1=drawn -or- 0=not drawn,default)
;	
VIEW	DEC	HL		;backup pointer (not sure if necessary...)
	CALL	CHRGTR		;get last char
;	Get upper left corner coordinate
	XOR	A		;to set upper left corner
	CALL	SCANVP		;scan viewport coordinate
	CALL	SYNCHR		;Must have a '-'
;	Get lower right corner coordinate
	DB	MINUTK
	LD	A,01H		;to set lower right corner
	CALL	SCANVP		;Scan Viewport Coordinate "(x,y)"
;	Check good ordering of coordinates
	PUSH	HL		;save text pointer
	LD	HL,(VIEWX1)	;get Viewport upper left X-coordinate
	EX	DE,HL
	LD	HL,(VIEWX2)	;get Viewport lower right X-coordinate
	CALL	ICOMP		;verify that VIEWX1 .LE. VIEWX2
	OR	A
	JP	M,FCERR		;if not, FC Error
	LD	HL,(VIEWY1)	;get Viewport upper left Y-coordinate
	EX	DE,HL
	LD	HL,(VIEWY2)	;get Viewport lower right Y-coordinate
	CALL	ICOMP		;verify that VIEWY1 .LE. VIEWY2
	OR	A
	JP	M,FCERR		;if not, FC Error
;	Prepare to scan optional fill and border colors
	LD	BC,0000H	;no fill (B), no border (C)
	POP	HL
	DEC	HL		;back up pointer
	CALL	CHRGTR		;re-get last char
	JR	Z,L8D7F		;finalize viewport init if no more params
	CALL	SYNCHR		;must have a comma
	DB	','
	CP	','		;followed by another comma?
	JR	Z,L8D6F		;Skip parameter if yes
;	Parse the fill color
	PUSH	BC		;save BC
	CALL	GETBYT		;get byte as fill color
	POP	BC		;restore BC
	CP	02H		;byte must be 0 or 1
	JP	NC,FCERR	;if not, FC Error
	INC	A		;Add 1
	LD	B,A		;Store fill color to B
	DEC	HL		;Back up ptr
	CALL	CHRGTR		;re-get last char
	JR	Z,L8D7F		;finalize viewport init if no more params
;	Parse the border color
L8D6F	CALL	SYNCHR		;Must have a comma
	DB	','
	PUSH	BC		;save BC
	CALL	GETBYT		;get byte as border color
	POP	BC		;restore BC
	CP	02H		;byte must be 0 or 1
	JP	NC,FCERR	;if not, FC Error
	INC	A		;Add 1
	LD	C,A		;Store border color to C
;	Finalize viewport initialization
L8D7F	PUSH	HL		;save text ptr
	PUSH	BC		;save fill & border
	LD	A,B		;get fill color
	OR	A		;is it defined?
	JR	Z,L8D89		;skip if not
;	Fill with color
	DEC	A		;adjust color to 0 or 1
	CALL	DOFILL		;Fill screen/viewport with A
L8D89	CALL	GINIVW		;Init viewport
	CALL	INIPOS		;Init position to viewport coords
	POP	BC		;restore fill & border
	LD	A,C		;get border color
	POP	HL		;restore text ptr
	OR	A		;is the border defined?
	RET	Z		;return if not
;	Draw border around viewport
	DEC	A		;adjust border color to 0 or 1
	PUSH	HL		;save text ptr
	LD	HL,0FFFFH	;set plain line style
	LD	(GSTYLE),HL	;
	CALL	STOATR		;Check and store -attr = 00 or FF
	LD	HL,(VIEWX2)	;get Viewport lower right X-coordinate
	PUSH	HL		;save it
	INC	HL		;incr it
	LD	(GRPACX),HL	;store to X part of graphics accumulator
	LD	(GXPOS),HL	;store to X Position of Second Coordinate
	LD	HL,(VIEWY2)	;get Viewport lower right Y-coordinate
	PUSH	HL		;save it
	INC	HL		;incr it
	LD	(GRPACY),HL	;store to Y part of graphics accumulator
	LD	(GYPOS),HL	;store to Y Position of Second Coordinate
	LD	HL,(VIEWX1)	;get Viewport upper left X-coordinate
	PUSH	HL		;save it
	DEC	HL		;decr it
	LD	B,H		;to BC
	LD	C,L
	LD	HL,(VIEWY1)	;get Viewport upper left Y-coordinate
	PUSH	HL		;save it
	EX	DE,HL		;to DE
	DEC	DE		;decr it
	LD	HL,0000H	;temporarily reset viewport to entire screen
	LD	(VIEWX1),HL	;Viewport upper left X-coordinate
	LD	(VIEWY1),HL	;Viewport upper left Y-coordinate
	LD	HL,639
	LD	(VIEWX2),HL	;Viewport lower right X-coordinate
	LD	HL,239
	LD	(VIEWY2),HL	;Viewport lower right Y-coordinate
	CALL	BOXLIN		;draw box around viewport (1 pix outside)
	POP	HL		;restore viewport coordinates
	LD	(VIEWY1),HL	;set Viewport upper left Y-coordinate
	POP	HL
	LD	(VIEWX1),HL	;set Viewport upper left X-coordinate
	POP	HL
	LD	(VIEWY2),HL	;set Viewport lower right Y-coordinate
	POP	HL
	LD	(VIEWX2),HL	;set Viewport lower right X-coordinate
	CALL	INIPOS		;Init position to viewport coords
	POP	HL		;restore text ptr
	RET

;-----------------------------------------------------------------------------
;	TRS-80 Scan Viewport Coordinate "(x,y)"
;	Entry:	HL	Text pointer
;		A	Set upper left (A=0) or lower right (A>0)
;	Exit:	HL	Updated text pointer
SCANVP	PUSH	AF		;Save PSW
	DEC	HL		;Back up pointer
	CALL	CHRGTR		;re-get char
	CALL	SYNCHR		;must have a '('
	DB	'('
	CALL	GETIN2		;Get X-coord in DE
	LD	A,D		;D must be positive
	OR	A
	JP	M,FCERR		;if not, FC Error
	PUSH	HL		;Save pointer
	LD	HL,640		;DE must be .LT. 640
	CALL	COMPAR
	JP	C,FCERR		;if .GT. 640, FC Error
	JP	Z,FCERR		;if = 640, FC Error
	EX	DE,HL		;X-coord to HL
	EX	(SP),HL		;X-coord on stack, recover text ptr in HL
	DEC	HL		;Back up pointer
	CALL	CHRGTR		;Re-get char
	CALL	SYNCHR		;Must have a comma
	DB	','
	CALL	GETIN2		;Get Y-coord in DE
	LD	A,D		;D must be positive
	OR	A
	JP	M,FCERR		;if not, FC Error
	DEC	HL		;Back up text ptr
	CALL	CHRGTR		;Re-get last char
	CALL	SYNCHR		;Must have a ')'
	DB	')'
	PUSH	HL		;Save text ptr
	LD	HL,240		;DE must be .LT. 240
	CALL	COMPAR
	JP	C,FCERR		;if .GT. 240, FC Error
	JP	Z,FCERR		;if = 240, FC Error
	POP	BC		;restore text ptr to BC
	POP	HL		;restore X-coord to HL
	POP	AF		;Restore PSW
	PUSH	BC		;save text ptr
	OR	A		;is upper left (A=0) or lower right (A>0)?
	JP	NZ,J8E45	;If lower right then go
	LD	(VIEWX1),HL	;Set Viewport upper left X-coordinate
	EX	DE,HL		;
	LD	(VIEWY1),HL	;set Viewport upper left Y-coordinate
	POP	HL		;restore text pointer
	RET

J8E45	LD	(VIEWX2),HL	;Set Viewport lower right X-coordinate
	EX	DE,HL
	LD	(VIEWY2),HL	;Set Viewport lower right Y-coordinate
	POP	HL
	RET

;-----------------------------------------------------------------------------
;	TRS-80 &VIEW function
;
;	Get one of the view port coordinates
;
;	Syntax: x = &VIEW(n)
;	with	n = 0 to return VIEWX1
;		n = 1 to return VIEWY1
;		n = 2 to return VIEWX2
;		n = 3 to return VIEWY2

FVIEW	CALL	CHRGTR		;eat char after
	CALL	SYNCHR		;eat left paren
	DB	'('
	CALL	GETBYT		;parse byte value
	PUSH	AF		;save it
	DEC	HL		;back up ptr
	CALL	CHRGTR		;eat char
	CALL	SYNCHR		;eat right paren
	DB	')'
	POP	AF		;restore byte value
	CP	04H		;must be between 0 and 3
	JP	NC,FCERR	;if not, FC Error
	PUSH	HL		;Save text ptr
	LD	HL,VIEWX1	;point to Viewport upper left X-coordinate
	RRCA			;check bit 0 of parameter
	JP	NC,L8E73	;skip if not set
	LD	DE,0004H	;else move pointer from VIEWX1 to VIEWY1
	ADD	HL,DE		;
L8E73	RRCA			;check bit 1 of parameter
	JP	NC,L8E79	;skip if not set
	INC	HL		;else move pointer from VIEW?1 to VIEW?2
	INC	HL
L8E79	LD	A,(HL)		;get coordinate
	INC	HL
	LD	H,(HL)		;  to HL
	LD	L,A
	CALL	MAKINT		;convert result to INT
	POP	HL		;restore text ptr
	RET

;-----------------------------------------------------------------------------
;	TRS-80 Fill screen/viewport with A
;	Entry:	A	Fill color
DOFILL	CALL	STOATR		;Check and store -attr = 00 or FF
	LD	HL,(VIEWX2)	;get Viewport lower right X-coordinate
	LD	(GXPOS),HL	;store as X Position of Second Coordinate
	LD	HL,(VIEWY2)	;get Viewport lower right Y-coordinate
	LD	(GYPOS),HL	;store as Y Position of Second Coordinate
	LD	HL,(VIEWX1)	;get Viewport upper left X-coordinate
	LD	B,H		;move to BC
	LD	C,L
	LD	HL,(VIEWY1)	;get Viewport upper left Y-coordinate
	EX	DE,HL		;move to DE
	CALL	DOBOXF		;fill the box
	RET

;-----------------------------------------------------------------------------
;	TRS-80 SCREEN command
;
;	Syntax: SCREEN s
;		s = 1 (text screen) or 0 (graphics screen)
;	
SCREEN	LD	B,00H
	PUSH	BC
	DEC	HL
	CALL	CHRGTR
	JR	Z,L8EAD
	CALL	GETBYT
	POP	BC
	LD	B,A
	PUSH	BC
L8EAD	POP	BC
	LD	A,B
	CP	02H
	JP	NC,FCERR	;Illegal function call
	CPL
	AND	01H
	LD	B,A
	LD	A,(OUT83H)	;TRS-80: Port 83H image (Graphics Mode)
	AND	0FEH
	OR	B
	OUT	(83H),A
	LD	(OUT83H),A	;TRS-80: Port 83H image (Graphics Mode)
	RET

;-----------------------------------------------------------------------------
;	TRS-80 CLR graphics statement
;	Clears the graphics screen
;
;	Syntax: CLR
;
CLR	PUSH	HL
	XOR	A
	CALL	DOFILL		;Fill screen/viewport with A
	POP	HL
	RET

;-----------------------------------------------------------------------------
;	Init position to viewport coords
INIPOS	LD	HL,(VIEWX1)	;Viewport upper left X-coordinate
	LD	(GRPACX),HL	;X part of graphics accumulator
	LD	(GXPOS),HL	;X Position of Second Coordinate
	LD	HL,(VIEWY1)	;Viewport upper left Y-coordinate
	LD	(GRPACY),HL	;Y part of graphics accumulator
	LD	(GYPOS),HL	;Y Position of Second Coordinate
	RET

;-----------------------------------------------------------------------------
;	Check out-of-bound for X or Y coordinate
;	relative to viewport
;	Used by ADJCVP, sub level 3
;	Entry:	HL	Viewport coordinates pointer
;		DE	X or Y-coord to check
;		BC	C to be added 0 to 3 times B
;	Exit:	BC	C + B if DE below lower bound
;			C + 2*B if DE above higher bound
;		HL	HL + 4
CKOOB	PUSH	BC		;Save status
	LD	C,(HL)		;Get viewport upper-left X or Y-coords @HL
	INC	HL
	LD	B,(HL)
	INC	HL
	PUSH	BC
	EX	(SP),HL		;pointer to stack, BC to HL
	CALL	ICOMP		;Compare with DE, coord to check
	OR	A		;set flags
	POP	HL		;restore pointer
	JP	M,L8EF4		;Skip if HL .LE. DE
	JR	Z,L8EF4
	POP	BC
	LD	A,C		;otherwise set status C = C * B
	ADD	A,B
	LD	C,A
	PUSH	BC		;re-save status
L8EF4	LD	C,(HL)		;Get viewport lower-right X or Y-coords @HL
	INC	HL
	LD	B,(HL)
	INC	HL
	PUSH	BC
	EX	(SP),HL		;pointer to stack, BC to HL
	CALL	ICOMP		;Compare with DE, coord to check
	OR	A		;set flags
	POP	HL		;restore pointer
	JP	P,L8F08		;Skip if HL .GE. DE
	POP	BC		
	LD	A,C		;otherwise set status C = C * B
	ADD	A,B
	ADD	A,B
	LD	C,A
	PUSH	BC		;re-save status
L8F08	POP	BC		;restore status
	RET

;-----------------------------------------------------------------------------
;	Check out-of-bounds for coordinate (BC,DE)
;	relative to viewport
;	Used by ADJCVP, sub level 2
;	Entry:	BC	X-coords to check
;		DE	Y-coords to check
;	Exit:	A	Out-of-bounds flags
;		All registers except A unaltered
CKOOBXY	PUSH	HL		;save registers
	PUSH	BC
	PUSH	DE
	LD	HL,VIEWX1	;Viewport upper left X-coordinate
	LD	D,B		;X-coords to DE
	LD	E,C
	LD	B,01H		;out-of-bounds  bit mask for X-coord
	LD	C,00H		;init out-of-bounds flags
	CALL	CKOOB		;Check X-coord
	LD	B,04H		;status bit mask for Y-coord
	POP	DE		;Y-coord to DE
	PUSH	DE		;Save
	CALL	CKOOB		;Check Y-coord
	POP	DE
	LD	A,C		;Get out-of-bounds flags to A
	POP	BC		;restore registers
	POP	HL
	RET

;-----------------------------------------------------------------------------
;	Check out-of-bounds for first and second coordinate
;	relative to viewport
;	Used by ADJCVP, sub level 1
;	Entry:	BC	First X-coord to check
;		DE	First Y-coord to check
;		GXPOS	Second X-coord to check
;		GYPOS	Second Y-coord to check
;	Exit:	L	Out-of-bounds flags for first coordinate
;		H	Out-of-bounds flags for second coordinate
CKOOB12	PUSH	BC		;Save regs
	PUSH	DE
	CALL	CKOOBXY		;Check first coordinate (BC,DE)
	PUSH	AF		;Save out-of-bounds flags
	LD	HL,(GXPOS)	;X Position of Second Coordinate
	LD	B,H		;to BC
	LD	C,L
	LD	HL,(GYPOS)	;Y Position of Second Coordinate
	EX	DE,HL		;to DE
	CALL	CKOOBXY		;Check second coordinate (BC,DE)
	LD	H,A		;OOB flags for 2nd coord to H
	POP	AF		;restore OOB flags for 1st coord
	LD	L,A		;OOB flags for 1st coord to L
	POP	DE		;Restore regs
	POP	BC
	RET

;-----------------------------------------------------------------------------
;	Adjust Out-of-bounds coordinates (BC,DE) and (GXPOS,GYPPOS)
;	relative to viewport
ADJCVP	CALL	CKOOB12		;Check out-of-bounds for 1st & 2nd coordinate
CVVPLP	LD	A,L		;Is out-of-bounds?
	OR	H
	RET	Z		;No, return
	LD	A,L		;Check if 1st and 2nd coord have same flag(s) set
	AND	H
	RET	NZ		;Yes, return (out of bounds on the same side)
	LD	A,L		;Is 1st coord out of bounds?
	OR	A
	JR	NZ,CVPNSWP	;No, swap coords and OOB flags
	CALL	XCHGAC		;exchanges ([B,C],[D,E]) with (GXPOS,GYPOS)
	LD	A,H		;Swap OOB flags
	LD	H,L
	LD	L,A
CVPNSWP	PUSH	HL		;Save flags
	LD	A,L		;1st coord OOB flags
	RRCA			;Is X below minimum?
	LD	HL,(VIEWX1)	;Viewport upper left X-coordinate
	CALL	C,ADJVWX	;yes, adjust (BC,DE) coordinate to HL
	JP	C,CVVPDON	; and done
	RRCA			;is X above maximum?			
	LD	HL,(VIEWX2)	;Viewport lower right X-coordinate
	CALL	C,ADJVWX	;yes, adjust (BC,DE) coordinate to HL
	JP	C,CVVPDON	; and done
	RRCA			;Is Y below minimum?
	LD	HL,(VIEWY1)	;Viewport upper left Y-coordinate
	CALL	C,ADJVWY	;adjust (GXPOS,GYPOS) coordinate to HL
	JP	C,CVVPDON	; and done
	RRCA			;is Y above maximum?			
	LD	HL,(VIEWY2)	;Viewport lower right Y-coordinate
	CALL	C,ADJVWY	;yes, adjust (GXPOS,GYPOS) coordinate to HL
CVVPDON	CALL	CKOOBXY		;Check again OOB for coord (BC,DE)
	POP	HL		;restore OOB flags for (GXPOS,GYPOS)
	LD	L,A		;Save OOB flags for (BC,DE) to L
	JR	CVVPLP		;Check next coordinate

;-----------------------------------------------------------------------------
;	Adjust OOB coordinate (BC,DE) to new X-coord in (HL)
;	Entry:	BC	1st X-coord
;		DE	1st Y-coord
;		GXPOS	2nd X-coord
;		GYPOS	2nd Y-coord
;		HL	new X-coord
;	Exit:	BC	new X-coord
;		DE	new Y-coord
;		AF left unaltered (OOB flags)
;	Computes DE = DE + (new X-coord - 1st X-coord )
;			 * (2nd Y-coord - 1st Y-coord )
;			 / (2nd X-coord - 1st X-coord )
;			 
ADJVWX	PUSH	AF		;Save OOB flags
	PUSH	HL		;New X-coord
	LD	HL,(GYPOS)	;Y Position of Second Coordinate
	OR	A
	SBC	HL,DE		;Y-delta in HL = 2nd Y-coord - 1st Y-coord
	PUSH	BC		;save 1st coordinate
	PUSH	DE
	CALL	MAKINT		;Y-delta to FAC as integer
	CALL	CSNG		;convert FAC to SNG
	LD	HL,GARG		;Move Y-delta to GARG
	CALL	$MOVMF		;
	POP	DE		;restore 1st coordinate
	POP	BC
	PUSH	DE		;save 1st Y-coord
	LD	D,B		;1st X-coord to DE
	LD	E,C
	LD	HL,(GXPOS)	;HL = 2nd X-coord
	OR	A
	SBC	HL,DE		;X-delta in HL = 2nd X-coord - 1st X-coord
	PUSH	BC		;save 1st X-coord
	CALL	MAKINT		;X-delta to FAC as integer
	CALL	CSNG		;convert FAC to SNG
	LD	HL,GARG		;HL = FP argument for graphics
	PUSH	HL		;Save GARG ptr
	CALL	$MOVRM		;Move GARG to registers (BC,DE)
	CALL	FDIV		;Ratio in FAC = Y-delta (in BC,DE) / X-delta (in FAC)
	POP	HL		;restore GARG ptr
	CALL	$MOVMF		;Move Ratio in FAC to GARG
	POP	BC		;restore coordinate
	POP	DE
	POP	HL		;restore new X-coord
	PUSH	DE		;save Y-coord
	LD	D,B		;1st X-coord to DE
	LD	E,C
	LD	B,H		;new X-coord, to HL
	LD	C,L
	PUSH	BC		;save new X-coord
	OR	A
	SBC	HL,DE		;X-inc HL = new X-coord - 1st X-coord
	CALL	MAKINT		;to FAC as integer
	CALL	CSNG		;convert to SNG
	LD	HL,GARG		;HL = FP argument for graphics
	CALL	$MOVRM		;Move GARG to registers (BC,DE)
	CALL	FMULT		;FAC = Ratio [in BC,DE] * X-inc [in FAC]
	CALL	CINT		;Convert FAC to INT
	POP	BC		;restore coord to adjust to
	POP	DE		;restore Y-coord
	ADD	HL,DE		;add Y-inc to Y-coord
	EX	DE,HL		;to DE
	POP	AF		;Restore OOB flags
	RET

;-----------------------------------------------------------------------------
;	Adjust OOB coordinate (BC,DE) to new Y-coord in (HL)
ADJVWY	PUSH	BC		;Swap X and Y coordinates
	PUSH	DE
	POP	BC
	POP	DE
	CALL	SWPPOS		;Swap GXPOS and GYPOS
	CALL	ADJVWX		;Divide & Multiply POS (aspect ratio?)
	PUSH	BC		;Swap X and Y coordinates back
	PUSH	DE
	POP	BC
	POP	DE
	CALL	SWPPOS		;Swap GXPOS and GYPOS back
	RET

;-----------------------------------------------------------------------------
;	Swap GXPOS with GYPOS
SWPPOS	PUSH	HL
	LD	HL,(GYPOS)	;Y Position of Second Coordinate
	PUSH	HL
	LD	HL,(GXPOS)	;X Position of Second Coordinate
	LD	(GYPOS),HL	;Y Position of Second Coordinate
	POP	HL
	LD	(GXPOS),HL	;X Position of Second Coordinate
	POP	HL
	RET

;-----------------------------------------------------------------------------
;	Variables for PRINT #-3
;
;	PRINT #-3 direction (0=L-R, 1=T-B, 2=R-L, 3=B-T)
GPTDIR	DB	00H
;	Save GRPACX
GPTSVX	DB	00H,00H
;	Save GRPACY
GPTSVY	DB	00H,00H
;	Char height Horiz=8, Vert=16
GPTHEI	DB	00H,00H
;	Char width = 8
GPTWID	DB	00H,00H
;	Char bitmap for PUT
GPTBMP	DB	00H,00H,00H,00H,00H,00H,00H,00H
	DB	00H,00H,00H,00H,00H,00H,00H,00H

;-----------------------------------------------------------------------------
;	Forcibly places the screen in text mode.
TOTEXT	PUSH	AF
	LD	A,(OUT83H)	;TRS-80: Port 83H image (Graphics Mode)
	AND	0FEH
	OUT	(83H),A
	POP	AF
	RET

;-----------------------------------------------------------------------------
;	Set graphics mode if enabled with SCREEN 0
TOGRPH	PUSH	AF
	LD	A,(OUT83H)	;TRS-80: Port 83H image (Graphics Mode)
	OUT	(83H),A
	POP	AF
	RET

;-----------------------------------------------------------------------------
;	GLOCATE command
;
;	Syntax: GLOCATE (x,y) [ ,direction ]
;
GLOCAT	CALL	SCAND
	CALL	CHKRNG		;Check X-Y coordinates, FC Error if bad
	DEC	HL
	CALL	CHRGTR		;Bug, end of statement not properly handled.
	OR	A		;  GLOCATE(0,0):CLS fails!
	RET	Z
	CALL	SYNCHR
	DB	','
	CALL	GETBYT
	CP	04H
	JP	NC,FCERR	;Illegal function call
	LD	(GPTDIR),A	;PRINT#-3 direction (0=L-R, 1=T-B, 2=R-L, 3=B-T)
	RET

;-----------------------------------------------------------------------------
;	TODO; id
L9042	LD	HL,(GRPACX)	;X part of graphics accumulator
	LD	(GXPOS),HL	;X Position of Second Coordinate
	LD	B,H
	LD	C,L
	LD	HL,(GRPACY)	;Y part of graphics accumulator
	LD	(GYPOS),HL	;Y Position of Second Coordinate
	EX	DE,HL
	CALL	CHKRNG		;Check X-Y coordinates, FC Error if bad
	CALL	MAPXYC		;Map into a "C"
	LD	B,04H
	LD	HL,GPTHEI	;Char height Horiz=8, Vert=16
	PUSH	HL
	LD	A,01H
	LD	(PUTFLG),A	;Whether doing PUT() or GET()
	JP	PUT2

;-----------------------------------------------------------------------------
;	PRINT#-3 flag
GPRFLG	DB	00H		;When set, redirect output to GPRINT

;-----------------------------------------------------------------------------
;	Do PRINT #-3 on graphics display
;	Char to display is on stack
GPRINT	POP	AF		;Get char to print in A
	PUSH	BC		;Save BC
	LD	C,A		;Char to C
	CALL	GPTCHR		;GPrint the char
	POP	BC		;Restore BD
	RET

;-----------------------------------------------------------------------------
;	GPrint a char in C
GPTCHR	LD	A,C		;Get char
	CP	' '		;is it displayable (.GE. ' ')?
	RET	C		;no, exit
	PUSH	HL
	PUSH	DE
	PUSH	BC
	CALL	TOGRPH		;Set graphics mode if enabled with SCREEN 0
	LD	HL,(GRPACX)	;X part of graphics accumulator
	LD	(GPTSVX),HL	;Save GRPACX
	LD	HL,(GRPACY)	;Y part of graphics accumulator
	LD	(GPTSVY),HL	;Save GRPACY
	LD	A,(GPTDIR)	;PRINT#-3 direction (0=L-R, 1=T-B, 2=R-L, 3=B-T)
	PUSH	AF
	ADD	A,A
	LD	HL,GPTDSP	;PRINT#-3 dispatch table
	ADD	A,L
	LD	L,A
	LD	A,00H
	ADC	A,H
	LD	H,A
	LD	A,(HL)
	INC	HL
	LD	H,(HL)
	LD	L,A
	LD	DE,GPTRET	;Return from dispatch
	PUSH	DE
	LD	DE,0008H
	JP	(HL)

;	Return from dispatch
GPTRET	POP	AF
	POP	BC
	PUSH	BC
	PUSH	AF
	LD	HL,0008H
	LD	(GPTWID),HL	;Char width = 8
	AND	01H
	JR	Z,L90AD
	ADD	HL,HL
L90AD	LD	(GPTHEI),HL	;Char height Horiz=8, Vert=16
	POP	AF
	PUSH	AF
	LD	DE,GPTPUT	;Second dispatch
	PUSH	DE
	CALL	GPTGET		;Get char bitmap address
	PUSH	HL
	ADD	A,A
	LD	HL,GPTDSC	;Dispatch table to copy char bitmap
	ADD	A,L
	LD	L,A
	LD	A,00H
	ADC	A,H
	LD	H,A
	LD	A,(HL)
	INC	HL
	LD	H,(HL)
	LD	L,A
	LD	DE,GPTBMP	;Char bitmap for PUT
	EX	(SP),HL
	RET

;	Second dispatch
GPTPUT	CALL	L9042
	POP	AF
	LD	HL,(GPTSVX)	;Save GRPACX
	LD	(GRPACX),HL	;X part of graphics accumulator
	LD	HL,(GPTSVY)	;Save GRPACY
	LD	(GRPACY),HL	;Y part of graphics accumulator
	ADD	A,A
	LD	HL,GPTDS2	;GPRINT Dispatch Table #2
	ADD	A,L
	LD	L,A
	LD	A,00H
	ADC	A,H
	LD	H,A
	LD	A,(HL)
	INC	HL
	LD	H,(HL)
	LD	L,A
	LD	DE,GPTFIN	;Return address to finalize GPRINT
	PUSH	DE
	LD	DE,0008H
	JP	(HL)

;	Return address to finalize GPRINT
GPTFIN	POP	BC
	POP	DE
	POP	HL
	RET

;	PRINT#-3 dispatch table
GPTDSP	DW	GPRTRG,GPRTDN,GPRTLF,GPRTUP

;	PRINT#-3 left-to-right
GPRTRG	LD	DE,0008H
	LD	HL,(GRPACX)	;X part of graphics accumulator
	ADD	HL,DE
	EX	DE,HL
	LD	HL,(VIEWX2)	;Viewport lower right X-coordinate
	INC	HL
	CALL	COMPAR
	JP	NC,L9117
	LD	HL,(VIEWX1)	;Viewport upper left X-coordinate
	LD	(GRPACX),HL	;X part of graphics accumulator
L9117	LD	HL,(GRPACY)	;Y part of graphics accumulator
	LD	DE,0008H
	ADD	HL,DE
	EX	DE,HL
	LD	HL,(VIEWY2)	;Viewport lower right Y-coordinate
	INC	HL
	CALL	COMPAR
	RET	NC
	LD	HL,(VIEWY1)	;Viewport upper left Y-coordinate
	LD	(GRPACY),HL	;Y part of graphics accumulator
	RET

;	PRINT#-3 top-to-down
GPRTDN	LD	HL,(GRPACX)	;X part of graphics accumulator
	OR	A
	SBC	HL,DE
L9134	OR	A
	SBC	HL,DE
	LD	(GRPACX),HL	;X part of graphics accumulator
	RET

;	PRINT#-3 right-to-left upside down
GPRTLF	CALL	GPRTUP		;PRINT#-3 bottom-up
	LD	HL,(GRPACX)	;X part of graphics accumulator
	JR	L9134

;	PRINT#-3 bottom-up
GPRTUP	LD	HL,(GRPACY)	;Y part of graphics accumulator
	OR	A
	SBC	HL,DE
	LD	(GRPACY),HL	;Y part of graphics accumulator
	RET

;	GPRINT Dispatch Table #2
GPTDS2	DW	GPT2RT,GPT2DN,GPT2LF,GPT2UP

;	Advance cursor to right; wrap at and of line
GPT2RT	LD	HL,(GRPACX)	;X part of graphics accumulator
	ADD	HL,DE
	LD	(GRPACX),HL	;X part of graphics accumulator
	ADD	HL,DE
	EX	DE,HL
	LD	HL,(VIEWX2)	;Viewport lower right X-coordinate
	CALL	COMPAR
	JP	NC,L9188
	LD	HL,(VIEWX1)	;Viewport upper left X-coordinate
	LD	(GRPACX),HL	;X part of graphics accumulator
	LD	HL,(GRPACY)	;Y part of graphics accumulator
	LD	DE,0008H
	ADD	HL,DE
	LD	(GRPACY),HL	;Y part of graphics accumulator
	ADD	HL,DE
	EX	DE,HL
	LD	HL,(VIEWY2)	;Viewport lower right Y-coordinate
	CALL	COMPAR
	JP	NC,L9188
	LD	HL,(VIEWY1)	;Viewport upper left Y-coordinate
	LD	(GRPACY),HL	;Y part of graphics accumulator
L9188	RET

;	Advance cursor to bottom
GPT2DN	LD	HL,(GRPACY)	;Y part of graphics accumulator
	ADD	HL,DE
	LD	(GRPACY),HL	;Y part of graphics accumulator
	RET

;	Advance cursor to left
GPT2LF	LD	HL,(GRPACX)	;X part of graphics accumulator
	OR	A
	SBC	HL,DE
	LD	(GRPACX),HL	;X part of graphics accumulator
	RET

;	Advance cursor to top
GPT2UP	LD	HL,(GRPACY)	;Y part of graphics accumulator
	OR	A
	SBC	HL,DE
	LD	(GRPACY),HL	;Y part of graphics accumulator
	RET

;	Dispatch table to copy char bitmap
GPTDSC	DW	GPTCPR,GPTCPD,GPTCPL,GPTCPU

;	Copy char (normal)
GPTCPR	LD	B,08H
L91AF	LD	A,(HL)
	INC	HL
	LD	(DE),A
	INC	DE
	DEC	B
	JP	NZ,L91AF
	RET

;	Copy char upside down
GPTCPL	LD	BC,0007H
	EX	DE,HL
	ADD	HL,BC
	EX	DE,HL
	LD	B,08H
L91C0	LD	A,(HL)
	CALL	GPTREV		;Reverse 8 pixels row
	LD	(DE),A
	DEC	DE
	INC	HL
	DEC	B
	JP	NZ,L91C0
	RET

;	Reverse 8 pixels row
GPTREV	PUSH	DE
	PUSH	BC
	LD	B,08H
	LD	C,A
	LD	E,80H
	LD	D,00H
L91D5	LD	A,C
	RRCA
	LD	C,A
	JP	NC,L91DE
	LD	A,D
	OR	E
	LD	D,A
L91DE	LD	A,E
	RRCA
	LD	E,A
	DEC	B
	JP	NZ,L91D5
	LD	A,D
	POP	BC
	POP	DE
	RET

;	Copy chr rotated 1/4 turn to left
GPTCPU	LD	BC,000EH
	EX	DE,HL
	ADD	HL,BC
	EX	DE,HL
	LD	B,10H
L91F1	LD	A,B
	CP	08H
	JR	NZ,L91F7
	INC	DE
L91F7	AND	01H
	JR	NZ,L91FF
	LD	A,(HL)
	INC	HL
	JR	L9200

L91FF	XOR	A
L9200	CALL	GPTROL		;Rotate 8 pixels to left
	DEC	B
	JP	NZ,L91F1
	RET

;	Rotate 8 pixels to left
GPTROL	PUSH	HL
	PUSH	DE
	PUSH	BC
	LD	B,08H
	EX	DE,HL
L920E	RLCA
	PUSH	AF
	LD	A,(HL)
	RLA
	LD	(HL),A
	POP	AF
	DEC	HL
	DEC	HL
	DEC	B
	JP	NZ,L920E
	POP	BC
	POP	DE
	POP	HL
	RET

;	Copy chr rotated 1/4 turn to right
GPTCPD	INC	DE
	LD	B,10H
L9221	LD	A,B
	CP	08H
	JR	NZ,L9227
	DEC	DE
L9227	AND	01H
	JR	NZ,L922F
	LD	A,(HL)
	INC	HL
	JR	L9230

L922F	XOR	A
L9230	CALL	GPTROR		;Rotate 8 pixels to right
	DEC	B
	JP	NZ,L9221
	RET

;	Rotate 8 pixels to right
GPTROR	PUSH	HL
	PUSH	DE
	PUSH	BC
	LD	B,08H
	EX	DE,HL
L923E	RLCA
	PUSH	AF
	LD	A,(HL)
	RRA
	LD	(HL),A
	POP	AF
	INC	HL
	INC	HL
	DEC	B
	JP	NZ,L923E
	POP	BC
	POP	DE
	POP	HL
	RET

;	Get char bitmap address
GPTGET	PUSH	AF
	LD	A,C
	SUB	20H
	LD	L,A
	LD	H,00H
	ADD	HL,HL
	ADD	HL,HL
	ADD	HL,HL
	PUSH	BC
	LD	BC,GFONT
	ADD	HL,BC
	POP	BC
	POP	AF
	RET


;-----------------------------------------------------------------------------
;	Graphics font data
;	8 bytes per char
;	96 displayable chars from $20 to $7F
GFONT	DB	00H,00H,00H,00H,00H,00H,00H,00H		; ' '
	DB	08H,08H,08H,08H,00H,00H,08H,00H		; '!'
	DB	14H,14H,14H,00H,00H,00H,00H,00H		; '"'
	DB	14H,14H,3EH,14H,3EH,14H,14H,00H		; '#'
	DB	08H,1EH,28H,1CH,0AH,3CH,08H,00H		; '$'
	DB	30H,32H,04H,08H,10H,26H,06H,00H		; '%'
	DB	18H,24H,28H,10H,2AH,24H,1AH,00H		; '&'
	DB	18H,08H,10H,00H,00H,00H,00H,00H		; '''
	DB	04H,08H,10H,10H,10H,08H,04H,00H		; '('
	DB	10H,08H,04H,04H,04H,08H,10H,00H		; ')'
	DB	00H,08H,2AH,1CH,2AH,08H,00H,00H		; '*'
	DB	00H,08H,08H,3EH,08H,08H,00H,00H		; '+'
	DB	00H,00H,00H,00H,18H,08H,10H,00H		; ','
	DB	00H,00H,00H,3EH,00H,00H,00H,00H		; '-'
	DB	00H,00H,00H,00H,00H,18H,18H,00H		; '.'
	DB	00H,02H,04H,08H,10H,20H,00H,00H		; '/'
	DB	1CH,22H,26H,2AH,32H,22H,1CH,00H		; '0'
	DB	08H,18H,08H,08H,08H,08H,1CH,00H		; '1'
	DB	1CH,22H,02H,04H,08H,10H,3EH,00H		; '2'
	DB	3EH,04H,08H,04H,02H,22H,1CH,00H		; '3'
	DB	04H,0CH,14H,24H,3EH,04H,04H,00H		; '4'
	DB	3EH,20H,3CH,02H,02H,22H,1CH,00H		; '5'
	DB	0CH,10H,20H,3CH,22H,22H,1CH,00H		; '6'
	DB	3EH,02H,04H,08H,10H,10H,10H,00H		; '7'
	DB	1CH,22H,22H,1CH,22H,22H,1CH,00H		; '8'
	DB	1CH,22H,22H,1EH,02H,04H,18H,00H		; '9'
	DB	00H,18H,18H,00H,18H,18H,00H,00H		; ':'
	DB	00H,18H,18H,00H,18H,08H,10H,00H		; ';'
	DB	04H,08H,10H,20H,10H,08H,04H,00H		; '<'
	DB	00H,00H,3EH,00H,3EH,00H,00H,00H		; '='
	DB	10H,08H,04H,02H,04H,08H,10H,00H		; '>'
	DB	1CH,22H,02H,04H,08H,00H,08H,00H		; '?'
	DB	1CH,22H,02H,1AH,2AH,2AH,1CH,00H		; '@'
	DB	1CH,22H,22H,22H,3EH,22H,22H,00H		; 'A'
	DB	3CH,22H,22H,3CH,22H,22H,3CH,00H		; 'B'
	DB	1CH,22H,20H,20H,20H,22H,1CH,00H		; 'C'
	DB	38H,24H,22H,22H,22H,24H,38H,00H		; 'D'
	DB	3EH,20H,20H,3CH,20H,20H,3EH,00H		; 'E'
	DB	3EH,20H,20H,3CH,20H,20H,20H,00H		; 'F'
	DB	1CH,22H,20H,2EH,22H,22H,1EH,00H		; 'G'
	DB	22H,22H,22H,3EH,22H,22H,22H,00H		; 'H'
	DB	1CH,08H,08H,08H,08H,08H,1CH,00H		; 'I'
	DB	0EH,04H,04H,04H,04H,24H,18H,00H		; 'J'
	DB	22H,24H,28H,30H,28H,24H,22H,00H		; 'K'
	DB	20H,20H,20H,20H,20H,20H,3EH,00H		; 'L'
	DB	22H,36H,2AH,2AH,22H,22H,22H,00H		; 'M'
	DB	22H,22H,32H,2AH,26H,22H,22H,00H		; 'N'
	DB	1CH,22H,22H,22H,22H,22H,1CH,00H		; 'O'
	DB	3CH,22H,22H,3CH,20H,20H,20H,00H		; 'P'
	DB	1CH,22H,22H,22H,2AH,24H,1AH,00H		; 'Q'
	DB	3CH,22H,22H,3CH,28H,24H,22H,00H		; 'R'
	DB	1EH,20H,20H,1CH,02H,02H,3CH,00H		; 'S'
	DB	3EH,08H,08H,08H,08H,08H,08H,00H		; 'T'
	DB	22H,22H,22H,22H,22H,22H,1CH,00H		; 'U'
	DB	22H,22H,22H,22H,22H,14H,08H,00H		; 'V'
	DB	22H,22H,22H,2AH,2AH,2AH,14H,00H		; 'W'
	DB	22H,22H,14H,08H,14H,22H,22H,00H		; 'X'
	DB	22H,22H,22H,14H,08H,08H,08H,00H		; 'Y'
	DB	3EH,02H,04H,08H,10H,20H,3EH,00H		; 'Z'
	DB	1CH,10H,10H,10H,10H,10H,1CH,00H		; '['
	DB	22H,14H,3EH,08H,3EH,08H,08H,00H		; '\' (Yen)
	DB	1CH,04H,04H,04H,04H,04H,1CH,00H		; ']'
	DB	00H,00H,08H,14H,22H,00H,00H,00H		; '^'
	DB	00H,00H,00H,00H,00H,00H,3EH,00H		; '_'
	DB	08H,04H,00H,00H,00H,00H,00H,00H		; '`'
	DB	00H,00H,1CH,02H,1EH,22H,1EH,00H		; 'a'
	DB	20H,20H,2CH,32H,22H,22H,3CH,00H		; 'b'
	DB	00H,00H,1CH,22H,20H,22H,1CH,00H		; 'c'
	DB	02H,02H,1AH,26H,22H,22H,1EH,00H		; 'd'
	DB	00H,00H,1CH,22H,3EH,20H,1EH,00H		; 'e'
	DB	08H,14H,10H,3CH,10H,10H,10H,00H		; 'f'
	DB	00H,1EH,22H,22H,1EH,02H,1CH,00H		; 'g'
	DB	20H,20H,2CH,32H,22H,22H,22H,00H		; 'h'
	DB	08H,00H,18H,08H,08H,08H,1CH,00H		; 'i'
	DB	02H,00H,06H,02H,02H,22H,1CH,00H		; 'j'
	DB	20H,20H,20H,24H,38H,28H,24H,00H		; 'k'
	DB	18H,08H,08H,08H,08H,08H,0CH,00H		; 'l'
	DB	00H,00H,34H,2AH,2AH,2AH,2AH,00H		; 'm'
	DB	00H,00H,2CH,32H,22H,22H,22H,00H		; 'n'
	DB	00H,00H,1CH,22H,22H,22H,1CH,00H		; 'o'
	DB	00H,2CH,32H,22H,3CH,20H,20H,00H		; 'p'
	DB	00H,1AH,26H,22H,1EH,02H,02H,00H		; 'q'
	DB	00H,00H,2CH,32H,20H,20H,20H,00H		; 'r'
	DB	00H,00H,1EH,20H,1CH,02H,3CH,00H		; 's'
	DB	10H,10H,3EH,10H,10H,12H,0CH,00H		; 't'
	DB	00H,00H,22H,22H,22H,26H,1AH,00H		; 'u'
	DB	00H,00H,22H,22H,22H,14H,08H,00H		; 'v'
	DB	00H,00H,2AH,2AH,2AH,2AH,14H,00H		; 'w'
	DB	00H,00H,22H,14H,08H,14H,22H,00H		; 'x'
	DB	00H,22H,22H,22H,1EH,02H,1CH,00H		; 'y'
	DB	00H,00H,3EH,04H,08H,10H,3EH,00H		; 'z'
	DB	06H,08H,08H,30H,08H,08H,06H,00H		; '{'
	DB	08H,08H,08H,08H,08H,08H,08H,00H		; '|'
	DB	30H,08H,08H,06H,08H,08H,30H,00H		; '}'
	DB	3EH,00H,00H,00H,00H,00H,00H,00H		; '~' (Macron)
	DB	00H,00H,00H,00H,00H,00H,00H,00H		; DEL (Blank)

;=============================================================================
;	<===	BASICG ADDITIONS END
	ENDIF
	
;=============================================================================
;	TRS-80: Data area for interacting with DOS
;
;	DOS File Control Block
DOSFCB	DB	00H,00H,00H,00H,00H,00H,00H,00H
	DB	00H,00H,00H,00H,00H,00H,00H,00H
	DB	00H,00H,00H,00H,00H,00H,00H,00H
	DB	00H,00H,00H,00H,00H,00H,00H,00H
;	Last DOS Error Code
DOSERR	DB	00H
;	WP flags for 8 drives :0 to :7
DRVFLG	DB	00H,00H,00H,00H,00H,00H,00H,00H
;	Message: "255-Command Aborted"
CMDABT	DB	'255-Command Aborted',00H
	;Ptr to DOS KFLAGS
KFLAGS	DB	00H,00H
	;Ptr to DOS SFLAGS
SFLAGS	DB	00H,00H
	;Ptr to DOS CFLAGS
CFLAGS	DB	00H,00H,00H,00H,00H,00H,00H,00H
	DB	00H,00H,00H,00H,00H,00H,00H,00H
	DB	00H,00H,00H,00H,00H,00H,00H,00H
	DB	00H,00H,00H,00H,00H,00H,00H,00H
	DB	00H,00H,00H,00H,00H,00H,00H,00H
	DB	00H,00H,00H,00H,00H,00H,00H,00H
	DB	00H,00H,00H,00H,00H,00H,00H,00H
	DB	00H,00H,00H,00H,00H,00H,00H,00H
	DB	00H,00H,00H,00H,00H,00H,00H,00H
	DB	00H,00H,00H,00H,00H,00H,00H,00H
	DB	00H,00H,00H,00H,00H,00H,00H,00H
	DB	00H,00H,00H,00H,00H,00H,00H,00H
	DB	00H,00H,00H,00H,00H,00H


;=============================================================================
;	INKEY$, PRINT@, CLS, RANDOM (TRS-80 SPECIFIC)
;-----------------------------------------------------------------------------
;	Check KFLAG$ for keystroke of BREAK or PAUSE (Shift-@)
;	Pause if PAUSE was pressed.
;	Called during BASIC program execution
ISCNTC	LD	A,03H
	CALL	CKFLGA		;TRS-80 Check KFLAG$ with mask
	RET	Z
	XOR	A
	JP	STOP


;-----------------------------------------------------------------------------
;	Wait for keystroke.
;	Call SVC @KBD, ignore Ctrl-C, Break and Shift-@
SVCKBD	PUSH	HL		;SAVE REGS
	PUSH	DE
	PUSH	BC
	;Loop
SVCKBL	LD	A,01H
	CALL	CKFLGA		;TRS-80 Check KFLAG$ with mask
	JR	NZ,SVCKBX	;Exit
	$SVC	@KBD		;scan keyboard
	JR	NZ,SVCKBL	;Loop
	CP	'`'		;Shift-@ ?
	JR	Z,SVCKBL	;Loop
	CP	80H
	JR	Z,SVCKBL	;Loop
	CP	03H
	JR	Z,SVCKBL	;Loop
	;Exit
SVCKBX	POP	BC		;RESTORE REGS
	POP	DE
	POP	HL
	RET


;-----------------------------------------------------------------------------
;	TRS-80 Check KFLAG$ status with mask in A
;	Pause if PAUSE pressed and A.1 set
CKFLGA	PUSH	HL
	LD	HL,(KFLAGS)	;Ptr to DOS KFLAGS
	AND	(HL)
	JR	NZ,CKFLGY	;Here if masked byte from KFLAG$ is not null
	POP	HL
;	Null $BREAK vector
BRKVEC	RET

;	Here if masked byte from KFLAG$ is not null
CKFLGY	PUSH	DE
	PUSH	BC
;	Loop to scan again
CKFLGL	PUSH	AF
	PUSH	HL
;	Wait for keystroke from @KBD
CKFLGW	$SVC	@KBD		;scan keyboard
	JR	Z,CKFLGW	;Wait for keystroke from @KBD
	POP	HL
	POP	AF
	AND	01H
	JR	Z,CKFLG1	;If PAUSE was pressed, pause (Shft-@)...
	LD	C,0F4H
	LD	B,03H
;	Check KFLAGS with mask in (C) and exit
CKFLGX	CALL	RSKFLG		;Reset KFLAG$ status with mask in C
	LD	A,B
	OR	A
	POP	BC
	POP	DE
	POP	HL
	RET

;	If PAUSE was pressed, pause (Shft-@)...
CKFLG1	LD	A,(HL)
	AND	01H
	JR	NZ,CKFLGL	;Loop to scan again
	PUSH	HL
	$SVC	@KBD		;scan keyboard
	POP	HL
	JR	NZ,CKFLG1	;If PAUSE was pressed, pause (Shft-@)...
	CP	'`'		;Shift-@ ?
	JR	Z,CKFLG1	;If PAUSE was pressed, pause (Shft-@)...
	LD	C,0F5H
	LD	B,00H
	JR	CKFLGX		;Check KFLAGS with mask in (C) and exit


;-----------------------------------------------------------------------------
;	TRS-80 Reset KFLAG$ status with mask in C
;	Re-check mask after a short delay
RSKFLG	LD	A,C
	LD	HL,(KFLAGS)	;Ptr to DOS KFLAGS
	DI
	AND	(HL)
	LD	(HL),A
	EI
	PUSH	BC
	LD	BC,0548H
	$SVC	@PAUSE		;wait for delay
	POP	BC
	LD	A,C
	CPL
	AND	(HL)
	JR	NZ,RSKFLG	;Reset KFLAG$ status with mask in C
	RET


;-----------------------------------------------------------------------------
;	Reset BREAK bit in KFLAG$
RSKBRK	PUSH	AF
	PUSH	BC
	PUSH	DE
	PUSH	HL
	LD	C,0FEH
	CALL	RSKFLG		;Reset KFLAG$ status with mask in C
	POP	HL
	POP	DE
	POP	BC
	POP	AF
	RET


;-----------------------------------------------------------------------------
INKEY$	CALL	CHRGTR
	PUSH	HL		;SAVE THE TEXT POINTER
	$SVC	@KBD		;SCAN KEYBOARD
	JR	NZ,MKNLST	;NO KEY, RETURN NULL STRING
	PUSH	AF
	LD	A,01H
	CALL	STRINI		;MAKE ONE CHAR STRING
	POP	AF
	LD	(DE),A
	JP	PUTNEW		;GOTO PUTNEW

MKNLST	LD	HL,DSEGZ2	;ZERO
	LD	(FACLO),HL	;POINTING AT A ZERO
	LD	A,03H
	LD	(VALTYP),A
	POP	HL
	RET

;	Empty String
DSEGZ2	DB	00H

;-----------------------------------------------------------------------------
;	Call SVC @DSP and update position in display line
SVCDSP	PUSH	HL
	PUSH	DE
	PUSH	BC
	PUSH	AF
	LD	C,A
	$SVC	@DSP		;display character
	LD	B,04H
	$SVC	@VDCTL		;control video display
	LD	A,L
	LD	(TTYPOS),A
	POP	AF
	POP	BC
	POP	DE
	POP	HL
	RET


;-----------------------------------------------------------------------------
;	'PRINT @' TRS-80 BASIC extension to move the cursor
PRINT@	CALL	CHRGTR
	CP	'('
	JR	NZ,PRT@ABS
	CALL	GETINT
	JP	NZ,FCERR	;Illegal function call
	LD	A,E
	CP	24
	JP	NC,FCERR	;Illegal function call
	LD	D,E
	PUSH	DE
	CALL	SYNCHR
	DB	','
	CALL	GETIN2		;=ADRGET in ## GIO86.ASM:759 ##
	JP	NZ,FCERR	;Illegal function call
	LD	A,E
	CP	80
	JP	NC,FCERR	;Illegal function call
	CALL	SYNCHR
	DB	')'
	EX	(SP),HL
	LD	L,E
	JR	FINPRT@

PRT@ABS	DEC	HL
	CALL	GETINT
	PUSH	HL
	LD	HL,077FH
	CALL	COMPAR
	JP	C,FCERR		;Illegal function call
	PUSH	DE
	LD	HL,80
	PUSH	HL
	CALL	IDIV		;(BX)=(DX)/(BX)
	LD	B,L
	POP	HL
	POP	DE
	PUSH	BC
	CALL	IMOD		;Integer MODulo
	POP	BC
	LD	H,B
FINPRT@	LD	A,L
	LD	(TTYPOS),A
	LD	B,03H
	$SVC	@VDCTL		;control video display
	POP	HL
	CALL	SYNCHR
	DB	','
	RET

LPTCHR	PUSH	HL
	PUSH	DE
	PUSH	BC
	PUSH	AF
	LD	C,A
	$SVC	@PRT		;send character to printer
	JP	NZ,FERROR	;Get Error Code for function
	POP	AF
	POP	BC
	POP	DE
	POP	HL
	RET


;=============================================================================
;	CLS [,LOCATE,WIDTH (of screen),LCOPY]
; ## GWSTS.ASM:74 ##
;
;	CLS: CLear Screen issues an escape sequence to clear
;	     the CRT.  Sequences are ANSI standard whereas the machine
;	     default is not.  CLS resets the graphics cursor position.
;	ENTRY - none
;	EXIT  - none
;	USES  - none

CLS	PUSH	HL
	LD	C,1CH
	$SVC	@DSP		;display character
	LD	C,1FH
	$SVC	@DSP		;display character
	POP	HL
	XOR	A
	LD	(TTYPOS),A
	RET


;=============================================================================
;	SCNEDT  Screen Oriented Editor for GW-BASIC
; ## SCNEDT.ASM ##
;
;	PRINT "?" BEFORE GETTING INPUT
;	(not ref'd)
QINLIN	LD	A,'?'
	CALL	OUTDO
	LD	A,' '
	CALL	OUTDO
;	INPUT STATEMENT (REDO)
PINLIN	LD	HL,BUF
	CALL	ISFLIO
	JR	NZ,INPFIL
	LD	BC,0F900H
	PUSH	DE
	$SVC	@KEYIN		;accept line of input
	POP	DE
	JP	NZ,FERROR	;Get Error Code for function
	PUSH	AF
	CALL	RSKBRK		;Clear BREAK bit in KFLAG$
	LD	C,B
	LD	B,00H
	ADD	HL,BC
	LD	(HL),B
	LD	B,C
	INC	B
	LD	HL,BUFMIN	;=BUF-1
	XOR	A
	LD	(TTYPOS),A
	POP	AF
	RET

;	TRS-80 input BASIC text from file
INPFIL	LD	B,01H
;	Loop
INPFLP	CALL	INCHR
	JR	NC,IFBRK	;Jump if got EOF
	CALL	LDREOF		;
	LD	HL,BUF
	LD	B,01H
	LD	A,0DH
;	Jump if got EOF
IFBRK	CP	0DH
	JR	NZ,IFNXIT	;Jump if no ENTER or BREAK
	XOR	A
	LD	(HL),A
	LD	HL,BUFMIN	;=BUF-1
	RET

;	Jump if no ENTER or BREAK
IFNXIT	CP	' '
	JR	NC,IFGOOD	;OK to store keystroke
	CP	09H
	JR	Z,IFGOOD	;OK to store keystroke
	CP	0AH
	JR	NZ,INPFLP	;Loop
;	OK to store keystroke
IFGOOD	INC	B
	DEC	B
	JR	Z,IFLINE	;Read line # from file
	LD	(HL),A
	INC	HL
	INC	B
	JR	INPFLP		;Loop

;	Read line # from file
IFLINE	LD	HL,BUF
	CALL	LINGET		;LINGET reads a line # from the current text position
	EX	DE,HL
	LD	(CURLIN),HL
	JP	LBOERR

;	Scan semicolon for NO-CR
SCNSEM	CP	';'
	RET	NZ
	JP	CHRGTR

; ## GWMAIN.ASM:3383 ##
RANDOM	LD	A,R
	LD	(RNDVAM),A
	RET


;=============================================================================
;	SOUND, DATE$, TIME$ ... (TRS-80 MODEL 4 SPECIFIC)
; ## GWSTS.ASM ##
;
;-----------------------------------------------------------------------------
;	 SOUND  -      Make SOUNDs with the speaker.
;
;		Syntax: SOUND x,y
;
;		Where: 	x is the Frequency in Hertz.
;			y is the Duration in Clock ticks. (currently 18.2/sec).
;
;			Frequency must be at least 37 Hz.
;			If Duration is 0, then just turn off current sound...
;
;		On the TRS-80,
;			x is a digit between 0 and 7. It specifies the frequency
;			  level.
;			y is an integer between 0 and 31. It specifies how long the
;			  sound is to be generated.
;
SOUND	CALL	GETBYT		;Get byte value in E and A
	CP	08H
	JR	NC,SOUND11
	CPL
	INC	A
	ADD	A,07H
	PUSH	AF
	CALL	SYNCHR
	DB	','
	CALL	GETBYT		;Get byte value in E and A
	CP	20H
SOUND11	JP	NC,FCERR	;Illegal function call
	RLCA
	RLCA
	RLCA
	POP	BC
	OR	B
	LD	B,A
	PUSH	HL
	$SVC	@SOUND		;beep through speaker
	POP	HL
	RET

;-----------------------------------------------------------------------------
;	X$=DATE$ returns "YYYY-MM-DD" if KANABS&KANADT else "MM-DD-YYYY"
;
;	On the TRS-80, returns "MM/DD/YY" USA Date Format.
;
DATE$	DB	0F6H

;-----------------------------------------------------------------------------
;	X$=TIME$ returns "HH:MM:SS"
;
TIME$	XOR	A
	PUSH	AF
	CALL	CHRGTR
	EX	(SP),HL
	PUSH	HL
	LD	A,08H
	CALL	STRINI
	EX	DE,HL
	POP	AF
	JR	NZ,TIME$12
	$SVC	@TIME		;get system time
TIME$11	JP	PUTNEW

TIME$12	$SVC	@DATE		;get system date
	JR	TIME$11

;-----------------------------------------------------------------------------
;	ROW(dummy) TRS-80 BASIC function
;
;	Get the current video row number (zero-based)
;
ROWFN	LD	B,04H
	$SVC	@VDCTL		;control video display
	LD	A,H
	JP	SNGFLT

;-----------------------------------------------------------------------------
;	'SYSTEM' BASIC command
;
;	SYSTEM alone to quit BASIC
;	SYSTEM "command" to execute a DOS command
;
SYSTEM	JR	NZ,SYSCMD	;Execute SYSTEM command
	CALL	CLSALL		;Close all files (?)
	LD	HL,0000H
	$SVC	@EXIT		;return to LS-DOS
;	Execute SYSTEM command
SYSCMD	CALL	FRMEVL		;Formula evaluator
	PUSH	HL
	CALL	FRESTR
	LD	C,(HL)
	INC	HL
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	LD	H,D
	LD	L,E
	LD	B,00H
	ADD	HL,BC
	LD	A,(HL)
	PUSH	HL
	PUSH	AF
	LD	(HL),0DH
	IF	GFX
	EX	DE,HL		;===>
	$SVC	@CMNDR		;	execute command, return
	PUSH	HL		;<===
	ELSE
	LD	HL,(CFLAGS)	;Ptr to DOS CFLAGS
	PUSH	HL
	DI
	LD	A,(HL)
	OR	10H
	LD	(HL),A
	EI
	EX	DE,HL
SYSCMD1	$SVC	@CMNDR		;execute command, return
	EX	(SP),HL
	DI
	LD	A,(HL)
	AND	0EFH
	LD	(HL),A
	EI
	ENDIF			;GFX
	IFEQ	TEST,0
	CALL	LOADOV1		;Load BASIC/OV1
	ENDIF
	POP	HL
	POP	AF
	POP	DE
	IF	GFX
SYSCMD1:			;<==>	Label moved here
	ENDIF
	LD	(DE),A
	LD	A,H
	OR	L
	JR	Z,SYSCMD2
	LD	A,L
	OR	A
	JR	NZ,ERRSYS
	DEC	A
ERRSYS	JP	FERROR		;Get Error Code for function

SYSCMD2	POP	HL
	RET

;-----------------------------------------------------------------------------
;	Load BASIC/OV1
	IFEQ	TEST,0
LOADOV1	LD	HL,OV1SPEC	;"BASIC/OV1"
	LD	DE,BUF
	PUSH	DE
LODOV11	LD	A,(HL)
	LD	(DE),A
	INC	HL
	INC	DE
	CP	0DH
	JR	NZ,LODOV11
	POP	DE
	$SVC	@LOAD		;load program file
	RET	Z
	LD	HL,CNTLOAD	;"Can't load "
	$SVC	@DSPLY		;display message line
	$SVC	@ABORT		;abort program execution

;	"Can't load "
CNTLOAD	DB	'Can''t load '
;	"BASIC[G]/OV1"
	IF	GFX
OV1SPEC	DB	'BASICG/OV1',0DH
	ELSE
OV1SPEC	DB	'BASIC/OV1',0DH
	ENDIF
	ENDIF

;-----------------------------------------------------------------------------
;	'ERRS$' TRS-80 BASIC special function
;
;	Get the last DOS error string message
;
ERRS$	CALL	CHRGTR
	PUSH	HL
	LD	A,(DOSERR)
	OR	A
	JP	Z,MKNLST
	CP	0FFH
	JR	NZ,ERRS$12
	LD	DE,BUF
	LD	HL,CMDABT
ERRS$11	LD	A,(HL)
	CP	00H
	JP	Z,ERRS$15
	LD	(DE),A
	INC	DE
	INC	HL
	JR	ERRS$11

ERRS$12	PUSH	AF
	LD	L,A
	LD	H,00H
	LD	DE,BUF
	$SVC	@HEXDEC		;hex to decimal ASCII
	LD	A,'-'
	LD	(DE),A
	INC	DE
	POP	AF
ERRS$13	OR	0C0H
	LD	C,A
	LD	HL,(CFLAGS)	;Ptr to DOS CFLAGS
	DI
	LD	A,(HL)
	OR	80H
	LD	(HL),A
	EI
	$SVC	@ERROR		;post error message
	LD	B,0F9H
ERRS$14	LD	A,(DE)
	CP	0DH
	JR	Z,ERRS$15
	INC	DE
	DEC	B
	JP	NZ,ERRS$14
ERRS$15	XOR	A
	LD	(DE),A
	LD	HL,BUFMIN	;=BUF-1
ERRS$16	INC	HL
	LD	A,(HL)
	CP	' '
	JR	Z,ERRS$16
	CALL	STRC		;Common code for HEX$, OCT$ & STR$
	POP	HL
	RET

;-----------------------------------------------------------------------------
;	ADRGET - parse 16 bit expression
;
;	Entry - [HL]=text pointer
;	Exit  - [DE]=result (0..65535)
;		[HL]=updated text pointer
;		Carry is set if not in range
;
ADRGET	CALL	CHRGTR
	CALL	FRMEVL		;Formula evaluator
	PUSH	HL
	CALL	CSNG		;=$FS - L2=FRCSNG
	CALL	SIGN
	JP	M,ADRGET1
	LD	A,(FAC)
	CP	91H
	JR	NC,ADRGET1
	CALL	QINT		;Make Unsigned 16 bits
	XOR	A
	DB	3EH
ADRGET1	SCF
	POP	HL
	RET			;Offset in [DE], text pointer in [HL]


;-----------------------------------------------------------------------------
; ## GIODSK.ASM:1044 ##
;	'NAME' BASIC command  (file rename)
;
;	NAME oldname AS newname
;	 Entry - [HL] = text pointer
;	 Exit  - [HL] = text pointer
NAME	LD	BC,FILNAM	;Old name buffer for NAME ... AS ...
	CALL	NAMFIL		;PICK UP THE OLD NAME TO USE
	CALL	SYNCHR
	DB	'A'		;MAKE SURE "AS" IS THERE
	CALL	SYNCHR
	DB	'S'
	LD	BC,FILNA2
	CALL	NAMFIL		;READ THE NEW NAME
	PUSH	HL		;SAVE THE TEXT POINTER
	LD	DE,BUF
	LD	HL,DOSBUF
	$SVC	@RENAM		;RENAME OPERATION
	JP	NZ,FERROR	;Get Error Code for function
	POP	HL		;RESTORE TEXT POINTER
	RET



;=============================================================================
;	GWINIT GW-BASIC-86 INITIALIZATION
; ## GWINIT.ASM ##
;
;	MOVE ROM INITIALIZATION VALUES TO RAM
;
; Memory map for GW-BASIC:
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
; note:  when [FILTAB] = [STKLOW], no FDB's are allocated.
;        when [FRETOP] = [MEMSIZ], IN-USE string space is empty.
;        when [SP] = [STKLOW], STACK is full.
;
;
;	TRS-80: Calculate DOS file buffers start
;	Returns (HL) - (NFILES) * 234H
CKDOSB	LD	BC,0FDCCH
	LD	A,(NFILES)	;Number of files
	INC	A
CKDOSB1	ADD	HL,BC
	RET	NC
	DEC	A
	JR	NZ,CKDOSB1
	SCF
	RET

;	TRS-80: Initialize table of DFBs with addresses of
;	entries in memory.
INIFDB	LD	HL,(FILTAB)	;points to 1st FDB (=STKLOW if no FDB's)
	INC	HL
	LD	(FILPT1),HL	;TODO (disk files...)
	LD	DE,FDBTAB	;Table of up to 16 FDB's
	LD	BC,0234H
	LD	A,(NFILES)	;Number of files
	INC	A
INIFDB2	EX	DE,HL
	LD	(HL),E
	INC	HL
	LD	(HL),D
	INC	HL
	EX	DE,HL
	ADD	HL,BC
	DEC	A
	JR	NZ,INIFDB2
	RET

;	Update PROFLG protection flag
;	Merge (.OR.) new protection flag value with current value
UPPROF	PUSH	AF
	PUSH	HL
	LD	HL,PROFLG	;non-zero if we have loaded a protected file w/o passwr
	LD	A,(NEWPRO)	;New Protection Flag
	OR	(HL)
	LD	(HL),A
	XOR	A
	LD	(NEWPRO),A	;New Protection Flag
	POP	HL
	POP	AF
	RET

;-----------------------------------------------------------------------------
;	Graphics statements dispatcher
	IF	GFX
GDISPA	SUB	64H		;===>
	JP	C,SNERR		;Syntax Error
	CP	08H
	JP	NC,SNERR	;Syntax Error
	RLCA
	LD	C,A
	LD	B,00H
	EX	DE,HL
	LD	HL,GRADSP
	ADD	HL,BC
	LD	C,(HL)
	INC	HL
	LD	B,(HL)
	PUSH	BC
	EX	DE,HL
	JP	CHRGTR

;	Graphics statements dispatch table
GRADSP	DW	CIRCLE,CLR,GLOCAT,PAINT
	DW	PRESET,PSET,SCREEN,VIEW
				;<===
	ENDIF
;-----------------------------------------------------------------------------
INITSA	CALL	NODSKS		;Clear Files Table, close all files, NEW
	LD	HL,(TXTTAB)
	DEC	HL
	LD	(HL),00H
	LD	HL,(CMDTRM)	;POINT TO START OF COMMAND LINE
	LD	A,H		;GET BYTE POINTED TO
	OR	L		;IF ZERO, NO FILE SEEN
	JP	Z,READY
	LD	(HL),00H
	LD	HL,(CMDPTR)	;Command line pointer
	DEC	HL
	LD	(HL),'"'
	JP	LRUN		;TRY TO RUN FILE

	;Command line pointer
CMDPTR	DW	0000H
	;Filespec terminating char pointer
CMDTRM	DW	0000H


;-----------------------------------------------------------------------------
;	BASIC INITIALIZATION
;
;	INIT - System Initialization Code
;
; ## GWINIT.ASM:56 ##
;
INIT:
	LD	A,(BASFLG)
BASTXT	EQU	$-2
	OR	A
	JR	Z,INIT11
	$SVC	@ABORT		;abort program execution
INIT11	INC	A
	LD	(BASFLG),A
	LD	(CMDPTR),HL	;Command line pointer
	LD	HL,TSTACK
	LD	(FILTAB),HL	;points to 1st FDB (=STKLOW if no FDB's)
	IFEQ	TEST,0
	CALL	LOADOV1		;Load BASIC/OV1
	ENDIF
	CALL	STKINI		;REALLY SET UP INIT'S TEMPORARY STACK
	LD	A,3AH		;INITIALIZE KBUF-1 WITH A COLON
	LD	(KBUF1),A	;DIRECT INPUTS RESTART OK.
	LD	HL,0FFFEH	;SAY INITIALIZATION IS EXECUTING
	LD	(CURLIN),HL	;IN CASE OF ERROR MESSAGE
	LD	HL,PRMSTK	;INITIALIZE PARAMETER BLOCK CHAIN
	LD	(PRMPRV),HL
	LD	A,03H
	LD	(NFILES),A	;Number of files
	LD	B,00H
	LD	H,B
	LD	L,B
	$SVC	@HIGH$		;get or set HIGH$
INIT12	EQU	$-1
	LD	(MAXMEM),HL
	LD	HL,(CMDPTR)	;Command line pointer
	LD	DE,BUF
	$SVC	@FSPEC		;parse filename
	JR	NZ,INIT13
	LD	(CMDTRM),HL	;Filespec terminating char pointer
	JR	INIT14

;-----------------------------------------------------------------------------
;	Read Operating System Parameters (memsiz etc.)
;
; ## GWINIT.ASM:170 ##
;
;	THE FOLLOWING CODE SCANS A CP/M COMMAND LINE FOR BASIC.
;	THE FORMAT OF THE COMMAND IS:
;
;	  BASIC <FILE NAME> [( [M[EMORY]=<TOPMEM>] [ [,] F[ILES]=<FILES>] )]
;
;	THE FOLLOWING PARAMETERS ARE RECOGNIZED:
;
;       	MEMORY=<TOPMEM>
;       	FILES=<FILES>
;
INIT13	LD	HL,(CMDPTR)	;Command line pointer
INIT14	LD	DE,PARAMS
	$SVC	@PARAM		;parse parameters
	JP	NZ,BADPAR
	LD	A,(FFILES)
	OR	A
	JR	Z,INIT15
	AND	80H
	JP	Z,BADPAR	;FUNCTION CALL ERROR
	LD	HL,(PFILES)
	LD	DE,0010H	;MUST BE .LT. 16
	CALL	COMPAR
	JP	NC,BADPAR
	LD	A,L
	LD	(NFILES),A	;HIGHEST FILE NUMBER ALLOWED
INIT15	LD	HL,(MAXMEM)
	LD	A,(FMEMORY)
	OR	A
	JR	Z,INIT16
	AND	80H
	JP	Z,BADPAR
	LD	DE,(PMEMORY)
	CALL	COMPAR
	JP	C,BADPAR
	EX	DE,HL
INIT16	CALL	CKDOSB
	JP	NC,BADPAR
	EX	DE,HL		;GET CURRENT MEMSIZ IN (DE)
	LD	HL,BASTXT	;BASIC text start
	LD	(TXTTAB),HL	;SAVE BOTTOM OF MEMORY
	LD	A,E		;CALC TOTAL FREE/8
	SUB	L
	LD	L,A
	LD	A,D
	SBC	A,H
	LD	H,A
	JR	C,BADPAR	;FUNCTION CALL ERROR
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
	JR	C,SMLSTK
	LD	HL,0200H
SMLSTK	LD	A,E		;SUBTRACT STACK SIZE FROM TOP MEM
	SUB	L
	LD	L,A
	LD	A,D
	SBC	A,H
	LD	H,A
	LD	(MEMSIZ),HL	;Save lowest legal value for [SP]
	LD	(FRETOP),HL	;REASON USES THIS...
	EX	DE,HL
	LD	(FILTAB),HL	;Initially there are no FDB's
	LD	SP,HL		;SET UP NEW STACK
	LD	(SAVSTK),HL
	LD	HL,(TXTTAB)
	EX	DE,HL
	CALL	REASON
	CALL	INIFDB		;Init FDBs
	$SVC	@FLAGS		;get system flags
	PUSH	IY
	POP	HL
	INC	HL
	INC	HL
	LD	(CFLAGS),HL	;Ptr to DOS CFLAGS
	DI
	LD	A,(HL)
	OR	01H
	LD	(HL),A
	EI
	LD	BC,0008H
	ADD	HL,BC
	LD	(KFLAGS),HL	;Ptr to DOS KFLAGS
	LD	C,08H
	ADD	HL,BC
	LD	(SFLAGS),HL	;Ptr to DOS SFLAGS
	LD	HL,BRKVEC	;Null $BREAK vector
	$SVC	@BREAK		;get or set <BREAK> vector
	CALL	CLS
	CALL	RSKBRK		;Clear BREAK bit in KFLAG$
	LD	HL,HEDING	;GET HEADING ("BASIC VERSION...")
	CALL	STROUT
	LD	HL,STROUT
	LD	(REPINI),HL	;Replace INIT with STROUT after init...
	LD	HL,0FFFFH
	LD	(CURLIN),HL
	IF	GFX
	CALL	GINIT		;<==>	Init graphics
	ENDIF
	JP	INITSA

BADPAR	LD	HL,MBADPAR
	CALL	STROUT
	$SVC	@ABORT		;abort program execution

;	DATA & MESSAGES
MBADPAR	DB	'Bad Command Line Parameter(s)',0DH,00H
PARAMS	DB	80H
	DB	95H
	DB	'FILES'
FFILES	DB	00H
	DW	PFILES
	DB	96H
	DB	'MEMORY'
FMEMORY	DB	00H
	DW	PMEMORY
	DB	00H
PFILES	DW	0000H
PMEMORY	DW	0000H
	IF	GFX
HEDING	DB	'BASICG 01.01.00 for TRSDOS Version 6',0DH
	ELSE
HEDING	DB	'BASIC 01.01.00 for TRSDOS Version 6',0DH
	ENDIF
	DB	'Copyright (c) 1984 By Microsoft, licensed to Tandy Corporation.',0DH
	DB	'All rights reserved.'
	IF	TEST
	DB	' ** TEST BUILD **'
	ENDIF
	DB	0DH,0DH,00H

;	Flag to prevent recursive invocation
;	of BASIC via SYSTEM("BASIC")
BASFLG	DB	00H
	IF	GFX
	DB	'13-Feb-84',00H
	ELSE
	DB	'24-Jan-84',00H
	ENDIF
	DC	190,0		;Stack space

TSTACK	DB	30H
;	=TSTACK+1
	IF	GFX
TSTACK1	DC	77,0
	ELSE
TSTACK1	DC	31,0
	ENDIF

	END	INIT

	IF	GFX
	ELSE
	ENDIF
