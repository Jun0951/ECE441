 *-----------------------------------------------------------
* Title      : ECE441 Design Project Part 1
* Written by : Guojun Yang
* Date       : JAN 03 2019
* Description:
*-----------------------------------------------------------
    ORG    $1000
START:      
    LEA MSG_WELCOME,A5                     ;LEA load effective address
    LEA MSG_WELCOME_ED,A6
    BSR PRINT                           ; at the beginning 
RESET:
    LEA HISTORY_BUFF,A2                 ;buff size 256
    MOVE.B #0,(A2)                      ; set 0 to the address whose value stored in A2
    LEA STACK,A7                        ;SET(RESET) A7           A7 is stack pointer
MAIN:    
    LEA MSG_PROM, A5                    ; display "ece441 monitor>"
    LEA MSG_PROM_ED, A6
    BSR PRINT_C                         ; wait for input
    BSR INPUT         
    BSR INTERPRETER
    BRA MAIN

*------------------SUBROUTINE------------*    
;Standard I/O
;Print WITH carrige return and line feed
;Print string stored within address range marked by A5 and A6
PRINT:
    MOVEM.L D0-D1/A1,-(A7)    ;move quad word into address reg  D0-D1/A1 -> A7  stack decrement
    MOVEA.L A5,A1             ;move A5 to A1
    SUBA.L A5,A6              ; SUBA = sub address ==> A6-A5->A6
    MOVE.L A6,D1              ; put the length of the string into D1
    MOVE.L #0,D0              ;trap task 0 displays string at (A1), D1.W bytes long with carriage return and LF
    TRAP #15
    MOVEM.L (A7)+,D0-D1/A1    ;pop out
    RTS                       ;RTS  return from subroutine
    
;Print WITHOUT carrige return and line feed
;Print string stored within address range marked by A5 and A6
PRINT_C:            
    MOVEM.L D0-D1/A1,-(A7)
    MOVEA.L A5,A1           
    SUBA.L A5,A6                ;A5-A6->A6 get the length of the string
    MOVE.L A6,D1                ;put the length into D1 for print
    MOVE.L #1,D0                ;trap task 1 display string at (A1) with D1.W bytes long, without CR and LF
    TRAP #15
    MOVEM.L (A7)+,D0-D1/A1      ;address stored in A7 increment
    RTS                         ;return from subroutine, terminate a subroutin
;Store input string to buffer
;Marked
INPUT:
    MOVEM.L D0-D1/A1,-(A7)
    LEA INPUT_BUFF,A1       ;EVERY WORDS COMES FROM KEYBOARD WILL BE STORED AT BUFF AREA
    MOVE.L #2,D0            ;trap task 2 read a string from keyboard and store in (A1),length retuned in D1.W(max80)
    TRAP #15                ;trap 15 is used for I/O, put the task no.to D0 to display unsigned number in D1.L
    ADDA.W D1,A1            ;D1 is the length of word in (A1)
    MOVE.B #NULL,(A1)       ;indirect address, put "NULL" at the end of the string
    CMPI.B #0,D1
    MOVEM.L (A7)+,D0-D1/A1
    RTS

;Determine which command been input and branch accordingly 
INTERPRETER:    
    ;Check if input buffer is empty
    LEA INPUT_BUFF,A1
    CMPI.B #NULL,(A1)           ;compare immediate if input_buff == NULL
    BEQ INTERPRETER_END
 
    ;Check if it's HELP command
    LEA MSG_CMD_HELP,A5
    LEA MSG_CMD_HELP_ED,A6
    LEA INPUT_BUFF,A1
    BSR COMPARE                     ;BSR  branch to subroutine compare, get to the next leve
    BEQ HELP   
    
    ;Block search
    LEA MSG_CMD_BSCH,A5
    LEA MSG_CMD_BSCH_ED,A6
    LEA INPUT_BUFF,A1
    BSR COMPARE
    BEQ BSCH

    ;Sort by word
    LEA MSG_CMD_SORTW,A5
    LEA MSG_CMD_SORTW_ED,A6
    LEA INPUT_BUFF,A1
    BSR COMPARE
    BEQ SORTW

    ;And
    *-------------------------------ADD YOUR CODE HERE START FROM HERE-------------------------------* 
    LEA MSG_CMD_AND,A5
    LEA MSG_CMD_AND_ED,A6
    LEA INPUT_BUFF,A1
    BSR COMPARE
    *--------------------------------------YOUR CODE STOPS HERE--------------------------------------* 
    BEQ AND

    ;Or
    *-------------------------------ADD YOUR CODE HERE START FROM HERE-------------------------------* 
    LEA MSG_CMD_OR,A5
    LEA MSG_CMD_OR_ED,A6
    LEA INPUT_BUFF,A1
    BSR COMPARE
    *--------------------------------------YOUR CODE STOPS HERE--------------------------------------* 
    BEQ OR
    
    ;Memory sort by word
    LEA MSG_CMD_SORTW,A5
    LEA MSG_CMD_SORTW_ED,A6
    LEA INPUT_BUFF,A1
    BSR COMPARE
    BEQ SORTW

    ;Recall Last command
    ;Courtesy of your TA make your debug slightly less painful
    LEA MSG_CMD_RC,A5
    LEA MSG_CMD_RC_ED,A6
    LEA INPUT_BUFF,A1
    BSR COMPARE
    BEQ RC

    ;Invalid input
    BSR SYNTAX_ER
INTERPRETER_END:    
    RTS

;Compare string from A1 to NULL or SPACE 
;With string from A5 to A6
COMPARE:                    
    CMPI.B #SPACE,(A1)      ;Check if end of command(SPACE)
    BEQ COMPARE_CHK_END
    CMPI.B #NULL, (A1)
    BEQ COMPARE_CHK_END     ;Check if end of command(NULL)
    CMPI.B #DOT, (A1)
    BEQ COMPARE_CHK_END
    CMPA.L A5,A6            ;A5 the start, A6 the end; A5==A6 means the string is over
    BEQ COMPARE_END
    CMP.B (A1)+,(A5)+       ;A1 contains the input msg, A5 has th target msg, 
    BNE COMPARE_END         ;If content not the same end
    BRA COMPARE             ;else cmp the next byte
COMPARE_CHK_END:
    CMPA.L A5,A6            ;CMPA compare address
COMPARE_END:
    RTS

; Convert HEX num (D0) to ASCII, length in D2 (Bytes) before use
; ASCII string buffer should start from A6
HEX2ASCII:
    MOVEM.L D1/D3, -(A7)
    ROL.L #1,D2                ; rotate to left
    ADD.L D2, A6
    MOVE.L D2,D3
HEX2ASCII_LOOP:
    MOVE.L D0, D1
    ANDI.L #$0000000F,D1	;Get last digit (4 bits)
    CMPI.B #10,D1			;Compare with 10
    BGE CONVERT_HEX         ;branch on greater than or equal  if D1 >= 10, go to branch
    ADDI.B #$30,D1			;Convert to 0-9
    JMP STORE_ASCII
CONVERT_HEX:
    ADDI.B #$37,D1			;Convert to A-F
STORE_ASCII:
    MOVE.B D1, -(A6)		;Store to Stack(self defined stack)
    ROR.L #4,D0
    SUBI.B #1, D2			;Count to 0
    BNE HEX2ASCII_LOOP
    ADD.L D3,A6
    MOVEM.L (A7)+, D1/D3
    RTS
    
; Convert ASCII(START FROM A1) to HEX num (Will be storeed in D0) 
; length will be stored in D2
ASCII2HEX:
    MOVEM.L D1,-(A7)
    CLR.L   D2                  ;set to 0
    CLR.L   D1
    CLR.L   D0
    CMPI.B #DOLLAR, (A1)    ;Get rid of $ first
    BNE ASCII2HEX_LOOP
    ADDA #1,A1
ASCII2HEX_LOOP:
    MOVE.B (A1)+,D1
    CMPI.B #$30,D1     ;0-9
    BLT SYNTAX_ER
    CMPI.B #$39,D1
    BLE ASCII2HEX_NUM 
    CMPI.B #F_ASC,D1  ;A-F
    BGT SYNTAX_ER
    CMPI.B #A_ASC,D1
    BLT SYNTAX_ER
    SUBI.B #$37,D1                  ;letter A-F to number A-F
    BRA ASCII2HEX_SUM
ASCII2HEX_NUM    
    SUBI.B #$30,D1
ASCII2HEX_SUM
    ROL.L #4,D0    
    ADD.L D1, D0
    ADDQ.B #1,D2
    CMPI.B #SPACE,(A1)  ;If next char is SPACE or NULL end sub routine
    BEQ ASCII2HEX_END   
    CMPI.B #NULL,(A1)
    BEQ ASCII2HEX_END
    BRA ASCII2HEX_LOOP
ASCII2HEX_END
    ADDQ #1,D2          ;CONVERT DIGIT TO BYTE
    BCLR #0,D2          ;addq and bclk makes sure D2 is even num
    ROR.L #1,D2    
    MOVEM.L (A7)+,D1
    RTS

;Store input buffer
STORE_HIS:
    MOVEM.L A1/A2,-(A7)
    LEA INPUT_BUFF,A1
    LEA HISTORY_BUFF,A2
STORE_HIST_LOOP:
    CMPI.B #0,(A1)
    BEQ STORE_HIS_END
    MOVE.B (A1)+,(A2)+
    BRA STORE_HIST_LOOP          ; move input buffer into history buffer
STORE_HIS_END:
    MOVE.B #0,(A2)               ; put 0 at the end of string in history buffer, end the string
    MOVEM.L (A7)+,A1/A2
    RTS  
  
*----------------COMMAND SUBROUTINE----------------------*
;Help
;Print help messages for each individual debugger command
HELP:
    BSR STORE_HIS 
    LEA MSG_HELP,A5            
    LEA MSG_HELP_ED,A6  
    BSR PRINT 
    LEA MSG_CMD_FST,A5
    LEA MSG_CMD_LST,A6      
    BSR PRINT 
HELP_LOOP:
    ;Print help console prompt
    LEA MSG_CMD_HELP,A5
    LEA MSG_CMD_HELP_ED,A6
    MOVE.B #LARGER,(A6)+
    BSR PRINT_C
    
    ;User input command to be displayed
    BSR INPUT
    LEA INPUT_BUFF,A1

    ;Check if buffer is empty
    CMPI.B #NULL,(A1)
    BEQ HELP_LOOP
    
    ;Sort memory
    LEA MSG_CMD_SORTW,A5
    LEA MSG_CMD_SORTW_ED,A6
    LEA INPUT_BUFF,A1
    BSR COMPARE
    BEQ HELP_SORTW
    
    ;Block search
    LEA MSG_CMD_BSCH,A5
    LEA MSG_CMD_BSCH_ED,A6
    LEA INPUT_BUFF,A1
    BSR COMPARE
    BEQ HELP_BSCH
    
    ;And
    *-------------------------------ADD YOUR CODE HERE START FROM HERE-------------------------------* 
    LEA MSG_CMD_AND,A5
    LEA MSG_CMD_AND_ED,A6
    LEA INPUT_BUFF,A1
    BSR COMPARE
    *--------------------------------------YOUR CODE STOPS HERE--------------------------------------* 
    BEQ HELP_AND

    
    ;Or
    *-------------------------------ADD YOUR CODE HERE START FROM HERE-------------------------------* 
    LEA MSG_CMD_OR,A5
    LEA MSG_CMD_OR_ED,A6
    LEA INPUT_BUFF,A1
    BSR COMPARE
    *--------------------------------------YOUR CODE STOPS HERE--------------------------------------* 
    BEQ HELP_OR

    
    ;Recall last command
    LEA MSG_CMD_RC,A5
    LEA MSG_CMD_RC_ED,A6
    LEA INPUT_BUFF,A1
    BSR COMPARE
    BEQ HELP_RC
    
    ;Exit help console
    LEA INPUT_BUFF,A1
    CMPI.B #Q_ASC,(A1)
    BEQ HELP_EXIT
    BRA HELP
HELP_EXIT:
    RTS

HELP_SORTW:
    LEA MSG_HELP_SORTW,A5
    LEA MSG_HELP_SORTW_ED,A6
    BSR PRINT
    BRA HELP_LOOP
HELP_BSCH:
    LEA MSG_HELP_BSCH,A5
    LEA MSG_HELP_BSCH_ED,A6
    BSR PRINT
    BRA HELP_LOOP
HELP_AND:
    *-------------------------------ADD YOUR CODE HERE START FROM HERE-------------------------------* 
    LEA MSG_HELP_AND,A5
    LEA MSG_HELP_AND_ED,A6
    BSR PRINT
    *--------------------------------------YOUR CODE STOPS HERE--------------------------------------* 
    BRA HELP_LOOP
HELP_OR:
    *-------------------------------ADD YOUR CODE HERE START FROM HERE-------------------------------* 
    LEA MSG_HELP_OR,A5
    LEA MSG_HELP_OR_ED,A6
    BSR PRINT
    *--------------------------------------YOUR CODE STOPS HERE--------------------------------------* 
    BRA HELP_LOOP
HELP_RC:
    LEA MSG_HELP_RC,A5
    LEA MSG_HELP_RC_ED,A6
    BSR PRINT
    BRA HELP_LOOP


;Sort word
;Sort content as HEX in memory
SORTW:
    *-------------------------------ADD YOUR CODE HERE START FROM HERE-------------------------------* 
    BSR STORE_HIS
    CMPI.B #SPACE,(A1)+ ;Get rid of the space after command
    BNE SYNTAX_ER       
    BSR ASCII2HEX               ;convert ASCII to hex, and store it in D0
    MOVE.L D0,A3                ;Parse the first address
    CMPI.B #SPACE,(A1)+ 
    BNE SYNTAX_ER       
    BSR ASCII2HEX
    MOVE.L D0,A4                ;Parse the second address

    BSR LOAD_S                  ;load the string into output buffer, and operate in output buffer
    CMPA.L A4,A3                ;Check if the first address is smalller
    BGE SYNTAX_ER               ;if A3 is greater than or equal to A4, go to error
    CMPI.B #SPACE,(A1)+
    BNE SYNTAX_ER
    MOVE.L A1,-(A7)
    CMPI.B #$41,(A1)
    BLT SYNTAX_ER
    CMPI.B #$44,(A1)+
    BGT SYNTAX_ER
    CMPI.B #NULL,(A1)         ;input command must end with A-D followed by NULL
    BNE SYNTAX_ER
    MOVE.L (A7)+,A1
    CMPI.B #$41,(A1)
    BEQ order_A
    CMPI.B #$44,(A1)
    BEQ order_D
    BSR SYNTAX_ER

LOAD_S:
    MOVEM.L A3,-(A7)        ;put the traget string into output buffer
    LEA OUTPUT_BUFF,A2
LOAD_LOOP:
    CMPI.B #0,(A3)
    BLE NEXT
    MOVE.B (A3)+,(A2)+
    CMPA A4,A3
    BLE LOAD_LOOP
    MOVEM.L (A7)+,A3
    RTS
NEXT:
    ADDA.L #1,A3
    CMPA A4,A3
    BLE LOAD_LOOP
    MOVEM.L (A7)+,A3
    RTS        

order_A:                        ;sort in ascending
    MOVEM.L A3-A4,-(A7)
    SUBA.L #1,A2
    LEA OUTPUT_BUFF,A3      ;A3 is set to the start of output buffer
    MOVEA.L A2,A4           ;A4 is set to the end of output buffer
SORT:
    MOVE.L A3,A2

CMP_LOOP: 
    ADDA.L #1,A2                    ;compare the item with all the items between A2 and A4 -- inner loop
    CMPA.L A4,A2
    BGT SORTW_DONE 
    MOVE.L A2,-(A7)

INNER_LOOP:
    MOVE.L A2,A6
    SUBA.L #1,A6
    CMPA.L A3,A6
    BLT NEXT_LOOP
    MOVE.B (A6),D6
    MOVE.B (A2),D5   
    CMP.B D6,D5
    BGE NEXT_LOOP
    MOVE.B D6,(A2)
    MOVE.B D5,(A6)  
    SUBA.L #1,A2
    BRA INNER_LOOP

NEXT_LOOP:
    MOVEM.L (A7)+,A2
    BRA CMP_LOOP

order_D:                    ;sort in descending
    MOVEM.L A3-A4,-(A7)
    SUBA.L #1,A2
    LEA OUTPUT_BUFF,A3
    MOVEA.L A2,A4

SORT1:
    MOVE.L A3,A2

CMP_LOOP1:
    ADDA.L #1,A2                    ;compare the item with all the items between A2 and A4 -- inner loop
    CMPA.L A4,A2
    BGT SORTW_DONE 
    MOVE.L A2,-(A7)
INNER_LOOP1:
    MOVE.L A2,A6
    SUBA.L #1,A6
    CMPA.L A3,A6
    BLT NEXT_LOOP1
    MOVE.B (A6),D6
    MOVE.B (A2),D5   
    CMP.B D6,D5
    BLE NEXT_LOOP1
    MOVE.B D6,(A2)
    MOVE.B D5,(A6)  
    SUBA.L #1,A2
    BRA INNER_LOOP1
NEXT_LOOP1:
    MOVEM.L (A7)+,A2
    BRA CMP_LOOP1

SORTW_DONE:
    LEA MSG_SORTW_SUCCESS,A5
    LEA MSG_SORTW_SUCCESS_ED,A6
    BSR PRINT_C
    MOVEA.L A4,A6
    LEA OUTPUT_BUFF,A5
    BSR PRINT
    MOVEM.L (A7)+,A3-A4
    *--------------------------------------YOUR CODE STOPS HERE--------------------------------------*
    RTS    

;Block search
BSCH:
    BSR STORE_HIS
    CMPI.B #SPACE,(A1)+ ;Get rid of the space after command
    BNE SYNTAX_ER       
    BSR ASCII2HEX               ;convert ASCII to hex, and store it in D0
    MOVE.L D0,A3                ;Parse the first address
    CMPI.B #SPACE,(A1)+ 
    BNE SYNTAX_ER       
    BSR ASCII2HEX
    MOVE.L D0,A4        ;Parse the second address
    CMPA.L A4,A3        ;Check if the first address is smalller
    BGE SYNTAX_ER       ;if A3 is greater than or equal to A4, go to error
    CMPI.B #SPACE,(A1)+ ;must end with Space
    BNE SYNTAX_ER
    MOVE.L A1,-(A7)
BSCH_LOOP_1:
    MOVE.L (A7),A1
    CMPA.L A3,A4
    BEQ BSCH_FAILD    
    CMPM.B (A3)+,(A1)+
    BNE BSCH_LOOP_1
    MOVE.L A3,D0
    SUBQ #1,D0
BSCH_LOOP_2:
    CMPA.L A3,A4
    BLT BSCH_FAILD 
    CMPM.B (A3)+,(A1)+
    BEQ BSCH_LOOP_2
    CMPI.B #0,-1(A1)
    BEQ BSCH_SUCCESS
    CMPA.L A3,A4
    BEQ BSCH_FAILD
    MOVE.L (A7),A1
    BRA BSCH_LOOP_1
BSCH_FAILD:
    LEA MSG_BSCH_FAILD,A5
    LEA MSG_BSCH_FAILD_ED,A6
    BSR PRINT
    BRA BSCH_END    
BSCH_SUCCESS:
    LEA MSG_BSCH_SUCCESS,A5
    LEA MSG_BSCH_SUCCESS_ED,A6
    BSR PRINT_C
    LEA OUTPUT_BUFF,A6
    BSR HEX2ASCII
    LEA OUTPUT_BUFF,A5
    BSR PRINT
    BRA BSCH_END
BSCH_END:
    ADDQ #4,A7    
    RTS

;Arithmatic and 
;And two hex number bitwise and display the result
AND:
    *-------------------------------ADD YOUR CODE HERE START FROM HERE-------------------------------* 
    MOVE.L D6,-(A7)
    BSR Store_HIS
    CMPI.B #SPACE,(A1)+
    BNE SYNTAX_ER
    BSR ASCII2HEX
    MOVE.L D2,D6
    MOVE.L D0,D4
    CMPI.B #SPACE,(A1)+
    BNE SYNTAX_ER
    BSR ASCII2HEX
    BSR DOUBLE
    MOVE.L D0,D5
    AND.L D4,D5
    MOVE.L D5,D0
    MOVE.L (A7)+,D6
    BRA Logic_SUCCESS
    *--------------------------------------YOUR CODE STOPS HERE--------------------------------------*   
    RTS

;Arithmatic or 
;Or two hex number bitwise and display the result
OR:
    *-------------------------------ADD YOUR CODE HERE START FROM HERE-------------------------------* 
    MOVE.L D6,-(A7)
    BSR Store_HIS
    CMPI.B #SPACE,(A1)+
    BNE SYNTAX_ER
    BSR ASCII2HEX
    MOVE.L D2,D6
    MOVE.L D0,D4
    CMPI.B #SPACE,(A1)+
    BNE SYNTAX_ER
    BSR ASCII2HEX
    BSR DOUBLE
    MOVE.L D0,D5
    OR.L D4,D5
    MOVE.L D5,D0
    MOVE.L (A7)+,D6
    BRA Logic_SUCCESS

DOUBLE:                     ;double the length of output string
    CMP.B D2,D6
    BGE DO
    ADD.B D2,D2
    RTS
DO:
    MOVE.B D6,D2
    ADD.B D2,D2
    RTS
    *--------------------------------------Print the Logic operation results--------------------------* 
Logic_SUCCESS:
    LEA MSG_Logic_SUCCESS,A5
    LEA MSG_Logic_SUCCESS_ED,A6
    BSR PRINT_C
    LEA OUTPUT_BUFF,A6
    BSR HEX2ASCII
    LEA OUTPUT_BUFF,A5
    BSR Print
    BRA Logic_END
Logic_END:
    *--------------------------------------YOUR CODE STOPS HERE--------------------------------------* 
    RTS


;Recall last command    
RC:
    LEA HISTORY_BUFF,A2
    CMPI.B #0,(A2)  ;Check if history buffer is empty
    BNE RC_PROCESS
    LEA MSG_HIS_EMPTY, A5
    LEA MSG_HIS_EMPTY_ED, A6
    BSR PRINT
    RTS
RC_PROCESS:
    LEA INPUT_BUFF, A1
RC_LOOP:    
    CMPI.B #0,(A2)
    MOVE.B (A2)+,(A1)+
    BNE RC_LOOP
RC_END:    
    MOVE A1,A6
    LEA INPUT_BUFF,A5
    BSR PRINT
    BRA INTERPRETER ;Return to interpreter   
    
;Syntax error
;Throw an message indicating syntax error
SYNTAX_ER:
    LEA MSG_WRONG_CMD,A5
    LEA MSG_WRONG_CMD_ED,A6         
    BSR PRINT                       ;print "wrong command"
    LEA MSG_HELP_PROM,A5
    LEA MSG_HELP_PROM_ED,A6
    BSR PRINT                       ;print "hint for help"
    BRA MAIN

* Put program code here

    SIMHALT             ; halt simulator

* Put variables and constants here

*----------------COMMON SYMBOLS--------------------------*
BUFFSIZE    EQU     $256    ;BUFF SIZE IS EQUAL TO $256
SPACE       EQU	    $20     ;SPACE IN ASCII
CR          EQU     $0D     ;CARRIGE RETURN IN ASCII
LF          EQU     $0A     ;LINE FEED IN ASCII
NULL        EQU     $00     ;NULL
COLON	    EQU     $3A     ; : IN ASCII
COMMA	    EQU	    $2C     ; , IN ASCII
DOT	        EQU	    $2E     ; . IN ASCII
DOLLAR	    EQU	    $24     ; $ IN ASCII
A_ASC	    EQU	    'A'
B_ASC	    EQU	    'B'
D_ASC	    EQU	    'D'
F_ASC       EQU     'F'
G_ASC	    EQU	    'G'
H_ASC	    EQU	    'H'
L_ASC       EQU     'L'
M_ASC	    EQU	    'M'
Q_ASC		EQU		'Q'
S_ASC	    EQU	    'S'
W_ASC       EQU     'W'
LARGER      EQU     '>'


*---------------COMMAND MESSAGES------------------*
MSG_CMD_HELP    DC.B    'HELP'
MSG_CMD_HELP_ED:   
	DC.B	CR,LF 
MSG_CMD_FST:
MSG_CMD_SORTW   DC.B    'SORTW'
MSG_CMD_SORTW_ED:
	DC.B	CR,LF
MSG_CMD_BSCH    DC.B    'BSCH'
MSG_CMD_BSCH_ED:
	DC.B	CR,LF
MSG_CMD_AND     DC.B    'AND'
MSG_CMD_AND_ED:
	DC.B	CR,LF
MSG_CMD_OR     DC.B    'OR'
MSG_CMD_OR_ED:
	DC.B	CR,LF
MSG_CMD_RC     DC.B    '<'
MSG_CMD_RC_ED:
MSG_CMD_LST:

*----------------COMMON MESSAGES------------------*
MSG_WELCOME:
    DC.B 'System initiallied. Please input command.' 
MSG_WELCOME_ED
MSG_PROM:
    DC.B 'ECE441 MONITOR>'
MSG_PROM_ED
MSG_HELP:
    DC.B	'Enter the command you need help.'
    DC.B 	CR,LF,'Press "Q" to exit.'
MSG_HELP_ED
MSG_SORTW_SUCCESS:
    DC.B 'The SORTW results: '
MSG_SORTW_SUCCESS_ED
MSG_BSCH_SUCCESS:
    DC.B 'Content found at: $'
MSG_BSCH_SUCCESS_ED
MSG_BSCH_FAILD:
    DC.B 'Content not found'
MSG_BSCH_FAILD_ED
MSG_Logic_SUCCESS:
    DC.B 'Result of this logic operation is $'
MSG_Logic_SUCCESS_ED  
MSG_WRONG_CMD:
    DC.B 'Wrong command'
MSG_WRONG_CMD_ED
MSG_HELP_PROM:
    DC.B	'Enter "HELP" for help info.'
MSG_HELP_PROM_ED
MSG_FINISH:
    DC.B 'Finshed'
MSG_FINISH_ED
MSG_HIS_EMPTY:
    DC.B 'No command to recall.'
MSG_HIS_EMPTY_ED

*--------------HELP MESSAGES----------------------*
;Sort word
MSG_HELP_SORTW:
    DC.B 	'The memory sort function can sort the',CR,LF 
    DC.B	'content (word)in given address range. "A"',CR,LF
    DC.B	'for ascending "D" for descending.',CR,LF 
    DC.B	'of memory will be displayed.',CR,LF
    DC.B	'SORTW <addr1> <addr2> M'
MSG_HELP_SORTW_ED

;Block search
MSG_HELP_BSCH:
    DC.B	'Search for specific pattern (input as string) within',CR,LF 
    DC.B	'a memory range. If found, pint the location of such'
    DC.B	'string, if not found print failed promotion.',CR,LF 
    DC.B	'BSCH <addr1> <addr2> string'
MSG_HELP_BSCH_ED

;And
MSG_HELP_AND:
    DC.B    'input two hex number then conduct bitwise AND operation and print the result in hex',CR,LF
    DC.B    'AND <addr1> <addr2>'
MSG_HELP_AND_ED

;Or
MSG_HELP_OR:
    DC.B    'input two hex number then conduct bitwise OR operation and print the result in hex',CR,LF
    DC.B    'OR <addr1> <addr2>'
MSG_HELP_OR_ED

;Recall command
MSG_HELP_RC:
    DC.B	'Redo last correct command.',CR,LF   ; MSG_HELP_RC has the memory address of the string
    DC.B	'<'
MSG_HELP_RC_ED:

    ORG $3000
INPUT_BUFF:
    DS.B    BUFFSIZE   ; define space for input buff = 256
OUTPUT_BUFF:
    DS.B    BUFFSIZE
HISTORY_BUFF:
    DS.B    BUFFSIZE    
ADDR_BUFF:
    DS.B    8
    ORG $4000
    DS.B    1024    ;RESERVE 1KB FOR A7 STACK
STACK:    
    DS.L    20      ;RESERVE 40 BYTES FOR
REGISTER_STACK: 
    DS.L    1
REGISTER_PC:   
    DS.L    1
    END    START        
    





















*~Font name~Courier New~
*~Font size~10~
*~Tab type~1~
*~Tab size~4~
