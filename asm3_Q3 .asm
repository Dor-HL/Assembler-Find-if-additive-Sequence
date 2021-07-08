
INCLUDE Irvine32.inc
INCLUDE asm3_Q3_data.inc

;What does the program do? This program recives a string and it's size, and checks if it is an additive sequence or not.
;if not - it will print "false", if it is an additive sequence it will print the numbers that form the given sequence .

;registers used: above every function is an explanation of what the function does and whar registers were used and for what.
;in main: edx was used to print strings, and the registers esi, edx and edi were used to send parameters to the main function
;of this prigram: "isAddSeq". 

.data
Input BYTE "Input: s = " , 0
Output BYTE "Output: ", 0
falseOutput BYTE "false", 0

.code
main PROC
mov edx, OFFSET INPUT
call WriteString
mov edx, OFFSET num
call writeString


mov esi, OFFSET NUM
mov EDX, N
mov EDI, OFFSET res
push esi
push edx
push edi
call IsAddSeq

 call crlf
 call crlf
mov edx, OFFSET Output
call writeString
mov edx, OFFSET res
cmp BYTE PTR[edx], 0
JZ ReturnedFalse

call writeString
jmp endMain 

ReturnedFalse:
mov edx, OFFSET falseOutput
call WriteString

endMain:


		exit

		main ENDP

;------------------------------------
;function explained: checks if string valid
;registers uded: parameters: esi - len, edi - adress of string. al - boolian result
;adress is 4 bytes to is stored in a full register, len is unkown so also stored in a 4 byte register
IsValid PROC uses ESI EDI ;boolian function, returns true/false in al
	push EBP
	mov EBP, ESP;ADDED USES + 8
	;---------------
	mov esi, [ebp + 20] ;len of string
	mov edi, [ebp + 16] ;adress of string
	cmp esi, 1
	jb notGreater
	cmp BYTE PTR[edi], "0"
	jnz notEquel
	mov al, 0
	jmp endOfFunction

	notGreater:
	notEquel:
	mov al, 1

	endOfFunction:
	;---------------
	mov ESP, EBP
	pop EBP
	ret 8 ;make sure  is 8
	IsValid ENDP

	;----------------------------------------------------------------------------------
	;function explained: this function returns the numeric value of the char recived in the string
	;registers used: parameter list: edx - position, esi - len, edi - adress of string. al holds result
	;adress are stored in a 4 byte register, pos and len are unkown so they also stored in a 4 byte register
	Val PROC uses EDX ESI EDI ;added uses, +12 to each ebp
	push EBP
	mov EBP, ESP
	;--------------
	mov edx, [ebp + 28]  ;position
	mov esi, [ebp + 24]  ;len of string
	mov edi, [ebp + 20]   ;adress of string

	dec esi ;len - 1
	cmp edx, esi
	JBE posInRnge
	mov al, 0
	jmp endOfFunction
	posInRnge:
	add edi, edx
	mov al, BYTE PTR[edi]
	sub al, "0"
	endOfFunction: 
	;---------------
	mov ESP, EBP
	pop EBP
	ret 12 ;make sure  is 8
	Val ENDP

	;-------------------------------------
	;function explained: this function recives two strings and returns their sum (as a string)
	;registers used: parmeters: edi - adress of string a, edx - len a, esi -adress of string b, ebx - len b
	;eax- holds adress of res string, al temp for val returned from val func, ecx + edx + eax also used for div and
	;edi also used to hold modelu of div
	;adresses are 4 bytes, len are unkown so they are stored also in a 4 byte register

	AddString PROC
	modeluTemp = -4
	push EBP
	mov EBP, ESP
	;-------------
	mov EAX, [ebp + 24] ;ah is sum
	mov edi, [ebp + 20] ;adress of string a
	mov edx, [ebp + 16]  ;len of string a
	mov esi, [ebp + 12]  ;adress of string b
	mov ebx, [ebp + 8]   ;len of string b
	sub esp, 4
	sub edx, 1  ; to get real size
	sub ebx, 1 ;to get real size
	mov ECX, 0 ;cl is t
	mov ESI, 0;ch is carry
	mov EAX, ESI  ;initalize sum to zero

	lea esi, [EBP + modeluTemp]
	inc esi;now we will put end of string symbol
	mov BYTE PTR[ESI], 0;modelu is of 10 so will always be 1- 9. only need 2 bytes
	mov esi, [ebp + 12]  ;adress of string b
	mov esi, 0
	addingStringLoop:
	mov ECX, 0 ;cl is t
	push edx
	push ebx
	;----------
	push EAX
	push ESI

	push edx ;call val with A
	push [ebp + 16]
	push [ebp + 20]
	call Val
	movzx ESI , al
	add ECX, ESI

	push ebx
	push [ebp + 8]
	push  [ebp + 12]
	call Val
	movzx ESI, al
	add ECX, ESI ;t=val+ val
	pop ESI

	pop EAX

	add ECX, ESI ;add carry to t

	;push EDX
	push EAX
	mov EDX, 0
	mov EAX, ECX ;moves t to EAX
	mov ECX, 10
	div ECX 
	mov ESI, EAX ; move carry to SI (t/10)
	pop EDI ;holds value of sum
	push EDI
	push ESI
	mov esi, [ebp + 24] ;puts sum in esi to send to function of pushback
	lea EDI,[ebp + modeluTemp] 
	;mov al, BYTE PTR EDX
	mov  [EDI], edx
	mov al, BYTE PTR[EDI]
	add al, '0'
	mov BYTE PTR[EDI], al
 
	call PushBack
	pop ESI
	pop EDI


	;add EDI, EDX ;adds modelu to sum
	;mov EAX, EDI
	mov EAX, [ebp + 24]
	;add EAX, "0" ;change to PUSH BACK
	pop EBX
	pop EDX
	sub edx, 1
	sub ebx, 1

	cmp edx, 0
	JGE addingStringLoop
	cmp ebx, 0
	JGE addingStringLoop

	lessThenZero:
	cmp ESI, 0
	JZ endOfFunction
	

	endOfFunction:
	cmp ESI, 0
	JZ isZero
	mov edx, ESI
	mov ESI, [ebp + 24]
	lea EDI, [ebp + modeluTemp]
	mov  [EDI], edx
	add EDX, '0'
	mov [EDI], EDX
	call PushBack

	isZero:
	mov ESI, [ebp + 24]
	call GetStringSize
	movzx ecx, ah
	mov ESI, [ebp + 24]
	call reverse_string

	add esp, 4
	;-------------
	mov ESP, EBP
	pop EBP
	ret 20 ;make sure  is 8
	AddString ENDP

	reverse_string PROC USES ECX ESI EBX EDX
	mov edx, ecx	; save ECX for the second loop
push_loop:
    ; we can push either word or dword so we
    ; copy the byte and extend it to a word
    mov bl, [esi]
    push bx ; we push also MSB = bh, but we ignore it
	inc esi
    loop push_loop
    
    ; now we pop all the values, in the same
    ; order (i.e. the array from end to start)
    ; ... but pop-push is actually reversing
	
    mov ecx, edx		; restore the counter
	sub esi, ecx ; restore ESI
pop_loop:
    pop bx
    mov [esi],bl
	inc esi
    loop pop_loop 
	ret
reverse_string ENDP

;---------------------------------------------------
;function explained: this function attaches a string to the end of another string (string b to the end of string a)
;registers used: parameters: esi - string a adress, edi - string b adress. al, ah holds sized of strings. cl holds current char
;of string b and ebx is used as temp for size
;edi, esi holds adresses so they are 4 bytes each
PushBack PROC USES esi edi
push edi
call GetStringSize
mov al, ah ;saves size of string in esi
push esi
mov esi, edi
call GetStringSize ;now size of string edi is in ah
skipThis:
pop esi
pop edi
movzx ebx, al ;check that works
;dec ebx;for debugging
add esi, ebx
;inc esi
cmp ah, 0
jz emptyBstring
pushBackLoop:
mov cl, BYTE PTR [EDI]
;mov ch, BYTE PTR [ESI] ;for debugging
mov BYTE PTR [ESI], cl
inc ESI
inc EDI
dec ah

jnz pushBackLoop
mov BYTE PTR[ESI], 0 ;end symbol
emptyBstring:
ret
PushBack ENDP
;-----------------------------------------------------

;-----------------------------------------------------
;function explained: this function attaches a string to the start of another string (string b to the start of string a)
;registers used: parameters: esi - string a adress, edi - string b adress. al, ah holds sized of strings. cl holds current char
;ebx holds temp string
;edi, esi holds adresses so they are 4 bytes each
PushFront PROC USES esi edi
push edi
call GetStringSize
mov al, ah ;saves size of string in esi
push esi
mov esi, edi
call GetStringSize ;now size of string edi is in ah
pop esi
pop edi
movzx ebx, ah ;check that works
;dec ebx;for debugging
push esi
push edi
add edi, ebx
;inc esi
cmp ah, 0
jz emptyBstring
pushFrontLoop:
mov cl, BYTE PTR [EsI]
;mov ch, BYTE PTR [ESI] ;for debugging
mov BYTE PTR [EdI], cl
inc ESI
inc EDI
dec al
jnz pushFrontLoop
mov BYTE PTR[EDI], 0
pop edi

mov esi, edi
call GetStringSize
pop esi
transferToAString:
mov cl, BYTE PTR[EDI]
mov BYTE PTR[ESI], cl
inc edi
inc esi
dec ah
jnz transferToAString
mov BYTE PTR[ESI], 0 ;end symbol
emptyBstring:
ret
PushFront ENDP

;-----------------------------------------------------------
;function explained: this function recives adress offset string in esi and returns it's len in ah
;registers used: esi holds the string adress, cl is temp for current charm ah holds res
; esi holds adresses so it is 4 bytes 

GetStringSize PROC uses esi
mov ah, 0
findsize:
mov cl, BYTE PTR[ESI]
cmp BYTE PTR [esi], 0
jz ending
inc ah
inc esi
jmp findsize
ending:
ret
GetStringSize ENDP

;----------------------------------------------------------------------
;function explanation: this functon recives two strings and returns true/ false if they are equal or not
;registers used: parameters: esi- first string adress, edi - second string adress, ch - holds string a size, ah - holds string b size
;dl and dh - temp to hold current charecters in both strings. al returns true/false if strings are equel or not
;edi, esi holds adresses so tehy are 4 bytes each
CmpStr PROC uses esi edi
call GetStringSize
mov ch, ah

push esi
mov esi, edi
call GetStringSize

pop esi

cmp ch, ah
JNZ ifNotEquel

mov al, 1 ;return 1 if not reached ifNotEquel
cmpLoop:
mov dl, BYTE PTR[esi]
mov DH, BYTE PTR[edi]
cmp dl, dh
jnz ifNotEquel
inc ESI
inc EDI
dec ah
jnz cmpLoop

jmp EndOfLoop

ifNotEquel:
mov al, 0 ;not same!

EndOfLoop:

ret
CmpStr ENDP

;----------------------------------------------------------------------------
;function explanation: this function checks every iteration if adress of esi (first location in string) is equel to adress
;of location of pos in string. if not - it will send to the next recurcive iteration esi++ and pos-- and will keep doing so until 
;adress esi == adress pos. if equel - it will put into dest adress the sub string that starts at adress esi and with the required len

;Registers used: parameters: esi-source, ecx-len of source, eax-pos, ebx-len, edi-dest
;al returns true/false if substring found or not. ecx also used to hold adress of pos
;all registers that holds adressed are 4 bytes, pos and len changes according to value (and this is why they are stored in a
;4 byte register) 
SubString PROC USES ESI ECX EAX EBX EDI ;esi - source ecx - len of source eax- pos ebx - len edi - dest
	push EBP
	mov EBP, ESP
	;------------
	mov edi, [ebp + 44];dest adress  now holds source
	mov ecx, [ebp + 40] ;len of source
	mov eax, [ebp + 36] ;pos
	mov ebx, [ebp + 32] ;len
	mov esi, [ebp + 28] ;source adress  origina;: 44 was souece and 28 was dest now holds dest

	mov eax, [ebp + 36] ;eax = pos + len
	add eax, [ebp + 32]
	cmp eax, [ebp + 40] ;maybe -1?
	JG OutOfBoundSubString
	push ecx
	mov eax, [ebp + 36] ;pos
	mov ecx, esi
	add ecx, eax ; now ecx holds adress of pos

	cmp ecx, esi
	JNZ notEqual
	pop ecx 
	push eax;need to pop

	insertSubStringToDes:
	mov al, BYTE PTR[EsI]
	mov BYTE PTR [edi], al
	inc ESI
	inc EDI
	dec EBX
	JNZ insertSubStringToDes
	pop eax
	mov al, 1 ;return true
	jmp EndOfFunction

	notEqual:
    pop ecx
	inc ESI
	dec EAX

	push edi
	push ecx
	push eax
	push ebx
	push esi
	call SubString ;RECURSIVE CALL
	 jmp EndOfFunction

	OutOfBoundSubString:
	mov al, 0 ; return false


	EndOfFunction:

	;-------------
	mov ESP, EBP
	pop EBP
	ret 20 ;make sure  is 8
SubString ENDP

;------------------------------------------------------------------------------------------------
;function explnation: this function looks for a combination of number sizes to see if there is a combination where the given
;sequence is addetive
;Registers used: esi holds adress of original string, edi adress of res string and edx size of original string
;al boolean that returns true/false if the string is addetive or not
;esi and edi both hokds adresses so they are 4 bytes, edx number of bytes changes according to size (1-4 bytes), but is send in a 4 
;byte register to accomidate every possible size
IsAddSeq PROC uses ESI EDI EDX
push EBP
mov EBP, ESP
;-------------
tempAstring = -(2*N)
tempBstring = -(4*N)
tempCstring = -(6*N)
mov ESI, [ebp + 28] ;adress of original str
mov EDX, [EBP + 24] ; len of original str
mov EDI, [EBP + 20] ; res adress

sub esp, N*6 ;temp for 3 sub strings - a,b,c
mov edx, 0
lea esi, [ebp + tempAstring]
mov DWORD PTR [esi], 0
lea esi, [ebp + tempBstring]
mov DWORD PTR [esi], 0
lea esi, [ebp + tempCstring]
mov DWORD PTR [esi], 0

mov eax, [EBP + 24]
dec eax; to get exact size
mov EDX, 0 ;for div
mov ecx, 2
div ecx ;now eax holds l/2
inc eax
;to save l/2
mov edi, 1 ;edx will be counter edx = i
outsideLoop:
push eax;to save val
mov eax, [EBP + 24]
dec eax
sub eax, edi ;l - i
mov EDX, 0 ;for div
mov ecx, 2
div ecx ;now eax holds l-i/2
inc eax
mov ecx, eax
mov ebx, 1 ;ebx counter = j
pop eax
innerLoop:
lea esi, [ebp + tempAstring]
push ecx
push edi
lea edi, [ebp + tempBstring]
mov cl, N*2
emptyTempsLoop:
mov BYTE PTR[ESI], 0
mov BYTE PTR[EDI], 0
inc esi
inc edi
dec cl
jnz emptyTempsLoop
pop edi
pop ecx
mov DWORD PTR [esi], 0
lea esi, [ebp + tempCstring]
mov DWORD PTR [esi], 0
push ecx
	;creating a substring
	lea esi, [ebp + tempAstring]
	push esi;dest for a string
	mov esi,  [EBP + 24]
	push esi; len of string source
	mov esi, 0
	push esi ;pos = 0
	push edi ;len = i
		mov ESI, [ebp + 28]
	push esi; original string
	call SubString ;now esp - N holds a sub string

	;creating b substring
	lea esi, [ebp + tempBstring]; temp for b substring
	push esi
	mov esi,  [EBP + 24]
	push esi; len of string source
	push edi ; pos = i
	push ebx ; len = j
	 mov ESI, [ebp + 28]
	push esi; original string
	call SubString

	push edi
	;creatung c substring
	lea esi, [ebp + tempCstring]; temp for c substring
	push esi
	mov esi,  [EBP + 24]
	push esi;size of original string
	mov esi, ebx ;esi = J
	add esi, edi ;esi = j + i
	push esi ;pos = i + j
	mov edi, [EBP + 24]
	sub edi, esi ;size of original - (i+j) = len
	push edi ;len
	  mov ESI, [ebp + 28]
	push esi; original string
	call SubString
	pop edi

push eax; to save value	
push ebx; to save value
push edi; to save value
;call check adition
	lea esi, [ebp + tempAstring];a subtring
	lea edi, [ebp + tempBstring]; b string
	lea eax, [ebp + tempCstring];c string
	push esi
push edi
push eax
mov ESI, [EBP + 28]
push ESI ; adress of original str
mov ESI, [EBP + 24] 
push ESI ;len of original str
mov ESI, OFFSET res
;------------make res empty
mov cl, N*2+1 
makeEmptyLoop:
mov BYTE PTR[ESI], 0
inc esi
dec cl
jnz makeEmptyLoop
;-----------------------
mov ESI, OFFSET res

push esi ;adress of res
call ChkAddition
pop edi
pop ebx

cmp al, 1
JNZ returendZero
mov esi, OFFSET res
lea edi, [EBP + tempCstring]

mov ah, N
loopMakeEmpty:
mov BYTE PTR[edi], 0
inc edi
dec ah
jnz loopMakeEmpty

lea edi, [EBP + tempCstring]
mov BYTE PTR[EdI], " "
call PushFront
mov esi, OFFSET RES
lea edi, [EBP + tempBstring] ;adress of temp b
call PushFront ;push front of b to res 

mov esi, OFFSET res
lea edi, [EBP + tempCstring]
mov BYTE PTR[EDI], " "
inc edi
mov BYTE PTR[EDI], 0
lea edi, [EBP + tempCstring]
call PushFront
mov esi, OFFSET RES
lea edi, [EBP + tempAstring] ;adress of temp b
call PushFront
jmp endOfFuncSuccesful
returendZero:
pop eax
pop ecx
inc ebx
cmp ebx, ecx
jnz innerLoop

;pop eax ;ecx holds value
inc edi
cmp edi, eax
jnz OutsideLoop

mov esi, OFFSET res ;res.clear()
mov edx, 0
mov DWORD PTR [esi], edx

endOfFuncSuccesful:

add esp, N*6
;---------------
mov ESP, EBP
pop EBP
ret 12 
IsAddSeq ENDP 
;-------------------------------------------------------------------------------------------------------------
;function explanation: this function checks if a string is addetive or not
;registers used: parameters: esi - adress of original str, eax - size of original str, edi - res adress, edx - a string
;ebx - b string, ecx - c string. esi and edi also used to send parameters to inner functions like get size for exmple.
;in this func we aldo allocated memory for local var on the stack (two strings with len N - one for sun and one for temp)
;all parameters were send in full registers and so they take 4 bytes (adresses for exmple). size of string can take less bytes,
;depending on size but is send in a 4 byte register becouse size is unknown 
ChkAddition PROC 
sum1 = -N
temp1 = -(2*N)
push EBP
mov EBP, ESP
;----------------------------
mov EDX, [EBP + 28];a string
mov EBX, [EBP + 24];b string
mov ECX, [ebp + 20];c string
mov ESI, [EBP + 16] ;adress original string
mov EAX, [EBP + 12] ;size of original string
mov EDI, [EBP + 8] ;result

sub ESP, N * 2 ; [ebp - n] is res adress, [ebp - n*2] is temp adress used later in function
mov ah, N

lea esi, [EBP + TEMP1]
lea edi, [EBP + SUM1]
loopMakeEmpty:
mov BYTE PTR [ESI], 0
mov BYTE PTR [EDI], 0
inc esi 
inc edi
dec ah
jnz loopMakeEmpty



mov ESI, [EBP + 28] ; check if a is valid
call GetStringSize
movzx edx, ah
push edx ;len of a
push ESI; adress of a
call IsValid
cmp al, 0
JZ EndOfFunction 

mov ESI, [EBP + 24] ; check if b is valid
call GetStringSize
movzx edx, ah
push edx ;len of a
push ESI; adress of a
call IsValid
cmp al, 0
JZ EndOfFunction 

mov EAX, [EBP + 12] ;size of original string

mov ah, N
lea ESI, [ebp + temp1] ;sum that is allocated in stack adress
lea EDI, [ebp + sum1]
;need to make it all zero so itll be empty/try to check without the add esi N
;--------------------------------------------------------------------------------------------------
;params for addString
lea ESI, [EBP + SUM1]
push ESI; adress of sum 
;sending parameter list to addd string
mov EDX, [EBP + 28]
push EDX ;adress string a
mov ESI, [EBP + 28]
call GetStringSize
movzx EDX, ah
push EDX ;size string a
mov ESI, [EBP + 24]
push ESI ; adress of b
call GetStringSize
movzx ESI, ah
push ESI ; size of b
call AddString

mov edi, [EBP + 20];c string
;esi holds sum 

lea esi, [EBP + SUM1];HHOLDS SUM

call CmpStr

cmp al, 1 ;if c == sum 
JNZ notEqual
mov esi, [ebp + 8] ;adress of res
LEA edi, [EBP + SUM1] ;adress of sum
call PushBack 
lea edi, [EBP + temp1]
mov esi, [ebp + 8] ;adress of res
mov BYTE PTR [edi], " "
call PushBack

mov al, 1 
jmp EndOfFunction;:;need to make sure al keeps res value 
notEqual: 
mov ESI, [EBP + 20] ;adress of c
call GetStringSize
mov al, ah ;not al holds size of string c
LEA ESI, [EBP + SUM1]
call GetStringSize ;now ah holds size of sum
cmp al, ah
JLE returnFalse

;params for subString
LEA esi, [EBP + TEMP1];adress of temp to save res of subStr function
mov edx, 0
mov [esi], edx ;make sure is empty
push esi
movzx esi, al
push ESI ; sends size of c
mov esi, 0
push esi ; sends pos = 0
movzx esi, ah 
push esi; size of sum sending as len
mov ESI, [ebp + 20] ; c adress
push esi;send adress of c

call SubString

lea ESI, [ebp + SUM1] ;now ESI holds sum
lea EDI, [EBP + temp1];now edi holds adress of temp that returned from substr

call CmpStr 

cmp al, 0 ;sum != c.substr(0, sum.size()
JZ returnFalse

lea EDI, [EBP + temp1]
mov ah, N
loopMakeEmpty2:
mov BYTE PTR [EDI], 0
inc edi
dec ah
jnz loopMakeEmpty2

mov esi, [ebp + 8] ;adress of res
lea edi, [EBP + SUM1] ;adress of sum
call PushBack 
lea edi, [EBP + temp1]
mov esi, [ebp + 8] ;adress of res
mov BYTE PTR [edi], " "
call PushBack

;now we need to get c.substr(sum.size())
lea esi, [ebp + temp1] ;adress of temp to save res of subStr function
mov edx, 0
mov [esi], edx ;make sure is empty
push esi ; sends temp adress to get substring res in temp
mov ESI, [EBP + 20]
call GetStringSize
mov al, ah;size of c in al
movzx esi, ah
push ESI ; sends size of c
lea esi, [EBP + SUM1]
call GetStringSize
movzx esi, ah ; ah = size of sum
push esi ; sends pos = size of sum
movzx edx, ah ; now edx holds pos
movzx esi, al;size of c
sub esi, edx ;size of c - pos
push esi; sends len (pos until end o c)
mov ESI, [ebp + 20] ; c adress
push esi;send adress of c
call SubString


;now params for recursive call
mov edi, [EBP + 24]
lea esi, [ebp + sum1]
lea edx, [ebp + temp1]
push edi ;send b string as a string
push esi ;sends sum as b string
push edx;sends ,c.substr(sum.size()) as c string
mov esi, [EBP + 16]
push esi;address of original string
mov esi, [EBP + 12]
push esi; size of original str
mov esi, OFFSET res
push esi
jmp recursiceCall

returnFalse:
mov al, 0
jmp EndOfFunction

recursiceCall:
call ChkAddition

EndOfFunction:
add esp, N*2
;----------------------
	mov ESP, EBP
	pop EBP
	ret 24 
ChkAddition ENDP 


		END main
		

	