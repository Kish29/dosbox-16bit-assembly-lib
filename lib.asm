assume cs:codeseg, ds:dataseg, ss:stackseg

dataseg segment
    var_buf     dd      61 dup(0), 0
    out_buf     dd      31 dup(0), 0
    tmp_buf     dd      31 dup(0), 0
    input_buf   dd      80h, 30 dup(0), 0
    cl_char     db      0ah, 0
dataseg ends

stackseg segment
    dd  63 dup(0), 0
stackseg ends

codeseg segment

; invoke params:
; @param ds:si source str address
; @param es:di destination str address
; return values -> null
strncpy:
    push si
    push di
    cld
    rep movsb
    ; input change line
    mov byte ptr [di], 0ah
    pop di
    pop si
    ret

; invoke params:
; @param es:di str store address
; return values -> ax length of string
str_input:
    push si
    push ax
    push dx
    push cx

    ; input buf
    lea dx, input_buf
    mov ah, 0ah
    int 21h

    lea si, input_buf
    xor ax, ax
    ; get length of str
    mov al, [si+1]
    mov cx, ax
    add si, 2
    call strncpy

    pop cx
    pop dx
    pop ax
    pop si
    ret

; invoke params:
; @param es:di -> str address 
; @param cx -> length of str
print:
    push cx
    push di
    push dx
    mov si, di

    lea di, out_buf
    ; change line
    mov byte ptr [di], 0ah
    inc di

    call strncpy
    add di, cx
    ; end falg
    mov byte ptr [di], '$'
    
    ; prink
    lea dx, out_buf
    mov ah, 09h
    int 21h

    pop dx
    pop di
    pop cx
    ret

; invoke params:
; @param es:di -> str address 
; @param cx -> length of str
reverse_print:
    push cx
    push di
    push dx
    push si

    mov si, di
    lea di, out_buf
    ; change line
    mov byte ptr [di], 0ah
    ; go to the str end
    add di, cx
    ; end flag
    mov byte ptr [di+1], '$'
reverse_copy:
    std
    mov al, [si]
    stosb
    inc si
    loop reverse_copy
    
    lea dx, out_buf
    mov ah, 09h
    int 21h

    pop si
    pop dx
    pop di
    pop cx
    ret


; @param ax -> to print value reg
; @param bx -> print scale
reg_print:
    ; store num
    push ax
    push dx
    push cx

    ; judge negative or positive
    mov cx, ax
    and cx, 8000h

    ; store flag
    mov dx, cx
    push dx
    
    cmp cx, 0
    jne negative
    jmp positive

negative:
    ; convert to complement
    not ax
    inc ax

positive:

    lea di, tmp_buf
    xor cx, cx

put_char:
    ; get reminder
    xor dx, dx
    div bx
    ; convert to ascii
    add dl, 30h
    mov [di], dl
    inc di
    inc cx
    cmp ax, 0
    jne put_char

    ; judge 16 scale
    cmp bx, 10h
    je set_16_sign
    jmp not_set_16_sign

set_16_sign:
    mov word ptr [di], 78h ; 'x'
    inc di
    mov byte ptr [di], 30h ; '0'
    inc di
    add cx, 2

not_set_16_sign:
    ; get flag
    pop dx
    cmp dx, 0
    jne set_sign
    jmp not_set

set_sign:
    mov byte ptr [di], '-'
    inc cx

not_set:

    lea di, tmp_buf
    call reverse_print

    pop cx
    pop dx
    pop ax
    ret

;============= convert str to integet ====
; @param es:di -> addres of str
; return value -> ax : converted num
atoi:
    push cx
    push bx
    push dx

    xor bx, bx
    mov al, [di]
    cmp al, 45  ; '-'
    jne pos_num
    ; 0 -> positive num
    ; 1 -> negative num
    inc di
    mov bx, 1
pos_num:
    ; store flag
    push bx

    ; convert to num
    xor bx, bx
    mov bl, 0ah
    xor ax, ax

go_on_convert:
    xor cx, cx
    mov cl, [di]
    inc di
    ; 1 ~ 9
    cmp cl, 30h
    jl cvnt_end
    cmp cl, 39h
    ja cvnt_end
    ; control
    cmp cl, 0dh
    je cvnt_end
    ; sapce
    cmp cl, 20h
    je cvnt_end
    sub cl, 30h

    mul bx
    add ax, cx
    jmp go_on_convert

cvnt_end:
    pop bx
    cmp bx, 0
    je cvnt_ret
    ; convert to complement
    not ax
    inc ax
cvnt_ret:
    pop dx
    pop bx
    pop cx
    ret
    
; @param ds:si -> variable address
integer_input:
    push di
    push ax

    lea di, tmp_buf
    call str_input
    call atoi

    mov [si], ax

    pop ax
    pop di
    ret

; @param ds:si -> address of array
; @param cx -> num -> cx >= 1
integer_num_input:
    push ax
    push bx
    push cx
    push dx
    push si

    mov bx, 0ah
input_loop:
    call integer_input ; 0129
    call print_line    ; 012c
    add si, 2          ; 012f   
    loop input_loop

    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

; print all values of array
; @param ds:si -> address of array
; @param cx -> length of array
array_print:
    push ax
    push bx
    push cx

    cmp cx, 0
    ja array_prt
    ret 
array_prt:
    xor ax, ax
    mov ax, [si]
    mov bx, 0ah
    call reg_print
    add si, 2
    loop array_prt

    pop cx
    pop bx
    pop ax
    ret

; change line in current screen
print_line:
    push cx
    push dx
    push di
    push ax

    lea di, out_buf
    mov byte ptr [di], 0ah
    mov byte ptr [di+1], '$'

    lea dx, out_buf
    mov ah, 09h
    int 21h

    pop ax
    pop di
    pop dx
    pop cx
    ret

start:
    ; initialize seg reg
    mov ax, dataseg
    mov ds, ax
    mov es, ax

    mov ax, stackseg
    mov ss, ax
    mov sp, 0100h
    
    lea si, var_buf
    mov cx, 6
    call integer_num_input  ; 188
    call array_print        ; 18b

    mov ax, 4c00h
    int 21h

codeseg ends
end start
