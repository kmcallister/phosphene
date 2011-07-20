;;;; fractal video feedback in the MBR

;;;; TO RUN:
;;;;   nasm -f bin -o demo.com demo.asm
;;;;   qemu -hda demo.com


;;;; CONSTANTS

width  equ 512
height equ 384

; viewport size = 2 log_2 e
; scale by the reciprocal 1
height_scale equ height * 1000 / 2886
width_scale  equ width  * 1000 / 2886

; MBR code runs in 16-bit mode at address 0x7C00
bits 16
org  0x7C00


;;;; TWEAKABLES
; Can optimize some of these to 'inc's if set to 1
;
; For hilarious reasons, inc_red is rounded to a multiple of 4
inc_red          equ  4    ; u8   red increment per palette entry
inc_green        equ  5    ; u8   green "
inc_blue         equ  3    ; u8   blue  "


;;;; TEXT PARAMETERS
text_width  equ  64
text_height equ  16
text_num    equ   2
text_y      equ  60
text_x      equ 192


;;;; GLOBAL VARIABLES
; ds, ss  = scratch RAM
; fs      = initial read segment
;
; bp      = frame count
; si      = 0


;;;; BUFFER FORMAT
; 128 lines of 512 per segment
;   3 segments per buf
;
; The three segments:
;   buffer A:  1000 2000 3000
;   buffer B:  4000 5000 6000
;
; Use bit ops to find these from fs = 1000 or 4000


; Start of scratch ram
ram_start     equ 0x7E00

; Address relative to ram_start where we
; store rendered text
rendered_text equ 0x2000

; Address of first element of stack, in ss
stack_start   equ 0x1000

; Start of video RAM
vram_start   equ 0xA0000

; VGA palette registers
vga_dac_addr equ 0x3C8
vga_dac_data equ 0x3C9


;;;; ENTRY POINT
main:

    ; set up segments
    xor  ax, ax
    mov  ds, ax
    mov  es, ax

    ; set up stack
    mov  ax, ram_start >> 4
    mov  ss, ax
    mov  sp, stack_start

    ; use mode 13h temporarily to render text
    mov  ax, 0x13
    int  0x10
    mov  bx, 0x0F
    mov  ah, 0x0E
    mov  si, text
load_text:
    lodsb
    int  0x10
    cmp  al, 0xAA
    jne  load_text

    ; save it to RAM
    push ds
    push vram_start >> 4
    pop  ds
    xor  si, si
    mov  di, ram_start + rendered_text
    mov  cx, 320*text_num*text_height / 2
    rep movsw
    pop ds

    ; switch back to text mode
    ; going directly from 13h to 101h upsets
    ; some non-emulated hardware
    xor ax, ax
    int 0x10

    ; get info for VESA mode 101h
    ; FIXME: more error checking
    mov  ax, 0x4F01
    mov  cx, 0x0101
    mov  di, ram_start
    int  0x10

    ; compute 64 / window_granularity
    ; VESA call successful => ah = 0
    mov  al, 64
    xor  dx, dx
    div  word [di+4]
    push ax  ;; MUST BE FIRST PUSH

    ; enter the VESA mode
    mov  ax, 0x4F02
    mov  bx, 0x0101
    int  0x10

    ; set up a palette
    mov  dx, vga_dac_addr
    xor  al, al
    out  dx, al
    inc  dx

    ; 6-bit RGB values in al, bh, ch
    ; have to initialize cx, used for termination
    xor  cx, cx
palette:
    ; al has short operands, so worth
    ; the trouble of saving
    push ax
    out  dx, al
    mov  al, bh
    out  dx, al
    mov  al, ch
    out  dx, al
    pop  ax
    add  al, inc_red   >> 2
    add  bx, inc_green << 6
    add  cx, inc_blue  << 6
    jnz  palette

    ; initialize the FPU with some constants
    fninit
    mov  word [di], height_scale
    fild word [di]
    mov  byte [di], width_scale
    fild word [di]

    ; initialize frame counter and segments
    xor  bp, bp
    push 0x1000
    pop  fs


;;;; MAIN LOOP
main_loop:

    push ss
    pop  ds


;;;; TEXT BLIT
    mov  ax, fs
    add  ah, 0x10
    mov  es, ax

    mov  si, rendered_text
    test bp, 0x400
    jz   text1
    add  si, text_height * 320
text1:
    mov  di, width * text_y + text_x
text_blit:
    ; cx cleared by previous loop
    mov  cl, text_width
text_blit_row:
    lodsb
    add  [es:di], al
    inc  di
    loop text_blit_row
    add  si, 320 - text_width
    add  di, width - text_width
    cmp  di, width * (text_y + text_height)
    jb   text_blit


;;;; FEEDBACK

    xor  si, si
    xor  di, di

    ; initialize write segment register
    mov  ax, fs
    xor  ah, 0x50
    mov  es, ax

    ; push frame count to the FPU stack and scale
    ; by width (arbitrary, convenient)
    mov [si], bp
    fild word [si]
    fdiv st1

    ; stack: t h w

    fld  st0
    fcos
    fxch
    fldl2e     ; rel. period of k control pt.
    fmul
    fcos

    ; j = cos(t)
    ; k = cos(log_10(2) * t)
    ;
    ; stack: j k h w

    ; offset control point to an interesting region
    fld1
    fadd  st0
    fdiv  st2, st0
    fdivp st1, st0
    fldln2
    fsubp st1, st0

    mov  dx, height
compute_row:
    mov  cx, width
compute_pix:

    pusha

    fldl2e  ; offset factor

    ; stack: o j k h w

    mov  [si], dx
    fild word [si]
    fdiv st5
    fsub st1

    ; stack: y o j k h w

    mov  [si], cx
    fild word [si]
    fdiv st5
    fsub st2

    ; stack: x y o j k h w

    fst   st2
    fmul  st2, st0
    fld   st1
    fmul  st0
    fsubp st3, st0

    ; stack: x y (x^2 - y^2) j k h w

    fmul
    fadd st0

    ; stack: 2xy (x^2 - y^2) j k h w

    fadd  st3
    fld   st2
    faddp st2, st0

    ; stack: 2xy+k (x^2 - y^2)+j j k h w

    fldl2e
    fadd  st2, st0
    faddp st1, st0

    ; stack: (2xy + 2) ((x^2 - y^2) + 2) j k h w

    fmul  st5
    fistp word [si]
    mov   dx, [si]
    ; dx <- scaled (2xy + 2)

    fmul  st3
    fistp word [si]
    mov   bx, [si]
    ; bx <- scaled (x^2 - y^2)


    ; wrap x
    and  bh, 0x01

    ; default for out-of-bounds pixels
    ; slowly vary color with time
    mov  ax, bp
    shr  ax, 6

    ; bounds check
    cmp  dx, height
    jae  write_new

in_bounds:
    ; extract segment from top 2 bits of y
    shl  dx, 1
    shl  dh, 4
    mov  ax, fs
    add  ah, dh
    mov  gs, ax

    xor  dh, dh
    shl  dx, 8
    add  bx, dx
    mov  al, [gs:bx]
    inc  al  ; color shift for interestingness

write_new:
    mov  [es:di], al

    popa

    inc  di          ; next output pixel (may wrap)
    loop compute_pix ; next col

    test di, di
    jnz  compute_no_seginc

    mov  ax, es
    add  ah, 0x10
    mov  es, ax

compute_no_seginc:
    dec  dx          ; new row
    jnz  compute_row

    inc  bp          ; next frame

    ; discard j, k from FPU stack
    fcompp


;;;; DRAW STEP
draw:

    ; swap memory buffers
    mov  ax, fs
    xor  ah, 0x50
    mov  fs, ax
    mov  ds, ax

    ; access graphics memory through segment es
    push vram_start >> 4
    pop  es

    ; reset our window into VRAM
    ; dx is 0 from earlier loop
    call setwin

    xor  si, si
    mov  di, 48*640 + 64

    mov  bx, height
draw_row:
    mov  cx, width
draw_pix:
    movsb

    test di, di
    jnz  draw_no_wininc

    ; advance the graphics window by 64k
    add  dl, [ss:stack_start-2]
    push bx
    call setwin
    pop  bx

draw_no_wininc:
    loop draw_pix

    add  di, 128  ; black border on left / right
    dec  bx
    test bl, 0x7F ; new read seg if !(row & 0x7F)
    jnz  draw_no_seginc

    mov  ax, ds
    add  ah, 0x10
    mov  ds, ax
    xor  si, si

draw_no_seginc:

    test bx, bx
    jnz  draw_row

    jmp  main_loop


setwin:
    mov  ax, 0x4F05
    xor  bx, bx
    int  0x10
    ret


text:
    db "I", 3, 0x0D, 0x0A, "io", 0x0D, 0x0A
    db "g: ", 0xEB, 0xEE, "i", 0x0D, 0x0A, "   mrule"

;;;; END

; MBR required data

padding:
    times 446 - ($-$$) db 0xff
partitiontable:
    times 64 db 0xff
signature:
    db 0x55, 0xaa

; vim: ft=tasm
