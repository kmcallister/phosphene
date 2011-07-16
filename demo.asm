;;;; fractal video feedback in the MBR

;;;; TO RUN:
;;;;   nasm -f bin -o demo.com demo.asm
;;;;   qemu -hda demo.com


;;;; CONSTANTS

width  equ 512
height equ 384

; MBR code runs in 16-bit mode at address 0x7C00
bits 16
org  0x7C00


;;; TWEAKABLES
; Can optimize some of these to 'inc's if set to 1
;
inc_red          equ  1    ; u8   red increment per palette entry
inc_green        equ  2    ; u8   green "
inc_blue         equ  3    ; u8   blue  "
init_frame       equ  256  ; u16  initial frame number
color_shift      equ  2    ; u8   color increment each time thru map
oob_color_speed  equ  6    ; u8   incr. color of OOB points every 2^n frames


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


; VGA palette registers
vga_dac_addr equ 0x3C8
vga_dac_data equ 0x3C9


;;;; ENTRY POINT
main:

    ; set up data segment and stack
    mov  ax, 0x07E0
    mov  ss, ax
    mov  sp, 0x1000

    ; get info for VESA mode 101h
    ; FIXME: more error checking
    mov  ax, 0x4F01
    mov  cx, 0x0101
    push cx
    mov  di, 0x7E00
    int  0x10

    ; compute 64 / window_granularity
    ; VESA call successful => ah = 0
    mov  al, 64
    xor  dx, dx
    div  word [di+4]
    mov  [window_advance+2], al

    ; enter the VESA mode
    mov  ax, 0x4F02
    pop  bx
    int  0x10

    ; set up a palette
    mov  dx, vga_dac_addr
    xor  al, al
    out  dx, al
    mov  dx, vga_dac_data

    ; RGB = al, bl, bh
    ; don't bother initializing bx
    xor  ax, ax
palette:
    pusha
    out  dx, al
    mov  al, bl
    out  dx, al
    mov  al, bh
    out  dx, al
    popa
    add  al, inc_red
    ; carry from bl to bh is harmless
    add  bx, (inc_blue << 8) | inc_green
    inc  ah
    jnz  palette

    ; initialize the FPU with some constants
    fninit
    mov  word [di], height * 1000 / 2886  ; 1 / (2 log_2 e)
    fild word [di]
    mov  word [di], width  * 1000 / 2886
    fild word [di]

    ; initialize frame counter and segments
    mov  bp, init_frame
    push 0x1000
    pop  fs


;;;; MAIN LOOP
main_loop:

    push ss
    pop  ds

    xor  si, si
    xor  di, di

    ; initialize write segment register
    mov  ax, fs
    xor  ah, 0x50
    mov  es, ax

    ; push frame count to the FPU stack and scale
    ; by height (arbitrary, convenient)
    mov [si], bp
    fild word [si]
    fdiv st2
    fadd st0

    ; stack: t h w

    fld  st0
    fld  st0
    fcos
    fstp st2
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


    ; bounds check
    cmp  bx, width
    jae  out_of_bounds
    cmp  dx, height
    jae  out_of_bounds

in_bounds:
    ; extract segment from top 2 bits of y
    mov  cx, dx
    shr  cx, 3
    and  cl, 0x30
    mov  ax, fs
    add  ah, cl
    mov  gs, ax

    and  dl, 0x7F
    shl  dx, 9
    add  bx, dx
    mov  al, [gs:bx]
    add  al, color_shift  ; color shift for interestingness
    jmp  write_new

out_of_bounds:
    ; slowly vary color with time
    mov  ax, bp
    shr  ax, oob_color_speed

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
    push 0xA000
    pop  es

    ; reset our window into VRAM
    xor  dx, dx
    call setwin

    xor  si, si
    mov  di, 64

    mov  bx, height
draw_row:
    mov  cx, width
draw_pix:
    movsb

    test di, di
    jnz  draw_no_wininc

    ; advance the graphics window by 64k
    ; init code writes here the number of
    ; "window increments" per 64 k
window_advance:
    add  dl, 0
    push bx
    call setwin
    pop  bx

draw_no_wininc:
    loop draw_pix

    add  di, 128  ; black border on left / right
    dec  bx
    mov  ax, bx
    shl  ax, 1
    test al, al   ; new read seg if !(row & 0x7F)
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


;;;; END

; MBR required data

padding:
    times 446 - ($-$$) db 0
partitiontable:
    times 64 db 0xff
signature:
    db 0x55, 0xaa
