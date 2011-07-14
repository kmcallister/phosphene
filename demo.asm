;;;; fractal video feedback in the MBR

;;;; TO RUN:
;;;;   nasm -f bin -o demo.com demo.asm
;;;;   qemu -hda demo.com


;;;; CONSTANTS

width  equ 320
height equ 200
pixels equ width*height
center equ width*(height/2) + width/2

; MBR code runs in 16-bit mode at address 0x7C00
bits 16
org  0x7C00

; VGA card registers
vga_sequ equ 0x3c4
vga_crtc equ 0x3d4


;;;; GLOBAL VARIABLES
; ds, ss  =  scratch RAM
; es      =  video   RAM
; fs, gs  =  source, dest buffers in scratch RAM
;
; bp      =  frame count
; si      = 0


;;;; ENTRY POINT
main:

    ; set up data segment and stack
    mov  ax, 0x07E0
    mov  ds, ax
    mov  ss, ax
    add  ax, 0x1000
    mov  fs, ax
    add  ax, 0x1000
    mov  gs, ax
    mov  sp, 0x1000

    ; enter VGA mode 13h
    mov  ax, 0x13
    int  0x10

    ; access graphics memory through segment es
    push 0xA000
    pop  es

    ; initialize the FPU with some constants
    fninit
    xor  si, si
    mov  word [si], height / 4
    fild word [si]
    mov  word [si], width / 4
    fild word [si]

    ; initialize frame counter
    mov  bp, 60


;;;; MAIN LOOP

main_loop:


;;;; COMPUTE STEP
compute:

    xor  di, di

    ; push frame count to the FPU stack and scale
    ; by height (arbitrary, convenient)
    mov [si], bp
    fild word [si]
    fdiv st2

    ; stack: t h w

    fld  st0
    fld  st0
    fcos
    fstp st2
    fldlg2
    fmul
    fcos

    ; j = cos(t)
    ; k = cos(log_10(2) * t)
    ;
    ; stack: j k h w

    mov  dx, height
compute_row:
    mov  cx, width
compute_pix:

    pusha

    fld1
    fadd st0

    ; stack: 2 j k h w

    mov  [si], dx
    fild word [si]
    fdiv st5
    fsub st1

    ; stack: y 2 j k h w

    mov  [si], cx
    fild word [si]
    fdiv st5
    fsub st2

    ; stack: x y 2 j k h w

    fst   st2
    fmul  st2, st0
    fld   st1
    fmul  st0
    fsubp st3, st0

    ; stack: x y (x^2 - y^2) j k h w

    fmul
    fadd st0

    ; stack: 2xy (x^2 - y^2) j k h w

    fld1
    fadd  st0
    fadd  st3
    fadd  st2, st0
    fadd  st4
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
    jge  out_of_bounds
    cmp  dx, height
    jge  out_of_bounds

in_bounds:
    imul dx, width
    add  bx, dx
    mov  al, [fs:bx]
    inc  al  ; color shift for interestingness
    jmp  write_new

out_of_bounds:
    ; slowly vary color with time
    mov  ax, bp
    shr  ax, 6

write_new:
    mov  [gs:di], al

    popa

    inc  di          ; next output pixel
    loop compute_pix ; next col
    dec  dx          ; new row
    jnz  compute_row

    inc  bp          ; next frame

    ; discard j, k from FPU stack
    fcompp


;;;; DRAW STEP
draw:

    push ds
    push gs
    pop  ds
    xor  si, si
    mov  di, si
    mov  cx, pixels / 2
    rep  movsw
    pop  ds

    ; swap memory buffers
    push fs
    push gs
    pop  fs
    pop  gs

    jmp main_loop


;;;; END

; MBR required data

padding:
    times 446 - ($-$$) db 0
partitiontable:
    times 64 db 0xff
signature:
    db 0x55, 0xaa
