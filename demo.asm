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

; address in data segment of a temporary variable
tmp    equ   0


;;;; GLOBAL VARIABLES
; ds, ss  =  scratch RAM
; es      =  video   RAM
; fs, gs  =  source, dest buffers in scratch RAM
;
; bp      =  frame count


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

    ; unchain the graphics bitplanes
    mov  dx, vga_sequ
    mov  ax, 0x0604
    out  dx, ax
    mov  dx, vga_crtc
    mov  ax, 0xE317
    out  dx, ax
    mov  ax, 0x0014
    out  dx, ax

    ; access graphics memory through segment es
    push 0xA000
    pop  es

    ; initialize the FPU with some constants
    fninit
    mov  word [tmp], height / 4
    fild word [tmp]
    mov  word [tmp], width / 4
    fild word [tmp]

    ; initialize frame counter
    xor  bp, bp


;;;; MAIN LOOP

main_loop:


;;;; COMPUTE STEP
compute:

    xor  di, di

    ; push frame count to the FPU stack and scale
    ; by height (arbitrary, convenient)
    mov [tmp], bp
    fild word [tmp]
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

    pusha
    mov  dx, height
compute_row:
    mov  cx, width
compute_pix:

    pusha

    fld1
    fadd st0

    ; stack: 2 j k h w

    mov  [tmp], dx
    fild word [tmp]
    fdiv st5
    fsub st1

    ; stack: y 2 j k h w

    mov  [tmp], cx
    fild word [tmp]
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
    fistp word [tmp]
    mov   dx, [tmp]
    ; dx <- scaled (2xy + 2)

    fmul  st3
    fistp word [tmp]
    mov   bx, [tmp]
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
    ; combine center color with coords
    mov  al, [fs:center]
    add  dx, bx
    or   al, dl

write_new:
    mov  [gs:di], al

    popa

    inc  di          ; next output pixel
    loop compute_pix ; next col
    dec  dx          ; new row
    jnz  compute_row
    popa

    inc  bp          ; next frame

    ; discard j, k from FPU stack
    fcompp


;;;; DRAW STEP
draw:

    xor  cl, cl
draw_plane:
    ; set the bitplane mask
    mov  dx, vga_sequ
    mov  ax, 0x0102
    shl  ah, cl
    out  dx, ax

    ; copy every 4th pixel
    xor  bx, bx
    mov  bl, cl
    xor  di, di
draw_loop:
    mov  al, [gs:bx]
    mov  [es:di], al

    add  bx, 4
    inc  di

    cmp  di, pixels / 4
    jl   draw_loop

    inc  cl
    cmp  cl, 4
    jl   draw_plane


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
