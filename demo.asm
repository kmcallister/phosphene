;;;; fractal video feedback in the MBR

;;;; TO RUN:
;;;;   nasm -f bin -o demo.com demo.asm
;;;;   qemu -hda demo.com


;;;; CONSTANTS

width  equ 320
height equ 200
center equ width*(height/2)/4 + width/8

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
;
; si, di  =  source, dest buffers in VRAM
; bp      =  (frame count << 2) | plane


;;;; ENTRY POINT
main:

    ; set up data segment and stack
    mov  ax, 0x07E0
    mov  ds, ax
    mov  ss, ax
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

    ; set source and dest buffer offsets
    xor  si, si
    mov  di, 0x3F00

    ; initialize frame counter
    xor  bp, bp


;;;; RENDER LOOP START
render:

    ; push frame count to the FPU stack and scale
    ; by height (arbitrary, convenient)
    mov [tmp], bp
    shr word [tmp], 2
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

render_plane:

    ; set the VGA write plane bitmask
    mov  cx, bp
    and  cl, 0x03
    mov  ax, 0x0102
    shl  ah, cl
    mov  dx, vga_sequ
    out  dx, ax

    ; loop over row (dx) and col (cx)

    pusha
    mov  dx, height
render_row:
    mov  cx, width / 4  ; one plane only
render_pix:

    pusha

    ; offset the x value by the plane index
    mov  ax, bp
    and  ax, 0x03
    shl  cx, 2
    or   cx, ax

    ; stack: j k h w

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
    shr  bx, 2
    cmp  bx, width / 4
    jge  out_of_bounds
    cmp  dx, height
    jge  out_of_bounds

in_bounds:
    imul dx, width / 4
    add  bx, dx
    mov  al, [es:si+bx]
    inc  al  ; color shift for interestingness
    jmp  draw

out_of_bounds:
    ; combine center color with coords
    mov  al, [es:si+center]
    add  dx, bx
    or   al, dl

draw:
    mov  [es:di], al

    popa

    inc  di          ; next output pixel
    loop render_pix  ; next col
    dec  dx          ; new row
    jnz  render_row
    popa

    inc  bp          ; next plane or frame
    test bp, 0x3
    jnz  render_plane

    ; discard j, k from FPU stack
    fcompp

    ; swap buffers
    mov  dx, vga_crtc
    mov  ax, di
    or   ax, 0x0C
    out  dx, ax
    xchg si, di

    jmp render


;;;; END

; MBR required data

padding:
    times 446 - ($-$$) db 0
partitiontable:
    times 64 db 0xff
signature:
    db 0x55, 0xaa
