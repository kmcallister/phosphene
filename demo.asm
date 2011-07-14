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


;;;; GLOBAL VARIABLES
; ds, ss  = scratch RAM
; fs      = initial read segment
;
; bp      = frame count
; si      = 0x100  (after VESA mode info block)


;;;; BUFFER FORMAT
; 128 lines of 512 per segment
;   3 segments per buf
;
; The three segments:
;   buffer A:  1000 2000 3000
;   buffer B:  4000 5000 6000
;
; Use bit ops to find these from fs = 1000 or 4000



; VESA mode info block at ds:0, offsets:
;
; window granularity
; NB: n := 64/n by us
mib_window_gran equ 4



; VGA palette registers
vga_dac_addr equ 0x3C8
vga_dac_data equ 0x3C9



;;;; ENTRY POINT
main:

    ; set up data segment and stack
    mov  ax, 0x07E0
    mov  ds, ax
    mov  es, ax     ; temporarily set
    mov  ss, ax
    mov  sp, 0x1000

    ; get window granularity for VESA mode 101h
    ; FIXME: more error checking
    mov  ax, 0x4F01
    mov  cx, 0x0101
    push cx
    xor  di, di
    int  0x10
    mov  ax, 64
    xor  dx, dx
    div  word [mib_window_gran]
    mov  [mib_window_gran], ax

    ; enter VESA mode 101h
    mov  ax, 0x4F02
    pop  bx
    int  0x10

    ; set up a palette
    mov  dx, vga_dac_addr
    xor  al, al
    out  dx, al
    mov  dx, vga_dac_data
    mov  cx, 64
palette:
    mov  bl, cl
    call palette_expand
    call palette_expand
    call palette_expand
    loop palette

    ; initialize the FPU with some constants
    fninit
    mov  si, 0x100
    mov  word [si], height * 1000 / 2886
    fild word [si]
    mov  word [si], width  * 1000 / 2886
    fild word [si]

    ; initialize frame counter and segments
    mov  bp, 0x100
    push 0x1000
    pop  fs


;;;; MAIN LOOP
main_loop:

    mov  si, 0x100
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
    fldl2e
    fmul
    fcos

    fldlg2
    fldlg2
    fmul
    fadd  st0
    fadd  st0

    fadd  st0
    fmul  st2, st0
    fmulp st1, st0

    ; j = cos(t)
    ; k = cos(log_10(2) * t)
    ;
    ; stack: j k h w

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

    fldl2e
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
    add  al, 16  ; color shift for interestingness
    jmp  write_new

out_of_bounds:
    ; slowly vary color with time
    mov  ax, bp
    shr  ax, 4

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
    mov  gs, ax

    ; access graphics memory through segment es
    push 0xA000
    pop  es

    ; set graphics window
    xor  dx, dx
    call setwin

    xor  si, si
    mov  di, 64

    mov  bx, height
draw_row:
    mov  cx, width
draw_pix:
    mov  al, [gs:si]
    and  al, 0x3F
    mov  [es:di], al
    inc  si

    inc  di
    jnz  draw_no_wininc

    add  dx, [mib_window_gran]
    pusha
    call setwin
    popa

draw_no_wininc:
    loop draw_pix

    add  di, 128
    dec  bx
    mov  ax, bx
    shl  ax, 1
    test al, al
    jnz  draw_no_seginc

    mov  ax, gs
    add  ah, 0x10
    mov  gs, ax
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


palette_expand:
    pusha
    xor  al, al
    test bl, 1
    jz   px_no_1
    or   al, 0x7
px_no_1:
    test bl, 2
    jz   px_no_2
    or   al, 0x38
px_no_2:
    out  dx, al
    popa
    shr  bl, 2
    ret


;;;; END

; MBR required data

padding:
    times 446 - ($-$$) db 0
partitiontable:
    times 64 db 0xff
signature:
    db 0x55, 0xaa
