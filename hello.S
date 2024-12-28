  .data
  .globl x
x:
  .zero 4

  .data
  .globl init
init:
  .word 1

  .text
  .globl main
main:
  addi sp, sp, -32
  sw ra, 28(sp)
main_0_entry_2:
  j main_0_bb_4
main_0_bb_4:
  la t0, x
  sw t0, 0(sp)
  lw a0, 0(sp)
  call putint
  li a0, 32
  call putch
  li a0, 10
  call putint
  li a0, 32
  call putch
  li a0, 11
  call putint
  li a0, 32
  call putch
  la t0, init
  sw t0, 4(sp)
  lw a0, 4(sp)
  call putint
  li a0, 10
  call putch
  li t0, 0
  sw t0, 8(sp)
  j main_0_exit_3
main_0_bb_1:
  j main_0_exit_3
main_0_exit_3:
  lw t0, 8(sp)
  sw t0, 12(sp)
  lw a0, 12(sp)
  lw ra, 28(sp)
  addi sp, sp, 32
  ret

