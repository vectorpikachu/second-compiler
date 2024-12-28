  .data
  .globl a
a:
  .word 1
  .word 2
  .word 3
  .word 0
  .word 0
  .word 0
  .word 0
  .word 0
  .word 0
  .word 0

  .text
  .globl main
main:
  addi sp, sp, -16
main_0_entry_1:
  j main_0_bb_4
main_0_bb_4:
  li t0, 0
  sw t0, 0(sp)
  j main_0_exit_3
main_0_bb_2:
  j main_0_exit_3
main_0_exit_3:
  lw t0, 0(sp)
  sw t0, 4(sp)
  lw a0, 4(sp)
  addi sp, sp, 16
  ret

