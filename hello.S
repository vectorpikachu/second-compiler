  .text
  .globl main
main:
  addi sp, sp, -16
entry:
  j bb2
bb2:
  li t0, 1
  sw t0, 4(sp)
  lw t0, 4(sp)
  sw t0, 8(sp)
  lw t0, 8(sp)
  sw t0, 12(sp)
  j exit
bb1:
  j exit
exit:
  lw t0, 12(sp)
  sw t0, 16(sp)
  lw a0, 16(sp)
  addi sp, sp, 16
  ret
