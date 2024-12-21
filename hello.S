  .text
  .globl main
main:
  addi sp, sp, -16
entry:
  j bb1
bb1:
  li t0, 3
  sw t0, 4(sp)
  j exit
bb2:
  j exit
exit:
  lw t0, 4(sp)
  mv a0, t0
  addi sp, sp, 16
  ret
