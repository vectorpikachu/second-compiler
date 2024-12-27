  .text
  .globl main
main:
  addi sp, sp, -32
entry:
  j bb1
bb1:
  li t0, 11
  li t1, 0
  sub t0, t0, t1
  snez t0, t0
  sw t0, 4(sp)
  lw t0, 4(sp)
  sw t0, 8(sp)
  lw t0, 4(sp)
  beqz t0, or_false
  j or_end
or_false:
  li t0, 0
  li t1, 0
  sub t0, t0, t1
  snez t0, t0
  sw t0, 12(sp)
  lw t0, 12(sp)
  sw t0, 8(sp)
  j or_end
or_end:
  lw t0, 8(sp)
  sw t0, 16(sp)
  lw t0, 16(sp)
  sw t0, 20(sp)
  j exit
bb2:
  j exit
exit:
  lw t0, 20(sp)
  sw t0, 24(sp)
  lw a0, 24(sp)
  addi sp, sp, 32
  ret
