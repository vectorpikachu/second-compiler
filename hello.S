  .text
  .globl main
main:
  addi sp, sp, -80
entry6:
  j bb9
bb9:
  li t0, 10
  sw t0, 4(sp)
  lw t0, 4(sp)
  sw t0, 8(sp)
  lw t0, 8(sp)
  li t1, 1
  sgt t0, t0, t1
  sw t0, 12(sp)
  lw t0, 12(sp)
  beqz t0, if_false16
  j if_true10
if_true10:
  lw t0, 4(sp)
  sw t0, 16(sp)
  lw t0, 16(sp)
  li t1, 2
  sgt t0, t0, t1
  sw t0, 20(sp)
  lw t0, 20(sp)
  beqz t0, if_false7
  j if_true11
if_true11:
  lw t0, 4(sp)
  sw t0, 24(sp)
  lw t0, 24(sp)
  li t1, 3
  slt t0, t0, t1
  sw t0, 28(sp)
  lw t0, 28(sp)
  beqz t0, if_false17
  j if_true1
if_true1:
  lw t0, 4(sp)
  sw t0, 32(sp)
  lw t0, 32(sp)
  sw t0, 36(sp)
  j exit3
bb13:
  j if_end4
if_false17:
  lw t0, 4(sp)
  sw t0, 40(sp)
  lw t0, 40(sp)
  li t1, 4
  sgt t0, t0, t1
  sw t0, 44(sp)
  lw t0, 44(sp)
  beqz t0, if_false18
  j if_true22
if_true22:
  lw t0, 4(sp)
  sw t0, 48(sp)
  lw t0, 48(sp)
  li t1, 5
  slt t0, t0, t1
  sw t0, 52(sp)
  lw t0, 52(sp)
  beqz t0, if_false12
  j if_true8
if_true8:
  lw t0, 4(sp)
  sw t0, 56(sp)
  lw t0, 56(sp)
  li t1, 1
  add t0, t0, t1
  sw t0, 60(sp)
  lw t0, 60(sp)
  sw t0, 36(sp)
  j exit3
bb20:
  j if_end15
if_false12:
  lw t0, 4(sp)
  sw t0, 64(sp)
  lw t0, 64(sp)
  li t1, 2
  add t0, t0, t1
  sw t0, 68(sp)
  lw t0, 68(sp)
  sw t0, 36(sp)
  j exit3
bb5:
  j if_end15
if_end15:
  j if_end2
if_false18:
  j if_end2
if_end2:
  j if_end4
if_end4:
  j if_end14
if_false7:
  j if_end14
if_end14:
  j if_end19
if_false16:
  j if_end19
if_end19:
  li t0, 0
  li t1, 1
  sub t0, t0, t1
  sw t0, 72(sp)
  lw t0, 72(sp)
  sw t0, 36(sp)
  j exit3
bb21:
  j exit3
exit3:
  lw t0, 36(sp)
  sw t0, 76(sp)
  lw a0, 76(sp)
  addi sp, sp, 80
  ret
