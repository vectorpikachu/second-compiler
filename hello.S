  .text
  .globl f1d
f1d:
  addi sp, sp, -64
f1d_0_entry_1:
  j f1d_0_bb_3
f1d_0_bb_3:
  mv t0, a0
  sw t0, 12(sp)
  li t0, 0
  sw t0, 16(sp)
  j f1d_0_while_entry_6
f1d_0_while_entry_6:
  lw t0, 16(sp)
  sw t0, 20(sp)
  lw t0, 20(sp)
  li t1, 10
  slt t0, t0, t1
  sw t0, 24(sp)
  lw t0, 24(sp)
  beqz t0, f1d_0_while_exit_5
  j f1d_0_while_body_4
f1d_0_while_body_4:
  lw t0, 12(sp)
  sw t0, 28(sp)
  lw t0, 16(sp)
  sw t0, 32(sp)
  lw t0, 28(sp)
  lw t1, 32(sp)
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 36(sp)
  lw t0, 16(sp)
  sw t0, 40(sp)
  lw t0, 40(sp)
  lw t1, 36(sp)
  sw t0, 0(t1)
  lw t0, 16(sp)
  sw t0, 44(sp)
  lw t0, 44(sp)
  li t1, 1
  add t0, t0, t1
  sw t0, 48(sp)
  lw t0, 48(sp)
  sw t0, 16(sp)
  j f1d_0_while_entry_6
f1d_0_while_exit_5:
  j f1d_0_exit_2
f1d_0_exit_2:
  addi sp, sp, 64
  ret

  .text
  .globl f2d
f2d:
  addi sp, sp, -80
  sw ra, 76(sp)
f2d_1_entry_2:
  j f2d_1_bb_6
f2d_1_bb_6:
  mv t0, a0
  sw t0, 12(sp)
  lw t0, 12(sp)
  sw t0, 16(sp)
  lw t0, 16(sp)
  li t1, 1
  li t2, 40
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 20(sp)
  lw t0, 20(sp)
  li t1, 2
  li t2, 40
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 24(sp)
  li t0, 3
  lw t1, 24(sp)
  sw t0, 0(t1)
  li t0, 0
  sw t0, 28(sp)
  j f2d_1_while_entry_3
f2d_1_while_entry_3:
  lw t0, 28(sp)
  sw t0, 32(sp)
  lw t0, 32(sp)
  li t1, 10
  slt t0, t0, t1
  sw t0, 36(sp)
  lw t0, 36(sp)
  beqz t0, f2d_1_while_exit_4
  j f2d_1_while_body_1
f2d_1_while_body_1:
  lw t0, 12(sp)
  sw t0, 40(sp)
  lw t0, 28(sp)
  sw t0, 44(sp)
  lw t0, 40(sp)
  lw t1, 44(sp)
  li t2, 40
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 48(sp)
  lw t0, 48(sp)
  li t1, 0
  li t2, 40
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 52(sp)
  lw a0, 52(sp)
  call f1d
  lw t0, 28(sp)
  sw t0, 56(sp)
  lw t0, 56(sp)
  li t1, 1
  add t0, t0, t1
  sw t0, 60(sp)
  lw t0, 60(sp)
  sw t0, 28(sp)
  j f2d_1_while_entry_3
f2d_1_while_exit_4:
  j f2d_1_exit_5
f2d_1_exit_5:
  lw ra, 76(sp)
  addi sp, sp, 80
  ret

  .text
  .globl main
main:
  addi sp, sp, -16
main_2_entry_1:
  j main_2_bb_4
main_2_bb_4:
  li t0, 33
  sw t0, 4(sp)
  j main_2_exit_3
main_2_bb_2:
  j main_2_exit_3
main_2_exit_3:
  lw t0, 4(sp)
  sw t0, 8(sp)
  lw a0, 8(sp)
  addi sp, sp, 16
  ret

