  .data
  .globl ga
ga:
  .zero 40

  .data
  .globl gb
gb:
  .word 5
  .word 6
  .word 7
  .word 8
  .word 0
  .word 0

  .text
  .globl main
main:
  addi sp, sp, -288
main_0_entry_7:
  j main_0_bb_12
main_0_bb_12:
  addi t0, sp, 44
  li t1, 0
  li t2, 12
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 80(sp)
  lw t0, 80(sp)
  li t1, 0
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 84(sp)
  li t0, 1
  lw t1, 84(sp)
  sw t0, 0(t1)
  lw t0, 80(sp)
  li t1, 1
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 88(sp)
  li t0, 2
  lw t1, 88(sp)
  sw t0, 0(t1)
  lw t0, 80(sp)
  li t1, 2
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 92(sp)
  li t0, 3
  lw t1, 92(sp)
  sw t0, 0(t1)
  addi t0, sp, 44
  li t1, 1
  li t2, 12
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 96(sp)
  lw t0, 96(sp)
  li t1, 0
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 100(sp)
  li t0, 4
  lw t1, 100(sp)
  sw t0, 0(t1)
  lw t0, 96(sp)
  li t1, 1
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 104(sp)
  li t0, 0
  lw t1, 104(sp)
  sw t0, 0(t1)
  lw t0, 96(sp)
  li t1, 2
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 108(sp)
  li t0, 0
  lw t1, 108(sp)
  sw t0, 0(t1)
  li t0, 0
  lw t1, 68(sp)
  sw t0, 0(t1)
  li t0, 0
  lw t1, 72(sp)
  sw t0, 0(t1)
  j main_0_while_entry_9
main_0_while_entry_9:
  lw t1, 72(sp)
  lw t0, 0(t1)
  sw t0, 112(sp)
  lw t0, 112(sp)
  li t1, 2
  slt t0, t0, t1
  sw t0, 116(sp)
  lw t0, 116(sp)
  beqz t0, main_0_while_exit_11
  j main_0_while_body_6
main_0_while_body_6:
  li t0, 0
  lw t1, 76(sp)
  sw t0, 0(t1)
  j main_0_while_entry_3
main_0_while_entry_3:
  lw t1, 76(sp)
  lw t0, 0(t1)
  sw t0, 120(sp)
  lw t0, 120(sp)
  li t1, 3
  slt t0, t0, t1
  sw t0, 124(sp)
  lw t0, 124(sp)
  beqz t0, main_0_while_exit_5
  j main_0_while_body_8
main_0_while_body_8:
  lw t1, 72(sp)
  lw t0, 0(t1)
  sw t0, 128(sp)
  addi t0, sp, 44
  lw t1, 128(sp)
  li t2, 12
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 132(sp)
  lw t1, 76(sp)
  lw t0, 0(t1)
  sw t0, 136(sp)
  lw t0, 132(sp)
  lw t1, 136(sp)
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 140(sp)
  lw t1, 68(sp)
  lw t0, 0(t1)
  sw t0, 144(sp)
  lw t1, 140(sp)
  lw t0, 0(t1)
  sw t0, 148(sp)
  lw t0, 144(sp)
  lw t1, 148(sp)
  add t0, t0, t1
  sw t0, 152(sp)
  lw t1, 72(sp)
  lw t0, 0(t1)
  sw t0, 156(sp)
  la t0, gb
  lw t1, 156(sp)
  li t2, 8
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 160(sp)
  lw t1, 76(sp)
  lw t0, 0(t1)
  sw t0, 164(sp)
  lw t0, 160(sp)
  lw t1, 164(sp)
  li t2, 8
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 168(sp)
  lw t1, 168(sp)
  lw t0, 0(t1)
  sw t0, 172(sp)
  lw t0, 152(sp)
  lw t1, 172(sp)
  add t0, t0, t1
  sw t0, 176(sp)
  lw t0, 176(sp)
  lw t1, 68(sp)
  sw t0, 0(t1)
  lw t1, 76(sp)
  lw t0, 0(t1)
  sw t0, 180(sp)
  lw t0, 180(sp)
  li t1, 1
  add t0, t0, t1
  sw t0, 184(sp)
  lw t0, 184(sp)
  lw t1, 76(sp)
  sw t0, 0(t1)
  j main_0_while_entry_3
main_0_while_exit_5:
  lw t1, 72(sp)
  lw t0, 0(t1)
  sw t0, 188(sp)
  lw t0, 188(sp)
  li t1, 1
  add t0, t0, t1
  sw t0, 192(sp)
  lw t0, 192(sp)
  lw t1, 72(sp)
  sw t0, 0(t1)
  j main_0_while_entry_9
main_0_while_exit_11:
  li t0, 0
  lw t1, 72(sp)
  sw t0, 0(t1)
  j main_0_while_entry_10
main_0_while_entry_10:
  lw t1, 72(sp)
  lw t0, 0(t1)
  sw t0, 196(sp)
  lw t0, 196(sp)
  li t1, 10
  slt t0, t0, t1
  sw t0, 200(sp)
  lw t0, 200(sp)
  beqz t0, main_0_while_exit_1
  j main_0_while_body_13
main_0_while_body_13:
  lw t1, 72(sp)
  lw t0, 0(t1)
  sw t0, 204(sp)
  addi t0, sp, 4
  lw t1, 204(sp)
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 208(sp)
  lw t1, 68(sp)
  lw t0, 0(t1)
  sw t0, 212(sp)
  lw t1, 72(sp)
  lw t0, 0(t1)
  sw t0, 216(sp)
  lw t0, 212(sp)
  lw t1, 216(sp)
  add t0, t0, t1
  sw t0, 220(sp)
  lw t0, 220(sp)
  lw t1, 208(sp)
  sw t0, 0(t1)
  lw t1, 72(sp)
  lw t0, 0(t1)
  sw t0, 224(sp)
  la t0, ga
  lw t1, 224(sp)
  li t2, 8
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 228(sp)
  lw t1, 68(sp)
  lw t0, 0(t1)
  sw t0, 232(sp)
  lw t1, 72(sp)
  lw t0, 0(t1)
  sw t0, 236(sp)
  lw t0, 232(sp)
  lw t1, 236(sp)
  add t0, t0, t1
  sw t0, 240(sp)
  lw t0, 240(sp)
  lw t1, 228(sp)
  sw t0, 0(t1)
  lw t1, 72(sp)
  lw t0, 0(t1)
  sw t0, 244(sp)
  lw t0, 244(sp)
  li t1, 1
  add t0, t0, t1
  sw t0, 248(sp)
  lw t0, 248(sp)
  lw t1, 72(sp)
  sw t0, 0(t1)
  j main_0_while_entry_10
main_0_while_exit_1:
  lw t1, 68(sp)
  lw t0, 0(t1)
  sw t0, 252(sp)
  lw t0, 252(sp)
  lw t1, 0(sp)
  sw t0, 0(t1)
  j main_0_exit_4
main_0_bb_2:
  j main_0_exit_4
main_0_exit_4:
  lw t1, 0(sp)
  lw t0, 0(t1)
  sw t0, 256(sp)
  lw a0, 256(sp)
  addi sp, sp, 288
  ret

