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
main_0_entry_9:
  j main_0_bb_8
main_0_bb_8:
  addi t0, sp, 0
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
  addi t0, sp, 0
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
  sw t0, 112(sp)
  li t0, 0
  sw t0, 116(sp)
  j main_0_while_entry_4
main_0_while_entry_4:
  lw t0, 116(sp)
  sw t0, 120(sp)
  lw t0, 120(sp)
  li t1, 2
  slt t0, t0, t1
  sw t0, 124(sp)
  lw t0, 124(sp)
  beqz t0, main_0_while_exit_7
  j main_0_while_body_1
main_0_while_body_1:
  li t0, 0
  sw t0, 128(sp)
  j main_0_while_entry_13
main_0_while_entry_13:
  lw t0, 128(sp)
  sw t0, 132(sp)
  lw t0, 132(sp)
  li t1, 3
  slt t0, t0, t1
  sw t0, 136(sp)
  lw t0, 136(sp)
  beqz t0, main_0_while_exit_6
  j main_0_while_body_10
main_0_while_body_10:
  lw t0, 116(sp)
  sw t0, 140(sp)
  addi t0, sp, 0
  lw t1, 140(sp)
  li t2, 12
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 144(sp)
  lw t0, 128(sp)
  sw t0, 148(sp)
  lw t0, 144(sp)
  lw t1, 148(sp)
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 152(sp)
  lw t0, 112(sp)
  sw t0, 156(sp)
  lw t1, 152(sp)
  lw t0, 0(t1)
  sw t0, 160(sp)
  lw t0, 156(sp)
  lw t1, 160(sp)
  add t0, t0, t1
  sw t0, 164(sp)
  lw t0, 116(sp)
  sw t0, 168(sp)
  la t0, gb
  lw t1, 168(sp)
  li t2, 8
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 172(sp)
  lw t0, 128(sp)
  sw t0, 176(sp)
  lw t0, 172(sp)
  lw t1, 176(sp)
  li t2, 8
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 180(sp)
  lw t1, 180(sp)
  lw t0, 0(t1)
  sw t0, 184(sp)
  lw t0, 164(sp)
  lw t1, 184(sp)
  add t0, t0, t1
  sw t0, 188(sp)
  lw t0, 188(sp)
  sw t0, 112(sp)
  lw t0, 128(sp)
  sw t0, 192(sp)
  lw t0, 192(sp)
  li t1, 1
  add t0, t0, t1
  sw t0, 196(sp)
  lw t0, 196(sp)
  sw t0, 128(sp)
  j main_0_while_entry_13
main_0_while_exit_6:
  lw t0, 116(sp)
  sw t0, 200(sp)
  lw t0, 200(sp)
  li t1, 1
  add t0, t0, t1
  sw t0, 204(sp)
  lw t0, 204(sp)
  sw t0, 116(sp)
  j main_0_while_entry_4
main_0_while_exit_7:
  li t0, 0
  sw t0, 116(sp)
  j main_0_while_entry_3
main_0_while_entry_3:
  lw t0, 116(sp)
  sw t0, 208(sp)
  lw t0, 208(sp)
  li t1, 10
  slt t0, t0, t1
  sw t0, 212(sp)
  lw t0, 212(sp)
  beqz t0, main_0_while_exit_5
  j main_0_while_body_11
main_0_while_body_11:
  lw t0, 116(sp)
  sw t0, 216(sp)
  addi t0, sp, 24
  lw t1, 216(sp)
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 220(sp)
  lw t0, 112(sp)
  sw t0, 224(sp)
  lw t0, 116(sp)
  sw t0, 228(sp)
  lw t0, 224(sp)
  lw t1, 228(sp)
  add t0, t0, t1
  sw t0, 232(sp)
  lw t0, 232(sp)
  lw t1, 220(sp)
  sw t0, 0(t1)
  lw t0, 116(sp)
  sw t0, 236(sp)
  la t0, ga
  lw t1, 236(sp)
  li t2, 8
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 240(sp)
  lw t0, 112(sp)
  sw t0, 244(sp)
  lw t0, 116(sp)
  sw t0, 248(sp)
  lw t0, 244(sp)
  lw t1, 248(sp)
  add t0, t0, t1
  sw t0, 252(sp)
  lw t0, 252(sp)
  lw t1, 240(sp)
  sw t0, 0(t1)
  lw t0, 116(sp)
  sw t0, 256(sp)
  lw t0, 256(sp)
  li t1, 1
  add t0, t0, t1
  sw t0, 260(sp)
  lw t0, 260(sp)
  sw t0, 116(sp)
  j main_0_while_entry_3
main_0_while_exit_5:
  lw t0, 112(sp)
  sw t0, 264(sp)
  lw t0, 264(sp)
  sw t0, 268(sp)
  j main_0_exit_2
main_0_bb_12:
  j main_0_exit_2
main_0_exit_2:
  lw t0, 268(sp)
  sw t0, 272(sp)
  lw a0, 272(sp)
  addi sp, sp, 288
  ret

