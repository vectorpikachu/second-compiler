  .text
  .globl init
init:
  addi sp, sp, -144
init_0_entry_11:
  j init_0_bb_2
init_0_bb_2:
  mv t0, a0
  sw t0, 16(sp)
  li t0, 0
  sw t0, 20(sp)
  j init_0_while_entry_12
init_0_while_entry_12:
  lw t0, 20(sp)
  sw t0, 24(sp)
  li t1, 10
  slt t0, t0, t1
  sw t0, 28(sp)
  beqz t0, init_0_while_exit_9
  j init_0_while_body_3
init_0_while_body_3:
  li t0, 0
  sw t0, 32(sp)
  j init_0_while_entry_10
init_0_while_entry_10:
  lw t0, 32(sp)
  sw t0, 36(sp)
  li t1, 10
  slt t0, t0, t1
  sw t0, 40(sp)
  beqz t0, init_0_while_exit_5
  j init_0_while_body_7
init_0_while_body_7:
  li t0, 0
  sw t0, 44(sp)
  j init_0_while_entry_6
init_0_while_entry_6:
  lw t0, 44(sp)
  sw t0, 48(sp)
  li t1, 10
  slt t0, t0, t1
  sw t0, 52(sp)
  beqz t0, init_0_while_exit_8
  j init_0_while_body_1
init_0_while_body_1:
  lw t0, 16(sp)
  sw t0, 56(sp)
  lw t0, 20(sp)
  sw t0, 60(sp)
  lw t0, 56(sp)
  lw t1, 60(sp)
  li t2, 400
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 64(sp)
  lw t0, 32(sp)
  sw t0, 68(sp)
  lw t0, 64(sp)
  lw t1, 68(sp)
  li t2, 400
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 72(sp)
  lw t0, 44(sp)
  sw t0, 76(sp)
  lw t0, 72(sp)
  lw t1, 76(sp)
  li t2, 40
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 80(sp)
  lw t0, 20(sp)
  sw t0, 84(sp)
  li t1, 100
  mul t0, t0, t1
  sw t0, 88(sp)
  lw t0, 32(sp)
  sw t0, 92(sp)
  li t1, 10
  mul t0, t0, t1
  sw t0, 96(sp)
  lw t0, 88(sp)
  lw t1, 96(sp)
  add t0, t0, t1
  sw t0, 100(sp)
  lw t0, 44(sp)
  sw t0, 104(sp)
  lw t0, 100(sp)
  lw t1, 104(sp)
  add t0, t0, t1
  sw t0, 108(sp)
  lw t1, 80(sp)
  sw t0, 0(t1)
  lw t0, 44(sp)
  sw t0, 112(sp)
  li t1, 1
  add t0, t0, t1
  sw t0, 116(sp)
  sw t0, 44(sp)
  j init_0_while_entry_6
init_0_while_exit_8:
  lw t0, 32(sp)
  sw t0, 120(sp)
  li t1, 1
  add t0, t0, t1
  sw t0, 124(sp)
  sw t0, 32(sp)
  j init_0_while_entry_10
init_0_while_exit_5:
  lw t0, 20(sp)
  sw t0, 128(sp)
  li t1, 1
  add t0, t0, t1
  sw t0, 132(sp)
  sw t0, 20(sp)
  j init_0_while_entry_12
init_0_while_exit_9:
  j init_0_exit_4
init_0_exit_4:
  addi sp, sp, 144
  ret
  .text
  .globl f1
f1:
  addi sp, sp, -256
f1_1_entry_2:
  li t0, 0
  sw t0, 44(sp)
  j f1_1_bb_4
f1_1_bb_4:
  mv t0, a0
  sw t0, 48(sp)
  mv t0, a1
  sw t0, 52(sp)
  mv t0, a2
  sw t0, 56(sp)
  mv t0, a3
  sw t0, 60(sp)
  mv t0, a4
  sw t0, 64(sp)
  mv t0, a5
  sw t0, 68(sp)
  mv t0, a6
  sw t0, 72(sp)
  mv t0, a7
  sw t0, 76(sp)
  lw t0, 256(sp)
  sw t0, 80(sp)
  lw t0, 260(sp)
  sw t0, 84(sp)
  lw t0, 48(sp)
  sw t0, 88(sp)
  li t1, 0
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 92(sp)
  lw t0, 52(sp)
  sw t0, 96(sp)
  li t1, 1
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 100(sp)
  lw t1, 92(sp)
  lw t0, 0(t1)
  sw t0, 104(sp)
  lw t1, 100(sp)
  lw t0, 0(t1)
  sw t0, 108(sp)
  lw t0, 104(sp)
  lw t1, 108(sp)
  add t0, t0, t1
  sw t0, 112(sp)
  lw t0, 56(sp)
  sw t0, 116(sp)
  li t1, 2
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 120(sp)
  mv t1, t0
  lw t0, 0(t1)
  sw t0, 124(sp)
  lw t0, 112(sp)
  lw t1, 124(sp)
  add t0, t0, t1
  sw t0, 128(sp)
  lw t0, 60(sp)
  sw t0, 132(sp)
  li t1, 3
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 136(sp)
  mv t1, t0
  lw t0, 0(t1)
  sw t0, 140(sp)
  lw t0, 128(sp)
  lw t1, 140(sp)
  add t0, t0, t1
  sw t0, 144(sp)
  lw t0, 64(sp)
  sw t0, 148(sp)
  li t1, 4
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 152(sp)
  mv t1, t0
  lw t0, 0(t1)
  sw t0, 156(sp)
  lw t0, 144(sp)
  lw t1, 156(sp)
  add t0, t0, t1
  sw t0, 160(sp)
  lw t0, 68(sp)
  sw t0, 164(sp)
  li t1, 5
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 168(sp)
  mv t1, t0
  lw t0, 0(t1)
  sw t0, 172(sp)
  lw t0, 160(sp)
  lw t1, 172(sp)
  add t0, t0, t1
  sw t0, 176(sp)
  lw t0, 72(sp)
  sw t0, 180(sp)
  li t1, 6
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 184(sp)
  mv t1, t0
  lw t0, 0(t1)
  sw t0, 188(sp)
  lw t0, 176(sp)
  lw t1, 188(sp)
  add t0, t0, t1
  sw t0, 192(sp)
  lw t0, 76(sp)
  sw t0, 196(sp)
  li t1, 7
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 200(sp)
  mv t1, t0
  lw t0, 0(t1)
  sw t0, 204(sp)
  lw t0, 192(sp)
  lw t1, 204(sp)
  add t0, t0, t1
  sw t0, 208(sp)
  lw t0, 80(sp)
  sw t0, 212(sp)
  li t1, 8
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 216(sp)
  mv t1, t0
  lw t0, 0(t1)
  sw t0, 220(sp)
  lw t0, 208(sp)
  lw t1, 220(sp)
  add t0, t0, t1
  sw t0, 224(sp)
  lw t0, 84(sp)
  sw t0, 228(sp)
  li t1, 9
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 232(sp)
  mv t1, t0
  lw t0, 0(t1)
  sw t0, 236(sp)
  lw t0, 224(sp)
  lw t1, 236(sp)
  add t0, t0, t1
  sw t0, 240(sp)
  sw t0, 44(sp)
  j f1_1_exit_3
f1_1_bb_1:
  j f1_1_exit_3
f1_1_exit_3:
  lw t0, 44(sp)
  sw t0, 244(sp)
  mv a0, t0
  addi sp, sp, 256
  ret
  .text
  .globl f2
f2:
  addi sp, sp, -256
f2_2_entry_1:
  li t0, 0
  sw t0, 44(sp)
  j f2_2_bb_4
f2_2_bb_4:
  mv t0, a0
  sw t0, 48(sp)
  mv t0, a1
  sw t0, 52(sp)
  mv t0, a2
  sw t0, 56(sp)
  mv t0, a3
  sw t0, 60(sp)
  mv t0, a4
  sw t0, 64(sp)
  mv t0, a5
  sw t0, 68(sp)
  mv t0, a6
  sw t0, 72(sp)
  mv t0, a7
  sw t0, 76(sp)
  lw t0, 256(sp)
  sw t0, 80(sp)
  lw t0, 260(sp)
  sw t0, 84(sp)
  lw t0, 48(sp)
  sw t0, 88(sp)
  li t1, 0
  li t2, 40
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 92(sp)
  li t1, 9
  li t2, 40
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 96(sp)
  lw t0, 52(sp)
  sw t0, 100(sp)
  li t1, 1
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 104(sp)
  lw t1, 96(sp)
  lw t0, 0(t1)
  sw t0, 108(sp)
  lw t1, 104(sp)
  lw t0, 0(t1)
  sw t0, 112(sp)
  lw t0, 108(sp)
  lw t1, 112(sp)
  add t0, t0, t1
  sw t0, 116(sp)
  lw t0, 56(sp)
  sw t0, 120(sp)
  lw t0, 116(sp)
  lw t1, 120(sp)
  add t0, t0, t1
  sw t0, 124(sp)
  lw t0, 60(sp)
  sw t0, 128(sp)
  li t1, 3
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 132(sp)
  mv t1, t0
  lw t0, 0(t1)
  sw t0, 136(sp)
  lw t0, 124(sp)
  lw t1, 136(sp)
  add t0, t0, t1
  sw t0, 140(sp)
  lw t0, 64(sp)
  sw t0, 144(sp)
  li t1, 4
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 148(sp)
  mv t1, t0
  lw t0, 0(t1)
  sw t0, 152(sp)
  lw t0, 140(sp)
  lw t1, 152(sp)
  add t0, t0, t1
  sw t0, 156(sp)
  lw t0, 68(sp)
  sw t0, 160(sp)
  li t1, 5
  li t2, 400
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 164(sp)
  li t1, 5
  li t2, 400
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 168(sp)
  li t1, 5
  li t2, 40
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 172(sp)
  mv t1, t0
  lw t0, 0(t1)
  sw t0, 176(sp)
  lw t0, 156(sp)
  lw t1, 176(sp)
  add t0, t0, t1
  sw t0, 180(sp)
  lw t0, 72(sp)
  sw t0, 184(sp)
  li t1, 6
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 188(sp)
  mv t1, t0
  lw t0, 0(t1)
  sw t0, 192(sp)
  lw t0, 180(sp)
  lw t1, 192(sp)
  add t0, t0, t1
  sw t0, 196(sp)
  lw t0, 76(sp)
  sw t0, 200(sp)
  li t1, 7
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 204(sp)
  mv t1, t0
  lw t0, 0(t1)
  sw t0, 208(sp)
  lw t0, 196(sp)
  lw t1, 208(sp)
  add t0, t0, t1
  sw t0, 212(sp)
  lw t0, 80(sp)
  sw t0, 216(sp)
  lw t0, 212(sp)
  lw t1, 216(sp)
  add t0, t0, t1
  sw t0, 220(sp)
  lw t0, 84(sp)
  sw t0, 224(sp)
  li t1, 9
  li t2, 40
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 228(sp)
  li t1, 8
  li t2, 40
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 232(sp)
  mv t1, t0
  lw t0, 0(t1)
  sw t0, 236(sp)
  lw t0, 220(sp)
  lw t1, 236(sp)
  add t0, t0, t1
  sw t0, 240(sp)
  sw t0, 44(sp)
  j f2_2_exit_2
f2_2_bb_3:
  j f2_2_exit_2
f2_2_exit_2:
  lw t0, 44(sp)
  sw t0, 244(sp)
  mv a0, t0
  addi sp, sp, 256
  ret
  .text
  .globl main
main:
  li t0, -4304
  add sp, sp, t0
  li t1, 4300
  add t1, sp, t1
  sw ra, 0(t1)
main_3_entry_1:
  li t0, 0
  li t1, 4016
  add t1, sp, t1
  sw t0, 0(t1)
  j main_3_bb_2
main_3_bb_2:
  li t0, 0
  li t1, 4020
  add t1, sp, t1
  sw t0, 0(t1)
  addi t0, sp, 12
  li t1, 0
  li t2, 400
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4024
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4024
  add t1, sp, t1
  lw a0, 0(t1)
  call init
  addi t0, sp, 12
  li t1, 0
  li t2, 400
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4028
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4028
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 0
  li t2, 40
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4032
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4032
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 0
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4036
  add t1, sp, t1
  sw t0, 0(t1)
  addi t0, sp, 12
  li t1, 1
  li t2, 400
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4040
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4040
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 1
  li t2, 40
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4044
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4044
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 0
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4048
  add t1, sp, t1
  sw t0, 0(t1)
  addi t0, sp, 12
  li t1, 2
  li t2, 400
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4052
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4052
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 2
  li t2, 40
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4056
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4056
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 0
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4060
  add t1, sp, t1
  sw t0, 0(t1)
  addi t0, sp, 12
  li t1, 3
  li t2, 400
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4064
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4064
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 3
  li t2, 40
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4068
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4068
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 0
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4072
  add t1, sp, t1
  sw t0, 0(t1)
  addi t0, sp, 12
  li t1, 4
  li t2, 400
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4076
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4076
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 4
  li t2, 40
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4080
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4080
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 0
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4084
  add t1, sp, t1
  sw t0, 0(t1)
  addi t0, sp, 12
  li t1, 5
  li t2, 400
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4088
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4088
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 5
  li t2, 40
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4092
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4092
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 0
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4096
  add t1, sp, t1
  sw t0, 0(t1)
  addi t0, sp, 12
  li t1, 6
  li t2, 400
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4100
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4100
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 6
  li t2, 40
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4104
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4104
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 0
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4108
  add t1, sp, t1
  sw t0, 0(t1)
  addi t0, sp, 12
  li t1, 7
  li t2, 400
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4112
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4112
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 7
  li t2, 40
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4116
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4116
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 0
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4120
  add t1, sp, t1
  sw t0, 0(t1)
  addi t0, sp, 12
  li t1, 8
  li t2, 400
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4124
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4124
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 8
  li t2, 40
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4128
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4128
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 0
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4132
  add t1, sp, t1
  sw t0, 0(t1)
  addi t0, sp, 12
  li t1, 9
  li t2, 400
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4136
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4136
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 9
  li t2, 40
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4140
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4140
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 0
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4144
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4036
  add t1, sp, t1
  lw a0, 0(t1)
  li t1, 4048
  add t1, sp, t1
  lw a1, 0(t1)
  li t1, 4060
  add t1, sp, t1
  lw a2, 0(t1)
  li t1, 4072
  add t1, sp, t1
  lw a3, 0(t1)
  li t1, 4084
  add t1, sp, t1
  lw a4, 0(t1)
  li t1, 4096
  add t1, sp, t1
  lw a5, 0(t1)
  li t1, 4108
  add t1, sp, t1
  lw a6, 0(t1)
  li t1, 4120
  add t1, sp, t1
  lw a7, 0(t1)
  li t1, 4132
  add t1, sp, t1
  lw t0, 0(t1)
  sw t0, 0(sp)
  li t1, 4144
  add t1, sp, t1
  lw t0, 0(t1)
  sw t0, 4(sp)
  call f1
  li t1, 4148
  add t1, sp, t1
  sw a0, 0(t1)
  li t1, 4020
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 4152
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4152
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 4148
  add t1, sp, t1
  lw t1, 0(t1)
  add t0, t0, t1
  li t1, 4156
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4156
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 4020
  add t1, sp, t1
  sw t0, 0(t1)
  addi t0, sp, 12
  li t1, 0
  li t2, 400
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4160
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4160
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 0
  li t2, 40
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4164
  add t1, sp, t1
  sw t0, 0(t1)
  addi t0, sp, 12
  li t1, 1
  li t2, 400
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4168
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4168
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 1
  li t2, 40
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4172
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4172
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 0
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4176
  add t1, sp, t1
  sw t0, 0(t1)
  addi t0, sp, 12
  li t1, 2
  li t2, 400
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4180
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4180
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 2
  li t2, 40
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4184
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4184
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 2
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4188
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4188
  add t1, sp, t1
  lw t1, 0(t1)
  lw t0, 0(t1)
  li t1, 4192
  add t1, sp, t1
  sw t0, 0(t1)
  addi t0, sp, 12
  li t1, 3
  li t2, 400
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4196
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4196
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 3
  li t2, 40
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4200
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4200
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 0
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4204
  add t1, sp, t1
  sw t0, 0(t1)
  addi t0, sp, 12
  li t1, 4
  li t2, 400
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4208
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4208
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 4
  li t2, 40
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4212
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4212
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 0
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4216
  add t1, sp, t1
  sw t0, 0(t1)
  addi t0, sp, 12
  li t1, 0
  li t2, 400
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4220
  add t1, sp, t1
  sw t0, 0(t1)
  addi t0, sp, 12
  li t1, 6
  li t2, 400
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4224
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4224
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 6
  li t2, 40
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4228
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4228
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 0
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4232
  add t1, sp, t1
  sw t0, 0(t1)
  addi t0, sp, 12
  li t1, 7
  li t2, 400
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4236
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4236
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 7
  li t2, 40
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4240
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4240
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 0
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4244
  add t1, sp, t1
  sw t0, 0(t1)
  addi t0, sp, 12
  li t1, 8
  li t2, 400
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4248
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4248
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 8
  li t2, 40
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4252
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4252
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 8
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4256
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4256
  add t1, sp, t1
  lw t1, 0(t1)
  lw t0, 0(t1)
  li t1, 4260
  add t1, sp, t1
  sw t0, 0(t1)
  addi t0, sp, 12
  li t1, 9
  li t2, 400
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4264
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4264
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 0
  li t2, 40
  mul t1, t1, t2
  add t0, t0, t1
  li t1, 4268
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4164
  add t1, sp, t1
  lw a0, 0(t1)
  li t1, 4176
  add t1, sp, t1
  lw a1, 0(t1)
  li t1, 4192
  add t1, sp, t1
  lw a2, 0(t1)
  li t1, 4204
  add t1, sp, t1
  lw a3, 0(t1)
  li t1, 4216
  add t1, sp, t1
  lw a4, 0(t1)
  li t1, 4220
  add t1, sp, t1
  lw a5, 0(t1)
  li t1, 4232
  add t1, sp, t1
  lw a6, 0(t1)
  li t1, 4244
  add t1, sp, t1
  lw a7, 0(t1)
  li t1, 4260
  add t1, sp, t1
  lw t0, 0(t1)
  sw t0, 0(sp)
  li t1, 4268
  add t1, sp, t1
  lw t0, 0(t1)
  sw t0, 4(sp)
  call f2
  li t1, 4272
  add t1, sp, t1
  sw a0, 0(t1)
  li t1, 4020
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 4276
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4276
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 4272
  add t1, sp, t1
  lw t1, 0(t1)
  add t0, t0, t1
  li t1, 4280
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4280
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 4020
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4020
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 4284
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4284
  add t1, sp, t1
  lw a0, 0(t1)
  call putint
  li a0, 10
  call putch
  li t0, 0
  li t1, 4016
  add t1, sp, t1
  sw t0, 0(t1)
  j main_3_exit_4
main_3_bb_3:
  j main_3_exit_4
main_3_exit_4:
  li t1, 4016
  add t1, sp, t1
  lw t0, 0(t1)
  li t1, 4288
  add t1, sp, t1
  sw t0, 0(t1)
  li t1, 4288
  add t1, sp, t1
  lw a0, 0(t1)
  li t1, 4300
  add t1, sp, t1
  lw ra, 0(t1)
  li t0, 4304
  add sp, sp, t0
  ret
