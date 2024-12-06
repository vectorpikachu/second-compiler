  .text
  .globl main
main:
  addi sp, sp, -352
entry:
  li t0, 1
  sw t0, 0(sp)
  li t0, 0
  sw t0, 4(sp)
  lw t0, 0(sp)
  sw t0, 8(sp)
  lw t0, 8(sp)
  li t1, 0
  sub t1, t0, t1
  snez t1, t1
  sw t1, 12(sp)
  lw t0, 12(sp)
  bnez t0, then1
  j short_circuit8
then1:
  li t0, 1
  sw t0, 4(sp)
  li t0, 0
  sw t0, 0(sp)
  j end1
short_circuit8:
  lw t0, 4(sp)
  sw t0, 16(sp)
  lw t0, 16(sp)
  li t1, 0
  sub t1, t0, t1
  snez t1, t1
  sw t1, 20(sp)
  lw t0, 20(sp)
  li t1, 0
  sub t1, t0, t1
  snez t1, t1
  sw t1, 24(sp)
  lw t0, 24(sp)
  bnez t0, then1
  j short_circuit7
end1:
  lw t0, 0(sp)
  sw t0, 28(sp)
  lw t0, 28(sp)
  li t1, 0
  sub t1, t0, t1
  snez t1, t1
  sw t1, 32(sp)
  lw t0, 32(sp)
  bnez t0, short_circuit11
  j end2
short_circuit7:
  li t0, 0
  li t1, 1
  add t1, t0, t1
  sw t1, 36(sp)
  lw t0, 36(sp)
  li t1, 0
  sub t1, t0, t1
  snez t1, t1
  sw t1, 40(sp)
  lw t0, 40(sp)
  li t1, 0
  sub t1, t0, t1
  snez t1, t1
  sw t1, 44(sp)
  lw t0, 44(sp)
  bnez t0, then1
  j short_circuit6
short_circuit11:
  lw t0, 4(sp)
  sw t0, 48(sp)
  lw t0, 48(sp)
  li t1, 0
  sub t1, t0, t1
  snez t1, t1
  sw t1, 52(sp)
  lw t0, 52(sp)
  li t1, 0
  sub t1, t0, t1
  snez t1, t1
  sw t1, 56(sp)
  lw t0, 56(sp)
  bnez t0, short_circuit10
  j end2
end2:
  li t0, 2
  sw t0, 60(sp)
  li t0, 5
  sw t0, 64(sp)
  li t0, 7
  sw t0, 68(sp)
  lw t0, 60(sp)
  sw t0, 72(sp)
  lw t0, 64(sp)
  sw t0, 76(sp)
  lw t0, 72(sp)
  lw t1, 76(sp)
  slt t1, t0, t1
  sw t1, 80(sp)
  lw t0, 80(sp)
  li t1, 0
  sub t1, t0, t1
  snez t1, t1
  sw t1, 84(sp)
  lw t0, 84(sp)
  bnez t0, short_circuit15
  j short_circuit14
short_circuit6:
  lw t0, 0(sp)
  sw t0, 88(sp)
  lw t0, 88(sp)
  li t1, 0
  sub t1, t0, t1
  seqz t1, t1
  sw t1, 92(sp)
  lw t0, 92(sp)
  li t1, 0
  sub t1, t0, t1
  snez t1, t1
  sw t1, 96(sp)
  lw t0, 96(sp)
  li t1, 0
  sub t1, t0, t1
  snez t1, t1
  sw t1, 100(sp)
  lw t0, 100(sp)
  bnez t0, then1
  j short_circuit5
short_circuit10:
  li t0, 1
  li t1, 0
  sub t1, t0, t1
  snez t1, t1
  sw t1, 104(sp)
  lw t0, 104(sp)
  li t1, 0
  sub t1, t0, t1
  snez t1, t1
  sw t1, 108(sp)
  lw t0, 108(sp)
  bnez t0, short_circuit9
  j end2
short_circuit15:
  lw t0, 64(sp)
  sw t0, 112(sp)
  lw t0, 68(sp)
  sw t0, 116(sp)
  lw t0, 112(sp)
  lw t1, 116(sp)
  slt t1, t0, t1
  sw t1, 120(sp)
  lw t0, 120(sp)
  li t1, 0
  sub t1, t0, t1
  snez t1, t1
  sw t1, 124(sp)
  lw t0, 124(sp)
  li t1, 0
  sub t1, t0, t1
  snez t1, t1
  sw t1, 128(sp)
  lw t0, 128(sp)
  bnez t0, then3
  j short_circuit14
short_circuit14:
  lw t0, 64(sp)
  sw t0, 132(sp)
  lw t0, 0(sp)
  sw t0, 136(sp)
  lw t0, 132(sp)
  lw t1, 136(sp)
  sgt t1, t0, t1
  sw t1, 140(sp)
  lw t0, 140(sp)
  li t1, 0
  sub t1, t0, t1
  snez t1, t1
  sw t1, 144(sp)
  lw t0, 144(sp)
  li t1, 0
  sub t1, t0, t1
  snez t1, t1
  sw t1, 148(sp)
  lw t0, 148(sp)
  bnez t0, then3
  j short_circuit13
short_circuit5:
  lw t0, 4(sp)
  sw t0, 152(sp)
  lw t0, 152(sp)
  li t1, 0
  sub t1, t0, t1
  seqz t1, t1
  sw t1, 156(sp)
  lw t0, 156(sp)
  li t1, 0
  sub t1, t0, t1
  snez t1, t1
  sw t1, 160(sp)
  lw t0, 160(sp)
  li t1, 0
  sub t1, t0, t1
  snez t1, t1
  sw t1, 164(sp)
  lw t0, 164(sp)
  bnez t0, then1
  j short_circuit4
short_circuit9:
  lw t0, 0(sp)
  sw t0, 168(sp)
  lw t0, 4(sp)
  sw t0, 172(sp)
  lw t0, 168(sp)
  lw t1, 172(sp)
  sub t1, t0, t1
  seqz t1, t1
  sw t1, 176(sp)
  lw t0, 176(sp)
  li t1, 0
  sub t1, t0, t1
  snez t1, t1
  sw t1, 180(sp)
  lw t0, 180(sp)
  bnez t0, then2
  j end2
then3:
  li a0, 5
  addi sp, sp, 352
  ret
short_circuit13:
  lw t0, 4(sp)
  sw t0, 184(sp)
  lw t0, 60(sp)
  sw t0, 188(sp)
  lw t0, 184(sp)
  lw t1, 188(sp)
  slt t1, t0, t1
  sw t1, 192(sp)
  lw t0, 192(sp)
  li t1, 0
  sub t1, t0, t1
  snez t1, t1
  sw t1, 196(sp)
  lw t0, 196(sp)
  li t1, 0
  sub t1, t0, t1
  snez t1, t1
  sw t1, 200(sp)
  lw t0, 200(sp)
  bnez t0, then3
  j short_circuit12
short_circuit4:
  lw t0, 0(sp)
  sw t0, 204(sp)
  lw t0, 4(sp)
  sw t0, 208(sp)
  lw t0, 208(sp)
  lw t1, 204(sp)
  add t1, t0, t1
  sw t1, 212(sp)
  lw t0, 212(sp)
  li t1, 0
  sub t1, t0, t1
  snez t1, t1
  sw t1, 216(sp)
  lw t0, 216(sp)
  li t1, 0
  sub t1, t0, t1
  snez t1, t1
  sw t1, 220(sp)
  lw t0, 220(sp)
  bnez t0, then1
  j short_circuit3
then2:
  li a0, 0
  addi sp, sp, 352
  ret
short_circuit12:
  lw t0, 4(sp)
  sw t0, 224(sp)
  lw t0, 0(sp)
  sw t0, 228(sp)
  lw t0, 228(sp)
  lw t1, 224(sp)
  add t1, t0, t1
  sw t1, 232(sp)
  lw t0, 60(sp)
  sw t0, 236(sp)
  lw t0, 232(sp)
  lw t1, 236(sp)
  slt t1, t0, t1
  sw t1, 240(sp)
  lw t0, 240(sp)
  li t1, 0
  sub t1, t0, t1
  snez t1, t1
  sw t1, 244(sp)
  lw t0, 244(sp)
  bnez t0, then3
  j end3
short_circuit3:
  lw t0, 0(sp)
  sw t0, 248(sp)
  lw t0, 4(sp)
  sw t0, 252(sp)
  lw t0, 248(sp)
  lw t1, 252(sp)
  mul t1, t0, t1
  sw t1, 256(sp)
  lw t0, 256(sp)
  li t1, 0
  sub t1, t0, t1
  snez t1, t1
  sw t1, 260(sp)
  lw t0, 260(sp)
  li t1, 0
  sub t1, t0, t1
  snez t1, t1
  sw t1, 264(sp)
  lw t0, 264(sp)
  bnez t0, then1
  j short_circuit2
end3:
  li t0, 6
  li t1, 6
  sub t1, t0, t1
  seqz t1, t1
  sw t1, 268(sp)
  lw t0, 268(sp)
  li t1, 0
  sub t1, t0, t1
  snez t1, t1
  sw t1, 272(sp)
  lw t0, 272(sp)
  bnez t0, then4
  j short_circuit17
short_circuit2:
  lw t0, 4(sp)
  sw t0, 276(sp)
  lw t0, 0(sp)
  sw t0, 280(sp)
  lw t0, 276(sp)
  lw t1, 280(sp)
  div t1, t0, t1
  sw t1, 284(sp)
  lw t0, 284(sp)
  li t1, 0
  sub t1, t0, t1
  snez t1, t1
  sw t1, 288(sp)
  lw t0, 288(sp)
  li t1, 0
  sub t1, t0, t1
  snez t1, t1
  sw t1, 292(sp)
  lw t0, 292(sp)
  bnez t0, then1
  j short_circuit1
then4:
  lw t0, 60(sp)
  sw t0, 296(sp)
  lw t0, 296(sp)
  li t1, 5
  add t1, t0, t1
  sw t1, 300(sp)
  lw t0, 300(sp)
  sw t0, 60(sp)
  lw t0, 60(sp)
  sw t0, 304(sp)
  lw a0, 304(sp)
  addi sp, sp, 352
  ret
short_circuit17:
  lw t0, 64(sp)
  sw t0, 308(sp)
  lw t0, 308(sp)
  li t1, 7
  sub t1, t0, t1
  seqz t1, t1
  sw t1, 312(sp)
  lw t0, 312(sp)
  li t1, 0
  sub t1, t0, t1
  snez t1, t1
  sw t1, 316(sp)
  lw t0, 316(sp)
  li t1, 0
  sub t1, t0, t1
  snez t1, t1
  sw t1, 320(sp)
  lw t0, 320(sp)
  bnez t0, then4
  j short_circuit16
short_circuit1:
  lw t0, 0(sp)
  sw t0, 324(sp)
  lw t0, 4(sp)
  sw t0, 328(sp)
  lw t0, 328(sp)
  lw t1, 324(sp)
  sub t1, t0, t1
  sw t1, 332(sp)
  lw t0, 332(sp)
  li t1, 0
  sub t1, t0, t1
  snez t1, t1
  sw t1, 336(sp)
  lw t0, 336(sp)
  bnez t0, then1
  j end1
short_circuit16:
  li t0, 6
  li t1, 0
  sub t1, t0, t1
  snez t1, t1
  sw t1, 340(sp)
  lw t0, 340(sp)
  bnez t0, then4
  j end4
end4:
  li a0, 77
  addi sp, sp, 352
  ret
