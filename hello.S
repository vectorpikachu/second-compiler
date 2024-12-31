  .data
  .globl ga
ga:
  .zero 12

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
  addi sp, sp, -160
main_0_entry_1:
  j main_0_bb_4
main_0_bb_4:
  addi t0, sp, 4
  li t1, 0
  li t2, 12
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 44(sp)
  lw t0, 44(sp)
  li t1, 0
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 48(sp)
  li t0, 1
  lw t1, 48(sp)
  sw t0, 0(t1)
  lw t0, 44(sp)
  li t1, 1
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 52(sp)
  li t0, 2
  lw t1, 52(sp)
  sw t0, 0(t1)
  lw t0, 44(sp)
  li t1, 2
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 56(sp)
  li t0, 3
  lw t1, 56(sp)
  sw t0, 0(t1)
  addi t0, sp, 4
  li t1, 1
  li t2, 12
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 60(sp)
  lw t0, 60(sp)
  li t1, 0
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 64(sp)
  li t0, 4
  lw t1, 64(sp)
  sw t0, 0(t1)
  lw t0, 60(sp)
  li t1, 1
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 68(sp)
  li t0, 0
  lw t1, 68(sp)
  sw t0, 0(t1)
  lw t0, 60(sp)
  li t1, 2
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 72(sp)
  li t0, 0
  lw t1, 72(sp)
  sw t0, 0(t1)
  li t0, 0
  sw t0, 76(sp)
  addi t0, sp, 28
  li t1, 0
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 80(sp)
  li t0, 5
  lw t1, 80(sp)
  sw t0, 0(t1)
  addi t0, sp, 4
  li t1, 1
  li t2, 12
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 84(sp)
  lw t0, 84(sp)
  li t1, 2
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 88(sp)
  addi t0, sp, 28
  li t1, 0
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 92(sp)
  lw t1, 88(sp)
  lw t0, 0(t1)
  sw t0, 96(sp)
  lw t1, 92(sp)
  lw t0, 0(t1)
  sw t0, 100(sp)
  lw t0, 96(sp)
  lw t1, 100(sp)
  add t0, t0, t1
  sw t0, 104(sp)
  lw t0, 104(sp)
  sw t0, 76(sp)
  la t0, ga
  li t1, 0
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 108(sp)
  lw t0, 76(sp)
  sw t0, 112(sp)
  lw t1, 108(sp)
  lw t0, 0(t1)
  sw t0, 116(sp)
  lw t0, 112(sp)
  lw t1, 116(sp)
  add t0, t0, t1
  sw t0, 120(sp)
  la t0, gb
  li t1, 1
  li t2, 12
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 124(sp)
  lw t0, 124(sp)
  li t1, 2
  li t2, 4
  mul t1, t1, t2
  add t0, t0, t1
  sw t0, 128(sp)
  lw t1, 128(sp)
  lw t0, 0(t1)
  sw t0, 132(sp)
  lw t0, 120(sp)
  lw t1, 132(sp)
  add t0, t0, t1
  sw t0, 136(sp)
  lw t0, 136(sp)
  sw t0, 76(sp)
  lw t0, 76(sp)
  sw t0, 140(sp)
  lw t0, 140(sp)
  sw t0, 144(sp)
  j main_0_exit_2
main_0_bb_3:
  j main_0_exit_2
main_0_exit_2:
  lw t0, 144(sp)
  sw t0, 148(sp)
  lw a0, 148(sp)
  addi sp, sp, 160
  ret

