  .text
  .globl f
f:
  addi sp, sp, -16
fentry2:
  j fbb3
fbb3:
  li t0, 0
  sw t0, 4(sp)
  j fexit1
fbb4:
  j fexit1
fexit1:
  lw t0, 4(sp)
  sw t0, 8(sp)
  lw a0, 8(sp)
  addi sp, sp, 16
  ret

  .text
  .globl main
main:
  addi sp, sp, -16
  sw ra, 12(sp)
mainentry1:
  j mainbb3
mainbb3:
  call f
  sw a0, 4(sp)
  lw t0, 4(sp)
  sw t0, 8(sp)
  j mainexit2
mainbb4:
  j mainexit2
mainexit2:
  lw t0, 8(sp)
  sw t0, 12(sp)
  lw a0, 12(sp)
  lw ra, 16(sp)
  addi sp, sp, 16
  ret

