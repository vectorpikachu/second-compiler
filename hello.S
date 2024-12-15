  .text
  .globl main
main:
entry:
  mv a0, x
  j 1
bb2:
  mv a0, x
  j 1
bb1:
  j 1
exit:
  mv a0, x
  li a0, 0
  ret
