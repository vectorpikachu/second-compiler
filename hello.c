int ga[3], gb[2][3] = {5, 6, 7, 8};

int main() {
  int a[3], b[2][3] = {1, 2, 3, 4}, sum = 0;
  a[0] = 5;
  sum = b[1][2] + a[0];
  sum = sum + ga[0] + gb[1][2];
  return sum;
}
