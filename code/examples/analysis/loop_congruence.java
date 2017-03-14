class A {
  static int x, y;
  static boolean b1, b2;

  static void main() {
    x = 0;
    y = 1;
    while (y < 1000) {
      x = x + 2;
      y = y + 1;
    }
    if (x == 3) {
      b1 = true;
    } else {
      b1 = false;
    }
  }
}
