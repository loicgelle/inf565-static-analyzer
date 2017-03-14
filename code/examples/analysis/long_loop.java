class A {
  static int x, y;
  static boolean b1, b2;

  static void main() {
    x = 1;
    y = 1;
    while (x < 100) {
      x = x + 1;
      y = y * 2;
    }
    if (y < 1) {
      b1 = true;
      b2 = false;
    } else {
      b1 = false;
      b2 = true;
    }
  }
}
