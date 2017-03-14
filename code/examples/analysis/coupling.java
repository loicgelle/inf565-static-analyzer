class A {
  static int x, y;
  static boolean b1, b2;

  static void main() {
    x = 1;
    while (x < 100) {
      x = x + 4;
    }
    y = 1;
    if (x < 4) {
      x = x + 1;
    } else {
      x = 2;
    }
    if (x == 2) {
      b1 = true;
    } else {
      b1 = false;
    }
  }
}
