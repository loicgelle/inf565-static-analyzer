class A {
  static int x, y;
  static boolean b1, b2;

  static void f() {
    x = 1;
    while (x < 100) {
      x = x + 4;
    }
    if (x == 3) {
      b1 = true;
    } else {
      b1 = false;
    }
  }
}
