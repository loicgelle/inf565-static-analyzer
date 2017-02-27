class A {
  static int x, y;
  static boolean b1, b2;

  static void f() {
    x = 4;
    while (x < 6) {
      y = 2;
      x = x + 1;
    }
  }
}
