class A {
  static int x, y;
  static boolean b;

  static void f() {
    x = 3;
    while (x < 10) {
      x = x + 2;
      if (x < 7) {
        y = 7;
      }
    }
  }
}
