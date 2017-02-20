class A {
  static int x, y;
  static boolean b;

  static void f() {
    x = 3;
    if (x < 4) {
      x = 4;
      if (x < 4) {
        x = 2;
      }
    }
  }
}
