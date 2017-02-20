class A {
  static int x, y;
  static boolean b;

  static void f() {
    x = Support.random(0, 1);
    if (x < 4) {
      x = 2;
      y = 3;
    } else {
      x = 2;
    }
    while (x < 7) {
      x = x + 2;
    }
  }
}
