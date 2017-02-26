class A {
  static int x, y;
  static boolean b1, b2;

  static void f() {
    x = Support.random(0, 3);
    y = 3;
    x = x + y;
    if (x + y < 7) {
      b1 = true;
    } else {
      b1 = false;
    }
  }
}
