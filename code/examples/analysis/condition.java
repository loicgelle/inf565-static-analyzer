class A {
  static int x, y;
  static boolean b1, b2;

  static void main() {
    x = Support.random(0, 4);
    if (x < 2) {
      x = x + 1;
    } else {
      x = x - 1;
    }
  }
}
