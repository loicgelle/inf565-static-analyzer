class A {
  static int x;
  static boolean b1, b2;

  static void main() {
    x = Support.random(0, 6);
    if (x < 3) {
      b1 = false;
    } else {
      b1 = true;
    }
    if (b1) {
      b2 = false;
    }
  }
}
