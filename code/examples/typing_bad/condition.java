class A {
  static int x;
  static boolean b1, b2;

  static void main() {
    x = 1;
    b1 = true;
    if (x < b1) {
      x = 1;
    }
  }
}
