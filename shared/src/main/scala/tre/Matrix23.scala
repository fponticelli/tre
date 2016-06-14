package tre

case class Matrix23(a: Double, b: Double, c: Double, d: Double, e: Double, f: Double) {
  def *(that: Matrix23): Matrix23 =
    Matrix23(
      a * that.a + c * that.b,
      b * that.a + d * that.b,
      a * that.c + c * that.d,
      b * that.c + d * that.d,
      a * that.e + c * that.f + e,
      b * that.e + d * that.f + f
    )

  def mirrorX(): Matrix23 = this * Matrix23.mirroringX
  def mirrorY(): Matrix23 = this * Matrix23.mirroringY

  def rotate(vector: tre.d2.Point): Matrix23 =
    rotate(Math.atan2(vector.y, vector.x))
  def rotate(angle: Double): Matrix23 =
    this * Matrix23.rotating(angle)

  def scale(x: Double, y: Double): Matrix23 =
    this * Matrix23.scaling(x, y)
  def scale(v: Double): Matrix23 =
    scale(v, v)

  def skewX(angle: Double): Matrix23 =
    this * Matrix23.skewingX(angle)
  def skewY(angle: Double): Matrix23 =
    this * Matrix23.skewingY(angle)

  def translate(vector: tre.d2.Point): Matrix23 =
    translate(vector.x, vector.y)
  def translate(x: Double, y: Double): Matrix23 =
    this * Matrix23.translating(x, y)
}

object Matrix23 {
  val identity = Matrix23( 1,  0,  0,  0,  1,  0)
  val mirroringX = Matrix23(-1,  0,  0,  1,  0,  0)
  val mirroringY = Matrix23( 1,  0,  0, -1,  0,  0)
  def rotating(angle: Double): Matrix23 = {
    val c = Math.cos(angle)
    val s = Math.sin(angle)
    Matrix23(c, s, -s, -c, 0, 0)
  }
  def scaling(x: Double, y: Double): Matrix23 =
    Matrix23(x, 0, 0, y, 0, 0)
  def translating(x: Double, y: Double): Matrix23 =
    Matrix23(1, 0, 0, 1, x, y)
  def skewingX(angle: Double): Matrix23 =
    Matrix23(1, 0, Math.tan(angle), 1, 0, 0)
  def skewingY(angle: Double): Matrix23 =
    Matrix23(1, Math.tan(angle), 0, 1, 0, 0)
}
