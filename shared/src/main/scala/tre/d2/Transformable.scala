package tre.d2

abstract trait Transformable[T] {
  def transform(matrix: Matrix23): T

  def mirrorX(): T =
    transform(Matrix23.mirroringX)
  def mirrorY(): T =
    transform(Matrix23.mirroringY)
  def rotate(angle: Double): T =
    transform(Matrix23.rotating(angle))
  def translate(x: Double, y: Double): T =
    transform(Matrix23.translating(x, y))
  def translate(vector: Point): T =
    translate(vector.x, vector.y)
  def translateX(x: Double): T =
    translate(x, 0)
  def translateY(y: Double): T =
    translate(0, y)
  def scale(x: Double, y: Double): T =
    transform(Matrix23.scaling(x, y))
  def scale(value: Double): T =
    scale(value, value)
  def scaleX(value: Double): T =
    scale(value, 1)
  def scaleY(value: Double): T =
    scale(1, value)
}
