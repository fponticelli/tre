package tre.d3

abstract trait Transformable[T] {
  def transform(matrix: Matrix44): T

  def mirrorXY(): T =
    transform(Matrix44.mirroringXY)
  def mirrorXZ(): T =
    transform(Matrix44.mirroringXZ)
  def mirrorYZ(): T =
    transform(Matrix44.mirroringYZ)
  def rotateX(angle: Double): T =
    transform(Matrix44.rotatingX(angle))
  def rotateY(angle: Double): T =
    transform(Matrix44.rotatingY(angle))
  def rotateZ(angle: Double): T =
    transform(Matrix44.rotatingZ(angle))
  def translate(x: Double, y: Double, z: Double): T =
    transform(Matrix44.translating(x, y, z))
  def translate(vector: Point): T =
    translate(vector.x, vector.y, vector.z)
  def translateX(x: Double): T =
    translate(x, 0, 0)
  def translateY(y: Double): T =
    translate(0, y, 0)
  def translateZ(z: Double): T =
    translate(0, 0, z)
  def scale(x: Double, y: Double, z: Double): T =
    transform(Matrix44.scaling(x, y, z))
  def scale(value: Double): T =
    scale(value, value, value)
  def scaleX(value: Double): T =
    scale(value, 1, 1)
  def scaleY(value: Double): T =
    scale(1, value, 1)
  def scaleZ(value: Double): T =
    scale(1, 1, value)
}
