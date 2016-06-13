package tre.d2

import tre.Precision._;
import Math._;

case class Point(x: Double, y: Double) {
  lazy val length = sqrt(lengthSquared)
  lazy val lengthSquared = x * x + y * y

  def abs() = Point(Math.abs(x), Math.abs(y))
  def interpolate(that: Point)(d: Double) = this + (that - this) * d
  def isZero() = this == Point.zero
  def normalize() = this / length

  def distance(that: Point) = Math.abs((this - that).length)
  def distanceSquared(that: Point) = Math.abs((this - that).lengthSquared)

  def min(that : Point) = Point(
    Math.min(x, that.x),
    Math.min(y, that.y)
  )

  def max(that : Point) = Point(
    Math.max(x, that.x),
    Math.max(y, that.y)
  )

  def to3DPoint(z: Double) = tre.d3.Point(x, y, z)

  def + (that: Point) = Point(x + that.x, y + that.y)
  def + (d: Double) = Point(x + d, y + d)
  def - (that: Point) = this + -that;
  def - (d: Double) = Point(x - d, y - d)
  def * (that: Point) = Point(x * that.x, y * that.y)
  def * (d: Double) = Point(x * d, y * d)
  def dot (that: Point) = x * that.x + y * that.y
  def / (that: Point) = Point(x / that.x, y / that.y)
  def / (d: Double) = Point(x / d, y / d)
  def unary_- () = Point(-x, -y)
  def ~= (that: Point) = x =~ that.x && y =~ that.y
  override def toString() = s"($x,$y)"
}

object Point {
  val zero = Point(0, 0)
  implicit def tuple2point(t : (Double, Double)) = Point(t._1, t._2)
  implicit def point2tuple(p : Point) = (p.x, p.y)
}
/*
  cross product

  public function nearEquals(p : Point)
    return Math.abs(x - p.x) <= Floats.EPSILON && Math.abs(y - p.y) <= Floats.EPSILON && Math.abs(z - p.z) <= Floats.EPSILON;

  inline public function notNearEquals(p : Point)
    return !nearEquals(p);

  public function isNearZero()
    return nearEquals(zero);

  // Returns a new Point3D
  inline public function transform(matrix : Matrix44)
    return matrix.leftMultiplyPoint3D((this : Point));

  // find a vector that is somewhat perpendicular to this one
  public function randomNonParallelVector() : Point {
    var a = abs();
    if((a.x <= a.y) && (a.x <= a.z)) {
      return Point.create(1, 0, 0);
    } else if((a.y <= a.x) && (a.y <= a.z)) {
      return Point.create(0, 1, 0);
    } else {
      return Point.create(0, 0, 1);
    }
  }

/*
  public function isOnLine(line : Line3D) : Bool {
    if(line.isHorizontal)
      return Floats.nearEquals(y, line.w);
    return Floats.nearEquals(line.xAtY(y), x);
  }
*/
 */
