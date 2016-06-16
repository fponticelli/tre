package tre.d2

import tre.Matrix23
import tre.Precision._
import Math._

case class Point(x: Double, y: Double) extends Transformable[Point] {
  lazy val length = sqrt(lengthSquared)
  lazy val lengthSquared = x * x + y * y

  def abs() = Point(Math.abs(x), Math.abs(y))
  def interpolate(that: Point)(d: Double) = this + (that - this) * d
  def isZero() = this == Point.zero
  def normal() = Point(y, -x)
  def normalize() = this / length

  def distance(that: Point) = Math.abs((this - that).length)
  def distanceSquared(that: Point) = Math.abs((this - that).lengthSquared)

  def min(that: Point) = Point(
    Math.min(x, that.x),
    Math.min(y, that.y)
  )

  def max(that: Point) = Point(
    Math.max(x, that.x),
    Math.max(y, that.y)
  )

  def transform(matrix: Matrix23): Point =
    matrix.leftMultiplyPoint(this)

  def to3DPoint(z: Double) = tre.d3.Point(x, y, z)

  def + (that: Point) = Point(x + that.x, y + that.y)
  def + (d: Double) = Point(x + d, y + d)
  def - (that: Point) = this + -that
  def - (d: Double) = Point(x - d, y - d)
  def * (that: Point) = Point(x * that.x, y * that.y)
  def * (d: Double) = Point(x * d, y * d)
  def dot (that: Point) = x * that.x + y * that.y
  def / (that: Point) = Point(x / that.x, y / that.y)
  def / (d: Double) = Point(x / d, y / d)
  def unary_- () = Point(-x, -y)
  def ~= (that: Point) = x =~ that.x && y =~ that.y

  def isOnline(line: Line): Boolean =
    if(line.isHorizontal)
      y =~ line.w
    else
      line.xAtY(y) =~ x

  override def toString() = s"($x,$y)"
}

object Point {
  val zero = Point(0, 0)
  implicit def tuple2point(t: (Double, Double)) = Point(t._1, t._2)
  implicit def point2tuple(p: Point) = (p.x, p.y)

  def solve2Linear(a: Double, b: Double, c: Double, d: Double, u: Double, v: Double): Option[Point] = {
    val det = a * d - b * c
    if(det == 0)
      None
    else {
      val invdet = 1.0 / det
      val x =  u * d - b * v
      val y = -u * c + a * v
      Some(Point(x * invdet, y * invdet))
    }
  }
}
