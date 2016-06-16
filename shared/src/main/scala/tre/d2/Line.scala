package tre.d2

class Line(val normal: Point, val w: Double) extends Transformable[Line] {
  def isHorizontal(): Boolean =
    normal.x == 0
  def isVertical(): Boolean =
    normal.y == 0
  def offset(value: Double): Line =
    Line(normal, w + value)
  def reverse(): Line =
    Line(-normal, -w)
  def origin(): Point =
    normal * w
  def direction(): Point =
    normal.normal()
  def xAtY(y: Double): Double =
    (w - normal.y * y) / normal.x
  def absDistanceToPoint(point: Point): Double =
    Math.abs(point.dot(normal) - w)
  def intersectionLine(line: Line): Option[Point] =
    Point.solve2Linear(normal.x, normal.y, line.normal.x, line.normal.y, w, line.w)

  def transform(matrix: Matrix23): Line = {
    val pointOnLine = normal * w
    val neworigin = Point.zero.transform(matrix)
    val neworiginPlusNormal = normal.transform(matrix)
    val newnormal = neworiginPlusNormal - neworigin
    val newpointOnLine = pointOnLine.transform(matrix)
    val neww = newnormal.dot(newpointOnLine)
    Line(newnormal, neww)
  }
}

object Line {
  def apply(normal: Point, w: Double): Line = {
    val l = normal.length
    Line(normal / l, w * l)
  }

  def apply(p1: Point, p2: Point): Line = {
    val direction = p2 - p1
    val normal = (-direction.normal()).normalize()
    val w= p1.dot(normal)
    apply(normal, w)
  }
}
