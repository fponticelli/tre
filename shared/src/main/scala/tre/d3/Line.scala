package tre.d3

import tre.Precision._

class Line(val point: Point, val direction: Point) extends Transformable[Line] {
  def transform(matrix: Matrix44): Line = {
    val newpoint = point.transform(matrix)
    val newdirection = (point + direction).transform(matrix) - newpoint
    Line(newpoint, newdirection)
  }

  def intersectWithPlane(plane: Plane): Point = {
    val lambda = (plane.w - plane.normal.dot(this.point)) / plane.normal.dot(direction)
    point + direction * lambda
  }

  def reverse(): Line =
    Line(point, -direction)
}

object Line {
  def apply(point: Point, direction: Point): Line =
    new Line(point, direction.normalize)

  def fromPlanes(p1: Plane, p2: Plane): Option[Line] = {
    val direction = p1.normal.cross(p2.normal)
    if(direction.length =~ 0)
      None
    else {
      val mabsx = direction.x.abs
      val mabsy = direction.y.abs
      val mabsz = direction.z.abs

      val origin = if((mabsx >= mabsy) && (mabsx >= mabsz)) {
        // direction vector is mostly pointing towards x
        // find a point p for which x is zero:
        tre.d2.Point.solve2Linear(p1.normal.y, p1.normal.z, p2.normal.y, p2.normal.z, p1.w, p2.w)
          .map(r => Point(0, r.x, r.y))
      } else if((mabsy >= mabsx) && (mabsy >= mabsz)) {
        // find a point p for which y is zero:
        tre.d2.Point.solve2Linear(p1.normal.x, p1.normal.z, p2.normal.x, p2.normal.z, p1.w, p2.w)
          .map(r => Point(r.x, 0, r.y))
      } else {
        // find a point p for which z is zero:
        tre.d2.Point.solve2Linear(p1.normal.x, p1.normal.y, p2.normal.x, p2.normal.y, p1.w, p2.w)
          .map(r => Point(r.x, r.y, 0))
      }
      origin.map { Line(_, direction) }
    }
  }

  def fromPoints(p1: Point, p2: Point): Line =
    Line(p1, (p2 - p1))
}
