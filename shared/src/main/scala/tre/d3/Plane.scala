package tre.d3

import tre.Precision._
import scala.collection.mutable

case class Plane(normal : Point, w : Double) extends Transformable[Plane] {
  def flip() = Plane(-normal, -w)

  private val COPLANAR = 0
  private val FRONT = 1
  private val BACK = 2
  private val SPANNING = 3

  def splitPolygon(polygon: Polygon): Vector[SplitPolygon] = {
    val (types, polygonType) = polygon.foldLeft((Vector[Int](), COPLANAR)){
      (acc: (Vector[Int], Int), vertex: Vertex) =>
        val t = normal.dot(vertex.position) - w
        val polygonType = if(t <~ 0) BACK else if(t >~ 0) FRONT else COPLANAR
        val list: Vector[Int] = acc._1 :+ polygonType
        (list, acc._2 | polygonType)
    }

    if(polygonType == COPLANAR) {
      if(normal.dot(polygon.plane.normal) > 0)
        Vector(CoplanarFront(polygon))
      else
        Vector(CoplanarBack(polygon))
    } else if(polygonType == FRONT) {
      Vector(Front(polygon))
    } else if(polygonType == BACK) {
      Vector(Back(polygon))
    } else { // SPANNING
      val len = polygon.vertices.length
      val range = 0 until len
      val (front: Vector[Vertex], back: Vector[Vertex]) = range.foldLeft((Vector[Vertex](), Vector[Vertex]()))
      {
        (acc: (Vector[Vertex], Vector[Vertex]), i: Int) =>
          val j = (i + 1) % len
          val ti = types(i)
          val tj = types(j)
          val vi = polygon.vertices(i)
          val vj = polygon.vertices(j)
          val append = if((ti | tj) == SPANNING) {
            val t = (w - normal.dot(vi.position)) / normal.dot(vj.position - vi.position)
            Vector(vi.interpolate(vj)(t))
          } else {
            Nil
          }
          val front = if(ti != BACK) acc._1 :+ vi else acc._1
          val back = if(ti != FRONT) acc._2 :+ vi else acc._2
          (
            front ++ append,
            back ++ append
          )
      }

      (if(front.length > 2) Vector(Front(Polygon(front))) else Vector()) ++ (if(back.length > 2) Vector(Back(Polygon(back))) else Vector())
    }
  }

  def transform(matrix: Matrix44): Plane = {
    // get two vectors in the plane:
    val r = normal.randomNonParallelVector()
    val u = normal.cross(r)
    val v = normal.cross(u)
    // get 3 points in the plane:
    val p1 = normal * w
    val p2 = p1 + u
    val p3 = p1 + v
    // transform the points and create a new plane from the transformed points:
    var newplane = Plane.fromPoints(
      p1.transform(matrix),
      p2.transform(matrix),
      p3.transform(matrix)
    )
    if(matrix.isMirroring())
      newplane.flip() // the transform is mirroring
    else
      newplane
  }

  override def toString(): String =
    s"Plane(normal=$normal,w=$w)"
}

abstract sealed class SplitPolygon

case class CoplanarFront(polygon: Polygon) extends SplitPolygon
case class CoplanarBack(polygon: Polygon) extends SplitPolygon
case class Front(polygon: Polygon) extends SplitPolygon
case class Back(polygon: Polygon) extends SplitPolygon

object Plane {
  val PX = Plane(Point(1, 0, 0), 0)
  val PY = Plane(Point(0, 1, 0), 0)
  val PZ = Plane(Point(0, 0, 1), 0)

  def fromPoints(a : Point, b : Point, c : Point) = {
    val n = ((b - a) cross (c - a)).normalize
    Plane(n, n dot a)
  }

  // like fromPoints, but allow the vectors to be on one point or one line
  // in such a case a random plane through the given points is constructed
  def anyPlaneFromPoints(a: Point, b: Point, c: Point): Plane = {
    var v1 = b - a
    var v2 = c - a
    if(v1.length =~ 0)
      v1 = v2.randomNonParallelVector
    if(v2.length =~ 0)
      v2 = v1.randomNonParallelVector
    var normal = v1 cross v2
    if(normal.length =~ 0) {
      // this would mean that v1 == -v2
      v2 = v1.randomNonParallelVector
      normal = v1 cross v2
    }
    normal = normal.normalize
    Plane(normal, normal dot a)
  }

  def fromNormalAndPoint(normal : Point, point : Point): Plane =
    Plane(normal, point.dot(normal.normalize))
}

/*
  // robust splitting of a line by a plane
  // will work even if the line is parallel to the plane
  public function splitLineBetweenPoints(p1 : Point, p2 : Point) {
    var direction = p2.subtractPoint(p1),
        lambda = (w - normal.dot(p1)) / normal.dot(direction)
    if(Math.isNaN(lambda))
      lambda = 0
    else if(lambda > 1)
      lambda = 1
    else if(lambda < 0)
      lambda = 0
    return p1.addPoint(direction.multiply(lambda))
  }

  public function intersectWithLine(line : Line3D) : Point
    return line.intersectWithPlane(this)

  // intersection of two planes
  public function intersectWithPlane(plane : Plane)
    return Line3D.fromPlanes(this, plane)

  public function signedDistanceToPoint(point)
    return normal.dot(point) - w

  public function mirrorPoint(point3d) {
    var distance = this.signedDistanceToPoint(point3d)
    var mirrored = point3d.subtractPoint(this.normal.multiply(distance * 2.0))
    return mirrored
  }
 */
