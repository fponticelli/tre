package tre.d3

import tre.Precision._

/**
  * Created by francoponticelli on 6/2/16.
  */
case class Plane(normal : Point, w : Double) {
  def flip() = Plane(-normal, -w)

  private val COPLANAR = 0;
  private val FRONT = 1;
  private val BACK = 2;
  private val SPANNING = 3;

  def splitPolygon(polygon: Polygon): List[SplitPolygon] = {
    val (types, polygonType) = polygon.foldLeft((List[Int](), COPLANAR)){
      (acc: (List[Int], Int), vertex: Vertex) =>
        val t = normal dot(vertex.position) - w
        val polygonType = if(t <~ 0) BACK else if(t >~ 0) FRONT else COPLANAR
        val list: List[Int] = acc._1 :+ polygonType
        (list, acc._2 | polygonType)
    }

    if(polygonType == COPLANAR) {
      if(normal.dot(polygon.plane.normal) > 0)
        List(CoplanarFront(polygon))
      else
        List(CoplanarBack(polygon))
    } else if(polygonType == FRONT) {
      List(Front(polygon));
    } else if(polygonType == BACK) {
      List(Back(polygon));
    } else { // SPANNING
      val len = polygon.vertices.length
      val range = 0 until len
      val (front: List[Vertex], back: List[Vertex]) = range.foldLeft((List[Vertex](), List[Vertex]()))
      {
        (acc: (List[Vertex], List[Vertex]), i: Int) =>
          val j = (i + 1) % len
          val ti = types(i)
          val tj = types(j)
          val vi = polygon.vertices(i)
          val vj = polygon.vertices(j)
          val append = if((ti | tj) == SPANNING) {
            val t = (w - normal.dot(vi.position)) / normal.dot(vj.position - vi.position)
            List(vi.interpolate(vj)(t))
          } else {
            Nil;
          }
          val front = if(ti != BACK) acc._1 :+ vi else acc._1
          val back = if(ti != FRONT) acc._2 :+ vi else acc._2
          (
            front ++ append,
            back ++ append
          )
      }

      (if(front.length > 2) List(Front(Polygon(front))) else Nil) ++ (if(back.length > 2) List(Back(Polygon(back))) else Nil)
    }
  }

  override def toString(): String =
    s"Plane(normal=$normal,w=$w)"


  sealed class SplitPolygon

  case class CoplanarFront(polygon: Polygon) extends SplitPolygon
  case class CoplanarBack(polygon: Polygon) extends SplitPolygon
  case class Front(polygon: Polygon) extends SplitPolygon
  case class Back(polygon: Polygon) extends SplitPolygon
}

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

  def fromNormalAndPoint(normal : Point, point : Point) {
    val n = normal.normalize
    new Plane(n, point dot n)
  }
}

/*
  public function equals(other : Plane)
    return normal.equals(other.normal) && (w == other.w);

  public function transform(matrix : Matrix44) {
    var ismirror = matrix.isMirroring(),
      // get two vectors in the plane:
      r = normal.randomNonParallelVector(),
      u = normal.cross(r),
      v = normal.cross(u),
      // get 3 points in the plane:
      point1 = normal.multiply(w),
      point2 = point1.addPoint(u),
      point3 = point1.addPoint(v);
    // transform the points:
    point1 = point1.transform(matrix);
    point2 = point2.transform(matrix);
    point3 = point3.transform(matrix);
    // and create a new plane from the transformed points:
    var newplane = Plane.fromPoints(point1, point2, point3);
    if(ismirror) {
      // the transform is mirroring
      // We should mirror the plane:
      newplane = newplane.flip();
    }
    return newplane;
  }

  // robust splitting of a line by a plane
  // will work even if the line is parallel to the plane
  public function splitLineBetweenPoints(p1 : Point, p2 : Point) {
    var direction = p2.subtractPoint(p1),
        lambda = (w - normal.dot(p1)) / normal.dot(direction);
    if(Math.isNaN(lambda))
      lambda = 0;
    else if(lambda > 1)
      lambda = 1;
    else if(lambda < 0)
      lambda = 0;
    return p1.addPoint(direction.multiply(lambda));
  }

  public function intersectWithLine(line : Line3D) : Point
    return line.intersectWithPlane(this);

  // intersection of two planes
  public function intersectWithPlane(plane : Plane)
    return Line3D.fromPlanes(this, plane);

  public function signedDistanceToPoint(point)
    return normal.dot(point) - w;

  public function mirrorPoint(point3d) {
    var distance = this.signedDistanceToPoint(point3d);
    var mirrored = point3d.subtractPoint(this.normal.multiply(distance * 2.0));
    return mirrored;
  }

  static inline var COPLANAR = 0;
  static inline var FRONT = 1;
  static inline var BACK = 2;
  static inline var SPANNING = 3;
 */