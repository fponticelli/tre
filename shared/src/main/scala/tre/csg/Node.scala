package tre.csg

import tre.d3._
import tre.d3.Polygon
import tre.Precision._
import scala.collection.mutable.ArrayBuffer

class Node(polys: ArrayBuffer[Polygon]) {
  def this(polys: Vector[Polygon]) = {
    this {
      val buf = new ArrayBuffer[Polygon]
      for(poly <- polys) buf += poly
      buf
    }
  }
  var plane: Plane = null
  var front: Node = null
  var back: Node = null
  var polygons: ArrayBuffer[Polygon] = new ArrayBuffer[Polygon]()

  private val COPLANAR = 0;
  private val FRONT = 1;
  private val BACK = 2;
  private val SPANNING = 3;

  build(polys)

  def build(polys: ArrayBuffer[Polygon]): Unit = {
    if(polys.length > 0) {
      if(null == plane)
      plane = polys(0).plane
      val front = new ArrayBuffer[Polygon]()
      val back  = new ArrayBuffer[Polygon]()
      for(poly <- polys)
        splitPolygonByPlane(plane, poly, this.polygons, this.polygons, front, back)
      if(front.length > 0) {
        if(null == this.front)
        this.front = new Node(new ArrayBuffer[Polygon])
        this.front.build(front)
      }
      if(back.length > 0) {
        if(null == this.back)
        this.back = new Node(new ArrayBuffer[Polygon])
        this.back.build(back)
      }
    }
  }

  def splitPolygonByPlane(plane: Plane, polygon: Polygon, coplanarFront: ArrayBuffer[Polygon], coplanarBack: ArrayBuffer[Polygon], front: ArrayBuffer[Polygon], back: ArrayBuffer[Polygon]) = {
    val types = new ArrayBuffer[Int]
    var polygonType = 0
    for(vertex <- polygon) {
      val t = plane.normal.dot(vertex.position) - plane.w
      val type_ = if(t <~ 0) BACK else if(t >~ 0) FRONT else COPLANAR
      polygonType |= type_
      types += type_
    }

    if(polygonType == COPLANAR) {
      if(plane.normal.dot(polygon.plane.normal) > 0)
        coplanarFront += polygon
      else
        coplanarBack += polygon
    } else if(polygonType == FRONT) {
      front += polygon
    } else if(polygonType == BACK) {
      back += polygon
    } else { // SPANNING
      // val range = 0 until len
      val f = new ArrayBuffer[Vertex]
      val b = new ArrayBuffer[Vertex]
      val len = polygon.vertices.length
      for(i <- 0 until len) {
        val j = (i + 1) % len
        val ti = types(i)
        val tj = types(j)
        val vi = polygon.vertices(i)
        val vj = polygon.vertices(j)
        if(ti != BACK)
          f += vi
        if(ti != FRONT)
          b += vi
        if((ti | tj) == SPANNING) {
          val t = (plane.w - plane.normal.dot(vi.position)) / plane.normal.dot(vj.position - vi.position)
          val v = vi.interpolate(vj)(t)
          f += v
          b += v
        }
      }
      if(f.length > 2)
        front += Polygon(f.toVector)
      if(b.length > 2)
        back += Polygon(b.toVector)
    }
  }

  def invert(): Unit = {
    polygons = polygons.map { _.flip() }
    if(null != plane) plane = plane.flip()
    if(null != front) front.invert();
    if(null != back) back.invert();
    val temp = front;
    front = back;
    back = temp;
  }

  def clipPolygons(polys: ArrayBuffer[Polygon]): ArrayBuffer[Polygon] = {
    if(null == this.plane)
      polys.clone()
    else {
      var f = new ArrayBuffer[Polygon]
      var b = new ArrayBuffer[Polygon]
      for(poly <- polys)
        splitPolygonByPlane(plane, poly, f, b, f, b)

      if(null != front)
        f = front.clipPolygons(f)
      if(null != back)
        b = back.clipPolygons(b)
      else
        b = new ArrayBuffer[Polygon]
      f ++ b
    }
  }

  def clipTo(other: Node): Unit = {
    polygons = other.clipPolygons(polygons)
    if(null != front)
      front.clipTo(other)
    if(null != back)
      back.clipTo(other)
  }

  def all(): ArrayBuffer[Polygon] = {
    var buf = polygons
    if(null != front) buf = buf ++ front.all
    if(null != back) buf = buf ++ back.all
    buf
  }
}
