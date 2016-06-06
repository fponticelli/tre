package tre.csg

import tre.d3._
import tre.d3.Polygon
import tre.Precision._

import scala.collection.mutable

class Node(polys: Option[List[Polygon]]) {
  var plane: Plane = null
  var front: Node = null
  var back: Node = null
  var polygons: mutable.ArrayBuffer[Polygon] = new mutable.ArrayBuffer[Polygon]()
  polys
    .map({
        xs => xs.foldLeft(new mutable.ArrayBuffer[Polygon]) {
          (buff, p) => buff += p
        }
      })
    .foreach(build)

  def build(poly: mutable.ArrayBuffer[Polygon]): Unit = {
    if(poly.length > 0) {
      if(null == plane)
        plane = poly(0).plane
      var front = new mutable.ArrayBuffer[Polygon]()
      var back  = new mutable.ArrayBuffer[Polygon]()
      poly.foreach(splitPolygon(plane, _, this.polygons, this.polygons, front, back))
      if(front.length > 0) {
        if(null == this.front)
          this.front = new Node(None)
        this.front.build(front)
      }
      if(back.length > 0) {
        if(null == this.back)
          this.back = new Node(None)
        this.back.build(back)
      }
    }
  }

  def splitPolygonByPlane(plane: Plane, polygon: Polygon, coplanarFront: mutable.ArrayBuffer[Polygon], coplanarBack: mutable.ArrayBuffer[Polygon], front: mutable.ArrayBuffer[Polygon], back: mutable.ArrayBuffer[Polygon]) = {
    plane.splitPolygon(polygon).foreach {
      (split: SplitPolygon) => split match {
        case CoplanarFront(p) => coplanarFront += p
        case CoplanarBack(p) => coplanarBack += p
        case Front(p) => front += p
        case Back(p) => back += p
      }
    }
  }

  def splitPolygon(plane: Plane, polygon: Polygon, coplanarFront: mutable.ArrayBuffer[Polygon], coplanarBack: mutable.ArrayBuffer[Polygon], front: mutable.ArrayBuffer[Polygon], back: mutable.ArrayBuffer[Polygon]) = {
    var polygonType = 0
    var t = 0.0
    var type_ = 0
    var types = new mutable.ArrayBuffer[Int]
    for(vertex <- polygon) {
      t = plane.normal.dot(vertex.position) - plane.w
      type_ = if(t <~ 0) BACK else if(t >~ 0) FRONT else COPLANAR
      polygonType |= type_
      types += type_
    }
    polygonType match {
      case COPLANAR => (if(plane.normal.dot(polygon.plane.normal) > 0) coplanarFront else coplanarBack) += polygon
      case FRONT => front += polygon
      case BACK => back += polygon
      case SPANNING =>
        var f = new mutable.ArrayBuffer[Vertex]
        var b = new mutable.ArrayBuffer[Vertex]
        var vertices = polygon.vertices.toArray
        var len = vertices.length
        var j = 0
        var t = 0.0
        var ti = 0
        var tj = 0
        var v: Vertex = null
        var vi: Vertex = null
        var vj: Vertex = null
        for(i <- 0 until len) {
          j = (i + 1) % len
          ti = types(i)
          tj = types(j)
          vi = vertices(i)
          vj = vertices(j)
          if(ti != BACK)
            f :+ vi
          if(ti != FRONT)
            b :+ vi
          if((ti | tj) == SPANNING) {
            t = (plane.w - plane.normal.dot(vi.position)) / plane.normal.dot(vj.position - vi.position)
            v = vi.interpolate(vj)(t)
            f :+ v
            b :+ v
          }
        }
        if(f.length >= 3)
          front :+ Polygon(f.toList)
        if(b.length >= 3)
          back :+ Polygon(b.toList)
    }
/*
  public function splitPolygon(polygon : Polygon, coplanarFront : Array<Polygon>, coplanarBack : Array<Polygon>, front : Array<Polygon>, back : Array<Polygon>) {
        f = [];
        b = [];
        len = polygon.vertices.length;
        for (i in 0...len) {
          j = (i + 1) % len;
          ti = types[i];
          tj = types[j];
          vi = polygon.vertices[i];
          vj = polygon.vertices[j];
          if (ti != BACK)
            f.push(vi);
          if (ti != FRONT)
            b.push(vi); // was: (ti != BACK ? vi.clone() : vi);
          if ((ti | tj) == SPANNING) {
            t = (w - normal.dot(vi.position)) /
                normal.dot(vj.position.subtractPoint(vi.position));
            v = vi.interpolate(vj, t);
            f.push(v);
            b.push(v); // was: v.clone()
          }
        }
        if (f.length >= 3)
          front.push(new Polygon(f));
        if (b.length >= 3)
          back.push(new Polygon(b));
    }
  }
 */
  }

  private val COPLANAR = 0;
  private val FRONT = 1;
  private val BACK = 2;
  private val SPANNING = 3;

  def invert(): Unit = {
    polygons = polygons.map { _.flip() }
    if(null != plane) plane = plane.flip()
    if(null != front) front.invert();
    if(null != back) back.invert();
    val temp = front;
    front = back;
    back = temp;
  }

  def clipPolygons(polys: mutable.ArrayBuffer[Polygon]): mutable.ArrayBuffer[Polygon] = {
    if(null == this.plane)
      polys.clone()
    else {
      var front = new mutable.ArrayBuffer[Polygon]
      var back  = new mutable.ArrayBuffer[Polygon]
      polys.foreach { splitPolygon(plane, _, front, back, front, back) }

      if(null != this.front)
        front = this.front.clipPolygons(front)
      if(null != this.back)
        back = this.back.clipPolygons(back)
      else
        back = new mutable.ArrayBuffer[Polygon]
      front ++ back
    }
  }

  def clipTo(other: Node): Unit = {
    polygons = other.clipPolygons(polygons)
    if(null != front)
      front.clipTo(other)
    if(null != back)
      back.clipTo(other)
  }

  def all(): mutable.ArrayBuffer[Polygon] =
    polygons ++
      (if(null == front) new mutable.ArrayBuffer[Polygon] else front.all) ++
      (if(null == back)  new mutable.ArrayBuffer[Polygon] else back.all)
}

/*
abstract sealed class Node {
  def invert(): Node
  def polygons(): List[Polygon]
  def clipPolygons(polygons: List[Polygon])
}

case class NodeWithFront(pnode: PNode, front: Node) extends Node {
  def invert() = NodeWithBack(pnode invert, front invert)
  def polygons() =
    front.polygons ++ pnode.polygons
}

case class NodeWithBack(pnode: PNode, back: Node) extends Node {
  def invert() = NodeWithFront(pnode invert, back invert)
  def polygons() =
    pnode.polygons ++ back.polygons
}

case class NodeWithFrontAndBack(pnode: PNode, front: Node, back: Node) extends Node {
  def invert() = NodeWithFrontAndBack(pnode invert, back invert, front invert)
  def polygons() =
    front.polygons ++ pnode.polygons ++ back.polygons
}

case class PNode(polygons: List[Polygon]) extends Node {
  def invert() = PNode(polygons map { _.flip() })
  def clipPolygons(that: List[Polygon]): List[Polygon] =
    if(polygons.length == 0)
      that
    else {
      val plane = polygons(0).plane
      val t = that.map(plane.splitPolygon(_)).foldLeft((List[Polygon](), List[Polygon]())) {
        (acc: (List[Polygon], List[Polygon]), splits: List[SplitPolygon]) =>
          val t = splits.foldLeft((List[Polygon](), List[Polygon]())) {
            (acc: (List[Polygon], List[Polygon]), split: SplitPolygon) =>
              split match {
                case CoplanarFront(p) =>
                  (acc._1 :+ p, acc._2)
                case Front(p) =>
                  (acc._1 :+ p, acc._2)
                case CoplanarBack(p) =>
                  (acc._1, acc._2 :+ p)
                case Back(p) =>
                  (acc._1, acc._2 :+ p)
              }
          }
          (acc._1 ++ t._1, acc._2 ++ t._2)
      }
      t._1 ++ t._2
    }
}

object Node {
  type FPB = (List[Polygon], List[Polygon], List[Polygon])
  def build(plane: Plane, polygons: List[Polygon]): Node = {
    val fpb = polygons
      .flatMap(plane.splitPolygon(_))
      .foldLeft(List[Polygon](), List[Polygon](), List[Polygon]()) {
      (acc: FPB, split: SplitPolygon) => {
        split match {
          case CoplanarFront(p) => (acc._1, acc._2 :+ p, acc._3)
          case CoplanarBack(p) => (acc._1, acc._2 :+ p, acc._3)
          case Front(p) => (acc._1 :+ p, acc._2, acc._3)
          case Back(p) => (acc._1, acc._2, acc._3 :+ p)
        }
      }
    }
    if(fpb._1.length == 0 && fpb._3.length == 0) {
      PNode(fpb._2)
    } else if(fpb._3.length == 0) {
      NodeWithFront(PNode(fpb._2), build(fpb._1))
    } else if(fpb._1.length == 0) {
      NodeWithBack(PNode(fpb._2), build(fpb._3))
    } else {
      NodeWithFrontAndBack(PNode(fpb._2), build(fpb._1), build(fpb._3))
    }
  }

  def build(polygons: List[Polygon]): Node =
    build(polygons(0).plane, polygons)

}
*/