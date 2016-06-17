package tre.csg

import tre.d3._
import tre.d3.Polygon
import tre.Precision._

import scala.collection.mutable

class Node(polys: Option[Vector[Polygon]]) {
  var plane: Plane = null
  var front: Node = null
  var back: Node = null
  var polygons: mutable.ArrayBuffer[Polygon] = new mutable.ArrayBuffer[Polygon]()

  private val COPLANAR = 0;
  private val FRONT = 1;
  private val BACK = 2;
  private val SPANNING = 3;

  polys
    .map { _.foldLeft(new mutable.ArrayBuffer[Polygon]) { _ += _ } }
    .foreach(build)

  def build(polys: mutable.ArrayBuffer[Polygon]): Unit = {
    if(polys.length > 0) {
      if(null == plane)
        plane = polys(0).plane
      val front = new mutable.ArrayBuffer[Polygon]()
      val back  = new mutable.ArrayBuffer[Polygon]()
      polys.foreach { splitPolygonByPlane(plane, _, this.polygons, this.polygons, front, back) }
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
        case CoplanarFront(p) => coplanarFront append p
        case CoplanarBack(p) => coplanarBack append p
        case Front(p) => front append p
        case Back(p) => back append p
      }
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

  def clipPolygons(polys: mutable.ArrayBuffer[Polygon]): mutable.ArrayBuffer[Polygon] = {
    if(null == this.plane)
      polys.clone()
    else {
      var f = new mutable.ArrayBuffer[Polygon]
      var b = new mutable.ArrayBuffer[Polygon]
      polys.foreach { splitPolygonByPlane(plane, _, f, b, f, b) }

      if(null != front)
        f = front.clipPolygons(f)
      if(null != back)
        b = back.clipPolygons(b)
      else
        b = new mutable.ArrayBuffer[Polygon]
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

  def all(): mutable.ArrayBuffer[Polygon] =
    polygons ++
      (if(null == front) new mutable.ArrayBuffer[Polygon] else front.all) ++
      (if(null == back)  new mutable.ArrayBuffer[Polygon] else back.all)
}

/*
abstract sealed class Node {
  def invert(): Node
  def polygons(): Vector[Polygon]
  def clipPolygons(polygons: Vector[Polygon])
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

case class PNode(polygons: Vector[Polygon]) extends Node {
  def invert() = PNode(polygons map { _.flip() })
  def clipPolygons(that: Vector[Polygon]): Vector[Polygon] =
    if(polygons.length == 0)
      that
    else {
      val plane = polygons(0).plane
      val t = that.map(plane.splitPolygon(_)).foldLeft((Vector[Polygon](), Vector[Polygon]())) {
        (acc: (Vector[Polygon], Vector[Polygon]), splits: Vector[SplitPolygon]) =>
          val t = splits.foldLeft((Vector[Polygon](), Vector[Polygon]())) {
            (acc: (Vector[Polygon], Vector[Polygon]), split: SplitPolygon) =>
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
  type FPB = (Vector[Polygon], Vector[Polygon], Vector[Polygon])
  def build(plane: Plane, polygons: Vector[Polygon]): Node = {
    val fpb = polygons
      .flatMap(plane.splitPolygon(_))
      .foldLeft(Vector[Polygon](), Vector[Polygon](), Vector[Polygon]()) {
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

  def build(polygons: Vector[Polygon]): Node =
    build(polygons(0).plane, polygons)

}
*/
