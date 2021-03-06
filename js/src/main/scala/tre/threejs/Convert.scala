package tre.threejs

import org.denigma.threejs.{ Geometry => TJSGeom, Vector3 => TJSVec, Face3 => TJSFace }
import tre.csg.Solid
import tre.d3.{Point, Polygon, Vertex}
import scala.collection.immutable.HashSet
import scala.scalajs.js

object Convert {
  implicit def solid2geometry(solid: Solid): TJSGeom = {
    val vertices = solid.flatMap(_.iterator)
    val set = vertices
      .map(_.position)
      .foldLeft(HashSet[Point]()) { _ + _ }

    val map = set.zipWithIndex.foldLeft(Map[Point, Int]()) { (map, t) => map + (t._1 -> t._2) }

    val geom = new TJSGeom()
    val vs = geom.vertices
    val fs = geom.faces

    solid.flatMap {
      polygon => polygon.triangles().map(t =>
        new TJSFace(
          map(t._1.position),
          map(t._2.position),
          map(t._3.position),
          js.Array[TJSVec](
            new TJSVec(t._1.normal.x, t._1.normal.y, t._1.normal.z),
            new TJSVec(t._2.normal.x, t._2.normal.y, t._2.normal.z),
            new TJSVec(t._3.normal.x, t._3.normal.y, t._3.normal.z)
          )
        )
      )
    }.foreach(fs.push(_))
    set.foreach(v => vs.push(new TJSVec(v.x, v.y, v.z)))

    geom.computeBoundingBox()
    geom.computeBoundingSphere()
    geom.computeFaceNormals()
    geom
  }
}
