package tre.d3

import tre.Matrix44

case class OrthoNormalBasis(plane: Plane, private val rightvector: Point) extends Transformable[OrthoNormalBasis]{
  val v = plane.normal.cross(rightvector).normalize
  val u = v.cross(plane.normal)
  val planeOrigin = plane.normal * plane.w

  def projectionMatrix() =
    Matrix44(
      u.x, v.x, plane.normal.x, 0,
      u.y, v.y, plane.normal.y, 0,
      u.z, v.z, plane.normal.z, 0,
      0, 0, -plane.w, 1
    )

  def inverseProjectionMatrix(): Matrix44 = {
    var p = plane.normal * plane.w
    Matrix44(
      u.x, u.y, u.z, 0,
      v.x, v.y, v.z, 0,
      plane.normal.x, plane.normal.y, plane.normal.z, 0,
      p.x, p.y, p.z, 1
    )
  }

  def projectPoint2D(p: Point): tre.d2.Point =
    tre.d2.Point(p.dot(u), p.dot(v))

  def projectPoint3D(p: tre.d2.Point): Point =
    planeOrigin + u * p.x + v * p.y

  def transform(matrix : Matrix44): OrthoNormalBasis = {
    // todo: may not work properly in case of mirroring
    val newplane = plane.transform(matrix)
    val rightpoint_transformed = u.transform(matrix)
    val origin_transformed = Point.zero.transform(matrix)
    val newrighthandvector = rightpoint_transformed - origin_transformed
    OrthoNormalBasis(newplane, newrighthandvector)
  }
}

object OrthoNormalBasis {
  val z0Plane = OrthoNormalBasis(Plane(Point(0,0,1),0), Point(1,0,0))
  def fromPlane(plane: Plane): OrthoNormalBasis =
    OrthoNormalBasis(plane, plane.normal.randomNonParallelVector)
}
