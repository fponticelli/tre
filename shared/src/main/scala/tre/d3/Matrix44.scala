package tre.d3

case class Matrix44(
  v11: Double,
  v12: Double,
  v13: Double,
  v14: Double,
  v21: Double,
  v22: Double,
  v23: Double,
  v24: Double,
  v31: Double,
  v32: Double,
  v33: Double,
  v34: Double,
  v41: Double,
  v42: Double,
  v43: Double,
  v44: Double
) extends tre.d3.Transformable[Matrix44] {
  def *(that: Matrix44): Matrix44 =
    Matrix44(
      v11 * that.v11 + v12 * that.v21 + v13 * that.v31 + v14 * that.v41,
      v11 * that.v12 + v12 * that.v22 + v13 * that.v32 + v14 * that.v42,
      v11 * that.v13 + v12 * that.v23 + v13 * that.v33 + v14 * that.v43,
      v11 * that.v14 + v12 * that.v24 + v13 * that.v34 + v14 * that.v44,
      v21 * that.v11 + v22 * that.v21 + v23 * that.v31 + v24 * that.v41,
      v21 * that.v12 + v22 * that.v22 + v23 * that.v32 + v24 * that.v42,
      v21 * that.v13 + v22 * that.v23 + v23 * that.v33 + v24 * that.v43,
      v21 * that.v14 + v22 * that.v24 + v23 * that.v34 + v24 * that.v44,
      v31 * that.v11 + v32 * that.v21 + v33 * that.v31 + v34 * that.v41,
      v31 * that.v12 + v32 * that.v22 + v33 * that.v32 + v34 * that.v42,
      v31 * that.v13 + v32 * that.v23 + v33 * that.v33 + v34 * that.v43,
      v31 * that.v14 + v32 * that.v24 + v33 * that.v34 + v34 * that.v44,
      v41 * that.v11 + v42 * that.v21 + v43 * that.v31 + v44 * that.v41,
      v41 * that.v12 + v42 * that.v22 + v43 * that.v32 + v44 * that.v42,
      v41 * that.v13 + v42 * that.v23 + v43 * that.v33 + v44 * that.v43,
      v41 * that.v14 + v42 * that.v24 + v43 * that.v34 + v44 * that.v44
    )

  def leftMultiplyPoint(point: tre.d3.Point): tre.d3.Point = {
    val v0 = point.x
    val v1 = point.y
    val v2 = point.z
    val v3 = 1
    val x = v0 * v11 + v1 * v21 + v2 * v31 + v3 * v41
    val y = v0 * v12 + v1 * v22 + v2 * v32 + v3 * v42
    val z = v0 * v13 + v1 * v23 + v2 * v33 + v3 * v43
    val w = v0 * v14 + v1 * v24 + v2 * v34 + v3 * v44
    tre.d3.Point(x / w, y / w, z / w)
  }

  def leftMultiplyPoint(point: tre.d2.Point): tre.d2.Point = {
    val v0 = point.x
    val v1 = point.y
    val v2 = 0
    val v3 = 1
    val x = v0 * v11 + v1 * v21 + v2 * v31 + v3 * v41
    val y = v0 * v12 + v1 * v22 + v2 * v32 + v3 * v42
    val w = v0 * v14 + v1 * v24 + v2 * v34 + v3 * v44
    tre.d2.Point(x / w, y / w)
  }

  // determine whether this matrix is a mirroring transformation
  def isMirroring(): Boolean = {
    val u = tre.d3.Point(v11, v21, v31)
    val v = tre.d3.Point(v12, v22, v32)
    val w = tre.d3.Point(v13, v23, v33)

    // for a true orthogonal, non-mirrored base, u.cross(v) == w
    // If they have an opposite direction then we are mirroring
    u.cross(v).dot(w) < 0
  }

  def inverse(): Option[Matrix44] = {
    var inv_11 =  v22  * v33  * v44 -
                  v22  * v34  * v43 -
                  v32  * v23  * v44 +
                  v32  * v24  * v43 +
                  v42  * v23  * v34 -
                  v42  * v24  * v33
    val inv_21 = -v21  * v33  * v44 +
                  v21  * v34  * v43 +
                  v31  * v23  * v44 -
                  v31  * v24  * v43 -
                  v41  * v23  * v34 +
                  v41  * v24  * v33
    val inv_31 =  v21  * v32  * v44 -
                  v21  * v34  * v42 -
                  v31  * v22  * v44 +
                  v31  * v24  * v42 +
                  v41  * v22  * v34 -
                  v41  * v24  * v32
    val inv_41 = -v21  * v32  * v43 +
                  v21  * v33  * v42 +
                  v31  * v22  * v43 -
                  v31  * v23  * v42 -
                  v41  * v22  * v33 +
                  v41  * v23  * v32
    val inv_12 = -v12  * v33  * v44 +
                  v12  * v34  * v43 +
                  v32  * v13  * v44 -
                  v32  * v14  * v43 -
                  v42  * v13  * v34 +
                  v42  * v14  * v33
    val inv_22 =  v11  * v33  * v44 -
                  v11  * v34  * v43 -
                  v31  * v13  * v44 +
                  v31  * v14  * v43 +
                  v41  * v13  * v34 -
                  v41  * v14  * v33
    val inv_32 = -v11  * v32  * v44 +
                  v11  * v34  * v42 +
                  v31  * v12  * v44 -
                  v31  * v14  * v42 -
                  v41  * v12  * v34 +
                  v41  * v14  * v32
    val inv_42 =  v11  * v32  * v43 -
                  v11  * v33  * v42 -
                  v31  * v12  * v43 +
                  v31  * v13  * v42 +
                  v41  * v12  * v33 -
                  v41  * v13  * v32
    val inv_13 =  v12  * v23  * v44 -
                  v12  * v24  * v43 -
                  v22  * v13  * v44 +
                  v22  * v14  * v43 +
                  v42  * v13  * v24 -
                  v42  * v14  * v23
    val inv_23 = -v11  * v23  * v44 +
                  v11  * v24  * v43 +
                  v21  * v13  * v44 -
                  v21  * v14  * v43 -
                  v41  * v13  * v24 +
                  v41  * v14  * v23
    val inv_33 =  v11  * v22  * v44 -
                  v11  * v24  * v42 -
                  v21  * v12  * v44 +
                  v21  * v14  * v42 +
                  v41  * v12  * v24 -
                  v41  * v14  * v22
    val inv_43 = -v11  * v22  * v43 +
                  v11  * v23  * v42 +
                  v21  * v12  * v43 -
                  v21  * v13  * v42 -
                  v41  * v12  * v23 +
                  v41  * v13  * v22
    val inv_14 = -v12  * v23  * v34 +
                  v12  * v24  * v33 +
                  v22  * v13  * v34 -
                  v22  * v14  * v33 -
                  v32  * v13  * v24 +
                  v32  * v14  * v23
    val inv_24 =  v11  * v23  * v34 -
                  v11  * v24  * v33 -
                  v21  * v13  * v34 +
                  v21  * v14  * v33 +
                  v31  * v13  * v24 -
                  v31  * v14  * v23
    val inv_34 = -v11  * v22  * v34 +
                  v11  * v24  * v32 +
                  v21  * v12  * v34 -
                  v21  * v14  * v32 -
                  v31  * v12  * v24 +
                  v31  * v14  * v22
    val inv_44 =  v11  * v22  * v33 -
                  v11  * v23  * v32 -
                  v21  * v12  * v33 +
                  v21  * v13  * v32 +
                  v31  * v12  * v23 -
                  v31  * v13  * v22
    val det = v11 * inv_11 + v12 * inv_21 + v13 * inv_31 + v14 * inv_41
    if(det == 0)
      None
    else
      Some(Matrix44(inv_11, inv_12, inv_13, inv_14, inv_21, inv_22, inv_23, inv_24, inv_31, inv_32, inv_33, inv_34, inv_41, inv_42, inv_43, inv_44))
  }

  def transform(matrix: Matrix44): Matrix44 =
    matrix * this // TODO or this * matrix ????
}

object Matrix44 {
  val identity = Matrix44( 1,  0,  0,  0,
                           0,  1,  0,  0,
                           0,  0,  1,  0,
                           0,  0,  0,  1)
  def mirroring(plane : tre.d3.Plane): Matrix44 = {
    val nx = plane.normal.x
    val ny = plane.normal.y
    val nz = plane.normal.z
    val w  = plane.w
    Matrix44(
      (1.0 - 2.0 * nx * nx), (-2.0 * ny * nx), (-2.0 * nz * nx), 0,
      (-2.0 * nx * ny), (1.0 - 2.0 * ny * ny), (-2.0 * nz * ny), 0,
      (-2.0 * nx * nz), (-2.0 * ny * nz), (1.0 - 2.0 * nz * nz), 0,
      (-2.0 * nx * w), (-2.0 * ny * w), (-2.0 * nz * w), 1
    )
  }
  val mirroringYZ = mirroring(tre.d3.Plane.PX)
  val mirroringXZ = mirroring(tre.d3.Plane.PY)
  val mirroringXY = mirroring(tre.d3.Plane.PZ)

  def scaling(x: Double, y: Double, z: Double): Matrix44 =
    Matrix44(x, 0, 0, 0, 0, y, 0, 0, 0, 0, z, 0, 0, 0, 0, 1)

  def rotating(rotationCenter: tre.d3.Point, rotationAxis: tre.d3.Point, radians:  Double): Matrix44 = {
    val rotationPlane = Plane.fromNormalAndPoint(rotationAxis, rotationCenter)
    val orthobasis = OrthoNormalBasis.fromPlane(rotationPlane)
    val negation = -rotationCenter
    val t = Matrix44.translating(negation.x, negation.y, negation.z)
    ((t * orthobasis.projectionMatrix).rotateZ(radians) * orthobasis.inverseProjectionMatrix).translate(rotationCenter.x, rotationCenter.y, rotationCenter.z)
  }

  def rotatingX(angle: Double): Matrix44 = {
    val c = Math.cos(angle)
    val s = Math.sin(angle)
    Matrix44(1, 0, 0, 0, 0, c, s, 0, 0, -s, c, 0, 0, 0, 0, 1)
  }

  def rotatingY(angle: Double): Matrix44 = {
    val c = Math.cos(angle)
    val s = Math.sin(angle)
    Matrix44(c, 0, -s, 0, 0, 1, 0, 0, s, 0, c, 0, 0, 0, 0, 1)
  }

  def rotatingZ(angle: Double): Matrix44 = {
    val c = Math.cos(angle)
    val s = Math.sin(angle)
    Matrix44(c, s, 0, 0, -s, c, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1)
  }

  def translating(x: Double, y: Double, z: Double): Matrix44 =
    Matrix44(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, x, y, z, 1)
}
