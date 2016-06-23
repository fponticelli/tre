package tre.csg

import scala.collection.mutable
import tre.d3.{Matrix44, Point, Polygon, Transformable, Vertex}

case class Solid(polygons: Vector[Polygon]) extends Iterable[Polygon] with Transformable[Solid] {
  def iterator() = polygons iterator

  def +(other: Solid): Solid = {
    val a = new Node(polygons)
    val b = new Node(other.polygons)

    a clipTo b
    b clipTo a
    b.invert
    b clipTo a
    b.invert
    a.build(b.all)

    Solid(a.all.toVector)
  }

  def -(other: Solid): Solid = {
    val a = new Node(polygons)
    val b = new Node(other.polygons)

    a.invert
    a clipTo b
    b clipTo a
    b.invert
    b clipTo a
    b.invert
    a.build(b.all)
    a.invert

    Solid(a.all.toVector)
  }

  def ^(other: Solid): Solid = {
    val a = new Node(polygons)
    val b = new Node(other.polygons)

    a.invert
    b clipTo a
    b.invert
    a clipTo b
    b clipTo a
    a.build(b.all)
    a.invert

    Solid(a.all.toVector)
  }

  def transform(matrix: Matrix44): Solid =
    Solid(polygons.map(_.transform(matrix)))

  override def toString() = s"Solid(polygons=${polygons.length})"
}

object Solid {
  private val baseCube = Vector(
    (Vector(0, 4, 6, 2), Point(-1.0,  0.0,  0.0)),
    (Vector(1, 3, 7, 5), Point( 1.0,  0.0,  0.0)),
    (Vector(0, 1, 5, 4), Point( 0.0, -1.0,  0.0)),
    (Vector(2, 6, 7, 3), Point( 0.0,  1.0,  0.0)),
    (Vector(0, 2, 3, 1), Point( 0.0,  0.0, -1.0)),
    (Vector(4, 5, 7, 6), Point( 0.0,  0.0,  1.0))
  )

  def cube(side: Double): Solid = cube(Point.zero, side)
  def cube(position: Point, side: Double): Solid = box(position, Point(side, side, side))

  def box(size: Point): Solid = box(Point.zero, size)
  def box(position: Point, size: Point): Solid =
    Solid(baseCube map {
      info =>
        Polygon(info._1 map {
          i =>
            val p = Point(
              position.x + size.x * (if((i & 1) != 0) 1 else 0),
              position.y + size.y * (if((i & 2) != 0) 1 else 0),
              position.z + size.z * (if((i & 4) != 0) 1 else 0)
            )
            Vertex(p, info._2)
        })
    })

  def sphere(position: Point, radius: Double)(implicit res: Resolution): Solid = {
    val slices = res.resolutionByRadius(radius)
    val stacks = Math.ceil(slices.toDouble / 2).toInt

    def vertex(theta: Double, phi: Double) = {
      val t = theta * Math.PI * 2
      val p = phi * Math.PI
      val dir = Point(
        Math.cos(t) * Math.sin(p),
        Math.cos(p),
        Math.sin(t) * Math.sin(p)
      )
      Vertex(position + dir * radius, dir)
    }

    val polygons = new mutable.ArrayBuffer[Polygon]
    for (i <- 0 until slices) {
      for (j <- 0 until stacks) {
        var vertices = new mutable.ArrayBuffer[Vertex]
        vertices append vertex(i.toDouble / slices, j.toDouble / stacks)
        if (j > 0)
          vertices append vertex((i.toDouble + 1) / slices, j.toDouble / stacks)
        if (j < stacks - 1)
          vertices append vertex((i.toDouble + 1) / slices, (j.toDouble + 1) / stacks)
        vertices append vertex(i.toDouble / slices, (j.toDouble + 1) / stacks)
        polygons append Polygon(vertices.toVector)
      }
    }
    Solid(polygons.toVector)
  }

  def cylinder(center: Point, length: Double, radius: Double)(implicit res: Resolution): Solid =
    cylinder(center, center translateZ length, radius)(res)

  def cylinder(start: Point, end: Point, radius: Double)(implicit res: Resolution): Solid = {
    val slices   = res.resolutionByRadius(radius)
    val ray      = end - (start)
    val axisZ    = ray.normalize()
    val isY      = (Math.abs(axisZ.y) > 0.5)
    val axisX    = Point(if(isY) 1 else 0, if(isY) 0 else 1, 0).cross(axisZ).normalize()
    val axisY    = axisX.cross(axisZ).normalize()
    val s        = Vertex(start, -axisZ)
    val e        = Vertex(end, axisZ.normalize())
    val polygons = new mutable.ArrayBuffer[Polygon]

    def vertex(stack: Int, slice: Double, normalBlend: Double): Vertex = {
      val angle = slice * Math.PI * 2;
      val out = axisX * Math.cos(angle) + axisY * Math.sin(angle)
      val pos = start + ray * stack + out * radius
      val normal = out * (1 - Math.abs(normalBlend)) + axisZ * normalBlend
      Vertex(pos, normal)
    }

    for(i <- 0 until slices) {
      val t0 = i.toDouble / slices
      val t1 = (i.toDouble + 1) / slices
      polygons append Polygon(Vector(s, vertex(0, t0, -1), vertex(0, t1, -1)))
      polygons append Polygon(Vector(vertex(0, t1, 0), vertex(0, t0, 0), vertex(1, t0, 0), vertex(1, t1, 0)))
      polygons append Polygon(Vector(e, vertex(1, t1, 1), vertex(1, t0, 1)))
    }
    return Solid(polygons.toVector)
  }
}
