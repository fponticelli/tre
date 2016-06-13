package tre.csg

import tre.d3.{Point, Polygon, Vertex}
import scala.collection.mutable

trait Solid extends Iterable[Polygon] {
  def polygons: List[Polygon]

  def iterator() = polygons iterator

  def +(other: Solid): Solid = {
    val a = new Node(Some(polygons))
    val b = new Node(Some(other.polygons))

    a clipTo b
    b clipTo a
    b.invert
    b clipTo a
    b.invert
    a.build(b.all)

    Solid.fromPolygons(a.all.toList)
  }

  def -(other: Solid): Solid = {
    val a = new Node(Some(polygons))
    val b = new Node(Some(other.polygons))

    a.invert
    a clipTo b
    b clipTo a
    b.invert
    b clipTo a
    b.invert
    a.build(b.all)
    a.invert

    Solid.fromPolygons(a.all.toList)
  }

  def ^(other: Solid): Solid = {
    val a = new Node(Some(polygons))
    val b = new Node(Some(other.polygons))

    a.invert
    b clipTo a
    b.invert
    a clipTo b
    b clipTo a
    a.build(b.all)
    a.invert

    Solid.fromPolygons(a.all.toList)
  }

  override def toString() = s"Solid(polygons=${polygons.length})"
}

object Solid {
  def fromPolygons(polys: List[Polygon]) =
    new Solid {
      def polygons = polys
    }


  private val baseCube = List(
    (List(0, 4, 6, 2), Point(-1.0,  0.0,  0.0)),
    (List(1, 3, 7, 5), Point( 1.0,  0.0,  0.0)),
    (List(0, 1, 5, 4), Point( 0.0, -1.0,  0.0)),
    (List(2, 6, 7, 3), Point( 0.0,  1.0,  0.0)),
    (List(0, 2, 3, 1), Point( 0.0,  0.0, -1.0)),
    (List(4, 5, 7, 6), Point( 0.0,  0.0,  1.0))
  )

  def box(size: Point): Solid = box(Point.zero, size)
  def box(position: Point, size: Point): Solid =
    fromPolygons(baseCube map {
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

  def sphere(position: Point, radius: Double = 1.0)(implicit res: Resolution): Solid = {
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
        polygons append Polygon(vertices.toList)
      }
    }
    Solid.fromPolygons(polygons.toList)
  }

  def cylinder(start: Point, end: Point, radius: Double = 1.0)(implicit res: Resolution): Solid = {
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
      polygons append Polygon(List(s, vertex(0, t0, -1), vertex(0, t1, -1)))
      polygons append Polygon(List(vertex(0, t1, 0), vertex(0, t0, 0), vertex(1, t0, 0), vertex(1, t1, 0)))
      polygons append Polygon(List(e, vertex(1, t1, 1), vertex(1, t0, 1)))
    }
    return Solid.fromPolygons(polygons.toList)
  }

  trait Resolution {
    def resolutionByRadius(radius: Double): Int
  }

  implicit object StandardResolution extends Resolution {
    def resolutionByRadius(radius: Double): Int = 36
  }
}

/*
  public static function cylinder(start: Point, end: Point, radius = 1.0, ?resolution: Double -> Int) {
    if(null == resolution)
      resolution = getResolution
    var slices   = resolution(radius),
        ray      = end.subtractPoint(start),
        axisZ    = ray.normalize(),
        isY      = (Math.abs(axisZ.y) > 0.5),
        axisX    = Point.create(isY ? 1: 0, isY ? 0: 1, 0).cross(axisZ).normalize(),
        axisY    = axisX.cross(axisZ).normalize(),
        s        = new Vertex(start, axisZ.negate()),
        e        = new Vertex(end, axisZ.normalize()),
        polygons = [],
        t0, t1
    function point(stack, slice: Double, normalBlend) {
      var angle = slice * Math.PI * 2,
          out = axisX.multiply(Math.cos(angle)).addPoint(axisY.multiply(Math.sin(angle))),
          pos = start.addPoint(ray.multiply(stack)).addPoint(out.multiply(radius)),
          normal = out.multiply(1 - Math.abs(normalBlend)).addPoint(axisZ.multiply(normalBlend))
      return new Vertex(pos, normal)
    }
    for (i in 0...slices) {
      t0 = i / slices
      t1 = (i + 1) / slices
      polygons.push(new Polygon([s, point(0, t0, -1), point(0, t1, -1)]))
      polygons.push(new Polygon([point(0, t1, 0), point(0, t0, 0), point(1, t0, 0), point(1, t1, 0)]))
      polygons.push(new Polygon([e, point(1, t1, 1), point(1, t0, 1)]))
    }
    return Solid.fromPolygons(polygons)
  }
 */
