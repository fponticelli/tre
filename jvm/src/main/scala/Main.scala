import tre.csg.Solid
import tre.csg.Solid._
import tre.d3._
import tre.d3.Point._
import tre.util.Measure
import tre.csg.Resolution

object Main {
  def main(args: Array[String]): Unit = {
    val m = Measure.time(buildSolids)
    println(s"CSG computation took: ${m.value / 1000000000.0}s")
  }

  def buildSolids(): Vector[Solid] = {
    implicit val res = Resolution.byFeature(0.08)
    val b = cube((-0.5,-0.5,-0.5), 1.0) +
            cube((0.1,0.1,0.1), 1.0) +
            cube((-1.1,-1.1,-1.1), 1.0) ^
            sphere((0.0,0.0,0.0), 0.7) -
            sphere((-0.5,-0.5,-0.5), 0.35) -
            cylinder((0.0,0.0,-0.95), (0.0,0.0,0.95), 0.25) -
            cylinder((0.0,-0.95,0.0), (0.0,0.95,0.0), 0.35) -
            cylinder((-0.95,0.0,0.0), (0.95,0.0,0.0), 0.15)
    Vector(b)
  }
}
