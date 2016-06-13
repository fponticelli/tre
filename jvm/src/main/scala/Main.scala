import tre.csg.Solid._
import tre.d3._
import tre.d3.Point._

object Main {
  def main(args: Array[String]): Unit = {
    println("before")
    def time[R](block: => R): R = {
      val t0 = System.nanoTime()
      val result = block    // call-by-name
      val t1 = System.nanoTime()
      println("Elapsed time: " + (t1 - t0) / 1000000 + "ms")
      result
    }
    val t = time {
        cube((-0.5,-0.5,-0.5), 1.0) +
        cube((0.1,0.1,0.1), 1.0) +
        cube((-1.1,-1.1,-1.1), 1.0) ^
        sphere((0.0,0.0,0.0), 0.7) -
        sphere((0.5,0.5,0.5), 0.35) -
        cylinder((0.0,0.0,-0.95), (0.0,0.0,0.95), 0.25) -
        cylinder((0.0,-0.95,0.0), (0.0,0.95,0.0), 0.35) -
        cylinder((-0.95,0.0,0.0), (0.95,0.0,0.0), 0.15)
      }
    println(t)
    println("after")
  }
}
