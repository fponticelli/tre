package tre.util

object Measure {
  def time[R](block: => R): Measurement[R] = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    Measurement(result, t1 - t0)
  }

  case class Measurement[R](computation: R, value: Double)
}
