package tre
import scala.annotation.tailrec


object Precision {
  val machineEpsilon = {
    @tailrec
    def calc(machEps: Float): Float = {
      if ((1.0 + (machEps / 2.0)).toFloat != 1.0)
        calc(machEps / 2f)
      else
        machEps
    }

    calc(1f).toDouble
  }

  implicit class DoubleWithAlmostEquals(val d:Double) extends AnyVal {
    def =~(d2: Double) = (d - d2).abs < machineEpsilon
    def <~(d2: Double) = d < (d2 - machineEpsilon)
    def >~(d2: Double) = d > (d2 + machineEpsilon)
  }
}