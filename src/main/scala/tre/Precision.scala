package tre
import scala.annotation.tailrec

/**
  * Created by francoponticelli on 6/3/16.
  */
case class Precision(val p:Double)

object Precision {
  implicit class DoubleWithAlmostEquals(val d:Double) extends AnyVal {
    def ~=(d2: Double)(implicit p: Precision) = (d - d2).abs < p.p
  }

  implicit object MachinePrecision extends Precision({
    @tailrec
    def calc(machEps: Float): Float = {
      if ((1.0 + (machEps / 2.0)).toFloat != 1.0)
        calc(machEps / 2f)
      else
        machEps
    }
    calc(1f).toDouble
  })
}
