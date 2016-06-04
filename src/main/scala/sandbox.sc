import tre.d3.Point
import tre.Precision._

val p = Point(1,2,3) * 7

1.0 =~ 1.000001
1.0 >~ 1
1.1 >~ 1

1.0 <~ 1
0.9 <~ 1