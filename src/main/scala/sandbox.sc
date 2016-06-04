import tre.d3.Point
import tre.Precision._
import tre.csg.Solid

val p = Point(1,2,3) * 7

1.0 =~ 1.000001
1.0 >~ 1
1.1 >~ 1

1.0 <~ 1
0.9 <~ 1

val cube1 = Solid.box(Point(10, 10, 10))
val cube2 = Solid.box(Point(5, 5, 5), Point(10, 10, 10))