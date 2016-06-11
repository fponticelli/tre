package tre.d3

case class Polygon(vertices: List[Vertex]) extends Iterable[Vertex] {
  def flip() = Polygon(vertices.reverse.map{_.flip})
  override def iterator = vertices.iterator

  lazy val plane =
    Plane.fromPoints(vertices(0).position, vertices(1).position, vertices(2).position)
