package tre.d3

case class Polygon(vertices: List[Vertex]) extends Iterable[Vertex] {
  def flip() = Polygon(vertices.reverse.map{_.flip})
  override def iterator = vertices.iterator

  lazy val plane =
    Plane.fromPoints(vertices(0).position, vertices(1).position, vertices(2).position)

  def triangles(): List[(Vertex, Vertex, Vertex)] = {
    val first = vertices.head
    (2 until vertices.length).foldLeft(List[(Vertex, Vertex, Vertex)]()) {
      (lst: List[(Vertex, Vertex, Vertex)], index: Int) => {
        lst :+ (first, vertices(index - 1), vertices(index))
      }
    }
  }
}
