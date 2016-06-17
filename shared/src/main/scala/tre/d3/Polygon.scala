package tre.d3

case class Polygon(vertices: Vector[Vertex]) extends Iterable[Vertex] with Transformable[Polygon] {
  def flip() = Polygon(vertices.reverse.map{_.flip})
  override def iterator = vertices.iterator

  lazy val plane =
    Plane.fromPoints(vertices(0).position, vertices(1).position, vertices(2).position)

  def triangles(): Vector[(Vertex, Vertex, Vertex)] = {
    val first = vertices.head
    (2 until vertices.length).foldLeft(Vector[(Vertex, Vertex, Vertex)]()) {
      (lst: Vector[(Vertex, Vertex, Vertex)], index: Int) => {
        lst :+ (first, vertices(index - 1), vertices(index))
      }
    }
  }

  def transform(matrix: Matrix44): Polygon =
    Polygon(vertices.map(_.transform(matrix)))
}
