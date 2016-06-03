package tre.d3

/**
  * Created by francoponticelli on 6/2/16.
  */
case class Polygon(vertices: List[Vertex]) {
  def flip() =
    Polygon(vertices.reverse.map{_.flip})
}

/*
  function get_plane()
    return Plane.fromPoints(vertices[0].position, vertices[1].position, vertices[2].position);
 */