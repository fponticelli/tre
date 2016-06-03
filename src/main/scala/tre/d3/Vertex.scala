package tre.d3

/**
  * Created by francoponticelli on 6/2/16.
  */
case class Vertex(position : Point, normal : Point) {
  def interpolate(that: Vertex)(t: Double) =
    Vertex(
      position.interpolate(that.position)(t),
      normal.interpolate(that.normal)(t)
    )

  def flip() = Vertex(position, -normal)

  override def toString() =
    s"Vertex(position=$position, normal=$normal)";
}

/*
  inline public function transform(matrix : Matrix44)
    return new Vertex(position.transform(matrix), normal.transform(matrix));
 */