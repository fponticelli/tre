package tre.d3

case class Vertex(position : Point, normal : Point) extends Transformable[Vertex] {
  def interpolate(that: Vertex)(t: Double) =
    Vertex(
      position.interpolate(that.position)(t),
      normal.interpolate(that.normal)(t)
    )

  def flip() = Vertex(position, -normal)

  def transform(matrix: Matrix44): Vertex =
    Vertex(matrix.leftMultiplyPoint(this.position), matrix.leftMultiplyPoint(this.normal))

  override def toString() =
    s"Vertex(position=$position, normal=$normal)";
}

/*
  inline public function transform(matrix : Matrix44)
    return new Vertex(position.transform(matrix), normal.transform(matrix));
 */
