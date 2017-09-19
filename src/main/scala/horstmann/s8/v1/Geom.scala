package horstmann.s8.v1

class Point(val x: Double, val y: Double) {

  def rotate(center: Point, angle: Double) = {
    val dx = this.x - center.x
    val dy = this.y - center.y
    //point.x * cos(angle) - point.y * sin(angle)
    val _x = dx * math.cos(angle) - dy * math.sin(angle) + center.x
    //point.x * sin(angle) + point.y * cos(angle)
    val _y = dx * math.sin(angle) + dy * math.cos(angle) + center.y

    //round is  try to avoid cos(90) problem
    Point(math.round(_x), math.round(_y))
  }

  def scale(center: Point, factor: Double): Point = {

    def linearScale(center: Double, dot: Double): Double = if (center == dot) {
      dot
    } else if (center > dot) {
      center - (center - dot) * factor
    } else center + (dot - center) * factor

    val _x = linearScale(center.x, this.x)
    val _y = linearScale(center.y, this.y)

    Point(_x, _y)
  }

  def move(dx: Double, dy: Double) = Point(this.x + dx, this.y + dy)

  override def toString: String = s"point(x:$x,y:$y)"
}

object Point {
  def apply(x: Double, y: Double) = new Point(x, y)
}

trait Figure {

  def area: Double

  def perimeter: Double

  def center: Point

  def rotate(angle: Double): Figure

  def scale(factor: Double): Figure

  def move(newCenter: Point): Figure

  def transform(trans: Figure => Figure): Figure = trans(this)

}

//type Vertex = Point

class Vertex(override val x: Double, override val y: Double)
  extends Point(x, y)

object Vertex {
  def apply(point: Point) = new Vertex(point.x, point.y)
}

case class LineSegment(val a: Vertex, val b: Vertex) {

  import LineSegment._

  val length = vectorLength(a, b)
}

object LineSegment {

  def apply(a: Vertex, b: Vertex): LineSegment = new LineSegment(a, b)

  def vectorLength(a: Vertex, b: Vertex): Double =
    Math.sqrt(Math.pow(b.x - a.x, 2) + Math.pow(b.y - a.y, 2))

  def middleOf(lineSegment: LineSegment): Point =
    new Point((lineSegment.a.x + lineSegment.b.x) / 2,
      (lineSegment.a.y + lineSegment.b.y) / 2)

}


case class Curve(val a: Point, val b: Point,
                 val center: Point, val radius: Double)

trait ClosedCurve extends Figure {
  def segments: Set[Curve]
}

class Circle(override val center: Point, override val radius: Double)
  extends Curve(Point(center.x + radius, center.y),
    Point(center.x + radius, center.y)
    , center, radius) with ClosedCurve {

  override def segments: Set[Curve] = Set(this)

  override def area: Double = Math.PI * radius * radius

  override def perimeter: Double = 2 * Math.PI * radius

  override def rotate(angle: Double): Figure = this

  override def scale(factor: Double): Figure = new Circle(center, radius * factor)

  override def move(newCenter: Point): Figure = new Circle(newCenter, radius)
}

trait Polygon[T <: Polygon[T]] extends Figure {

  def corners: List[Vertex]

  def segments: List[LineSegment]

  def translate(f: Vertex => Vertex): T

}

trait Quadrilateral[T <: Quadrilateral[T]] extends Polygon[T] {
  def a: Vertex

  def b: Vertex

  val c: Vertex

  def d: Vertex

  override def corners: List[Vertex] = List(a, b, c, d)

  override def segments: List[LineSegment] = List(LineSegment(a, b),
    LineSegment(a, d), LineSegment(b, c), LineSegment(d, c))

}

class Square(val a: Vertex, val b: Vertex, val c: Vertex, val d: Vertex)
  extends Quadrilateral[Square] {

  import horstmann.s8.v1.LineSegment._

  val side = LineSegment(a, b).length

  override def translate(f: (Vertex) => Vertex): Square = {
    val _corners = corners.map(f)
    new Square(_corners(0), _corners(1), _corners(2), _corners(3))
  }

  override def area: Double = side * side

  override def perimeter: Double = 4 * side

  override def center: Point = middleOf(LineSegment(a, c))

  override def rotate(angle: Double): Figure = translate(vertex => {
    Vertex(vertex.rotate(center, angle))
  })

  override def scale(factor: Double): Figure = translate(vertex => {
    Vertex(vertex.scale(center, factor))
  })

  override def move(newCenter: Point): Figure = {
    val dX = newCenter.x - center.x
    val dY = newCenter.x - center.x
    translate(vertex => {
      Vertex(vertex.move(dX, dY))
    })
  }
}

