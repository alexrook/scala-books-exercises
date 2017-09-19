package horstmann.s8.geom


trait Coordinate[B, A <: Coordinate[B, A]] {
  def distance(other: A)(implicit num: Numeric[B]): B
}

class Cartesian2D[N: Numeric](val x: N, val y: N) extends Coordinate[N, Cartesian2D[N]] {
  override def distance(other: Cartesian2D[N])(implicit num: Numeric[N]): N = {

    import num.mkNumericOps

    val x = this.x + other.x
    val y = this.y - other.y
    val xx = x * x
    val yy = y * y
    val pls = xx + yy
    println(num.getClass.getName + ":" + num.isInstanceOf[Integral[Int]])
    val can = math.sqrt(num.toDouble(pls)).asInstanceOf[N] // TODO: pls avoid type cast !
    //if (num.isInstanceOf[])
    can

  }
}


case class Point[N: Numeric, C <: Coordinate[N, C]](val dot: C) {
  def distance(other: Point[N, C]): N = dot.distance(other.dot)
}

case class LineSegment[N: Numeric, C <: Coordinate[N, C]](val a: Point[N, C],
                                                          val b: Point[N, C]) {
  def length(implicit num: Numeric[N]): N = a.distance(b)
}


trait Figure[N, C <: Coordinate[N, C]] {

  def area(implicit num: Numeric[N]): N

  def perimeter(implicit num: Numeric[N]): N

  def center: Point[N, C]

  def move(newCenter: Point[N, C]): Figure[N, C]

  def rotate(angle: N)(implicit num: Numeric[N]): Figure[N, C]

  def scale(factor: N)(implicit num: Numeric[N]): Figure[N, C]

}

trait Polygon[N, C <: Coordinate[N, C]] extends Figure[N, C] {

  def corners: Set[Point[N, C]]

  def segments: Set[LineSegment[N, C]]

}

case class Square[N: Numeric, C <: Coordinate[N, C]](val a: Point[N, C],
                                                     val b: Point[N, C],
                                                     val c: Point[N, C],
                                                     val d: Point[N, C])
  extends Polygon[N, C] {

  val side: N = segments.head.length

  override def corners = Set(a, b, c, d)

  override def segments = Set(
    LineSegment(a, b),
    LineSegment(b, c),
    LineSegment(c, d),
    LineSegment(d, a))

  override def area(implicit num: Numeric[N]) = {
    println("side:" + side)
    num.times(side, side)
  }

  override def perimeter(implicit num: Numeric[N]) = num.times(side, 4.asInstanceOf[N])

  override def center = ???

  override def move(newCenter: Point[N, C]) = ???

  override def rotate(angle: N)(implicit num: Numeric[N]) = ???

  override def scale(factor: N)(implicit num: Numeric[N]) = ???
}

object Test extends App {


  val cart1i = new Cartesian2D[Int](2, 2)
  val cart2i = new Cartesian2D[Int](1, 2)

  println(cart1i.distance(cart2i))

  val cart1f = new Cartesian2D[Float](2.1f, 2)
  val cart2f = new Cartesian2D[Float](1, 3.1f)

  println(cart1f.distance(cart2f))
  //---
  val point1i: Point[Int, Cartesian2D[Int]] = Point(cart1i)
  val point2i: Point[Int, Cartesian2D[Int]] = Point(cart2i)
  val lineSegment = LineSegment(point1i, point2i)
  println(lineSegment.length)
  val point1f: Point[Float, Cartesian2D[Float]] = Point(new Cartesian2D[Float](2.1f, 2))
  val point2f: Point[Float, Cartesian2D[Float]] = Point(cart2f)
  val lineSegment2 = LineSegment(point1f, point2f)
  println(lineSegment2.length)

  type Cartesian2dDouble = Cartesian2D[Double]
  type PointCartesian2dDouble = Point[Double, Cartesian2dDouble]

  val point1d: PointCartesian2dDouble = Point(new Cartesian2dDouble(21.3d, 11))
  val point2d: PointCartesian2dDouble = Point(new Cartesian2dDouble(14d, 1.3))
  val lineSegment3 = LineSegment(point1d, point2d)
  println(lineSegment3.length)

  val square: Square[Double, Cartesian2D[Double]] = new Square(
    Point(new Cartesian2D(0d, 0)),
    Point(new Cartesian2D(0, 2d)),
    Point(new Cartesian2D(2d, 2)),
    Point(new Cartesian2D(2, 0d)))

  println(square.area)

  //
  //  val squareI: Square[Float, Cartesian2D[Float]] = new Square(
  //    Point(new Cartesian2D(0, 0)),
  //    Point(new Cartesian2D(0, 2)),
  //    Point(new Cartesian2D(2, 2)),
  //    Point(new Cartesian2D(2, 0)))
  //
  //  println(squareI.area)

}
