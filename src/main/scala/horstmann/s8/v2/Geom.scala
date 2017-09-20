package horstmann.s8.geom.v2

import horstmann.s8.math._


trait Coordinate[B, A <: Coordinate[B, A]] {
  def distance(other: A)(implicit num: FractionalExt[B]): B

  def middleTo(other: A)(implicit num: FractionalExt[B]): A

  def delta(other: A)(implicit num: FractionalExt[B]): A

  def rotate(center: A, angle: B)(implicit num: FractionalExt[B]): A

  def scale(center: A, factor: B)(implicit num: FractionalExt[B]): A
}

class Cartesian2D[N](val x: N, val y: N)
  extends Coordinate[N, Cartesian2D[N]] {

  override def distance(other: Cartesian2D[N])
                       (implicit num: FractionalExt[N]): N = {

    import num.mkNumericOps

    val x = this.x - other.x
    val y = this.y - other.y
    val xx = x * x
    val yy = y * y
    val pls = xx + yy
    num.sqrt(pls)

  }

  override def middleTo(other: Cartesian2D[N])(implicit num: FractionalExt[N]) = {
    import num.mkNumericOps
    new Cartesian2D[N]((this.x - other.x) / num.fromInt(2),
      (this.y - other.y) / num.fromInt(2))
  }

  override def delta(other: Cartesian2D[N])(implicit num: FractionalExt[N]) = {
    import num.mkNumericOps
    new Cartesian2D[N](this.x - other.x, this.y - other.y)
  }

  override def rotate(center: Cartesian2D[N], angle: N)(implicit num: FractionalExt[N]) = {
    import num.mkNumericOps
    val dx = this.x - center.x
    val dy = this.y - center.y
    //point.x * cos(angle) - point.y * sin(angle)
    val _x = dx * num.cos(angle) - dy * num.sin(angle) + center.x
    //point.x * sin(angle) + point.y * cos(angle)
    val _y = dx * num.sin(angle) + dy * num.cos(angle) + center.y


    new Cartesian2D(num.round(_x), num.round(_y))
  }

  override def scale(center: Cartesian2D[N], factor: N)(implicit num: FractionalExt[N]) = {
    import num.mkNumericOps

    def linearScale(center: N, dot: N): N = if (num.equiv(center, dot)) {
      dot
    } else if (num.gt(center, dot)) {
      center - (center - dot) * factor
    } else center + (dot - center) * factor

    val _x = linearScale(center.x, this.x)
    val _y = linearScale(center.y, this.y)

    new Cartesian2D[N](_x, _y)
  }

  override def toString = s"$x,$y"
}


case class Point[N: FractionalExt, C <: Coordinate[N, C]](val dot: C) {
  def distance(other: Point[N, C]): N = dot.distance(other.dot)

  def middleTo(other: Point[N, C]): Point[N, C] = Point(dot.middleTo(other.dot))

  def move(coordinate: C): Point[N, C] = Point[N, C](coordinate)//todo: shift by vect

  def move(point: Point[N, C]): Point[N, C] = point

  def rotate(center: Point[N, C], angle: N): Point[N, C] = Point(dot.rotate(center.dot, angle))

  def scale(center: Point[N, C], factor: N): Point[N, C] = Point(dot.scale(center.dot, factor))
}

case class LineSegment[N: FractionalExt, C <: Coordinate[N, C]]
(val a: Point[N, C], val b: Point[N, C]) {
  def length: N = a.distance(b)
}


trait Figure[N, C <: Coordinate[N, C]] {

  def area(implicit num: FractionalExt[N]): N

  def perimeter(implicit num: FractionalExt[N]): N

  def center: Point[N, C]

  def move(newCenter: Point[N, C])(implicit num: FractionalExt[N]): Figure[N, C]

  def rotate(angle: N)(implicit num: FractionalExt[N]): Figure[N, C]

  def scale(factor: N)(implicit num: FractionalExt[N]): Figure[N, C]

}

trait Polygon[N, C <: Coordinate[N, C]]
  extends Figure[N, C] {

  def corners: List[Point[N, C]]

  def segments: List[LineSegment[N, C]]

  def translate(f: Point[N, C] => Point[N, C]): Polygon[N, C]

  override def rotate(angle: N)(implicit num: FractionalExt[N]): Polygon[N, C] = {
    translate(point => point.rotate(center, angle))
  }

  override def move(newCenter: Point[N, C])(implicit num: FractionalExt[N]): Polygon[N, C] = {
    val delta = center.dot.delta(newCenter.dot)
    translate(point => point.move(delta))
  }

  override def scale(factor: N)(implicit num: FractionalExt[N]) =
    translate(point => point.scale(center, factor))

  override def perimeter(implicit num: FractionalExt[N]) =
    segments.foldLeft(num.zero)((acc, segment) => num.plus(acc, segment.length))
}

abstract class Quadrilateral[N: FractionalExt, C <: Coordinate[N, C]]
  extends Polygon[N, C] {

  def a: Point[N, C]

  def b: Point[N, C]

  def c: Point[N, C]

  def d: Point[N, C]


  override def corners: List[Point[N, C]] = List(a, b, c, d)

  override def segments = List(LineSegment(a, b),
    LineSegment[N, C](a, d), LineSegment[N, C](b, c), LineSegment[N, C](d, c))


}

case class Square[N: FractionalExt, C <: Coordinate[N, C]](val a: Point[N, C],
                                                           val b: Point[N, C],
                                                           val c: Point[N, C],
                                                           val d: Point[N, C])
  extends Quadrilateral[N, C] {

  val side: N = segments(0).length

  override def translate(f: (Point[N, C]) => Point[N, C]): Square[N, C] = {
    val _corners = corners.map(f)
    new Square(_corners(0), _corners(1), _corners(2), _corners(3))
  }

  override def area(implicit num: FractionalExt[N]) = {
    num.times(side, side)
  }

  override def center = c.middleTo(a)

}

object Test extends App {

  val cart1f = new Cartesian2D[Float](2, 2)
  val cart2f = new Cartesian2D[Float](1, 2)

  println(cart1f.distance(cart2f))


  val cart1d = new Cartesian2D[Double](2.1, 2)
  val cart2d = new Cartesian2D[Double](1, 3.1)

  println(cart1d.distance(cart2d))
  //---
  val point1f: Point[Float, Cartesian2D[Float]] = Point(cart1f)
  val point2f: Point[Float, Cartesian2D[Float]] = Point(cart2f)
  val lineSegment = LineSegment(point1f, point2f)
  println(lineSegment.length)
  val point1d: Point[Double, Cartesian2D[Double]] = Point(new Cartesian2D[Double](2.1, 2))
  val point2d: Point[Double, Cartesian2D[Double]] = Point(cart2d)
  val lineSegment2 = LineSegment(point1d, point2d)
  println(lineSegment2.length)
  //--
  type Cartesian2dDouble = Cartesian2D[Double]
  type PointCartesian2dDouble = Point[Double, Cartesian2dDouble]

  val point11d: PointCartesian2dDouble = Point(new Cartesian2dDouble(21.3d, 11))
  val point22d: PointCartesian2dDouble = Point(new Cartesian2dDouble(14d, 1.3))
  val lineSegment3 = LineSegment(point11d, point22d)
  println(lineSegment3.length)
  //--
  val square: Square[Double, Cartesian2D[Double]] = new Square(
    Point(new Cartesian2D(0d, 0)),
    Point(new Cartesian2D(0, 2d)),
    Point(new Cartesian2D(2d, 2)),
    Point(new Cartesian2D(2, 0d)))

  println(square)
  println("area="+square.area)
  println("side="+square.side)
  println("perimeter="+square.perimeter)
  println("center="+square.center)

  val m1=square.move(Point(new Cartesian2dDouble(0,0)))
  println(m1)

  val squareF: Square[Float, Cartesian2D[Float]] = new Square(
    Point(new Cartesian2D(0, 0)),
    Point(new Cartesian2D(0.1f, 2)),
    Point(new Cartesian2D(2, 2.4f)),
    Point(new Cartesian2D(2, 0)))

  println(squareF.area)
  println(squareF.side)
  println(squareF.perimeter)
}
