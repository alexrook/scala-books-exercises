package horstmann.s8.math

//http://matheusdev.tumblr.com/post/51071594017/scala-and-extending-numerict

import scala.math.Numeric._

trait FractionalExt[T] extends Fractional[T] {

  def sqrt(v: T): T

  def cos(v: T): T

  def sin(v: T): T

  def round(v: T): T

}

object FractionalExt {

  //  trait FloatIsFractionalExt extends FractionalExt[Float] {
  //    override def sqrt(v: Float): Float = math.sqrt(v).toFloat
  //  }

  //  implicit object FloatIsFractionalExt extends FloatIsFractional
  //    with FloatIsFractionalExt
  //    with Ordering.FloatOrdering

  implicit object FloatIsFractionalExt extends FloatIsFractional
    with FractionalExt[Float]
    with Ordering.FloatOrdering {

    override def sqrt(v: Float): Float = math.sqrt(v).toFloat

    override def cos(v: Float): Float = math.cos(v).toFloat

    override def sin(v: Float): Float = math.sin(v).toFloat

    override def round(v: Float): Float = math.round(v).toFloat
  }

  implicit object DoubleIsFractionalExt extends DoubleIsFractional
    with FractionalExt[Double]
    with Ordering.DoubleOrdering {

    override def sqrt(v: Double): Double = math.sqrt(v)

    override def cos(v: Double): Double = math.cos(v)

    override def sin(v: Double): Double = math.sin(v)

    override def round(v: Double): Double = math.round(v).toDouble
  }

}

