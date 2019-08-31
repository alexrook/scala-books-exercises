package cs.math

//http://matheusdev.tumblr.com/post/51071594017/scala-and-extending-numerict
import scala.math.Numeric._

trait FractionalExt[T] extends Fractional[T] {

  protected var roundingMode: BigDecimal.RoundingMode.RoundingMode = BigDecimal.RoundingMode.HALF_UP

  protected var scale: Int = 4

  def sqrt(v: T): T

  def cos(v: T): T

  def sin(v: T): T

  def round(v: T): Long

  def rounded(v: T): T

  def setScale(scale: Int, roundingMode: BigDecimal.RoundingMode.RoundingMode): Unit = {
    this.scale = scale
    this.roundingMode = roundingMode
  }


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
    with Ordering.Float.TotalOrdering {

    override def sqrt(v: Float): Float = math.sqrt(v).toFloat

    override def cos(v: Float): Float = math.cos(v).toFloat

    override def sin(v: Float): Float = math.sin(v).toFloat

    override def round(v: Float): Long = math.round(v)

    override def rounded(v: Float): Float = BigDecimal.decimal(v).setScale(scale, roundingMode).toFloat

  }

  implicit object DoubleIsFractionalExt extends DoubleIsFractional
    with FractionalExt[Double]
    with Ordering.Double.TotalOrdering {

    override def sqrt(v: Double): Double = math.sqrt(v)

    override def cos(v: Double): Double = math.cos(v)

    override def sin(v: Double): Double = math.sin(v)

    override def round(v: Double): Long = math.round(v)

    override def rounded(v: Double): Double = BigDecimal.decimal(v).setScale(scale, roundingMode).toDouble
  }

  implicit object BigDecimalIsFractionalExt extends BigDecimalIsFractional
    with FractionalExt[BigDecimal]
    with Ordering.BigDecimalOrdering {

    override def sqrt(v: BigDecimal): BigDecimal =   BigDecimal.decimal(math.sqrt(v.doubleValue))

    override def cos(v: BigDecimal): BigDecimal =
      BigDecimal.decimal(math.cos(v.doubleValue))

    override def sin(v: BigDecimal): BigDecimal =
      BigDecimal.decimal(math.sin(v.doubleValue))

    override def round(v: BigDecimal): Long = math.round(v.doubleValue)

    override def rounded(v: BigDecimal): BigDecimal = v.setScale(scale, roundingMode)

  }


}

