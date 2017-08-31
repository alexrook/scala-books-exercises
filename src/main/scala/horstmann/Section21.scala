package horstmann

import scala.language.implicitConversions

//some useful cmd : scalac -Xprint:typer Section21.scala

object Section21 extends App {

  def printRet(ret: Any, i: Int): Unit = {
    println("ret" + i + " = " + ret + ", " + ret.getClass.getName)
  }

  object l1 {

    object l211 {
      println("----------l211-----------")

      case class Fraction(val numerator: Int, val denominator: Int) {
        def *(other: Fraction): Fraction =
          Fraction(this.numerator * other.numerator, this.denominator * other.denominator)

        override def toString: String = numerator + "/" + denominator
      }

      object Fraction { //scalac ищет неявние функции в объектах компаньонах и как простые функции
        implicit def int2Fraction(v: Int): Fraction = Fraction(v, 1)

        implicit def fraction2Double(f: Fraction): Double = f.numerator.toDouble / f.denominator
      }

      class RichFraction(val from: Fraction) {
        def /(other: Fraction): Fraction =
          Fraction(from.numerator * other.denominator, from.denominator * other.numerator)

        def /(other: RichFraction): RichFraction =
          new RichFraction(this / other.from)

        override def toString: String = from.toString
      }

      implicit def fraction2RichFraction(from: Fraction) = new RichFraction(from)

      val ret0 = Fraction(2, 3) * 3 //Fraction(3,1) * (Int->Fraction)
      val ret1 = 3 * Fraction(2, 3) // Int * (Fraction ->Double)

      printRet(ret0, 0)
      printRet(ret1, 1)

      // (implicit Fraction->RichFraction) / (Int->Fraction)
      val ret2 = Fraction(1, 3) / 3
      printRet(ret2, 2)
      // (implicit Fraction->RichFraction) / RichFraction
      val ret3 = Fraction(1, 2) / new RichFraction(new Fraction(1, 2))
      printRet(ret3, 3)

      val ret4 = Fraction(1, 2) * 3.0 //(Fraction->Double)*Double
      printRet(ret4, 4)
    }

    object l216 {

      println("----------l216-----------")

      def smaller[T](a: T, b: T)(implicit conv: T => Ordered[T]) = if (a < b) a else b

     // def smaller[T<: Ordered[T]](a: T, b: T) = if (a < b) a else b

      printRet(smaller(1, 2), 1)

    }

    l211
    l216

  }

  l1
}
