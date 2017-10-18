package horstmann

import scala.math.abs

//https://stackoverflow.com/questions/7656937/valid-identifier-characters-in-scala
/*

 An operator identifier consists
 of one or more operator characters.
 Operator characters are printable ASCII
 characters such as +, :, ?, ~ or #.

 More precisely, an operator character belongs to the Unicode set
 of mathematical symbols(Sm)
 or other symbols(So),or to the 7-bit
 ASCII characters that are not letters, digits, parentheses,
 square brackets, curly braces, single or double quote,
 or an underscore, period, semi-colon, comma, or back tick character.

*/

object Section11 extends App {

  object l11 {

    object l111 {
      println("---l111---")

      class Some(val value: String) {
        def +(other: Some) = new Some(s"${value}+${other.value}")

        override def toString: String = s"Some[$value]"

      }

      val some1 = new Some("some1")
      val some2 = new Some("some2")

      println((some1 + some2).value)

      //some1 += some2 //compiler try: some1= some1 + some2 and fire error "reassignment to val"
      //val some4= some1 += some2 // error "reassignment to val"

      var some3 = new Some("some3")
      some3 += some1 //its ok for var
      println(some3)

    }

    object l112 {

      println("---l112---")

      class Foo(val fooVal: String) {
        def +(other: Foo) = new Foo(s"${fooVal}_${other.fooVal}")

        def +=(other: Foo) = new Foo(s"${other.fooVal} -> ${fooVal}")

        override def toString: String = s"Foo[$fooVal]"
      }

      val foo1 = new Foo("foo1")
      val foo2 = new Foo("foo2")

      val foo3 = foo1 += foo2 // += call

      println(foo1) //Foo[foo1]
      println(foo3) //Foo[foo2 -> foo1]

    }

    object l113 {

      println("---l113---")

      class Bar(val data: String) {

        def +(other: Bar) = new Bar(s"+ ${other.data}")

        def *(other: Bar) = new Bar(s"* ${other.data}")

        def +!(other: Bar) = new Bar(s"+! ${other.data}")

        def +&(other: Bar) = new Bar(s"+& ${other.data}")

        def postfix = new Bar(s"postfix ${data}")

        override def toString: String = s"Bar[$data]"

      }

      val bar1 = new Bar("bar1")
      val bar2 = new Bar("bar2")
      val bar3 = new Bar("bar3")
      //http://docs.scala-lang.org/tour/operators.html
      // When an expression uses multiple operators,
      // the operators are evaluated based on
      // the priority of the first character:

      val bar4 = bar1 + bar2 * bar3
      val bar5 = bar1 + (bar2 * bar3)
      //
      val bar6 = bar1 +& bar2 +! bar3
      val bar61 = bar1 +! bar2 +& bar3
      val bar7 = bar1 +& (bar2 +! bar3)

      val pf = bar1 + bar2 postfix
      val pf1 = (bar1 + bar2) postfix

      println(s"$bar1, $bar2, $bar3")

      println(bar4) //Bar[plus asterisk bar3]
      println(bar6)
      println(bar61)
      println(bar7)
      println(pf)

      assert(bar4.data.equals(bar5.data))
      assert(!bar6.data.equals(bar7.data))
      assert(pf.data.equals(pf1.data))

    }

    object l114 {

      class Point(x: Int, y: Int) {

        def update(name: String, data: Int): Point = name match {
          case "x" => new Point(data, y)
          case "y" => new Point(x, data)
          case _ => throw new IllegalArgumentException("unsupported property:" + name)
        }

        override def toString: String = s"Point[x=$x, y=$y]"
      }

      val point1 = new Point(1, 1)
      val point2 = point1("x") = 10 //point1.update("x",10)
      val point3 = point1("y") = 10 //point1.update("y",10)
      try {
        val point4 = point1("z") = 10 //point1.update("y",10)
        println(point4) //just for compiler happiness
      } catch {
        case e: IllegalArgumentException => println("expected exception:" + e.getMessage)
      }

      println(s"point1=$point1, point2=$point2, point3=$point3")

    }

    object l115 {

      class Rectangle(val width: Int, val height: Int) {

        import Rectangle._

        def ==(other: Rectangle): Boolean = (isRect(other)) && (area(this) == area(other))

        def >(other: Rectangle): Boolean = (isRect(other)) && area(this) > area(other)

        def <(other: Rectangle): Boolean = (isRect(other)) && area(this) < area(other)

        override def toString: String = s"Rect[width=$width, height=$height]"
      }

      object Rectangle {

        def isRect(rect: Rectangle): Boolean = rect.isInstanceOf[Rectangle]

        def area(rect: Rectangle) = rect.width * rect.height

        def apply(width: Int, height: Int): Rectangle = new Rectangle(width, height)

        def unapply(rect: Rectangle): Option[(Int, Int)] = if (rect == null) None else Some((rect.width, rect.height))

        def unapply(pair: (Int, Int)): Option[(Int, Int)] = if (pair._1 <= 0 || pair._2 <= 0) None else Some(pair)

      }

      val rect1 = Rectangle(1, 3)

      val Rectangle(a, b) = rect1
      println(s"a=$a, b=$b")

      val rect2 = Rectangle(2, 2)

      println(rect2 > rect1)
      println(null.isInstanceOf[Rectangle])

      val Rectangle(a1, b1) = (12, 11)
      println(s"a1=$a1, b1=$b1")

      try {
        val Rectangle(a2, b2) = (-1, -1)
        println(s"a2=$a2, b2=$b2")
      } catch {
        case _: MatchError => println("match error here")
      }

    }

    //    l111
    //    l112
    //    l113
    //    l114
    l115
  }

  // l11

  object q1 {

    case class Foo(val a: Int) {
      def +(other: Foo) = new Foo(this.a + other.a)

      def ->(other: Foo) = new Foo((this.a.toString + other.a.toString).toInt)
    }

    println(Foo(3) + Foo(4) -> Foo(5)) //'3+4'+'5'=75
    println(Foo(3) -> Foo(4) + Foo(5)) //34+5=39
  }

  object q2 {

    case class MyBigInt(a: BigInt) {
      def **(exp: Int) = new MyBigInt(a.pow(exp))

      def ^(exp: Int) = this.**(exp)
    }

    println(MyBigInt(2) ** 2)
    println(MyBigInt(2) ^ 2)
  }

  object q3 {

    case class Fraction(n: Int, d: Int) {

      import Fraction._

      private val num = if (d == 0) 1 else n * sign(d) / gcd(n, d)
      private val den = if (d == 0) 0 else d * sign(d) / gcd(n, d)

      override def toString: String = s"$num/$den"

      override def equals(obj: scala.Any): Boolean = {
        obj.isInstanceOf[Fraction] &&
          (num == obj.asInstanceOf[Fraction].num) &&
          (den == obj.asInstanceOf[Fraction].den)
      }

      //def +(other:Fraction)=

      def *(other: Fraction) = Fraction(this.num * other.num, this.den * other.den)

      def *(a: Int) = Fraction(this.num * a, this.den)

      def /(other: Fraction) = Fraction(this.num * other.den, this.den * other.num)

      def /(a: Int) = Fraction(this.num, this.den * a)

      def +(other: Fraction) = Fraction(num * other.den + other.den * num, den * other.den)

      def -(other: Fraction) = this + other * (-1)
    }

    object Fraction {

      import math._

      def sign(a: Int) = if (a > 0) 1 else if (a < 0) -1 else 0

      def gcd(a: Int, b: Int): Int = if (b == 0) abs(a) else gcd(b, a % b)
    }

    val f1 = Fraction(15, -6)
    println(f1) // -5/2
    println("0/0=" + Fraction(0, 0))
    println(Fraction.gcd(2, 3))
    println(Fraction.gcd(3, 3))
    println(Fraction.gcd(6, 3))
    println(Fraction.gcd(3, 6))
    // '*'
    println(Fraction(1, 2) * Fraction(1, 3))
    println(Fraction(1, 2) * Fraction(0, 0))
    println(Fraction(1, 2) * 2)
    println(Fraction(1, 2) * 0)
    // '/'

    println(Fraction(1, 2) / Fraction(1, 2)) // 1/1
    println(Fraction(1, 2) / Fraction(0, 0)) // 1/1
    println(Fraction(1, 2) / 2) // 1/4

    assert(Fraction(1, 4) + Fraction(1, 4) == Fraction(1, 2)) // 1/2
    assert(Fraction(1, 4) + Fraction(1, 4) == Fraction(1, 2)) // 1/2

  }

  //q1
  q2
  q3
}
