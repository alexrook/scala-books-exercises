package horstmann

import scala.collection.mutable.ArrayBuffer

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

      private val num: Int = if (d == 0) 0 else n * sign(d) / gcd(n, d)
      private val den: Int = if (d == 0) 1 else d * sign(d) / gcd(n, d)

      override def toString: String = s"$num/$den"

      override def equals(obj: scala.Any): Boolean = {
        obj.isInstanceOf[Fraction] &&
          (num == obj.asInstanceOf[Fraction].num) &&
          (den == obj.asInstanceOf[Fraction].den)
      }

      def *(other: Fraction): Fraction = Fraction(this.num * other.num, this.den * other.den)

      def *(a: Int): Fraction = Fraction(this.num * a, this.den)

      def /(other: Fraction): Fraction = if (nonZero(other)) Fraction(this.num * other.den, this.den * other.num)
      else throw new ArithmeticException("division by zero")

      def /(a: Int): Fraction = if (a != 0) Fraction(this.num, this.den * a)
      else throw new ArithmeticException("division by zero")

      def +(other: Fraction): Fraction = Fraction(num * other.den + den * other.num, den * other.den)

      def -(other: Fraction): Fraction = this + other * (-1)

      def unary_- = Fraction(num, -den)

      def ++ = this + Fraction.One
    }

    object Fraction {

      import math._

      val Zero = Fraction(0, 0)

      val One = Fraction(1, 1)

      def sign(a: Int) = if (a > 0) 1 else if (a < 0) -1 else 0

      def gcd(a: Int, b: Int): Int = if (b == 0) abs(a) else gcd(b, a % b)

      def isZero(a: Fraction) = a == Zero

      def nonZero(a: Fraction) = !(isZero(a))
    }

    println(Fraction(15, -6)) // -5/2
    println("2/2=" + Fraction(2, 2))
    println("0/0=" + Fraction(0, 0))
    println("-1/-2=" + Fraction(-1, -2))
    println("-1/2=" + Fraction(-1, 2))
    println("1/-2=" + Fraction(1, -2))
    println("unary - 1/2= " + (-Fraction(1, 2)))

    assert(Fraction.gcd(2, 3) == 1)
    assert(Fraction.gcd(3, 3) == 3)
    assert(Fraction.gcd(6, 3) == 3)
    assert(Fraction.gcd(3, 6) == 3)

    // multiplication
    assert(Fraction(1, 2) * Fraction(1, 3) == Fraction(1, 6))
    assert(Fraction(1, 2) * Fraction(0, 0) == Fraction.Zero)
    assert(Fraction(1, 2) * Fraction.Zero == Fraction.Zero)
    assert(Fraction(1, 2) * Fraction.One == Fraction(1, 2))

    assert(Fraction(1, 2) * 2 == Fraction.One)
    assert(Fraction(1, 2) * 0 == Fraction.Zero)

    // division
    assert(Fraction(1, 2) / Fraction(1, 2) == Fraction.One) // 1/1
    assert(Fraction(1, 2) / 2 == Fraction(1, 4))

    assert(try {
      println(Fraction(1, 2) / Fraction(0, 0)) // div by zero
      false
    } catch {
      case _: ArithmeticException => {
        println("exception 'division by zero fraction' throw")
        true
      }
    })

    assert(try {
      println(Fraction(1, 2) / 0) // div by zero
      false
    } catch {
      case _: ArithmeticException => {
        println("exception 'division by zero' throw")
        true
      }
    })

    // '+'
    assert(Fraction(1, 4) + Fraction(1, 4) == Fraction(1, 2))
    assert(Fraction(1, 2) + Fraction(1, 2) == Fraction.One)
    assert(Fraction(2, 3) + Fraction(4, 5) == Fraction(22, 15))
    assert(Fraction(2, 3) + Fraction(1, 5) == Fraction(13, 15))
    assert(Fraction(1, 2) + -Fraction(1, 2) == Fraction.Zero)

    // '-'
    assert(Fraction(1, 4) - Fraction(1, 4) == Fraction.Zero)
    assert(Fraction(1, 2) - Fraction(1, 2) == Fraction.Zero)
    assert(Fraction(1, 2) - Fraction(2, 4) == Fraction.Zero)
    assert(Fraction(2, 3) - Fraction(4, 5) == -Fraction(2, 15))
    assert(Fraction(2, 3) - Fraction(1, 5) == Fraction(7, 15))
    assert(Fraction(1, 2) - -Fraction(1, 2) == Fraction.One)

    // ++ :-)
    println("1/2+1=" + (Fraction(1, 2) ++))
    assert(((Fraction(1, 2) ++) ++) == Fraction(5, 2))

  }

  object q4 {

    class Money private(private var amount: Long) {

      import Money._

      def this(d: Long, c: Long) {
        this(d * Money.c_IN_d + c)
      }

      val cent: Long = amount - dollar * c_IN_d

      def dollar: Long = amount / c_IN_d

      def +(other: Money) = new Money(amount + other.amount)

      def unary_- = new Money(-amount)

      def -(other: Money) = this + -other

      def <(other: Money) = amount < other.amount

      def >(other: Money) = amount > other.amount

      def *(a: Double) = new Money((amount * a).toInt)

      def /(a: Double) = new Money((amount / a).toInt)

      override def equals(obj: scala.Any): Boolean = obj.isInstanceOf[Money] &&
        (obj.asInstanceOf[Money].amount == amount)

      override def toString: String = f"$$${amount / Money.c_IN_d.toDouble}%2.2f"

    }

    object Money {
      val c_IN_d = 100
      val Zero = Money(0, 0)

      val OneCent = Money(0, 1)

      val OneDollar = Money(1, 0)

      def apply(d: Int, c: Int) = new Money(d, c)

    }

    // creation
    val m075 = Money(0, 75)
    assert(m075.dollar == 0)
    assert(m075.cent == 75)
    assert(Money(3, 75).dollar == 3)
    assert(Money(3, 75).cent == 75)
    assert(Money(75, 0).dollar == 75)
    assert(Money(75, 0).cent == 0)

    assert(Money(3, 275).dollar == 5)
    assert(Money(3, 275).cent == 75)

    //sum
    assert(Money.Zero + Money.OneCent == Money.OneCent)
    assert(Money.OneCent + Money.OneCent == Money(0, 2))
    assert(Money(1, 75) + Money(0, 50) == Money(2, 25))

    //neg
    assert(Money(1, 75) - Money(0, 50) == Money(1, 25))
    assert(Money(1, 75) - Money(2, 50) == -Money(0, 75))
    assert(Money(1, 75) - Money(3, 50) == -Money(1, 75))
    assert(Money(1, 75) - Money(1, 75) == Money.Zero)

    //compare
    assert(Money.Zero.equals(0) == false)
    assert(Money(1, 75) > Money(0, 50))
    assert(Money(1, 75) < Money(3, 50))
    assert(Money(1, 475) > Money(5, 50))
    assert(Money(2, 331) == Money(5, 31))

    println(Money(1, 75) / 2)
    println(Money.OneDollar * 1000000)
  }

  object q5 {

    case class Table(){

      import scala.collection.mutable.ArrayBuffer

      private val rows = ArrayBuffer.empty[ArrayBuffer[String]]

      def |(data: String): Table = {
        if (rows.isEmpty) tr()
        td(data)
        this
      }

      def ||(data: String) = {
        tr()
        td(data)
        this
      }

      def tr(): Table = {
        rows += ArrayBuffer.empty[String]
        this
      }

      def td(data: String): Table = {
        rows.last += data
        this
      }

      override def toString: String = {
        val ret = new StringBuilder
        ret.append("<table>")
        for (row <- rows) {
          ret.append("<tr>")
          for (cell <- row) {
            ret.append("<td>")
            ret.append(cell)
            ret.append("</td>")
          }
          ret.append("</tr>")
        }
        ret.append("</table>")
        ret.mkString
      }
    }

    //creation
    val table1 = new Table
    //   println(table1)

    // |
    table1 | "aaa" | "bbb"
    // println(table1)

    // ||
    table1.tr()
    table1 | "ccc" | "ddd"
    println(table1)

    //question
    val qt = Table() | "Java" | "Scala" || "Gosling" | "Odersky" || "JVM" | "JVM .Net"
    println(qt)

  }

  //q1
  //q2
  //q3
  //q4
  q5
}
