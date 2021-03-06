package horstmann

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
import scala.language.postfixOps

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

    case class Table() {

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

  object q6 {

    case class ASCIIArt(private val _art: String, var lineSeparator: String) {

      private val art: Seq[String] = _art.split(lineSeparator)

      private val maxLength: Int = art.maxBy(str => str.length).length

      def &(other: ASCIIArt): ASCIIArt = {

        val _art: String = art.map(s => s.padTo(maxLength, ' '))
          .zipAll(other.art, "".padTo(maxLength, ' '), "")
          .map(t => t._1 + t._2 + lineSeparator)
          .mkString

        ASCIIArt(_art, lineSeparator)
      }

      def ^(other: ASCIIArt): ASCIIArt = {
        val _art: String = (art.map(s => s + lineSeparator) ++ other.art.map(s => s + lineSeparator)).mkString
        ASCIIArt(_art, lineSeparator)
      }

      override def toString: String = art.map(s => s + lineSeparator).mkString
    }

    val art1 = ASCIIArt(
      """|  ^_^
         |( ' ' )
         |(  -  )
         |  |||
         |(__|__)""".stripMargin, "\n")

    val art2 = ASCIIArt(
      """|  _______
         | / Hello \
         |<  Scala  |
         | \ Coder /
         |  ```````""".stripMargin, "\n")

    val art3 = ASCIIArt(
      """|33333
         | 333
         |  3""".stripMargin, "\n")

    val art4 = ASCIIArt(
      """|444444
         |4    4
         |4    4
         |444444""".stripMargin, "\n")

    print(art1)
    print(art2)
    print(art1 & art2)
    print(art3 & art4)
    print(art4 & art3)
    print(art1 ^ art2)

  }

  object q7 {

    class BitSequence(private val _data: Long) {

      private var data: Long = _data

      def apply(index: Int): Long = {
        checkIndex(index)
        data >> index & 1L
      }

      def update(index: Int, bit: Long) = {
        checkIndex(index)
        if (bit > 0) {
          data = data | (1L << index)
        } else {
          data = data & (-1L ^ (1L << index))
        }
      }

      private def checkIndex(index: Int): Unit = {
        if ((index < 0) || (index > 63))
          throw new IllegalArgumentException("index must be >0 and <64")
      }

    }

    //get
    val sec = new BitSequence(87L) //1010111
    val checkA = Array[Long](1, 0, 1, 0, 1, 1, 1).reverse
    for (i <- 0 until checkA.length) {
      assert(sec(i) == checkA(i))
    }

    //update
    val sec1 = new BitSequence(-1L) //11111...11111  for signed long

    for (i <- 0 to 63 if i % 2 == 0) sec1(i) = 1 //must nothing change
    for (i <- 0 until 64) {
      assert(sec1(i) == 1)
    }

    for (i <- 0 to 63 if i % 2 == 0) sec1(i) = 0
    for (i <- 0 until 64) {
      if (i % 2 == 0) assert(sec1(i) == 0) else assert(sec1(i) == 1)
    }
  }

  object q8 {

    case class Matrix[T: Numeric](cols: Int, rows: Int) {

      import scala.collection.mutable.ArrayBuffer

      private val data: ArrayBuffer[ArrayBuffer[T]] = new ArrayBuffer(rows)

      for (_ <- 0 until rows) {
        val row = new ArrayBuffer[T](cols)
        for (_ <- 0 until cols) row += zero
        data += row
      }

      def forEach(f: (Int, Int) => Unit): Matrix[T] = {
        for (col <- 0 until cols; row <- 0 until rows) f(col, row)
        this
      }

      def forEach(f: (this.type, Int, Int) => Unit): Matrix[T] = forEach((col, row) => f(this, col, row))

      def forall(f: (Int, Int) => Boolean): Boolean = {
        forEach((col, row) => if (!f(col, row)) {
          return false
        })
        true
      }

      def forall(f: (this.type, Int, Int) => Boolean): Boolean =
        forall((col, row) => if (f(this, col, row)) true else false)

      def foldRowByValue[A](row: Int, a: A, f: (A, T) => A): A = {
        def loop(ret: A, col: Int): A = if (col < cols) loop(f(ret, this (col, row)), col + 1) else ret

        loop(a, 0)
      }

      def foldRowByIndex[A](row: Int, a: A, f: (A, this.type, Int, Int) => A): A = {
        def loop(ret: A, col: Int): A = if (col < cols) loop(f(ret, this, col, row), col + 1) else ret

        loop(a, 0)
      }

      def foldCol[A](col: Int, a: A, f: (A, T) => A): A = {
        def loop(ret: A, row: Int): A = if (row < rows) loop(f(ret, this (col, row)), row + 1) else ret

        loop(a, 0)
      }

      def zero(implicit ev: Numeric[T]) = ev.zero

      def one(implicit ev: Numeric[T]) = ev.one

      def negate()(implicit ev: Numeric[T]) = {
        new Matrix[T](cols, rows)(ev).forEach((m, col, row) => m(col, row) = ev.negate(this (col, row)))
      }

      def *(k: T)(implicit ev: Numeric[T]) = {
        forEach((col, row) => this (col, row) = ev.times(this (col, row), k))
        this
      }

      def +(k: T)(implicit ev: Numeric[T]) = {
        forEach((col, row) => this (col, row) = ev.plus(this (col, row), k))
        this
      }

      def -(k: T)(implicit ev: Numeric[T]) = this.+(ev.negate(k))(ev)

      def *(other: Matrix[T])(implicit ev: Numeric[T]) = if (cols == other.rows) {

        new Matrix[T](other.cols, this.rows)(ev).forEach((m, mc, mr) => {
          m(mc, mr) = foldRowByIndex[T](mr, ev.zero, (acc: T, these, col, row) => {
            ev.plus(acc, ev.times(these(col, row), other(mc, col)))
          })
        })

      } else throw new ArithmeticException("matrices has different size")

      def +(other: Matrix[T])(implicit ev: Numeric[T]) = {
        if ((other.rows != rows) || (other.cols != cols))
          throw new ArithmeticException("matrices has different size")
        new Matrix[T](cols, rows)(ev).
          forEach((m, col, row) => m(col, row) = ev.plus(this (col, row), other(col, row)))
      }

      def -(other: Matrix[T])(implicit ev: Numeric[T]) = this.+(other.negate()(ev))(ev)

      def apply(col: Int, row: Int): T = data(row)(col)

      def update(col: Int, row: Int, a: T) = data(row)(col) = a

      override def equals(obj: scala.Any): Boolean = if (obj.isInstanceOf[Matrix[T]]) {
        val other = obj.asInstanceOf[Matrix[T]]
        other.cols == cols && other.rows == rows &&
          forall((col, row) => this (col, row) == other(col, row))
      } else false

    }

    object Matrix {

      def apply[T: Numeric](str: String)(implicit f: (String) => T): Matrix[T] = {
        val lines = str.split("\n")
        val rows = lines.size
        val cols = lines(0).split("\\s+").size
        new Matrix[T](cols, rows).
          forEach((m, col, row) => {
            m(col, row) = f(lines(row).split("\\s+")(col))
          })
      }

      def ZERO[T: Numeric](rows: Int, cols: Int) = new Matrix[T](rows, cols)

      def ONE[T: Numeric](rows: Int, cols: Int): Matrix[T] = ZERO(rows, cols).
        forEach((ret, row, col) => if (row == col) ret(row, col) = ret.one)

      def printMatrix[T: Numeric](m: Matrix[T]) = {
        val ret = new StringBuilder
        for (row <- 0 until m.rows) {
          m.foldRowByValue[StringBuilder](row, ret, (r, v) => r.append(v).append(' '))
          ret.append("\n")
        }
        print(ret)
      }
    }

    //  square
    assert(Matrix.ZERO[Int](4, 4).forall((m, i, j) => m(i, j) == 0) == true)
    assert(Matrix.ZERO[Int](4, 4) == Matrix.ZERO[Int](4, 4))
    //Matrix.printMatrix(Matrix.ONE[Int](2, 2))

    val m1 = new Matrix[Int](3, 3)
    m1.forEach((col, row) => m1(col, row) = col + row)
    //Matrix.printMatrix(m1)

    // + -
    val m2 = new Matrix[Int](2, 2).forEach((m, col, row) => m(col, row) = col + row)
    //Matrix.printMatrix(m2)
    assert((m2 + Matrix.ZERO[Int](2, 2) == m2) && (Matrix.ZERO[Int](2, 2) + m2 == m2))
    //Matrix.printMatrix(m2 + m2)
    assert(m2 - m2 == Matrix.ZERO[Int](2, 2))
    val m3 = new Matrix[Int](2, 2).forEach((m, col, row) => m(col, row) = (col + row) * 2)
    //    Matrix.printMatrix(m3)
    assert(m2 + m2 == m3)
    assert(m2 - m2 == Matrix.ZERO[Int](2, 2))

    //

    val m4 = new Matrix[Double](3, 3).forEach((m, col, row) => m(col, row) = 1 + col)
    // Matrix.printMatrix(m4)

    assert(m4.foldRowByValue[Double](0, 0d, (ret, v) => v + ret) == 6)
    assert(m4.foldCol[Double](2, 0d, (ret, v) => v + ret) == 9)

    val m5 = Matrix.ZERO[Int](2, 3).forEach((m, col, row) => m(col, row) = 1 + col + row)
    println("m5")
    Matrix.printMatrix(m5)
    val m6 = Matrix.ZERO[Int](3, 2)
    println("m5*m6")
    Matrix.printMatrix(m5 * m6)

    //checked by http://ru.onlinemschool.com/math/library/matrix/multiply/
    implicit def strToInt(s: String): Int = java.lang.Integer.parseInt(s)

    val m7 = Matrix[Int](
      """|2  1
         |-3  0
         |4 -1""".stripMargin)
    println("m7")
    Matrix.printMatrix(m7)

    val m8 = Matrix[Int](
      """|5 	-1 	 6
         |-3 	 0 	 7""".stripMargin)
    println("m8")
    Matrix.printMatrix(m8)

    val m7_x_m8 = Matrix[Int](
      """|7  -2  19
         |-15  3  -18
         |23 -4  17""".stripMargin)
    println("m7*m8")
    Matrix.printMatrix(m7 * m8)

    assert(m7_x_m8 == m7 * m8)
  }

  object q9_10 {

    import java.io.File

    class RichFile(private val self: File) {

      import RichFile._

      val ext: String = {
        val doti = self.getName.lastIndexOf('.')
        if (doti > -1) {
          self.getName.substring(doti)
        } else ""
      }

      val name: String = if (ext.length > 0) {
        self.getName.dropRight(ext.length)
      } else self.getName

      val parent: String = if (self.getParent != null) {
        self.getParent
      } else ""

      val parentSeq: Seq[String] = {
        def loop(acc: Seq[String], file: File): Seq[String] = if (file != null) loop(file.getName +: acc, file.getParentFile) else acc

        loop(Nil, self.getParentFile).filter(s => s.length > 0)
      }

      def ~(other: RichFile) = if (ext.length > 0) {
        if (other.self.getName.endsWith(ext)) true else false
      } else {
        if (other.name.endsWith(name)) true else false
      }

      override def toString: String = s"RichFile[$self]"
    }

    object RichFile {

      def apply(self: File): RichFile = new RichFile(self)

      def apply(parent: String, name: String, ext: String): RichFile = new RichFile(new File(s"$parent/$name$ext"))

      //      def unapply(file: RichFile): Option[(String, String, String)] = {
      //        println("RichFile.unapply")
      //        if ((file.parent.length > 0) && (file.name.length > 0) && (file.ext.length > 0))
      //          Some((file.parent, file.name, file.ext))
      //        else None
      //      }

      def unapplySeq(arg: RichFile): Option[Seq[String]] = {
        import scala.collection.mutable.ArrayBuffer
        val ret = new ArrayBuffer[String](3)
        ret ++= arg.parentSeq ++= Array(arg.name, arg.ext).filter(s => s.length > 0)

        Option(ret.toSeq)
      }

    }

    implicit def fileToRichFile(from: File): RichFile = RichFile(from)

    def printInfo(file: File): Unit = {
      println("\n" + file)
      println("getPath=" + file.getPath)
      println("getAbsolutePath=" + file.getAbsolutePath)
      println("getParent=" + file.getParent)
      println("getParentFile=" + file.getParentFile)
      println("RichFile.name=" + file.name)
      println("RichFile.ext=" + file.ext)
      println("RichFile.parentSeq=" + file.parentSeq)
      //unapply
      RichFile(file) match {
        case RichFile(path1, path2, name, ext) => println(s"RichFile.unapply:4=[$path1,$path2,$name,$ext]")
        case RichFile(path, name, ext) => println(s"RichFile.unapply:3=[$path,$name,$ext]")
        case RichFile(path, name) => println(s"RichFile.unapply:2=[$path,$name]")
        case RichFile(path) => println(s"RichFile.unapply:1=[$path]")
        case _ => println(27.toChar + "[31m"+ "unapply do not work"+27.toChar + "[0m")
      }

    }

    //creation
    var f0 = new File("home")
    printInfo(f0)
    var f01 = new File("/home")
    printInfo(f01)
    var f02 = new File("home.txt")
    printInfo(f02)
    var f1 = new File("/home/some.txt")
    printInfo(f1)
    var f2 = new File("home/cay")
    printInfo(f2)
    var f3 = new File("/home/cay/bar.txt")
    printInfo(f3)
    println(f1 ~ f2)
    println(f1 ~ f3)

  }

  //  q1
  //  q2
  //  q3
  //  q4
  //  q5
  //  q6
  //  q7
  //  q8

  q9_10

}
