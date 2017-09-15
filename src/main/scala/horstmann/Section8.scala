package horstmann


/**
  * Created by moroz on 17.05.17.
  */

object Section8 extends App {

  object l8 {

    object l8x {

      class Person(val name: String) {
        def nameLength = name.length

        val my_type = "Person"
      }

      class Customer(name: String, val id: Int) extends Person(name) {
        override def nameLength: Int = super.nameLength + id

        override val my_type: String = "Customer"

      }

      object Customer {
        def apply(name: String, id: Int): Customer = new Customer(name, id)
      }

      class Agent(codename: String) extends Person(codename) {
        override val name = "secret"
        private[this] val cipher_key = 13
        private[this] val _codename = codename

        def getCodeName(cipher: Int) = if (cipher == cipher_key) _codename.toInt + cipher
        else throw new IllegalAccessException("big brother log you")
      }

      class Creature {
        val range: Int = 10
        val env: Array[Int] = new Array[Int](range)
      }

      class Ant extends Creature {
        override val range: Int = 2
      }

      /*
      *Unfortunately, we now have a problem. The range value is used in the superclass
      constructor, and the superclass constructor runs before the subclass constructor.
      Specifically, here is what happens:
      * 1. The Ant constructor calls the Creature constructor before doing its own construction.
      2. The Creature constructor sets its range field to 10.
      3. The Creature constructor, in order to initialize the env array, calls the range() getter.
      4. That method is overridden to yield the (as yet uninitialized) range field of the Ant class.
      5. The range method returns 0. (That is the initial value of all integer fields when an object is allocated.)
      6. env is set to an array of length 0.
      7. The Ant constructor continues, setting its range field to 2.
      */
      //Solution
      class AntFixed extends {
        override val range: Int = 2
      } with Creature

      var list: List[Person] = Nil

      for (i <- 0 to 3) {
        list = Customer("c" + i, i) :: list
      }

      for (customer <- list) {
        println("Customer?:" + customer.isInstanceOf[Customer] + "," + customer.my_type + "," + customer.name)
        val person = customer.asInstanceOf[Person]
        println("asInstanceOf[Person] Customer?" + person.isInstanceOf[Customer]
          + "," + person.my_type + ", classOf[Person]" + person.getClass() == classOf[Person])
      }

      import scala.collection.mutable

      val s: mutable.Set[Int] = mutable.Set(1, 2, 4)
      s.add(34)

      println(s.mkString(","))

      println(classOf[List[Int]])

      val agent: Agent = new Agent("007")

      println(agent.name)
      println(agent.getCodeName(13))

      val ant: Ant = new Ant()
      println("ant not fixed:" + ant.env.length) //0 here
      val antFixed: AntFixed = new AntFixed()
      println("ant not fixed:" + antFixed.env.length) //2 here


    }

    object l85 {

      import horstmann.JSection8._

      class MySub(x: Int, y: Int) extends L82JBase(x: Int, y: Int) {
        def this(x: Int) {
          this(x, 0)
        }
      }

      val ms1 = new MySub(1)
      println(ms1.getX + ":" + ms1.getY)
      val ms2 = new MySub(1, 2)
      println(ms2.getX + ":" + ms2.getY)

    }

    object l87 {

      import horstmann.JSection8._

      val l = new L82JBase(1, 2) {
        def toStr: String = "local anonymous"

        def calc = getX + getY
      }

      def printStructL82(v: L82JBase {
        def toStr: String
        def calc: Int
      }): Unit = {
        println(v.getClass.getName + ":" + v.toStr + ":" + v.calc)
      }

      printStructL82(l)


    }

  }

  object q1 {

    class BankAccount(initialBalance: Double) {
      protected var _balance = initialBalance

      def deposit(amount: Double) = {
        _balance += amount;
        _balance
      }

      def balance = _balance

      def withdraw(amount: Double) = if (amount <= _balance) {
        _balance -= amount;
        _balance
      } else throw new IllegalArgumentException("insufficient funds")

    }

    class CheckingAccount(initialBalance: Double) extends BankAccount(initialBalance) {
      val bankInterest = 1

      override def withdraw(amount: Double): Double = super.withdraw(amount + bankInterest)
    }

    val a1 = new CheckingAccount(23)
    a1.withdraw(12)
    assert(a1.balance == 10)

  }

  object q2 {

    import q1._

    class SavingAccount(initialBalance: Double) extends CheckingAccount(initialBalance) {

      var freeWithDrawCount = 3

      def earnMonthlyInterest(percent: Double): Unit = {
        freeWithDrawCount = 3
        val sum = (balance / 100) * percent
        deposit(sum)
      }

      override def withdraw(amount: Double): Double = if (freeWithDrawCount > 0) {
        _balance -= amount;

        freeWithDrawCount -= 1

        _balance
      } else super.withdraw(amount)

    }

    val a1 = new SavingAccount(23)
    a1.withdraw(1)
    a1.withdraw(1)
    a1.withdraw(1)
    assert(a1.balance == 20)
    a1.withdraw(1)
    assert(a1.balance == 18)
    a1.earnMonthlyInterest(1)
    a1.withdraw(1)
    assert(a1.balance == 17.18)

  }

  object q3 {

    case class Point(val x: Double, val y: Double)

    trait Figure {

      def area: Double

      def perimeter: Double

      def center: Point

      def rotate(corner: Double): Figure

      def scale(factor: Double): Figure

      def move(newCenter: Point): Figure

      def transform(trans: Figure => Figure): Figure = trans(this)

    }

    type Vertex = Point

    case class LineSegment(val a: Vertex, val b: Vertex) {
      import LineSegment._
      val length = vectorLength(a,b)
    }

    object LineSegment {
      def vectorLength(a: Vertex, b: Vertex):Double =
        Math.sqrt(Math.pow(b.x - a.x, 2) + Math.pow(b.y - a.y, 2))
    }

    trait Polygon extends Figure {

      def corners: Set[Vertex]

      def segments: Set[LineSegment]

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

      override def rotate(corner: Double): Figure = this

      override def scale(factor: Double): Figure = new Circle(center, radius * factor)

      override def move(newCenter: Point): Figure = new Circle(newCenter, radius)
    }

    trait Quadrilateral extends Polygon {
      def a: Vertex

      def b: Vertex

      val c: Vertex

      def d: Vertex

      override def corners: Set[Vertex] = Set(a, b, c, d)

      override def segments: Set[LineSegment] = Set(LineSegment(a, b),
        LineSegment(a, d), LineSegment(b, c), LineSegment(d, c))

    }

    class Square(val a: Vertex, val d: Vertex /*diagonal corners*/)
      extends Quadrilateral {

      val side = LineSegment(a, b).length
      val b: Vertex = ???
      val c: Vertex = ???

      override def area: Double = side * side

      override def perimeter: Double = 4 * side

      override def center: Point = ???

      override def rotate(corner: Double): Figure = ???

      override def scale(factor: Double): Figure = ???

      override def move(newCenter: Point): Figure = ???
    }


  }

  //  l8.l8x
  // l8.l85
  //l8.l87

  q1
  q2
}


