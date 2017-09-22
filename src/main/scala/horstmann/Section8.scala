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

    import horstmann.s8.geom.v1._

    val point1 = Point(0, 3)

    val point1_rotate = point1.rotate(Point(0, 0), Math.toRadians(90))
    val point2_rotate = point1.rotate(Point(0, 0), Math.toRadians(180))
    val point3_rotate = point1.rotate(Point(0, 0), Math.toRadians(45))
    println("before:" + point1)
    println("rotate 90: " + point1_rotate)
    println("rotate 180: " + point2_rotate)
    println("rotate 45: " + point3_rotate)
  }


  object q4 {

    abstract class Item {

      def price: BigDecimal

      def description: String
    }

    case class SimpleItem(price: BigDecimal, description: String) extends Item

    final class Bundle(var description: String) {

      import scala.collection.mutable._

      private[this] val items: ListBuffer[Item] = new ListBuffer[Item]

      def add(item: Item): Unit = items += item

      def get(index: Int): Item = items(index)

      def contains(item: Item): Boolean = items.find(i => i.equals(item)).nonEmpty

      def remove(index: Int) = items.remove(index)

      def remove(item: Item) = items -= item

      def sum: BigDecimal = items.foldLeft(BigDecimal(0))((acc, item) => acc + item.price)

    }

    val bundle = new Bundle("hipster staff")

    bundle.add(SimpleItem(44.11, "notebook"))
    bundle.add(SimpleItem(99.11, "cell phone"))
    bundle.add(SimpleItem(99.99, "sandwich"))

    assert(bundle.contains(SimpleItem(44.11, "notebook")))
    println(bundle.sum)

  }

  object q5 {

    abstract class Point() {
      def x: Double

      def y: Double
    }

    case class LabeledPoint(x: Double, y: Double, label: String) extends Point

    println(LabeledPoint(0, 0, "zero"))
    println(LabeledPoint(1, 1, "1,1 vector"))

  }

  object q6 {

    import q5._

    abstract class Shape {
      def center: Point
    }

    case class Circle(radius: Double, center: Point) extends Shape

    case class Rectangle(a: Point, b: Point, c: Point, d: Point) extends Shape {
      override def center: Point = LabeledPoint((a.x + c.x) / 2, (a.y + c.y) / 2, "Rectangle center")
    }

    val rect=Rectangle(LabeledPoint(-3, 1, "A"),
      LabeledPoint(-3, 3, "B"),
      LabeledPoint(-1, 3, "C"),
      LabeledPoint(-1, 1, "D"))

    println(s"$rect, center:${rect.center}")
  }

  //  l8.l8x
  // l8.l85
  //l8.l87

  //  q1
  //  q2
  //q3
  q4
  q5
  q6
}


