package horstmann


object Section6 extends App {

  object l6 {

    object l61 {

      class Account {
        val id = Account.genId()
        private[this] var _deposit = 0d

        def deposit(sum: Double) = _deposit += sum

        def deposit = _deposit

        def +(sum: Double): Account = {
          deposit(sum);
          this
        }

      }

      object Account {

        private var id = 0;

        def genId(): Int = {
          id += 1;
          id
        }

        def apply(): Account = new Account
      }


      println("hello section 6 !")
      val a: Account = new Account
      a.deposit(12.7)
      println("a.id=" + a.id + ", deposit=" + a.deposit)
      val b = Account()
      val c = b + 17
      println("b.id=" + b.id + ", deposit=" + b.deposit + ", java to string:" + b.toString)
      println("c.id=" + c.id + ", deposit=" + c.deposit + ", java to string:" + c.toString)
      assert(c == b)

    }


    object l62 {

      class Account2 private(_id: Int, initialBalance: Double) {
        val id = _id
        private[this] var _deposit = initialBalance

        def deposit(sum: Double) = _deposit += sum

        def deposit = _deposit

        def +(sum: Double): Account2 = {
          deposit(sum);
          this
        }

      }

      object Account2 {
        private var id = 0;

        def genId(): Int = {
          id += 1;
          id
        }

        def apply(initialBalance: Double): Account2 = new Account2(genId(), initialBalance)
      }

      val a2 = Account2(136.34)

      println("a2.id=" + a2.id + ", deposit=" + a2.deposit)
      a2 + 67
      println("a2.id=" + a2.id + ", deposit=" + a2.deposit)

      val i, k, j = 1
      println("i=" + i + ", k=" + k + ", j=" + j)

    }

    object l63 {

      object MyColors extends Enumeration {
        val Red, Blue, Green = Value
      }

      for (color <- MyColors.values) {
        println(color + ":" + color.id)
      }

      val color = MyColors(0)
      println(color + ":" + color.id) //Red (id autonum)
    }

  }


  object q1_2 {

    //q1
    object Conversion {

      //Британская и американская (статутная) миля
      def milesToKilometers(miles: Int): Double = 1609.34 * miles

      //Английский дюйм или имперский дюйм (англ. inch от лат. uncia — 1⁄12 часть)
      // с 1958 года приравнивается точно к 2,54 см
      def inchesToCentimeters(inches: Int): Double = inches * 2.54

      //английский галлон = 4,5461 литра.
      def gallonsToLiters(gallons: Int) = 4.5461 * gallons
    }

    //q2

    abstract class UnitConversion {
      val conversionConst: Double

      def convert(units: Int): Double = {
        conversionConst * units
      }
    }

    object MilesToKilometers extends UnitConversion {
      final val conversionConst = 1609.34
    }

    object GallonsToLiters extends UnitConversion {
      final val conversionConst = 4.5461
    }

    object InchesToCentimeters extends UnitConversion {
      final val conversionConst = 2.54
    }


    assert(Conversion.milesToKilometers(3) == MilesToKilometers.convert(3))
    assert(Conversion.gallonsToLiters(3) == GallonsToLiters.convert(3))
    assert(Conversion.inchesToCentimeters(3) == InchesToCentimeters.convert(3))

  }

  object q3_4 {


    class Origin extends java.awt.Point {

    }

    class Point(val x: Int, val y: Int)

    object Point {
      def apply(x: Int, y: Int): Point = new Point(x, y)
    }

    val point = Point(1, 2)
    println(point.x)
    println(point.y)
    assert(point.x == 1)
    assert(point.y == 2)

  }

  object q5 {

    def printReverse(args: Array[String]): Unit = {
      println()
      for (arg <- args.reverse) {
        print(arg + " ")
      }
    }
  }

  object q6_7 {

    object Suits extends Enumeration {
      val Clubs = Value("\u2663")
      val Tiles = Value("\u2662")
      val Hearts = Value("\u2661")
      val Pikes = Value("\u2660")

      class Card(val name: String, val suit: Suits.Value)

      def isRed(card: Card): Boolean = {
        card.suit == Tiles || card.suit == Hearts
      }

      def isBlack(card: Card) = !isRed(card)
    }

    for (suit <- Suits.values) print(suit.toString + " ")

    val card = new Suits.Card("King", Suits.Hearts)
    assert(Suits.isRed(card))

  }

  object q8 {

    object RGBCube extends Enumeration {

      val Black = Value(0x000000)
      val Blue = Value(0x0000ff)
      val Green = Value(0x00ff00)
      val Cyan = Value(0x00ffff)
      val Red = Value(0xff0000)
      val Magenta = Value(0xff00ff)
      val Yellow = Value(0xffff00)
      val White = Value(0xffffff)

    }

    for (color <- RGBCube.values) {
      println(color.id + ":" + color.toString)
    }

  }


  //  l6.l61
  //  l6.l62
  //  l6.l63

  q1_2
  q3_4
  q5.printReverse(args)
  q6_7
  q8
}