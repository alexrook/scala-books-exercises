package horstmann


object MyColors extends Enumeration {

  val Red, Blue, Green = Value

}

object Step {
  def step01(): Unit = {

    import horstmann.section6._

    println("hello section 6 !")

    val a: Account = new Account

    a.deposit(12.7)

    println("a.id=" + a.id + ", deposit=" + a.deposit)

    val b = Account()

    val c = b + 17

    println("b.id=" + b.id + ", deposit=" + b.deposit + ", java to string:" + b.toString)
    println("c.id=" + c.id + ", deposit=" + c.deposit + ", java to string:" + c.toString)
    assert(c == b)
    //---------Account2

    val a2 = Account2(136.34)


    println("a2.id=" + a2.id + ", deposit=" + a2.deposit)
    a2 + 67
    println("a2.id=" + a2.id + ", deposit=" + a2.deposit)

    //val b2=new Account2() -- error no public constructor

    val i, k, j = 1
    println("i=" + i + ", k=" + k + ", j=" + j)

  }

  def step02(): Unit = {
    for (color <- MyColors.values) {
      println(color + ":" + color.id)
    }

    val color = MyColors(0)
    println(color + ":" + color.id) //Red (id autonum)
  }
}

object Exercises {

  object st1_2 {

    object Conversion {

      //Британская и американская (статутная) миля
      def milesToKilometers(miles: Int): Double = 1609.34 * miles

      //Английский дюйм или имперский дюйм (англ. inch от лат. uncia — 1⁄12 часть)
      // с 1958 года приравнивается точно к 2,54 см
      def inchesToCentimeters(inches: Int): Double = inches * 2.54

      //английский галлон = 4,5461 литра.
      def gallonsToLiters(gallons: Int) = 4.5461 * gallons
    }

    abstract class UnitConversion {
      val conversionConst: Double

      def convert(units: Int): Double = {
        conversionConst * units
      }
    }

    class MilesToKilometers extends UnitConversion {
      final val conversionConst = 1609.34
    }

    class GallonsToLiters extends UnitConversion {
      final val conversionConst = 4.5461
    }

    class InchesToCentimeters extends UnitConversion {
      final val conversionConst = 2.54
    }

  }

  object st3_4 {


    class Origin extends java.awt.Point {

    }

    class Point(val x: Int, val y: Int)

    object Point {
      def apply(x: Int, y: Int): Point = new Point(x, y)
    }


  }

  object st6_7 {

    object Must extends Enumeration {
      val Trefa = Value("\u2663")
      val Bubna = Value("\u2662")
      val Cherva = Value("\u2661")
      val Pike = Value("\u2660")

      class Card(val name: String, val must: Must.Value)

      def isRed(card: Card) = {
        card.must == Cherva || card.must == Bubna
      }
    }

  }

  object q8 {
    //TODO RGB Cube Enum
  }


}

object q5 extends App {
  println(args.reverse.mkString(" "))


}


object Main extends App {

  // import Step._
  //step01()
  //step02()

  import Exercises.st1_2._

  assert(Conversion.milesToKilometers(3) == (new MilesToKilometers).convert(3))
  assert(Conversion.gallonsToLiters(3) == (new GallonsToLiters).convert(3))
  assert(Conversion.inchesToCentimeters(3) == (new InchesToCentimeters).convert(3))

  import Exercises.st3_4._

  val point = Point(1, 2)
  println(point.x)
  println(point.y)
  assert(point.x == 1)
  assert(point.y == 2)

  import Exercises.st6_7._

  for (must <- Must.values) print(must.toString + " ")
  val card = new Must.Card("king", Must.Cherva)
  assert(Must.isRed(card))

}




