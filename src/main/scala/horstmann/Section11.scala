package horstmann

//https://stackoverflow.com/questions/7656937/valid-identifier-characters-in-scala
/*
 An operator identifier consists of one or more operator characters.
 Operator characters are printable ASCII characters such as +, :, ?, ~ or #.

 More precisely, an operator character belongs to the Unicode set of mathematical symbols(Sm) or other symbols(So),
 or to the 7-bit ASCII characters that are not letters, digits, parentheses, square brackets, curly braces, single or double quote,
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

      // some1 += some2 //compiler try: some1= some1 + some2 and fire error "reassignment to val"
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

        def +(other: Bar) = new Bar(s"plus ${other.data}")

        def *(other: Bar) = new Bar(s"asterisk ${other.data}")

        override def toString: String = s"Bar[$data]"
      }

      val bar1 = new Bar("bar1")
      val bar2 = new Bar("bar2")
      val bar3 = new Bar("bar3")

      val bar4 = bar1 + bar2 * bar3

      println(s"$bar1, $bar2, $bar3")

      println(bar4) //Bar[plus asterisk bar3]

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
      }

      val rect1 = Rectangle(1, 3)

      val Rectangle(a, b) = rect1
      println(s"a=$a, b=$b")

      val rect2 = Rectangle(2, 2)

      println(rect2 > rect1)
      println(null.isInstanceOf[Rectangle])

    }

    //    l111
    //    l112
    //    l113
    //    l114
    l115
  }

  l11

}
