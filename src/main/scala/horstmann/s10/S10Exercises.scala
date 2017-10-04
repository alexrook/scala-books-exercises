package horstmann.s10

object S10Exercises extends App {

  object v1 {

    trait One {
      def some(msg: String) = println(msg)
    }

    trait Two {
      this: One =>
      def done(msg: String) = some(msg)
    }

    val a = new One with Two
    a.done("A")

    class Three {
      def alone(msg: String) = println(msg)
    }

    val b = new Three with One with Two
  }

  object v2 {

    trait One {
      def some(msg: String) = println(msg)
    }

    trait Two extends One {
      def done(msg: String) = some(msg)
    }

    val a = new One with Two
    a.done("B")

    class Three extends java.awt.Point {
      def alone(msg: String) = println(msg)
    }

    val b = new Three with One with Two
  }

  v1
  v2
}
