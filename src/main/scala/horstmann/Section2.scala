package horstmann

object Section2 extends App {

  object q1 {

    def signum(x: Double): Int = if (x == 0) 0
    else if (x > 0) 1 else -1
  }

  object q2 {
    def a: Unit = {}
  }

  object q3 {
    var y: Int = 1
    var x: Unit = {}
    x = y = 1
  }

  object q4 {
    for (i <- 10.to(0,-1)) println(i)
  }

  println(q1.signum(-12.3))
  println(q1.signum(0))
  println(q1.signum(45))
  println(q2.a.getClass.getName)
  q3
  q4

}
