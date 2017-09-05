package horstmann

object Section5 extends App {

  object l5 {

    object l51 {

      class Counter {
        private[this] var value: Int = _

        def inc() = value += 1

        def ++() = inc()

        def cur = value
      }

      val c = new Counter
      println(c.cur)
      c.inc()
      println(c.cur)
      c.++
      println(c.cur)
    }

  }

  l5.l51

}
