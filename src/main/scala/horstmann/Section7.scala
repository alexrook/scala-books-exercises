package horstmann

import java.io.{InputStreamReader, LineNumberReader}

package level_1 {

  class Level_1_A {
    override def toString: String = "Level_1_A"
  }

  class Level_1_B {

    private[level_1] val pp = "private package value"

    override def toString: String = "Level_1_B"
  }

  package level_2 {

    class Level_2_A {
      override def toString: String = "Level_2_A"
    }
    package level_3 {

      object tst {
        val level_1_A = new Level_1_A
        val level_2_A = new Level_2_A
      }

    }

  }

}


package p_l7.p_l74.test {

  class A_74 {
    override def toString: String = "A_74"
  }

}

package p_75 {

}

package object p_75 { //object package -> horstmann.p_75
  val p75_some_value = "p_75 some value"
}

package random {}

package object random {

  private[random] var seed: Double = 0
  val a = 1664525
  val b = 1013904223
  val m = Math.pow(2, 32).toLong

  def nextInt(): Int = nextDouble().toInt

  def nextDouble(): Double = {
    seed = (seed * a + b) % m
    seed
  }

  def setSeed(seed: Int): Unit = {
    this.seed = seed
  }

}


object Section7 extends App {

  object l7 {

    object l71 {

      import level_1.level_2.Level_2_A

      val a = new Level_2_A
      println(a)
    }

    object l72 {

      import section7.JSection7 //relative import

      val jSection7 = new JSection7
      println(jSection7)
    }

    object l73 {

      import _root_.horstmann.level_1.level_2.Level_2_A //absolute paths start with _root_
      val level_2_A = new Level_2_A
      println(level_2_A)

    }

    object l74 {

      import p_l7.p_l74.test.A_74

      val a_74 = new A_74
      println(a_74)
    }

    object l75 {
      println(p_75.p75_some_value)
    }

    object l76 {

      import horstmann.level_1.Level_1_B

      val a = new Level_1_B
      // println(a.pp) //compile error
    }

  }

  object q3 {

    import random._

    def testRets()= {
      val count = 5
      println()
      var ints: List[Int] = List.empty
      for (_ <- 0 to count) {
        ints = nextInt() :: ints
      }
      println(ints.sorted)
      var dbls: List[Double] = List.empty
      for (_ <- 0 to count) {
        dbls = nextDouble() :: dbls
      }
      println(dbls.sorted)
    }

    testRets()
    setSeed(1)
    testRets()
  }

  object q6_7 {

    import java.util.{HashSet => JHashSet}
    import scala.collection.mutable.HashSet

    val jSet = new JHashSet[Int]();
    for (i <- 0 to 12 by 2) {
      jSet.add(i)
    }

    println("java set:" + jSet)

    val set = HashSet.empty[Int]
    val iter = jSet.iterator()

    while (iter.hasNext) set += iter.next()
    assert(jSet.size() == set.size)
    println("scala set:" + set)


  }

  object q9 {

    def isSecretEnough(password: String): Boolean =
      (password.nonEmpty) && (password.length > 8) && (password.matches("[\\w+\\d+]+"))


    def run(): Unit = {
      import java.lang.System._

      val user = getProperty("user.name")

      val reader = new LineNumberReader(new InputStreamReader(in))
      while (true) {
        println(s"Enter password for $user>")
        val pass = reader.readLine()
        if (pass.equalsIgnoreCase("quit")) return
        if (isSecretEnough(pass)) {
          println(s"Welcome $user")
        } else println("bad password")
      }


    }

    //    println(isSecretEnough(""))
    //    println(isSecretEnough("123"))
    //    println(isSecretEnough("123456 789"))
    //    println(isSecretEnough("123456789"))
    //    println(isSecretEnough("1234567_sdcsd89"))

    run()

  }
  

  //  l7.l71
  //  l7.l72
  //  l7.l73
  //  l7.l74
  //  l7.l75

  q3
  q6_7
  q9
}
