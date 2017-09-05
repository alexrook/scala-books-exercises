package horstmann

class PersonWithDefs(val name: String = "", var age: Int = 0)

object Section5 extends App {

  class Test(val s: String)

  object l5 {

    object l51 {

      class Counter {
        private[this] var value: Int = _ //only private field see javap -p

        def inc() = value += 1

        def ++() = inc()

        def cur = value
      }

      class Counter2 {
        private var value: Int = _ //only private field & private get/set methods

        def inc() = value += 1

        def cur = value
      }

      val c = new Counter
      println(c.cur)
      c.inc()
      println(c.cur)
      c.++
      println(c.cur)
    }

    object l52 {

      class Person {
        var age: Int = 0
      }

      val p = new Person

      p.age = 1 //scala setter: age_=(Int) (age_$eq for java)
      println(p.age) //scala getter: age (age for java)

      class Person2 {
        var _age: Int = 0

        def age = _age

        def age_=(v: Int): Unit = if (v > age) _age = v else throw new IllegalArgumentException("v<age") //own setter
      }

      val p2 = new Person2

      p2.age = 3
      println(p.age)
      try {
        p2.age = 1
        println(p.age)
      } catch {
        case _ => println("expected exception")
      }


    }

    object l55 {

      class Person {

        import scala.beans.BeanProperty

        @BeanProperty val name: String = "Agent Smith"
      }

      class Person2 {

        import scala.beans.BeanProperty

        @BeanProperty var name: String = _
      }

      val p1 = new Person
      println(p1.getName)

      val p2 = new Person2
      p2.setName("Mike")
      println(p2.getName)

      val p21 = new Person2
      p21.name = "Mari" // p21._=(String)
      println(p21.getName)
    }

    object l56 {

      //primary constructors
      class Person(name: String) { //private final name (same as private[this] val ...)
        def some = "some:" + name
      }

      val p1 = new Person("p1")
      println(p1.some)

      class Person2(val name: String)

      //private final name & public getter: name()

      val p2 = new Person2("p2")
      println(p2.name)

      //private name & public getter: name(), setter: name+=(String)
      class Person3(var name: String)

      val p3 = new Person3("p3")
      println(p3.name)
      p3.name = "other p3"
      println(p3.name)

      //auxiliary constructors
      class Person4 {
        var name: String = _
        var age: Int = _

        def this(name: String) = {
          this() //must be first line
          this.name = name // name_=(String)
        }

        def this(name: String, age: Int) = {
          this(name) //prev defined auxiliary or primary constructor call
          this.age = age
        }
      }

      val p4 = new Person4("p4")
      println(p4.age)
      println(p4.name)

      val p41 = new Person4("Anna", 23)
      println(p41.age)
      println(p41.name)


    }

    object l58 {

      class Network {

        import scala.collection.mutable.ArrayBuffer

        class Member(val name: String) {
          val contacts = new ArrayBuffer[Member]

          override def toString: String = this.getClass + ":" + name
        }

        val members = new ArrayBuffer[Member]

        def join(memberName: String): Member = {
          val member = new Member(memberName)
          members += member
          member
        }

      }

      val group1 = new Network
      val group2 = new Network

      val fred = group1.join("fred")
      println("fred:" + fred)
      val ann = group1.join("ann")
      println(fred.getClass.getName)
      val mari = group2.join("mari")
      println("mari:" + mari)
      println(mari.getClass.getName)
      println(mari.getClass == fred.getClass) //TODO: ? Why true here?

      fred.contacts += ann
      ann.contacts += fred

      //  mari.contacts+=fred //compile error type mismatch

      class AllInclusiveNetwork {outer =>

        import scala.collection.mutable.ArrayBuffer

        class Member(val name: String) {
          val contacts = new ArrayBuffer[AllInclusiveNetwork#Member] //type projection Section18
          override def toString: String =AllInclusiveNetwork.this+":"+ this.getClass + ":" + name +":"+ outer
        }

        val members = new ArrayBuffer[Member]

        def join(memberName: String): Member = {
          val member = new Member(memberName)
          members += member
          member
        }

      }

      val aGroup1 = new AllInclusiveNetwork
      val aGroup2 = new AllInclusiveNetwork

      val mike: aGroup1.Member = aGroup1.join("mike")
      println("mike:" + mike)
      val alex = aGroup1.join("alex")
      println(mike.getClass.getName)
      val natali: aGroup2.Member = aGroup2.join("natali")
      println("natali:" + natali)
      println(natali.getClass.getName)
      println(natali.getClass == mike.getClass)

      mike.contacts += alex
      alex.contacts += mike
      natali.contacts += mike //ok

    }

  }

  //l5.l51
  // l5.l52
  //l5.l55
  // l5.l56
  l5.l58

}
