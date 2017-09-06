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
        case _: IllegalArgumentException => println("expected exception")
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

      class AllInclusiveNetwork {
        outer =>

        import scala.collection.mutable.ArrayBuffer

        class Member(val name: String) {
          val contacts = new ArrayBuffer[AllInclusiveNetwork#Member] //type projection Section18
          override def toString: String = AllInclusiveNetwork.this + ":" + this.getClass + ":" + name + ":" + outer
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
      val lucia: aGroup2.Member = aGroup2.join("natali")
      println("lucia:" + lucia)
      println(lucia.getClass.getName)
      println(lucia.getClass == mike.getClass)

      mike.contacts += alex
      alex.contacts += mike
      lucia.contacts += mike //ok

    }

  }

  //l5.l51
  // l5.l52
  //l5.l55
  // l5.l56
  //  l5.l58

  object q1 {

    class Counter {

      private[this] var _current: Int = _

      def current = _current

      def inc() = if (_current == Int.MaxValue) _current = 1 else _current += 1

    }

    val counter = new Counter
    counter.inc()
    //for (_ <- 0 to Int.MaxValue) counter.inc()
    println(counter.current)

  }

  object q2 {

    class BankAccount(initialBalance: Double) {
      private[this] var _balance = initialBalance

      def balance = _balance

      def deposit(sum: Double) = _balance += sum

      def withdraw(sum: Double) = if (sum <= _balance) _balance -= sum
      else throw new IllegalArgumentException("un sufficient funds")

    }

    val account = new BankAccount(0)
    account.deposit(12)
    println(account.balance)
    account.withdraw(12)
    assert(account.balance == 0)
    println(account.balance)

  }

  object q3_4 {

    //A Matter of Time

    abstract class Time(_hours: Int, _minutes: Int) {
      def hours: Int

      def minutes: Int

      def before(other: Time) = if (other.hours > this.hours) true
      else if (other.hours == this.hours) {
        if (other.minutes > this.minutes) true else false
      } else false

      override def toString: String = f"$hours%02d :$minutes%02d"
    }

    class TimeQ3(_hours: Int, _minutes: Int)
      extends Time(_hours: Int, _minutes: Int) {
      val hours = if (_hours == 24) {
        if (_minutes == 60) {
          1
        } else 0

      } else if (_hours >= 0 && _hours < 24) {
        if (_minutes == 60) {
          _hours + 1
        } else _hours
      }
      else throw new IllegalArgumentException("hours must be >=0 and <=24")

      val minutes = if (_minutes == 60) 0
      else if (_minutes >= 0 && _minutes < 60) _minutes
      else throw new IllegalArgumentException("minutes must be >=0 and <=60")

    }

    class TimeQ4(h: Int, m: Int)
      extends Time(h: Int, m: Int) {

      val MINS_IN_DAY = 24 * 60

      private def check(h: Int, m: Int) = {
        if (m < 0 || m > 60) throw new IllegalArgumentException("minutes must be >=0 and <=60")
        if (h < 0 || h > 24) throw new IllegalArgumentException("hours must be >=0 and <=24")
      }

      val _minutes: Int = {

        check(h, m)

        val maybe = h * 60 + m

        if (maybe == MINS_IN_DAY) {
          0
        } else if (maybe > MINS_IN_DAY) {
          maybe - MINS_IN_DAY
        } else {
          maybe
        }

      }

      override def hours: Int = _minutes / 60

      override def minutes: Int = _minutes - (hours * 60)

    }

    def timeTest(conv: (Int, Int) => Time): Unit = {

      val t1: Time = conv(12, 11)
      val t2 = conv(14, 45)
      println(s"'12:11'=$t1, '14:45'=$t2")
      assert(t1.before(t2))
      assert(t2.before(t1) == false)


      val t3 = conv(24, 45)
      val t4 = conv(24, 60)
      val t5 = conv(24, 0)
      println(s"'24:45'=$t3 '24:60'=$t4 '24:00'=$t5")


    }

    println("\ntest Time q3")
    timeTest((h, m) => new TimeQ3(h, m))
    println("\ntest Time q4")
    timeTest((h, m) => new TimeQ4(h, m))

  }

  object q5 {

    import scala.beans.BeanProperty

    class Student(@BeanProperty var id: Long, @BeanProperty var name: String) {
      override def toString: String = s"student($id):$name"
    }

    val mari = new Student(1, "mari")
    val fred = new Student(1, "fred")

    mari.setId(2) //JavaBeans
    fred.id = 3 //scala id_=(Long) , id_$eq for Java
    println(s"\n $mari, $fred")

  }

  object q6 {

    class Person(val name: String, _age: Int) {
      val age = if (_age >= 0) _age else 0

      override def toString: String = s"I'm $name, and I'm $age years old"

    }

    println(new Person("Ann", -12) + ", " + new Person("Butch", 23))

  }

  object q7 {

    class Person(val fullName: String) {
      private val split = fullName.split(" ")
      val firstName = split(0)
      val lastName = if (split.length > 1) split(1) else "unknown"

      override def toString: String = s"My name $firstName, and last name $lastName"
    }

    println(new Person("Ann Golon") + "; " + new Person("Butch Marcus") + "; " + new Person("007"))

  }

  object q8 {

    class Car(val manufacturer: String, val model: String, val year: Int, var regNum: String) {

      def this(manufacturer: String, model: String) {
        this(manufacturer, model, -1, "")
      }

      def this(manufacturer: String, model: String, year: Int) {
        this(manufacturer, model, year, "")
      }

      def this(manufacturer: String, model: String, regNum: String) {
        this(manufacturer, model, -1, regNum)
      }

      override def toString: String = s"$manufacturer:$model, $year, $regNum"
    }

    println("Scala Section5.q8.Car's")
    val blackMoon = new Car("BigFactory", "BlackMoon", 1977, "#1")
    println(blackMoon)
    blackMoon.regNum = "1"
    println(blackMoon)

    val cabby = new Car("GarageInc", "Yellow Cab")
    println(cabby)
    cabby.regNum = "#77799"
    println(cabby)

  }

  object q9 {

    import horstmann.JSection5_q8.Car

    println("Scala JSection8_q8.Cars")
    val blackMoon = new Car("BigFactory", "BlackMoon", 1977, "#1")
    println(blackMoon)
    blackMoon.setRegNum("1")
    println(blackMoon)

    val cabby = new Car("GarageInc", "Yellow Cab")
    println(cabby)
    cabby.setRegNum("#77799")
    println(cabby)

  }

  object q10 {

    trait Emp {
      def name: String

      def salary: Double

      override def toString: String = s"$name with salary:$salary"
    }

    class Employee(val name: String, val salary: Double) extends Emp {
      def this() {
        this("John Q. Public", 0d)
      }
    }

    class EmployeeRev() extends Emp {

      var _name = "John Q. Public"
      var _salary = 0d

      def name = _name

      def salary = _salary

      def this(name: String, salary: Double) {
        this()
        this._name = name
        this._salary = salary
      }
    }

    def testEmp(conv0:()=>Emp,conv1:(String,Double)=>Emp){

      val john:Emp=conv0()
      println(john)

      val  king=conv1("King Kong",Double.MaxValue-1)
      println(king)
    }

    testEmp(()=>new Employee(),(n,s)=>new Employee(n,s))
    testEmp(()=>new EmployeeRev(),(n,s)=>new EmployeeRev(n,s))

  }

  //  q1
  //  q2
  //  q3_4
  //  q5
  //  q6
  // q7
  q8
  q9
  q10
}
