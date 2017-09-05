package horstmann
/**
  * Created by moroz on 17.05.17.
  */

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

object Section8App extends App {
  val a = List(1, 2, 3, 4, 5)
  println(a.getClass.getName)
  println(a.toString())
  val b = 1 :: 2 :: 3 :: 4 :: 5 :: Nil
  println(b.getClass.getName)
  println(b.toString())

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
  println(ant.env.length) //0 here
  val antFixed: AntFixed = new AntFixed()
  println(antFixed.env.length) //2 here

}
