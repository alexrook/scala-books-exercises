
import shapeless.{HNil, ::, HList}

val product: String :: Int :: Boolean :: HNil =
  "Sunday" :: 1 :: false :: HNil

val first = product.head

// first: String = Sunday
val second = product.tail.head
println(second)
// second: Int = 1
val rest = product.tail.tail
println(rest)
// rest: shapeless.::[Boolean,shapeless.HNil] = false :: HNil

import shapeless.Generic
case class IceCream(name: String, numCherries: Int, inCone: Boolean)
val iceCreamGen = Generic[IceCream]

val iceCream = IceCream("Sundae", 1, false)
// iceCream: IceCream = IceCream(Sundae,1,false)
val repr: ::[String, ::[Int, ::[Boolean, HNil]]] = iceCreamGen.to(iceCream)



class Foo {

  class Bar

}

val foo1 = new Foo
val foo2 = new Foo

/***
    `#` means that we don’t refer to any specific instance,
    in this case Foo#Bar, every Bar inside every instance
    of Foo will be a valid instance
 */
var a: Foo#Bar = new foo1.Bar
val b: Foo#Bar = new foo2.Bar

a=b
a==b

/**
   `.` means that we can only refer the Bar instances
      that belong to a specific instance of Foo
  */
val c: foo1.Bar = new foo1.Bar
//val d: foo2.Bar = new foo1.Bar
// [error]  found   : console.foo1.Bar
// [error]  required: console.foo2.Bar

trait Out {
  type In
  def get: In
}

def foo(o: Out): o.In = o.get


case class Custom(v:String) extends Out {
  type In=Int
  override def get: Int = 42
}

foo(Custom("a"))

trait Out1[T] {
   def get: T
}

def foo1[T](o: Out1[T]): T = o.get


case class Custom1(v:String) extends Out1[String] {
  override def get: String = "Custom1"
}

foo1(Custom1("b"))

import shapeless.{Coproduct, :+:, CNil, Inl, Inr}

sealed trait Light
case class Red() extends Light
case class Amber() extends Light
case class Green() extends Light
case class Black() extends Light
//type Light = Red :+: Amber :+: Green :+: CNil
///with type hier alphabeticaly constructs
val amber=Inr(Inl(Amber))

val genL = Generic[Light]

val amber1=genL.to(Amber())

val green=genL.to(Green())

val red=genL.to(Red())

val black=genL.to(Black())

//val green: Light =

import shapeless.Generic

sealed trait Shape
final case class Rectangle(width: Double, height: Double) extends Shape
final case class Circle(radius: Double) extends Shape

val gen = Generic[Shape]

gen.to(Rectangle(3.0, 4.0))
// res3: gen.Repr = Inl(Rectangle(3.0,4.0))
gen.to(Circle(1.0))
// res4: gen.Repr = Inr(Inl(Circle(1.0)))

