package essentilas

object Underscore  extends App{


  import scala.collection.mutable.ArrayBuffer

  case class Holder1[T](v:T)

  val buf1=ArrayBuffer(Holder1(1),Holder1('a'),Holder1("aa"))

  println(buf1.getClass.getName)

  val buf2:ArrayBuffer[Any]=ArrayBuffer(Holder1(1),Holder1('a'),Holder1("aa"))

  println(buf1.getClass.getName)


  val buf3:ArrayBuffer[Any]=ArrayBuffer(Holder1(1))

  buf3 += Holder1("aaaa")

  println(buf3.getClass.getName)

  val buf4:ArrayBuffer[Holder1[Any]]=ArrayBuffer(Holder1(1))

  buf4 += Holder1("aaaa")

  println(buf4.getClass.getName)

  val list1:List[Holder1[Any]]=List(Holder1(1),Holder1('a'),Holder1("aa"))

  println(list1.getClass.getName)

  val holder1: Holder1[String] =Holder1("aaa")

  val holder2: Holder1[Symbol] =Holder1('aaa)

//  val list2:List[Holder1[Any]]=List(holder1,holder2) don't compile

  val list3:List[Holder1[_]]=List(holder1,holder2)

  println(list1.getClass.getName)

  val k=11
  val c=123

  val e: Int =if (c>k) 123 else throw new Exception("bad") // Nothing <: Int поэтому тип e:Int


}
