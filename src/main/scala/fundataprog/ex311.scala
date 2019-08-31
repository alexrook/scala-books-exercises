sealed trait List[+A]{

    def getTail:List[A]=this match{
        case Nil => throw new UnsupportedOperationException("tail on empty list")
        case Cons(_,tail)=>tail
    }
    
}

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
object List {

    def apply[A](as: A*): List[A] = if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
    
 
    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
        as match {
          case Nil => z
            case Cons(x, xs) => f(x, foldRight(xs, z)(f))
        }

   def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B={
    
    @annotation.tailrec
    def loop(acc:B,as:List[A]):B =as match
        {
            case Nil => acc
            case Cons(x,xs)=>loop(f(acc,x),xs)
        }

    loop(z,as)

   }

    def foldLeft2[A,B](as: List[A], z: B)(f: (B, A) => B): B=
        foldRight(as,z){(x,acc)=>
                f(acc,x)
        }


     def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B=
        foldLeft(as,z){(acc,x)=>
                f(x,acc)
        }    


  def length[A](as: List[A]): Int=foldLeft(as,0)((acc,_ )=>acc+1)

  def sum[A](as: List[A])(implicit ev:Numeric[A]): A=foldLeft(as,ev.zero)((acc,x )=>ev.plus(acc ,x))
  
  def product[A](as: List[A])(implicit ev:Numeric[A]): A=foldLeft(as,ev.one)((acc,x )=>ev.times(acc ,x))

  def reverse[A](as:List[A]):List[A]=foldLeft[A,List[A]](as,Nil)((acc,x)=>Cons(x,acc))

  def append[A](as:List[A],x:A):List[A]=foldRight(as,Cons(x,Nil))((h,acc)=>Cons(h,acc))
  
  def prepend[A](as:List[A],x:A):List[A]=Cons(x,as)

  def concat[A](left: List[A], right: List[A]): List[A] =
   left match {
     case Nil => right
     case Cons(h,t) => Cons(h, concat(t, right))
   }

  //   Hard: Write a function that concatenates a list of lists into a single list. Its runtime
  //   should be linear in the total length of all lists. Try to use functions we have already
  //   defined.

  def flatten[A](as:List[List[A]]):List[A]=foldLeft(as,Nil:List[A]){
      case (acc,head)=>
      println(head)
      concat(acc,head)
  }

}

val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
    
}

List(1,2,3,4).tail