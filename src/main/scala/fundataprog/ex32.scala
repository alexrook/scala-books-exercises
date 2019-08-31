sealed trait List[+A]{

    def getTail:List[A]=this match{
        case Nil => throw new UnsupportedOperationException("tail on empty list")
        case Cons(_,tail)=>tail
    }
    
}

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
object List {
    def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x,xs) => x * product(xs)
        }

    def setHead[A](h:A,l:List[A]):List[A]=Cons(h,l.getTail)   
    
    def apply[A](as: A*): List[A] = if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

    def drop[A](l: List[A], n: Int): List[A]=if (n==0) {
        l
    } else {
        l match{
            case Nil => throw new UnsupportedOperationException("drop on empty list")
            case Cons(_,t)=>drop(t,n-1)
        }
    }

    def dropWhile[A](l: List[A], f: A=>Boolean): List[A]=
        l match{
            case Nil => l
            case Cons(head,tail)=>if (f(head)) {
                dropWhile(tail,f)
            } else {
                l
            }
        }
    
    def append[A](left: List[A], right: List[A]): List[A] =
        left match {
            case Nil => right
            case Cons(h,t) => Cons(h, append(t, right))
        }

    def init[A](l: List[A]): List[A] =
     l match {
       case Nil => sys.error("init of empty list")
       case Cons(_,Nil) => Nil
       case Cons(h,t) => Cons(h,init(t))
     }
 
  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => List(buf.toList: _*)
      case Cons(h,t) => buf += h; go(t)
    }
    go(l)
  }

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

  def length[A](as: List[A]): Int=foldRight(as,0)((_,acc)=>acc+1)

}

val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
}

List(1,2,3,4).tail