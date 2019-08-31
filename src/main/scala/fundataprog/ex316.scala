sealed trait List[+A]{

    def getTail:List[A]=this match{
        case Nil => throw new UnsupportedOperationException("tail on empty list")
        case Cons(_,tail)=>tail
    }

    def foreach(f:A=>Unit):Unit
}

case object Nil extends List[Nothing]{
    def foreach(f:Nothing=>Unit):Unit=()
}


case class Cons[+A](head: A, tail: List[A]) extends List[A]{

    def foreach(f:A=>Unit):Unit={
        f(head)
        tail.foreach(f)
    }
}

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

  def map[A,B](as:List[A])(f:A=>B):List[B]=as match{
        case Cons(head,tail)=>Cons(f(head),map(tail)(f))
        case _ => Nil
  }

  //   Write a function that transforms a list of integers by adding 1 to each element.
  // (Reminder: this should be a pure function that returns a new List !)
  def plus[A](as:List[A],n:A)(implicit ev:Numeric[A])=map(as)(i=>ev.plus(i,n))  

  //   Write a function that turns each value in a List[Double] into a String . You can use
  // the expression d.toString to convert some d: Double to a String
  def show[A](as:List[A])=map(as)(_.toString)  
  

  // Write a function filter that removes elements from a list unless they satisfy a given
  // predicate. Use it to remove all odd numbers from a List[Int] .
  def filter[A](as: List[A])(f: A => Boolean): List[A]=foldLeft(as,Nil:List[A]){
      case (acc,head)=>if (f(head)) {
          Cons(head,acc)
      } else {
          acc
      }
  }

  //   Write a function flatMap that works like map except that the function given will return
  //   a list instead of a single result, and that list should be inserted into the final resulting
  //   list. Here is its signature:
  // For instance, flatMap(List(1,2,3))(i => List(i,i)) should result in
  // List(1,1,2,2,3,3) .
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B]=foldLeft(as,Nil:List[B]){
      case (acc,head)=>concat(acc,f(head))
  }

  //Use flatMap to implement filter .
  def filter2[A](as: List[A])(f: A => Boolean): List[A]=flatMap(as){
      head => if (f(head)) {
          List(head)
      } else {
          Nil:List[A]
      }
  }

  //Write a function that accepts two lists and constructs a new list by adding correspond-
  // ing elements. For example, List(1,2,3) and List(4,5,6) become List(5,7,9)
  def zip[A,B](left:List[A],right:List[A])(f:(A,A)=>B):List[B] ={

      def loop(acc:List[B],left:List[A],right:List[A]):List[B]=left match {
          case Cons(lHead,lTail)=>right match {
              case Cons(rHead,rTail)=>loop(append(acc,f(lHead,rHead)),lTail,rTail)
              case _ => acc
          }
          case _ =>acc
      }

      loop(Nil:List[B],left,right)
  }

  def zipPlus[A](left:List[A],right:List[A])(implicit ev:Numeric[A]):List[A]=zip(left,right)(ev.plus)
  

    // Hard: As an example, implement hasSubsequence for checking whether a List con-
    // tains another List as a subsequence. For instance, List(1,2,3,4) would have
    // List(1,2) , List(2,3) , and List(4) as subsequences, among others. You may have
    // some difficulty finding a concise purely functional implementation that is also effi-
    // cient. That’s okay. Implement the function however comes most naturally. We’ll
    // return to this implementation in chapter 5 and hopefully improve on it. Note: Any
    // two values x and y can be compared for equality in Scala using the expression x == y .
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean={ //FIXME: very difficult

      def findFirst[A](v:A,l:List[A],r:Boolean):(Boolean,List[A]) = l match {
          case Cons(h,t)=>if (h==v) {
              (true,t)
          } else {
              findFirst(v,t,false)
          }
          case _ => (r,Nil)
      }

      def loop(left:List[A],right:List[A],ff:Boolean):Boolean = right match {
          case Cons(rHead,rTail)=>left match {
              
              case Cons(lHead,lTail)=> 
                    if (ff) {
                       val (f,t)=findFirst(rHead,left,false)     
                       if (f) {
                           loop(t,rTail,false)
                       } else {
                           false
                       }
                    } else {
                        if (rHead==lHead) {
                            loop(lTail,rTail,false)
                        } else {
                            false
                        }
                    }

              
              case _ => false //left is empty

          }
          case _ => true //right is empty
      }

      loop(sup,sub,true)

  }
        
}


object tests extends App {
    import List._

    assert(List.hasSubsequence(List(1,2,3),List(2))) 
    assert(List.hasSubsequence(List(1,2,3),List(1,2)))
    assert(List.hasSubsequence(List(1,2,3),List(1,2,3)))
    assert(! List.hasSubsequence(List(1,2,3),List(1,3))) //false
    assert(List.hasSubsequence(List(1,2,3),List(3)))
    assert(! List.hasSubsequence(List(1,2,3),List(5))) //false
    assert(List.hasSubsequence(List(1,2,3),List(1,2)))
    assert(List.hasSubsequence(List(1,2,3),List(2,3)))
    assert(! List.hasSubsequence(List(1,2,3),List(23))) //false
    assert(! List.hasSubsequence(List(1,2,3),List(3,2))) //false
    assert(! List.hasSubsequence(List(1,2,3),List(3,2,1))) //false
    assert(! List.hasSubsequence(List(),List(3,2,1))) //false
    assert(List.hasSubsequence(List(),List()))
    assert(List.hasSubsequence(List(1),List()))
    assert(List.hasSubsequence(List(1),List(1)))
    assert(!List.hasSubsequence(List(1),List(4)))//false
}