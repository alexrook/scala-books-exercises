sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

    def fold[A,B](tree:Tree[A],acc:Option[B])(f:A=>B)(comb:(B,B)=>B):B=tree match {
        case Leaf(value)=>acc.map(comb(_,f(value))).getOrElse(f(value))
        case Branch(left,right)=>
            val lRet=fold(left,acc)(f)(comb)
            val rRet=fold(right,acc)(f)(comb)
            comb(lRet,rRet)
    }

    //Write a function size that counts the number of nodes (leaves and branches) in a tree.
    def size(tree:Tree[_]):Int=tree match {
        case _:Leaf[_]=>1
        case Branch(left:Tree[_],right:Tree[_])=>size(left) + size(right)
    }

    // Write a function maximum that returns the maximum element in a Tree[Int] . (Note:
    // In Scala, you can use x.max(y) or x max y to compute the maximum of two integers x and y)
    import scala.math.Ordering
    def max[A](tree:Tree[A])(implicit ev:Ordering[A])=fold(tree,None)(identity)(ev.max)

    // Write a function depth that returns the maximum path length from the root of a tree
    // to any leaf.
    def depth(tree:Tree[_]):Int={
        def loop(v:Int,tree:Tree[_]):Int=tree match {
            case _ :Leaf[_]=>v
            case Branch(left,right)=>
             val lRet=loop(v+1,left)
             val rRet=loop(v+1,right)
             lRet max rRet
        }

        loop(0,tree)
    } 
    
    def apply[A](value:A):Tree[A]=Leaf(value)
    def apply[A](left:Tree[A],right:Tree[A]):Tree[A]=Branch(left,right)

}

object tests {

  import Tree._  

  assert(size(Tree(1))==1)
  assert(size(Tree(Tree(1),Tree(Tree(2),Tree(3))))==3)
  assert(size(Branch(Leaf(1),Branch(Leaf(2),Leaf(3))))==3) //the same

  assert(max(Leaf(1))==1)
  assert(max(Leaf(0))==0)
  assert(max(Leaf(-142))== -142)
  assert(max(Branch(Leaf(-1),Branch(Leaf(42),Leaf(3))))== 42) 
  assert(max(Branch(Leaf(-1),Branch(Leaf(-42),Leaf(-3))))== -1) 
  assert(Tree.depth((Branch(Leaf(-1),Branch(Leaf(-42),Leaf(-3))==2)

  assert(Tree.depth(
                      Branch(
                        Branch(Leaf(1),Leaf(1)),
                        Branch(Leaf(-42),Leaf(-3))
                      )
                    )
        ==2)

   assert(Tree.depth(
                      Branch(
                        Branch( Leaf(1),
                                Branch(Leaf(1),Leaf(1))
                                ),
                        Branch(Leaf(-42),Leaf(-3))
                      )
                    )
        ==3)     

  println("All Ok")   

}