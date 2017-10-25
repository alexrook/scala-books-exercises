package horstmann

object Section12 extends App{

  object l12{
    object l121{

      def foo(f:(Int)=>Double)=f(3)

      println(foo(_*2d))
    }

    l121
  }

  l12
}
