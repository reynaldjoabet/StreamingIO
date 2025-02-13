import simple.SimplePulls.Pull
import simple.SimplePulls.Stream
import scala.compiletime.*
def mystream(stream:Stream[Int]):Pull[Int,Unit]={
stream.toPull.uncons.flatMap{
  case Left(value) =>Pull.done 
  case Right((h,tl)) => Pull.Output(h)>>mystream(tl.toStream)
}
}

mystream(Stream(1,2,3,4,5,6,7,8))

//on Pulls, flaptMap or map works on the result R, so mapOutput and flatMapOutput work on the output O
val pullIntegers= 
  Pull.fromList(List(1, 2, 3)).mapOutput(i => i * 2)

val pullStrings= pullIntegers.flatMapOutput(i => 
  Pull.Output(i.toString) //Convert each integer to a string
)


//   def flatMap[O2 >: O, R2](f: R => Pull[O2, R2]): Pull[O2, R2] =FlatMap(this, f)
Pull.Output(1) >> Pull.Output(2) >> Pull.Output(3) >> Pull.done

//FlatMap(FlatMap(FlatMap(Output(1),simple.SimplePulls$Pull$$Lambda$11485/),simple.SimplePulls$Pull$$Lambda$11485/0x0000007002),simple.SimplePulls$Pull$$Lambda$11485/0x000000700)

//FlatMap(FlatMap(FlatMap(1,f),g),h)

Pull.Output(1) >> Pull.Output(2) >> Pull.Output(3) >> Pull.Output(4)>> Pull.Output(5) >> Pull.done

//FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(Output(1),simple.SimplePulls$Pull$$Lambda$11485),simple.SimplePulls$Pull$$Lambda$11485/0x000000700),simple.SimplePulls$Pull$$Lambda$11),simple.SimplePulls$Pull$$Lambda$11485),simple.SimplePulls$Pull$$Lambda$11485)

Pull.Output(1).flatMap(x=>Pull.Output(2).flatMap(n=>Pull.Output(3).flatMap(x=>Pull.Output(4)).flatMap(x=>Pull.Output(5))))

//FlatMap(Output(1),repl.MdocSession$MdocApp$$Lambda$12813)


//In this case, when 
//Pull.Output(1).flatMap(...) is called, it creates a new stack frame. Inside that frame, when Pull.Output(2).flatMap(...) is called, another frame is added, and so on for each subsequent flatMap

//(1 to 10000).foldLeft(Pull.done: Pull[Int, Unit])((acc, num) => acc.flatMap(_ => Pull.Output(num)))//StackOverflowError

//he goal is to avoid excessive nesting in the flatMap chain, which can lead to stack overflow due to deep recursion
//(1 to 10000).foldLeft(Pull.done: Pull[Int, Unit])((acc, num) => acc>>(Pull.Output(num)))
//  the >> operator sequences the pulls without deep recursion. Alternatively, if you must use flatMap, you can do so with Pull.suspend or Pull.eval as an intermediary step. These options help ensure that each computation suspends properly, avoiding stack overflows by releasing the stack at every iteration

// (1 to 100000).foldRight(Pull.done: Pull[Int, Unit]) {
//   (num, acc) => acc.flatMap(x=>Pull.Output(num))
// }



(1 to 100).foldLeft(Pull.done: Pull[Int, Unit])((acc, num) => acc.flatMap(_ => Pull.Output(num)))
// first Pullor initial Pull is Pull.done
// FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(FlatMap(Result(()),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024e),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@43333b8),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@380b3db),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@5216797a),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@7468f7be),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@695bb478),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@6f43cce3),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@effb5d8),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@3d9966be),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@9d5fd01),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@78800c0c),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@b2227b1),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@7df80cfd),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@5b63dbb6),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@23fb5cba),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@397ee8db),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@4dc84ed),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@412741ab),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@38c0cbb9),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@76b6c44a),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@162ddd9d),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@6dfcecea),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@535f6fef),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@130b755a),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@6bb28393),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@6123a9b3),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@492b926f),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@1c29fb5),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@7e932a05),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@2816a55),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@172c2880),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@20632e7),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@65a2e3a0),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@6cf8ee83),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@1a66a3b4),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@29442b15),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@5b09727e),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@702e136a),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@5ffa65d8),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@57eee1b1),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@47e4e3b0),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@47511d65),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@4b8964f2),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@6bcae75b),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@b5360f1),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@4426fecb),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@4488ac49),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@78c46bbc),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@6880c8bd),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@1b23a3c0),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@1ac8be09),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@418abf67),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@2fa152cb),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@291ad95c),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@76fd1280),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@6298fabf),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@470adb2d),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@38253aa9),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@2652563f),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@33a0d48),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@12f87db1),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@399ed35),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@374ea0d7),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@644dbb3b),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@350957f6),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@7935e427),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@3b3fabce),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@32a24bf1),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@4d6b0cb4),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@1b29dde2),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@5d0e7492),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@7d535101),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@68c8fcb3),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@3ddbea85),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@13dfa348),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@74e3336b),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@5b617420),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@390c220b),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@57a70381),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@3ab21a75),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@6aa2a977),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@32e6d252),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@5d327484),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@77f9bce5),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@8ef4a7c),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@c1f6d71),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@42c7ae22),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@78632c0b),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@a2d25d1),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@7daeb500),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@267a229),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@6385c51a),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@e56716),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@86bd4bf),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@37a25971),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@7a42e78a),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@51b4e203),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@7ac0aa6),repl.MdocSession$MdocApp$$Lambda$13180/0x00000070024ee060@22e0a),repl.MdocSession$MdocApp$$Lambda$13180/0x0000007002)

////   def flatMap[O2 >: O, R2](f: R => Pull[O2, R2]): Pull[O2, R2] =FlatMap(this, f)

(1 to 1).foldLeft(Pull.done: Pull[Int, Unit])((acc, num) => acc.flatMap(_ => Pull.Output(num)))

//FlatMap(Result(()),repl.MdocSession$MdocApp$$Lambda$13587/0x00000070024f2500@208cd1bd)


(1 to 2).foldLeft(Pull.done: Pull[Int, Unit])((acc, num) => acc.flatMap(_ => Pull.Output(num)))

//FlatMap(FlatMap(Result(()),repl.MdocSession$MdocApp$$Lambda$13503/0x00000070024f2720@76609a55),repl.MdocSession$MdocApp$$Lambda$13503/0x00000070024f2720@2d7f9a34)

// first time,we just have Pull.done and use it to create FlatMap(Result(()),f)
//second time,this in FlatMap(this, f) becomes FlatMap(Result(()),f), which results in 
//FlatMap(FlatMap(Result(()),f),g)
//case Output[+O](value: O) extends Pull[O, Unit]
//R is Unit for Output
Pull.Output(6).flatMap(x=>Pull.Output(2)>>Pull.done).toList

//FlatMap(Output(6),FlatMap(x=>Output(2),Pull.done))


Pull.Output(6).flatMap(x=>Pull.Result(()))


//FlatMap(Output(6),x=>Result(()))

Pull.done.flatMap(x=>Pull.Result(()))

summon[Pull[Nothing, Unit]<:< Pull[String,Unit]]


Pull.done.asInstanceOf[Pull[Int,Int]]



val fa: Option[Int] = Some(2)
val f: Int => Option[Int] = x => Some(x + 1)
val g: Int => Option[Int] = x => Some(x * 2)

fa.flatMap(f).flatMap(g)               // Option[Int]
fa.flatMap(a => f(a).flatMap(g))        // Option[Int]

Pull.fromList((1 to 100).toList)

Pull.fromList((1 to 100).toList).flatMapOutput(i=>Pull.fromList(List(i->1))).toList



(1 to 2).foldLeft(Pull.done: Pull[Int, Unit])((acc, num) => acc.flatMap(_ => Pull.Output(num)))

// right associated
//flatMap takes a function from R=>Pull[O2,R2]
// everything else is f _ => Pull.Output(2).flatMap(_ => Pull.done)
Pull.Output(1).flatMap(_ => Pull.Output(2).flatMap(_ => Pull.done))

//FlatMap(Output(1),repl.MdocSession$MdocApp$$Lambda$11673/0x0)


Pull.done.flatMap(x=>Pull.Output(2).flatMap(z=>Pull.done))

Pull.Output(1).flatMap(_ => Pull.Output(2).flatMap(_ => Pull.done)).toList

Pull.done.flatMap(x=>Pull.Output(2).flatMap(z=>Pull.done)).toList


Pull.Output(1).flatMap(x=>Pull.Output(2).flatMap(n=>Pull.Output(3)).flatMap(x=>Pull.Output(4)).flatMap(x=>Pull.Output(5))).toList


Pull.Output(1).flatMap(x=>Pull.Output(2).flatMap(n=>Pull.Output(3).flatMap(x=>Pull.Output(4)).flatMap(x=>Pull.Output(5)))).toList



Pull.Output(1).flatMap(x=>Pull.Output(2).flatMap(n=>Pull.Output(3).flatMap(x=>Pull.Output(4).flatMap(x=>Pull.Output(5))))).toList


//mystream(Stream(1,2,3,4,5,6,7,8)).toList

Stream(1,2,3)

Stream(1,2,3).toList

Stream(1,2,3).++(Stream(4,5))

Stream(1,2,3).++(Stream(4,5)).toList

Stream(1,2,3).forever

//The flatMap method represents a chaining operation, where g(x).flatMap(y => f(y)) only executes after x has been produced by s2

//s2 must produce a value before g(x).flatMap(y => f(y)) can be executed

//The monad law of associativity does state that the order of nesting flatMap operations does not affect the final outcome.

Stream(1).flatMap(x=>Stream(3)).flatMap(x=>Stream(4)).toList

Stream(1).toList

(Pull.Output(1)>>Pull.done).toList

//FlatMap(Output(1),(_)=> Result(()))

Pull.FlatMap(Pull.Output(1),(_)=> Pull.Result(())).toList

Pull.FlatMap(Pull.Output(1),(_)=> Pull.Result(()))>>Pull.FlatMap(Pull.Output(3),(_)=> Pull.Result(()))

(Stream(1).++(Stream(3))).toList
(Stream(1).*>(Stream(3))).toList
Pull.FlatMap(Pull.FlatMap(Pull.Output(1),(_)=> Pull.Result(())),(_)=>Pull.FlatMap(Pull.Output(3),(_)=> Pull.Result(()))).toList



val h=(x:Int)=>2

h(4)

val l=(x:Any)=>2

l(3)

val n:Any=>Int=(_)=>2

n(5)

val m:(Any,Any)=>Int=(_,_)=>2

m(3,"p")

val p: (Any, Any) => Unit=(_,_)=>()

  p.apply(1,4)
  p("hello",2)
  p(1.3,5)
  p(Pull.Result(2),1)


  val k:Function2[Any,Any,String]=(x,y)=>"hello"


  k(12,2)

  inline def printConstValue[T]: Unit = {
    val const = constValue[T]
    println(s"Constant value: $const")
  }

  printConstValue[3]


  printConstValue["hello"]

  //printConstValue[Pull[Int,Int]] wont work

  inline def printTypeName[T]: Unit = inline erasedValue[T] match {
    case _: Int => println("Type is Int")
    case _: String => println("Type is String")
    case _ => println("Unknown type")
  }

  printTypeName[Int]     
  printTypeName[String]  
  printTypeName[Double] 


  import scala.compiletime.erasedValue

  inline def inspectType[T]: Unit = inline erasedValue[T] match {
    case _: Pull[_, _] => println("This is a Pull type")
    case _: Int => println("This is an Int")
    case _: String => println("This is a String")
    case _ => println("Unknown type")
  }

  inspectType[Pull[Int, Int]] 
  inspectType[Int]  


  trait Semigroup[A]:
    def combine(x:A,y:A):A
    extension(x:A)
      inline def |+|(y:A)=combine(x,y) 


  given Semigroup[Int] with
        override  def combine(x:Int,y:Int):Int=x+y 

 8.|+|(9)

 // no imports
 // just put the extension method directly as a member of the type class

           