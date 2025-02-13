import java.util.concurrent.Executors

import effectful.EffectfulPulls.Pull
import effectful.EffectfulPulls.Stream
import iomonad.IO

Pull.Uncons[IO, Int, Unit](Pull.Output(1) >> Pull.Output(2) >> Pull.done).toList
val pool = Executors.newFixedThreadPool(8)
val h    = Stream.eval(IO.now(9)).map(_ + 90)

Stream.eval(IO.now(9)).map(_ + 90).run

h

h.run.unsafeRunSync(pool)

Stream.eval(IO.now(9))

Stream.eval(IO.now(9)).run
Stream.eval(IO.now(9)).run.unsafeRunSync(pool)

Pull
  .Uncons[IO, Int, Unit](Pull.Output(1) >> Pull.Output(2) >> Pull.done)
  .take(2)
  .toList
  .unsafeRunSync(pool)

Stream.unfold(0)(x => Some(x, x + 1))
//
//res8: Stream[Nothing1, Int] = FlatMap(FlatMap(Output(0),effectful.EffectfulPulls$Pull$$Lambda$20471/0x000000060386fb10@13eb73b4),iomonad.Monad$$Lambda$20479/0x00000006038747a0@210bc55)

Stream.unfold(0)(x => Some(x, x + 1)).take(10).toList
//.toList

Pull
  .Uncons[IO, Int, Unit](Pull.Output(1) >> Pull.Output(2) >> Pull.done)
  .take(1)
  .toList
  .unsafeRunSync(pool)
import scala.util.control.TailCalls._

//  def isEven(xs: List[Int]): TailRec[Boolean] =
//    if (xs.isEmpty) done(true) else tailcall(isOdd(xs.tail))

//  def isOdd(xs: List[Int]): TailRec[Boolean] =
//   if (xs.isEmpty) done(false) else tailcall(isEven(xs.tail))

//  isEven((1 to 10000).toList).result

//  def fib(n: Int): TailRec[Int] =
//   if (n < 2) done(n) else for {
//      x <- tailcall(fib(n - 1))
//      y <- tailcall(fib(n - 2))
//    } yield x + y

//  fib(40).result

val f: (Any, Any) => Unit = (_, _) => ()

f(1, 4)
f("hello", 2)
f(1.3, 5)
f(Pull.Result(2), 1)

//Pull.unfold(0)(x=>if(x%2==0) Right(x,x/2) else Right(x,(x*3)+1)).take(5).toList
