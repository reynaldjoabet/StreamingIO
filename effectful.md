Let’s parameterize both `Pull` and `Stream` with
the ability to evaluate an effect to generate either a result value or an output value. To do this, we’ll add an additional data constructor to `Pull`:


```scala
 enum Pull[+F[_], +O, +R]:
    case Result[+R](result: R) extends Pull[Nothing, Nothing, R]
    case Output[+O](value: O) extends Pull[Nothing, O, Unit]
    case Eval[+F[_], R](action: F[R]) extends Pull[F, Nothing, R]
    case FlatMap[+F[_], X, +O, +R](
      source: Pull[F, O, X], f: X => Pull[F, O, R]) extends Pull[F, O, R]
    case Uncons[+F[_], +O, +R](source: Pull[F, O, R])
      extends Pull[F, Nothing, Either[R, (O, Pull[F, O, R])]]

    def step[F2[x] >: F[x], O2 >: O, R2 >: R](
      using F: Monad[F2]
    ): F2[Either[R2, (O2, Pull[F2, O2, R2])]] =
      this match
        case Result(r) => F.unit(Left(r))
        case Output(o) => F.unit(Right(o, Pull.done))
        case Eval(action) => action.map(Left(_))
        case Uncons(source) =>
          source.step.map(s => Left(s.asInstanceOf[R2]))
        case FlatMap(source, f) => 
          source match
            case FlatMap(s2, g) =>
              s2.flatMap(x => g(x).flatMap(y => f(y))).step
            case other => other.step.flatMap:
              case Left(r) => f(r).step
              case Right((hd, tl)) => F.unit(Right((hd, tl.flatMap(f))))

```

```scala
//SimplePulls
    @annotation.tailrec
    final def fold[A](init: A)(f: (A, O) => A): (R, A) = 
      step match
        case Left(r) => (r, init)
        case Right((hd, tl)) => tl.fold(f(init, hd))(f)

// EffectfulPulls
     def fold[F2[x] >: F[x], R2 >: R, A](init: A)(f: (A, O) => A)(
      using F: Monad[F2]
    ): F2[(R2, A)] = 
      step.flatMap:
        case Left(r) => F.unit((r, init))
        case Right((hd, tl)) => tl.fold(f(init, hd))(f)       

```

`fold` can no longer be annotated with the `tailrec` annotation, since it’s now using
monadic recursion instead of regular recursion. Again, we rely on `flatMap` of the effect monad to be stack safe.


 monadic recursion refers to recursive functions that involve monadic values at each recursive step, where each recursive call’s return type is wrapped in a monad (like `Option`, `Either`, `Future`, or `IO`)

 - `Monad Encapsulation`: Each recursive step produces a monadic value. In the case of `IO`, each recursive call returns an `IO` instance that represents a deferred computation or an effectful action. Monads encapsulate these computations, adding structure and control around the effects or context.

- `Chaining Through Monad Operations`: In monadic recursion, recursion steps are chained using monadic operations like flatMap (or sometimes map). flatMap allows each recursive call’s result to be processed or combined with the next step in the recursion, which makes it possible to chain computations in a structured way without direct side effects at each ste

```scala
case class Uncons[+F[_], +O, +R](source: Pull[F, O, R])
      extends Pull[F, Nothing, Either[R, (O, Pull[F, O, R])]]

```      
The definition of `Uncons` is interesting—it extends `Pull[F, Nothing, Either[R, (O,Pull[F, O, R])]]` and references an underlying `Pull[F, O, R]`. The type says that `unconsing` a `Pull[F, O, R]` results in a pull that may evaluate effects in `F`, outputs no elements, and results in either the final value `R` of the source pull or an output value `O`
and a remainder `Pull[F, O, R]`


When we encounter an `Uncons` node in the implementation of `step`, we step the source pull and wrap its result in a `Left`. We need a `Monad[F2]` instance to do so, and step already has that instance in scope


Like `fold` and `toList` on `Pull`, the equivalent operations on `Stream` now return an effectful action. We say that `fold` and `toList` are `eliminators` of the `Stream` type—`inter-preting` or `compiling` the algebra of streams into a target monad.

```scala
  def fold[A](init: A)(f: (A, O) => A)(using Monad[F]): F[A] = 
        self.fold(init)(f).map(_(1))

//let’s fold and discard each output value.
  def run(using Monad[F]): F[Unit] =
        fold(())((_, _) => ()).map(_(1))

```
`init` is the initial value (here `()`).

`f` is a function that takes two arguments: an accumulator of type `A` and an element of type `O`. It processes these two inputs and returns a new accumulator of type `A`.
`f` is `(_, _) => ()`, a function that takes two inputs and returns `()`, essentially ignoring both inputs.


in a naive free interpreter (which is what you effectively have here), you have to "hold on" to the things on the right while you interpret the left that happens regardless of what the left looks like
but if the left is just `Pure` then it's really easy and you haven't held onto the right for very long! however, if the left is itself a really long chain of FlatMaps, well now you have a problem
because you just keep building up stuff to "remember" `right-associated` binds don't have that problem, since you've reassociated things to guarantee that `FlatMap` never occurs on the left side


```scala


((((io1 >> io2) >> io3) >> io4) >> io5) >> io6

IO.FlatMap(
  IO.FlatMap(
    IO.FlatMap(
      ...),
    _ => io5),
  _ => io6)


//This is a right-associative sequence, meaning each IO action is nested within another FlatMap so that each action executes only after the one before it completes.
io1 >> (io2 >> (io3 >> (io4 >> (io5 >> io6))))

IO.FlatMap(
  io1,
  _ => 
    IO.FlatMap(
      io2,
      _ =>
        IO.FlatMap(
          ...)))


```


```scala
IO.FlatMap(
  IO.FlatMap(
    IO.FlatMap(
      IO.FlatMap(
        IO.FlatMap(
          io1, _ => io2
        ), _ => io3
      ), _ => io4
    ), _ => io5
  ), _ => io6
)


IO.FlatMap(
  io1,
  _ => IO.FlatMap(
    io2,
    _ => IO.FlatMap(
      io3,
      _ => IO.FlatMap(
        io4,
        _ => IO.FlatMap(
          io5,
          _ => io6
        )
      )
    )
  )
)


```

`foldRight`: This is a right-associative fold, meaning it starts folding from the rightmost element of a collection

`List(1, 2, 3).foldRight(zero)(f)` evaluates as `f(1, f(2, f(3, zero)))`

`foldLeft`: This is left-associative, where folding starts from the leftmost element. For `List(1, 2, 3).foldLeft(zero)(f)`, it evaluates as `f(f(f(zero, 1), 2), 3)`, applying `f` sequentially and maintaining a cumulative result as it proceeds.


`val myPull: Pull[IO, Nothing, Int] = ???`
the only way to evaluate a Pull is to turn it into a stream in your case, you can emit (output) the int to get a `Pull[IO, Int, Unit]` then you can compile the stream 


```scala
    def uncons: Pull[F, Nothing, Either[R, (O, Pull[F, O, R])]] =
      Uncons(this)
   case Uncons[+F[_], +O, +R](source: Pull[F, O, R])
      extends Pull[F, Nothing, Either[R, (O, Pull[F, O, R])]]
 extension [F[_], O](self: Pull[F, O, Unit])
      def flatMapOutput[O2](f: O => Pull[F, O2, Unit]): Pull[F, O2, Unit] =
        self.uncons.flatMap:
          case Left(()) => Result(())
          case Right((hd, tl)) =>
            f(hd) >> tl.flatMapOutput(f)


```



```scala

 def step[F2[x] >: F[x], O2 >: O, R2 >: R](
      using F: Monad[F2]
    ): F2[Either[R2, (O2, Pull[F2, O2, R2])]] =
      this match
        case Result(r) => println(s"Final Result ${r}");F.unit(Left(r))
        case Output(o) => println(s"Output ${o}");F.unit(Right(o, Pull.done))
        case Eval(action) => action.map(Left(_))
        case Uncons(source) =>
          println(s"Uncons ${source}");source.step.map(s => Left(s.asInstanceOf[R2]))
        case FlatMap(source, f) => 
          source match
            case FlatMap(s2, g) =>
             println(s"Nested FlatMap ${source}");s2.flatMap(x => g(x).flatMap(y => f(y))).step
            case other => other.step.flatMap:
              case Left(r) => println("other result");f(r).step
              case Right((hd, tl)) => println(s"Output value ${hd} and and tail ${tl}");F.unit(Right((hd, tl.flatMap(f))))

    def fold[F2[x] >: F[x], R2 >: R, A](init: A)(f: (A, O) => A)(
      using F: Monad[F2]
    ): F2[(R2, A)] = 
      step.flatMap:
        case Left(r) => F.unit((r, init))
        case Right((hd, tl)) => tl.fold(f(init, hd))(f)

    def toList[F2[x] >: F[x]: Monad, O2 >: O]: F2[List[O2]] =
      fold(List.newBuilder[O]){(bldr, o) => println(s"end of stream value ${o} and ${bldr.result()}");{bldr += o}}
      .map(_(1).result)

 extension [O](self: Stream[Nothing, O])
      def fold[A](init: A)(f: (A, O) => A): A = 
        self.fold(init)(f)(using Monad.tailrecMonad).result(1)

      def toList: List[O] =
        self.toList(using Monad.tailrecMonad).result

```

```scala


(Pull.Output(1) >>Pull.Output(2) >>Pull.done).toStream.toList
//same as below
Pull.FlatMap(Pull.FlatMap(Pull.Output(1),(_)=>Pull.Output(2)),(_)=>Pull.done).toStream.toList
//List(1, 2)
// Nested FlatMap FlatMap(Output(1),effectful.EffectfulPulls$Pull$$Lambda$21148/0x0000000601ef22b0@24888b0e)
// Output 1
// Output value 1 and and tail Result(())
// end of stream value 1 and List()
// Final Result ()
// other result
// Output 2
// Output value 2 and and tail Result(())
// end of stream value 2 and List(1)
// Final Result ()
// other result
// Final Result ()



```

`toList` uses `fold`, which uses `step`


```scala
    def fold[F2[x] >: F[x], R2 >: R, A](init: A)(f: (A, O) => A)(
      using F: Monad[F2]
    ): F2[(R2, A)] = 
      //flatMap on F[_]
      step.flatMap:
        case Left(r) => F.unit((r, init))
        case Right((hd, tl)) => tl.fold(f(init, hd))(f)

//After fold completes, it returns a result wrapped in a monadic context F2.
//The call to .map(_(1)) attempts to extract the second element in the tuple returned by fold, which is (R2, A).
 def run(using Monad[F]): F[Unit] =
        fold(())((_, _) => ()).map(_(1))   


 def eval[F[_], O](fo: F[O]): Stream[F, O] =
      Pull.Eval(fo).flatMap(Pull.Output(_))
val pool = Executors.newFixedThreadPool(8)
Stream.eval(IO.now(9)).run.unsafeRunSync(pool)
```
Stream.eval(IO.now(9)).run
`res3: IO[Unit] = FlatMap(FlatMap(FlatMap(FlatMap(Return(9),iomonad.Monad$$Lambda$10235/0x0000000602e6a4c0@4288874),effectful.EffectfulPulls$Pull$$Lambda$10233/0x0000000602e69b00@662ec63b),effectful.EffectfulPulls$Pull$$Lambda$10236/0x0000000602e6a890@74a5f577),iomonad.Monad$$Lambda$10235/0x0000000602e6a4c0@77e6bfe2)`


Stream.eval(IO.now(9))
`res2: Stream[IO, Int] = FlatMap(Eval(Return(9)),effectful.EffectfulPulls$Stream$$$Lambda$10238/0x0000000602e6b678@746ccebf)`

- Based on `step` we will hit `FlatMap` node and then `other` case. stepping on the `other` case will hit `Eval` node and run the effect and produce 9. Its value would be mapped to a `Left`
- `flatMapping` on this would match `Left(r)` and hence `f(r).step` will be called.

- `f` is a function that when given unit would produce `Output(9)`

- `f(r).step` would result in `Right(9,Pull.done)` wrapped in `F`(ie `IO` in this case)
- `flatMapping` on `IO` would match `Right(9,Result(()))` then we do `tl.fold(f(init, hd))(f)`

- for `run`, `fold` takes an initial value of `init` as `()` and a function `f` `(_,_)=>()`. A function that takes any two parameters and produces Unit, basically discarding the inputs. ` val f: (Any, Any) => Unit=(_,_)=>()`

- `f: (A, O) => A`: A function that combines the accumulator (`A`) and an output value (`O`) to produce a new accumulator value.
- The accumulator is updated by applying `f(init, hd)`, resulting in a new accumulator value.

- Then, the function recurses, calling tl.`fold(f(init, hd))(f)` to process the remainder of the `Pull` with the updated accumulator.

recursion in a monadic context (monadic recursion) involves chaining each recursive step within the monad using flatMap or >>=, thus preserving the monadic structure throughout the recursion. 
```scala
//monadic recursion
    def fold[F2[x] >: F[x], R2 >: R, A](init: A)(f: (A, O) => A)(
      using F: Monad[F2]
    ): F2[(R2, A)] = 
      //flatMap on F[_]
      step.flatMap:
        case Left(r) => F.unit((r, init))
        case Right((hd, tl)) =>tl.fold(f(init, hd))(f)

```



```scala
val h=Stream.eval(IO.now(9)).map(_+90)

h: Stream[IO, Int] = FlatMap(Uncons(FlatMap(Eval(Return(9)),effectful.EffectfulPulls$Stream$$$Lambda$17126/0x00000006035c20b8@3bc53565)),effectful.EffectfulPulls$Pull$$Lambda$17128/0x00000006035c2488@6b44b7f1)

h.run.unsafeRunSync(pool)

  def fold[F2[x] >: F[x], R2 >: R, A](init: A)(f: (A, O) => A)(
      using F: Monad[F2]
    ): F2[(R2, A)] = 
      //flatMap on F[_]
      step.flatMap:
        case Left(r) => F.unit((r, init))
        case Right((hd, tl)) =>tl.fold(f(init, hd))(f)
def run(using Monad[F]): F[Unit] =
  fold(())((_, _) => ()).map(_(1))
 def eval[F[_], O](fo: F[O]): Stream[F, O] =
      Pull.Eval(fo).flatMap(Pull.Output(_))

  def uncons: Pull[F, Nothing, Either[R, (O, Pull[F, O, R])]] =
      Uncons(this)

  def mapOutput[O2](f: O => O2): Pull[F, O2, R] =
      uncons.flatMap:
        case Left(r) => Result(r)
        case Right((hd, tl)) => Output(f(hd)) >> tl.mapOutput(f)
```
```scala
step.flatMap:
        case Left(r) => F.unit((r, init))
        case Right((hd, tl)) =>tl.fold(f(init, hd))(f)
 ``` 
 - The above defers until we have a result from `step`      
- It first hits the `FlatMap` node in `step` and then the `other` case. stepping on the `other` case hits `Uncons` we have kept the right hand side of the `FlatMap` node `f`
 The below is deferred until we get a result from the `Uncons`
```scala
    case other => other.step.flatMap:
      case Left(r) => f(r).step
      case Right((hd, tl)) => F.unit(Right((hd, tl.flatMap(f))))
```
- We now `step` on the pull inside `Uncons`. This will hit the `FlatMap` node and then `other` case. Stepping on the `other` case would run the effect and map the result to `Left`. 
- `flatMapping` on this would match `Left(r)`. Applying the inner `f` and stepping on it  `f(r).step`. It would yield `Output(9)`  and stepping on it would produce `Right((9,Result(())))` and now we map the result to as `instanceOf` `R2` as in `source.step.map(s =>Left(s.asInstanceOf[R2]))`

- back to the below
```scala
    case other => other.step.flatMap:
      case Left(r) => f(r).step
      case Right((hd, tl)) => F.unit(Right((hd, tl.flatMap(f))))
```
We now hit `Left` and apply the outer `f`

```scala
def mapOutput[O2](f: O => O2): Pull[F, O2, R] =
      uncons.flatMap:
        case Left(r) => Result(r)
        case Right((hd, tl)) => Output(f(hd)) >> tl.mapOutput(f)
```
 the above leads to two `Uncons` node

 based on the `Right(hd,tl)` case, we produce `FlatMap(Output(hd),(_)=>tl.mapOutput(f))`

```scala
// Uncons FlatMap(Eval(Return(9)),effectful.EffectfulPulls$Stream$$$Lambda$17514/0x00000006036431f0@179fec79)
// other result 9 and f is effectful.EffectfulPulls$Stream$$$Lambda$17514/0x00000006036431f0@179fec79 and it is Output(9)
// Output 9
// hello inside uncons Right((9,Result(()))) and Right((9,Result(())))
// other result Right((9,Result(()))) and f is effectful.EffectfulPulls$Pull$$Lambda$17516/0x00000006036435c0@4053c19e and it is FlatMap(Output(99),effectful.EffectfulPulls$Pull$$Lambda$17503/0x000000060363db10@73dae1f1)
// Output 99
// Output value 99 and and tail Result(())
// Monadic flatMap value 99 and tail FlatMap(Result(()),effectful.EffectfulPulls$Pull$$Lambda$17503/0x000000060363db10@2f5d5fed)
// Final Result ()
// other result () and f is effectful.EffectfulPulls$Pull$$Lambda$17503/0x000000060363db10@2f5d5fed and it is FlatMap(Uncons(Result(())),effectful.EffectfulPulls$Pull$$Lambda$17516/0x00000006036435c0@5edb3466)
// Uncons Result(())
// Final Result ()
// hello inside uncons Left(()) and Left(())
// other result Left(()) and f is effectful.EffectfulPulls$Pull$$Lambda$17516/0x00000006036435c0@37ccf853 and it is Result(())
// Final Result ()
// final result in fold is () and value is ()



```


`dropWhile` drops the output elements so the output type can be Nothing, indicating the pull is not capable of output

a `takeWhile` emits elements, and then returns another Pull, which in turn will/might emit more elements 
a `dropWhile` does not emit any elements,  and then returns another Pull, which in turn will/might emit more elements

`Pull.fromList(...).dropWhile(...).toList `

and that's what's happening here
the `.toList` only deals with the output channel of the `Pull`
but in `dropWhile` the output channel is empty, and the result type contains another `Pull` which in turn emits, which gets discarded resulting in the empty list


```scala

trait Semigroup[A]:
  def combine(x:A,y:A):A
  extension(x:A)
    inline def |+|(y:A)=combine(x,y)


```


```scala
//Pull
def unfold[O, R](init: R)(f: R => Either[R, (O, R)]): Pull[Nothing1, O, R] =f(init) match
      case Left(r) => Result(r)
      case Right((o, r2)) => Output(o) >> unfold(r2)(f)
//Stream
  def unfold[O, R](init: R)(f: R => Option[(O, R)]): Stream[Nothing1, O] =
      Pull.unfold(init)(r => f(r).toRight(r)).void
Flatmap(Output(1),(_)=>unfold(r2)(f))
//void for Pull is map(_=>()) and map is def map[B](f: A => B): F[B] = flatMap(a => unit(f(a))) which is FlatMap(Flatmap(Output(1),(_)=>unfold(r2)(f)),(_)=>Result(()))


Stream.unfold(0)(x=>Some(x,x+1))
//res8: Stream[Nothing1, Int] = FlatMap(FlatMap(Output(0),effectful.EffectfulPulls$Pull$$Lambda$20471/0x000000060386fb10@13eb73b4),iomonad.Monad$$Lambda$20479/0x00000006038747a0@210bc55)
```

```sh
 Scala provides an alternative way of writing anonymous functions: A pattern matching anonymous function is an anonymous function that is defined as a block consisting of a sequence of cases, surrounded as usual by curly braces, but without a match keyword before the block. 

 ```

 ## pattern matching anonymous functions.
 ` BlockExpr ::= ‘{’ CaseClauses ‘}’`
 An anonymous function can be defined by a sequence of cases

 ```{ case 𝑝1=> 𝑏1…case 𝑝𝑛=> 𝑏𝑛 }```

 which appear as an expression without a prior match. The expected type of such an expression must in part be defined. It must be either scala.`Function𝑘[𝑆1,…,𝑆𝑘, 𝑅]` for some `𝑘>0`, or `scala.PartialFunction[𝑆1, 𝑅]`, where the argument type(s) `𝑆1,…,𝑆𝑘` must be fully determined, but the result type `𝑅` may be undetermined.


 #### Case sequences as partial functions

A sequence of cases (i.e., alternatives) in curly braces can be used anywhere a function literal can be used. Essentially, a case sequence is a function literal, only more general. Instead of having a single entry point and list of parameters, a case sequence has multiple entry points, each with their own list of parameters. Each case is an entry point to the function, and the parameters are specified with the pattern. The body of each entry point is the right-hand side of the case.


Here is a simple example:
```scala
  val withDefault: Option[Int] => Int = {
    case Some(x) => x
    case None => 0
  }
```


```scala

    def take(n: Int): Pull[F, O, Option[R]] =
      if n <= 0 then Result(None)
      else uncons.flatMap:
        case Left(r) => Result(Some(r))
        case Right((hd, tl)) => Output(hd) >> tl.take(n - 1)

Stream.unfold(0)(x=>Some(x,x+1)).take(10)
FlatMap(FlatMap(Uncons(FlatMap(Flatmap(Output(0),(_)=>unfold(r2)(f)),(_)=>Result(()))),(_)=>Output(value)),(_)=>tl.take(n - 1))
```

res9: Stream[Nothing, Int] = FlatMap(FlatMap(Uncons(FlatMap(FlatMap(Output(0),effectful.EffectfulPulls$Pull$$Lambda$20471/0x000000060386fb10@60654151),iomonad.Monad$$Lambda$20479/0x00000006038747a0@1cd0f070)),effectful.EffectfulPulls$Pull$$Lambda$20508/0x000000060387f2b0@64b57308),iomonad.Monad$$Lambda$20479/0x00000006038747a0@6bed158)


```scala

    def fold[F2[x] >: F[x], R2 >: R, A](init: A)(f: (A, O) => A)(
      using F: Monad[F2]
    ): F2[(R2, A)] = 
      //flatMap on F[_]
      step.flatMap:
        case Left(r) => F.unit((r, init))
        case Right((hd, tl)) => tl.fold(f(init, hd))(f)
```
we initiall step in `fold` while deferring  to wait for the value of `step`

- We hit a FlatMap node, and then a FlatMap case and do a right association
- We now `step` on it and hit `Uncons` node and then step on the pull inside `Uncons` which is `FlatMap(Flatmap(Output(0),(_)=>unfold(r2)(f)),(_)=>Result(())))`

- stepping on this will hit FlatMap node and then FlatMap case and we right associate and then `step`
- It will be `Output(0)` and will return `Right(0,Result(()))`
- and flatMapping will hit the `Right((hd, tl))` case
