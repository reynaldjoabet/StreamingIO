
```scala

def mystream(stream:Stream[Int]):Pull[Int,Unit]={
stream.toPull.uncons.flatMap{
  case Left(value) =>Pull.done 
  case Right((h,tl)) => Pull.Output(h)>>mystream(tl.toStream)
}
}

mystream(Stream(1,2,3,4,5,6,7,8))
```
`FlatMap(FlatMap(Result(()),f),g)` is the structure

Initial Call: 
`mystream(Stream(1,2,3,4,5,6,7,8))`

`uncons` on `Stream(1,2,3,4,5,6,7,8)` returns `Right((1, Stream(2,3,4,5,6,7,8)))`.
This produces `Pull.Output(1) >> mystream(Stream(2,3,4,5,6,7,8))`.

Next Step: 
`mystream(Stream(2,3,4,5,6,7,8))`

`uncons` on `Stream(2,3,4,5,6,7,8)` returns `Right((2, Stream(3,4,5,6,7,8)))`.
This produces `Pull.Output(2) >> mystream(Stream(3,4,5,6,7,8))`.

Each call follows the same pattern:
- `mystream(Stream(3,4,5,6,7,8))` → `Pull.Output(3) >> mystream(Stream(4,5,6,7,8))`
- `mystream(Stream(4,5,6,7,8))` → `Pull.Output(4) >> mystream(Stream(5,6,7,8))`
- `mystream(Stream(5,6,7,8))` → `Pull.Output(5) >> mystream(Stream(6,7,8))`
- `mystream(Stream(6,7,8))` → `Pull.Output(6) >> mystream(Stream(7,8))`
- `mystream(Stream(7,8))` → `Pull.Output(7) >> mystream(Stream(8))`
- `mystream(Stream(8))` → `Pull.Output(8) >> mystream(Stream())`

End of Stream:
 `mystream(Stream())`

 `uncons` on the empty stream returns `Left`.
 This produces `Pull.done`.

 ### Resulting Pull Structure

 The final `Pull` structure will look like this:
 ```scala
Pull.Output(1) >>
  Pull.Output(2) >>
    Pull.Output(3) >>
      Pull.Output(4) >>
        Pull.Output(5) >>
          Pull.Output(6) >>
            Pull.Output(7) >>
              Pull.Output(8) >>
                Pull.done

 ```

 ```scala
 Pull.Output(1).flatMap(_ =>
  Pull.Output(2).flatMap(_ =>
    Pull.Output(3).flatMap(_ =>
      Pull.Output(4).flatMap(_ =>
        Pull.Output(5).flatMap(_ =>
          Pull.Output(6).flatMap(_ =>
            Pull.Output(7).flatMap(_ =>
              Pull.Output(8) >>
                Pull.done
            )
          )
        )
      )
    )
  )
)
```

 ### Evaluation Process
When evaluated, this structure will:

- Begin at `Pull.Output(1)`, outputting `1`.
Continue to `Pull.Output(2)`, outputting `2`.
This continues sequentially through each `Pull.Output(n)`, outputting each number until `Pull.done` is reached.

 In functional streaming terms, this structure effectively pushes each integer in the stream through the Pull chain, stopping when the stream is exhausted. This lazy, demand-driven evaluation ensures that elements are processed one by one, only as needed

 - The initial `Pull.Output(1)` emits the first element and suspends further computation.
- The next `Pull.Output(2)` is only executed if downstream processing demands more data.
This process continues until `Pull.done` is reached, at which point no more elements are available.

 Processing each element only on demand avoids the need to hold all elements in memory.

 the method that drives the actual evaluation of the `Pull` is `step`. Here’s how it works in the context of evaluating the `Pull`:

 `step`: The `step` method performs the fundamental work of "stepping" through the structure of the `Pull`. It recursively deconstructs each `Pull` type (`Output`, `FlatMap`, and `Done`) in a lazy fashion. When encountering `Output`, it directly emits the stored value. When it encounters `FlatMap`, it chains actions by continuing to evaluate based on the function `f`. Reaching `Done` signals the end of evaluation.

 ### How step Drives Evaluation
`step` is what interprets each node in the `Pull` structure. For example:

If `step` encounters `Pull.Output(value)`, it will emit this value immediately and continue to the next part of the Pull.
With `Pull.FlatMap(p, f)`, `step` will recursively evaluate `p` and then apply `f` to its result, creating a new `Pull`.
Upon reaching `Pull.Done`, it completes the evaluation process.

`step is the primary method for evaluating a Pull by interpreting and executing its structure one piece at a time`


### How step Works with Pull.FlatMap


 ```scala

case class FlatMap[X, +O, +R](source: Pull[O, X], f: X => Pull[O, R]) extends Pull[O, R]

Pull.FlatMap(Pull.Output(1), _ => 
  Pull.FlatMap(Pull.Output(2), _ => 
    Pull.FlatMap(Pull.Output(3), _ => 
      Pull.FlatMap(Pull.Output(4), _ => 
        Pull.FlatMap(Pull.Output(5), _ => 
          Pull.FlatMap(Pull.Output(6), _ => 
            Pull.FlatMap(Pull.Output(7), _ => 
              Pull.FlatMap(Pull.Output(8), _ => 
                Pull.done
              )
            )
          )
        )
      )
    )
  )
)
```
Initial Evaluation:

When you call `step` on the outermost `Pull.FlatMap`, it checks the source `Pull`, which is `Pull.Output(1)`.
The `step` method of `Pull.FlatMap` first evaluates the step of the source.

Each `Pull.Output(n)` (where n is 1 through 8) will result in a `Right` value when `step` is called, indicating that an output has been produced.

The `step` method will return `Right((n, nextPull))`, where `nextPull` is the subsequent `Pull` to be processed.


### Purpose of Pull.done

```scala
    def step: Either[R, (O, Pull[O, R])] = this match
      case Result(r) => Left(r)
      case Output(o) => Right(o, Pull.done)
```

- Indicating Completion:
When `Pull.Output(o)` produces an output `o`, it signifies that this part of the `Pull` has completed its work for that step. In this scenario, after yielding the output, there are no further computations or outputs to provide, which is why it transitions to `Pull.done`.
`Pull.done` represents a terminal state where the pull process is complete, signaling that there are no more values to yield.
- Flow Control:
The design of the `Pull` type is such that it represents a computation that can yield zero or more values and then terminate. By using `Pull.done`, it clearly defines the end of a pull computation.
This aids in flow control and ensures that consumers of the `Pull` know when to stop expecting further values


When you run the entire `Pull` through a stream processor (like `outputs.compile.toList`), the outputs are collected in order:

The first state of the `Pull` from `mystream` results in

`Pull.Output(1) >> mystream(Stream(2, 3, 4, 5, 6, 7, 8))`

The `step` method then returns `Right(1, Pull.done)`, indicating that the value `1` is produced as output, and there are no further computations (hence `Pull.done`).


```scala
Pull
 ├── Output(1)
 └── >>
     └── Pull
         ├── Output(2)
         └── >>
             └── Pull.done


```





`Pull.Output(1).flatMap(x=>Pull.Output(2).flatMap(n=>Pull.Output(3).flatMap(x=>Pull.Output(4)).flatMap(x=>Pull.Output(5))))`

In this case, when `Pull.Output(1).flatMap(...)` is called, it creates a new stack frame. Inside that frame, when `Pull.Output(2).flatMap(...)` is called, another frame is added, and so on for each subsequent `flatMap`


### Left-Associative flatMap Chain
- `Structure`: `((a flatMap f) flatMap g) flatMap h`
`Evaluation`: Each `flatMap` is applied immediately to the result of the previous `flatMap`, causing the chain to build up from the left.

`Pull.Output(1).flatMap(f).flatMap(g).flatMap(h)`

`Behavior`: In a `left-associative chain`, each `flatMap` operation depends on the result of the previous one. This can cause the stack depth to grow as each `flatMap` waits for the previous operation to complete, consuming more memory and stack space for each step in the chain.

### Right-Associative flatMap Chain

`Structure`:`a flatMap (x => f(x) flatMap (y => g(y) flatMap h))`
`Evaluation`: Each `flatMap` expression is structured to defer application until the innermost operation is reached, which allows evaluating from the "right" side of the chain.

```scala
Pull.Output(1).flatMap(x => Pull.Output(2).flatMap(y => Pull.Output(3).flatMap(z => Pull.done)))
```

`Behavior`: `Right-associative` chaining allows for better stack management because the innermost operation is evaluated first. This often reduces the need for nested frames on the stack, as each subsequent `flatMap` can directly use the result from its inner operation without adding additional stack depth

```scala

Pull.Output(1).flatMap(_ =>
  Pull.Output(2).flatMap(_ =>
    Pull.Output(3).flatMap(_ =>
      Pull.Output(4).flatMap(_ =>
        Pull.done
      )
    )
  )
)



```
Each `flatMap` layer is waiting for the result of the next nested `flatMap` without immediately creating a stack frame

`Initial Call`: We start with `Pull.Output(1).flatMap(...)`. Here, `Pull.Output(1)` creates a `Pull` that, when executed, will produce the output 1

- Instead of immediately processing `Pull.Output(1).flatMap(...)`, we store it as a "continuation"—meaning we note that, after `Pull.Output(1)`, we have more to evaluate.

- The next level is `Pull.Output(2).flatMap(...)`, which will also wait until `Pull.Output(3).flatMap(...)` finishes, and so on

Eventually, we reach the innermost call `Pull.done`. This signals the end of the chain, meaning all outputs can be collected

Starting from here, each `flatMap` layer can now "unwrap" from the innermost result outwards


Now, we evaluate `Pull.Output(4)`, then `Pull.Output(3)`, then `Pull.Output(2)`, and finally `Pull.Output(1)`.

This `continuation` doesn’t actually execute right away. Instead, it notes that, once the `current` step `(Pull.Output(1))` is finished, there is a next step `(Pull.Output(2).flatMap(...))` that should happen.


```scala
    def step: Either[R, (O, Pull[O, R])] = this match
      case Result(r) => Left(r)
      case Output(o) => Right(o, Pull.done)
      case FlatMap(source, f) => 
        source match
          case FlatMap(s2, g) => s2.flatMap(x => g(x).flatMap(y => f(y))).step
          case other => other.step match
            case Left(r) => f(r).step
            case Right((hd, tl)) => Right((hd, tl.flatMap(f)))

```
`Pull.Output(6).flatMap(x=>Pull.Result(()))`

//FlatMap(Output(6),x=>Result(()))
using `step`, we hit `case FlatMap(source, f) => ` then 
 ```scala
 case other => other.step match
            case Left(r) => f(r).step
            case Right((hd, tl)) => Right((hd, tl.flatMap(f)))
```
`other.step` results in `case Output(o) => Right(o, Pull.done)`
so we have `case Right((hd, tl)) => Right((hd, tl.flatMap(f)))`


When we want to run the final stream transformation, we can use `fold`, which repeatedly steps the pull until termination.


```scala

def step: Either[R, (O, Pull[O, R])] = this match
      case Result(r) => Left(r)
      case Output(o) => Right(o, Pull.done)
      case FlatMap(source, f) => 
        source match
          case FlatMap(s2, g) => s2.flatMap(x => g(x).flatMap(y => f(y))).step
          case other => other.step match
            case Left(r) => f(r).step
            case Right((hd, tl)) => Right((hd, tl.flatMap(f)))

    @annotation.tailrec
    final def fold[A](init: A)(f: (A, O) => A): (R, A) = 
      step match
        case Left(r) => (r, init)
        case Right((hd, tl)) => tl.fold(f(init, hd))(f)


```


we’re relying on the laziness of
`flatMap` to defer the evaluation of the tail.

`unfold` starts with an initial seed value of type `R` and an iteration function, which is repeatedly invoked, producing either a final result of `R` or an output of `O` and a new seed
of `R`

evaluation currently only occurs when folding a pull (or using an operation, like `toList`, which is built on
`fold`)

delay this evaluation via a new combinator on `Pull`:
`def uncons: Pull[Nothing, Either[R, (O, Pull[O, R])]]`

With this definition, we can make partially eval-uated infinite streams

Monad associativity laws
`fa.flatMap(f).flatMap(g) == fa.flatMap(a => f(a).flatMap(g))`

In other words, if you first apply `f` to `fa`, producing `M[B]`, and then apply `g` to the result, you should get the same result as if you had applied `f` and then immediately applied `g` to the intermediate result within `f`.

 ```scala
(1 to 100000).foldLeft(Pull.done: Pull[Int, Unit]) { (acc, num) =>
  acc.flatMap(_ => Pull.suspend(Pull.Output(num)))
}


 ```
Using `Pull.suspend` defers each `Pull.Output` until the previous one completes, making it stack-safe for large sequences

Stack Frame Creation: A new stack frame is created for the function call, which contains information like local variables, parameters, and the return address (where to return control after the function completes).

`(m flatMap f) flatMap g=== m flatMap{x=>f(x)flatMap g}`

The associativity law for monads states that for any monadic values `m`, `f`, and `g`:
`m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))`


On the JVM, every method call adds an entry to the call stack of the Thread, like adding to the front of a List. When the method completes, the method at the head is thrown away. The maximum length of the call stack is determined by the `-Xss` flag when starting up java. Tail recursive methods are detected by the Scala compiler and do not add an entry. If we hit the limit, by calling too many
chained methods, we get a StackOverflowError.

Unfortunately, every nested call to our `IO’s` `.flatMap` adds another method call to the stack

The way to achieve stack safety is to convert method calls into references to an `ADT`, the `Free` monad:


When an `ADT` mirrors the arguments of related functions, it is called a `Church encoding`.

Associating flatMap calls to the right instead of left ensures that each recursive call can be processed efficiently in a tail-recursive way, reducing the risk of stack overflows.

Called `Trampoline` because every time we `.bind` on the stack, we bounce back to the heap.

"bounce back to the heap" describes how:

Each `flatMap` or `bind` step in a trampoline style returns a continuation to the interpreter, which typically resides in the heap.

representation of the continuation of the computation being stored as data on the heap, rather than executing it immediately in the call stack.
When using a trampoline, instead of directly calling the next function, each step yields control back to a central loop (or interpreter) that "continues" from where it left off. This continuation is represented as data on the heap, meaning each step in the sequence is stored as an object in memory, rather than occupying the stack.
This allows the interpreter to handle each continuation (next step) as a new task on the heap rather than as a stack frame, preventing stack overflow

In a trampoline-based approach:

Continuation means the next computation step, represented as data rather than as a function call.
In our case, each flatMap or bind operation creates a new Pull (such as FlatMap, Result, or Output), describing what should happen next.
This Pull object represents the next step of the computation and is effectively a recipe for what to do next rather than the actual call to be made immediately.

These continuation objects (like `Pull.FlatMap`, `Pull.Result`, etc.) are allocated on the heap because they are data structures representing deferred actions

When `flatMap` or `bind` is called, it creates a `FlatMap` instance, which is an object on the heap containing the source and the function (f or g) representing the remaining computation.

For example, with `s2.flatMap(x => g(x).flatMap(y => f(y)))`, instead of calling `g(x).flatMap(y => f(y))` directly, this is encoded as a `Pull.FlatMap` object that represents the structure of the call. The central interpreter will later look at `Pull.FlatMap` and decide how to proceed without adding frames to the stack.

When `step` rewrites `s2.flatMap(x => g(x).flatMap(y => f(y)))`, it’s effectively converting a nested chain into a flat sequence of steps:


 IO is a free data structure specialised for use as a general effect monad.

 The IO interpreter is called RTS, for runtime system

 A function like `run`(step) is sometimes called a `trampoline`, and the overall tech-
nique of returning control to a single loop to eliminate the stack is called `trampolining`.

 FlatMap constructors nested on the left like this: `FlatMap(FlatMap(y,g),f)`.

In order to continue running the program in that case, the next thing we naturally want to do is look at `y` to see if it is another `FlatMap` constructor, but the expression may be arbitrarily deep and we want to remain tail-recursive. We reassociate this to the right,
effectively turning `(y flatMap g) flatMap f` into `y flatMap (a => g(a) flatMap f)`.We’re just taking advantage of the monad associativity law!

`(y flatMap g) flatMap f` becomes `y flatMap (a => g(a) flatMap f)`(right-associative)
- We combine `g` and `f` into a single function `a => g(a).flatMap(f)`.
- Then we apply this function to the result of `y`, achieving the same computation but without the left-heavy nesting.

```scala

object Pull:
    val done: Pull[Nothing, Unit] = Result(())

    def fromList[O](os: List[O]): Pull[O, Unit] =
      os match
        case Nil => done
        case hd :: tl => Output(hd) >> fromList(tl)

 (Output(1)>>Result(()))>> Output(3)>>Result(())   
 //we treat the output of Stream(3) as a continuation of the output from Stream(1)   
Stream(1).flatMap(x=>Stream(3)).toList

// First FlatMap
// second FlatMap
// First FlatMap
// result ()
// ()
// Result(())
// First FlatMap
// First FlatMap
// result Right((1,FlatMap(Result(()),simple.SimplePulls$Pull$$Lambda$15732/0x0000000601e36d50@7555873c)))
// Right((1,FlatMap(Result(()),simple.SimplePulls$Pull$$Lambda$15732/0x0000000601e36d50@7555873c)))
// Result(Right((1,FlatMap(Result(()),simple.SimplePulls$Pull$$Lambda$15732/0x0000000601e36d50@7555873c))))
// First FlatMap
// second FlatMap
// First FlatMap
// First FlatMap
// result ()
// ()
// Result(())
// First FlatMap
// result ()
// ()
// Result(())
// First FlatMap
// second FlatMap
// First FlatMap
// result ()
// ()
// Result(())
// First FlatMap
// result ()
// ()
// Result(())
// result ()
// First FlatMap
// result Left(())
// Left(())
// Result(Left(()))
// result ()

```


```scala
def uncons: Pull[Nothing, Either[R, (O, Pull[O, R])]] =
      Pull.done >> Result(step)


 extension [O](self: Pull[O, Unit])
      def flatMapOutput[O2](f: O => Pull[O2, Unit]): Pull[O2, Unit] =
        self.uncons.flatMap:
          case Left(()) => Result(())
          case Right((hd, tl)) =>
            f(hd) >> tl.flatMapOutput(f)
```

```scala

Stream(1).flatMap(x=>Stream(3))
//fromList(List(1)) produces Output(1) >> Result(())
Result(())>>Result(step)
Pull.done >> Result(step)
```


`cs install bloop:2.0.2`
`cs install bloop:2.0.4`


