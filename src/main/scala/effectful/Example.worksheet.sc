import scala.util.Failure
import scala.util.Success
import scala.util.Try

import effectful.EffectfulPulls.Pull
import effectful.EffectfulPulls.Stream

List(1, 3).fold(0)((a, acc) => a + acc)

val f = (x: Int) => x

//We construct a composite function g that consists of 100,000 functions where each one calls the next.
val g = List.fill(100000)(f).foldLeft(f)(_ compose _)

//g(42)//StackOverflowError

//effectful.EffectfulPulls.Stream(1,2).run()
Pull.Output(3).flatMap(x => Pull.Output(5))

Pull.Output(3).flatMap(x => Pull.Output(5)).toStream.toList

Pull.Output(1) >> Pull.done
Pull.Output(1) >> Pull.Output(2) >> Pull.done

(Pull.Output(1) >> Pull.Output(2) >> Pull.done).toStream.toList

Pull.FlatMap(Pull.FlatMap(Pull.Output(1), _ => Pull.Output(2)), _ => Pull.done).toStream.toList

Pull.FlatMap(Pull.FlatMap(Pull.Output(1), _ => Pull.Output(2)), _ => Pull.done).take(2)
//res7: Pull[Nothing, Int, Option[Unit]] = FlatMap(Uncons(FlatMap(FlatMap(Output(1),repl.MdocSession$MdocApp$$Lambda$12733/0x000000c002fa2000@250ca8c0),repl.MdocSession$MdocApp$$Lambda$12734/0x000000c002fa23d0@1b010da5)),effectful.EffectfulPulls$Pull$$Lambda$12637/0x000000c002f97238@19e8cbf7)

Pull.FlatMap(Pull.FlatMap(Pull.Output(1), _ => Pull.Output(2)), _ => Pull.done)
Pull.Uncons(Pull.Output(1) >> Pull.Output(2) >> Pull.done).source.toStream.toList

(Pull.Output(1) >> Pull.Output(2) >> Pull.done).uncons
