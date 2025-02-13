
object Main extends App {

  println("Hello, World!")

  import scala.collection.mutable.ListBuffer
  import scala.language.higherKinds
  import scala.language.implicitConversions

  /**
    * Traverse depends on the Applicative type class because we need to combine effects. An
    * Applicative provides methods to wrap values (pure) and to apply functions over effects (map2
    * or ap)
    */
  trait Traverse[F[_]] {

    // Traverse applies a function to each element in F[A], collecting results in an effect G[_]
    def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit applicative: Applicative[G]): G[F[B]]

    // Sequence converts F[G[A]] into G[F[A]]
    def sequence[G[_], A](fga: F[G[A]])(implicit applicative: Applicative[G]): G[F[A]] =
      traverse(fga)(identity)

  }

  trait Applicative[F[_]] {

    def pure[A](a: A): F[A]

    def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] =
      map2(ff, fa)((f, a) => f(a))

    // Combines two effects F[A] and F[B] into F[C] using a function (A, B) => C
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  }

  object ListTraverse extends Traverse[List] {

    def traverse[G[_], A, B](
      fa: List[A]
    )(f: A => G[B])(implicit applicative: Applicative[G]): G[List[B]] = {
      fa.foldRight(applicative.pure(List.empty[B])) { (a, acc) =>
        applicative.map2(f(a), acc)(_ :: _)
      }
    }

  }

  object OptionApplicative extends Applicative[Option] {

    def pure[A](a: A): Option[A] = Some(a)

    override def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] =
      map2(ff, fa)((f, a) => f(a))

    def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] =
      (fa, fb) match {
        case (Some(a), Some(b)) => Some(f(a, b))
        case _                  => None
      }

  }

  val result = ListTraverse
    .traverse(List(1, 2, 3))(n => if (n % 2 == 0) Some(n) else None)(OptionApplicative)

  println(result) // Output: None, since there's an odd number in the list.

  implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {

    def pure[A](a: A): Option[A] = Some(a)

    def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] =
      for {
        a <- fa
        b <- fb
      } yield f(a, b)

  }

  implicit val listTraverse: Traverse[List] = new Traverse[List] {

    def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      fa.foldRight(G.pure(List.empty[B])) { (a, acc) =>
        G.map2(f(a), acc)(_ :: _)
      }

  }

  //Some(List[Int]),Some(2)
  //for {
   //     2 <- Some(2)
   //     List[Int] <- Some(List[Int])
   //   } yield (2::list[Int])
  val listOfOptions: List[Option[Int]] = List(Some(1), Some(2), Some(3))
  val result2: Option[List[Int]]       = listTraverse.sequence(listOfOptions)

}
