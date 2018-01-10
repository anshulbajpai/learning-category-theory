package categorytheory.datatypes

import categorytheory.core.{Functor, ~}

case class Coproduct[F[_], G[_], A](run: Either[F[A], G[A]]) {
  def fold[H[_]](f: F ~ H, g: G ~ H): H[A] = run.fold(f.apply, g.apply)
}

object Coproduct {

  import categorytheory.core.ops._

  implicit def coproduct[F[_] : Functor, G[_] : Functor] = new Functor[({type λ[α] = Coproduct[F, G, α]})#λ] {
    override def map[A, B](c: Coproduct[F, G, A], f: A => B): Coproduct[F, G, B] =
      Coproduct(c.run match {
        case Left(fa) => Left(fa.map(f))
        case Right(ga) => Right(ga.map(f))
      })
  }
}