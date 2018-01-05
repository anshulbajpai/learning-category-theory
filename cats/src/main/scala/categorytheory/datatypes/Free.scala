package categorytheory.datatypes

import categorytheory.core.{Functor, Monad, ~}
import categorytheory.core.ops._

sealed trait Free[F[_], A]
case class Pure[F[_], A](a: A) extends Free[F, A]
case class Suspend[F[_], A](fa: F[A]) extends Free[F, A]
case class FlatMap[F[_], A, X](freeX: Free[F, X], f: X => Free[F, A]) extends Free[F, A]

object Free {
  implicit def free[F[_]] = new Functor[({type λ[α] = Free[F, α]})#λ] with Monad[({type λ[α] = Free[F, α]})#λ] {
    override def map[A, B](freeA: Free[F, A], f: A => B): Free[F, B] = flatMap(freeA, (a: A) => pure(f(a)))

    override def flatMap[A, B](freeA: Free[F, A], f: A => Free[F, B]): Free[F, B] = FlatMap(freeA, f)

    override def pure[A](a: A): Free[F, A] = Pure(a)
  }

  implicit class FreeOps[F[_], A](target: Free[F,A]){

    private type MF[X[_]] = Monad[X] with Functor[X]

    def foldMap[G[_]: MF](transformation: F ~ G): G[A] = target match {
      case Pure(a) => implicitly[MF[G]].pure(a)
      case Suspend(fa) => transformation(fa)
      case FlatMap(Pure(x), xToFreeA) => xToFreeA(x).foldMap(transformation)
      case FlatMap(FlatMap(freeY, yToFreeX), xToFreeA) => freeY.flatMap(y => yToFreeX(y).flatMap(xToFreeA)).foldMap(transformation)
      case FlatMap(freeX, xToFreeA) =>
        def function[T]: T => G[A] = t => xToFreeA(t).foldMap(transformation)
        implicitly[MF[G]].flatMap(freeX.foldMap(transformation), function)
    }
  }

  def liftF[F[_], A](fa: F[A]): Free[F, A] = Suspend(fa)
}