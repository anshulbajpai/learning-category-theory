package categorytheory.datatypes

import categorytheory.core.ops._
import categorytheory.core.{Inject, Monad, ~>}

sealed trait FreeMonad[F[_], A]
case class Pure[F[_], A](a: A) extends FreeMonad[F, A]
case class Suspend[F[_], A](fa: F[A]) extends FreeMonad[F, A]
case class FlatMap[F[_], A, X](freeX: FreeMonad[F, X], f: X => FreeMonad[F, A]) extends FreeMonad[F, A]

object FreeMonad {
  implicit def free[F[_]] = new Monad[({type λ[α] = FreeMonad[F, α]})#λ] {
    override def flatMap[A, B](freeA: FreeMonad[F, A], f: A => FreeMonad[F, B]): FreeMonad[F, B] = FlatMap(freeA, f)

    override def pure[A](a: A): FreeMonad[F, A] = Pure(a)
  }

  implicit class FreeOps[F[_], A](target: FreeMonad[F, A]) {

    def foldMap[G[_] : Monad](transformation: F ~> G): G[A] = target match {
      case Pure(a) => implicitly[Monad[G]].pure(a)
      case Suspend(fa) => transformation(fa)
      case FlatMap(Pure(x), xToFreeA) => xToFreeA(x).foldMap(transformation)
      case FlatMap(FlatMap(freeY, yToFreeX), xToFreeA) => freeY.flatMap(y => yToFreeX(y).flatMap(xToFreeA)).foldMap(transformation)
      case FlatMap(freeX, xToFreeA) =>
        implicitly[Monad[G]].flatMap(freeX.foldMap(transformation), (x: Any) => xToFreeA(x).foldMap(transformation))
    }
  }

  def liftF[F[_], A](fa: F[A]): FreeMonad[F, A] = Suspend(fa)

  def inject[F[_], G[_]] = new PartialInject[F, G]

  private[FreeMonad] class PartialInject[F[_], G[_]] {
    def apply[A](fa: F[A])(implicit I: Inject[F, G]): FreeMonad[G, A] = liftF(I.inj(fa))
  }
}