package categorytheory.datatypes

import categorytheory.core.ops._
import categorytheory.core.{Inject, Monad, ~}

sealed trait Free[F[_], A]
case class Pure[F[_], A](a: A) extends Free[F, A]
case class Suspend[F[_], A](fa: F[A]) extends Free[F, A]
case class FlatMap[F[_], A, X](freeX: Free[F, X], f: X => Free[F, A]) extends Free[F, A]

object Free {
  implicit def free[F[_]] = new Monad[({type λ[α] = Free[F, α]})#λ] {
    override def flatMap[A, B](freeA: Free[F, A], f: A => Free[F, B]): Free[F, B] = FlatMap(freeA, f)

    override def pure[A](a: A): Free[F, A] = Pure(a)
  }

  implicit class FreeOps[F[_], A](target: Free[F, A]) {

    def foldMap[G[_] : Monad](transformation: F ~ G): G[A] = target match {
      case Pure(a) => implicitly[Monad[G]].pure(a)
      case Suspend(fa) => transformation(fa)
      case FlatMap(Pure(x), xToFreeA) => xToFreeA(x).foldMap(transformation)
      case FlatMap(FlatMap(freeY, yToFreeX), xToFreeA) => freeY.flatMap(y => yToFreeX(y).flatMap(xToFreeA)).foldMap(transformation)
      case FlatMap(freeX, xToFreeA) =>
        implicitly[Monad[G]].flatMap(freeX.foldMap(transformation), (x: Any) => xToFreeA(x).foldMap(transformation))
    }
  }

  def liftF[F[_], A](fa: F[A]): Free[F, A] = Suspend(fa)

  def inject[F[_], G[_]] = new PartialInject[F, G]

  private[Free] class PartialInject[F[_], G[_]] {
    def apply[A](fa: F[A])(implicit I: Inject[F, G]): Free[G, A] = liftF(I.inj(fa))
  }
}