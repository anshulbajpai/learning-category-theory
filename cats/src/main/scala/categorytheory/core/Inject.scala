package categorytheory.core

import categorytheory.datatypes.Coproduct

abstract class Inject[F[_], G[_]] {
  def inj: ~>[F, G]

  final def apply[A](fa: F[A]): G[A] = inj(fa)
}

sealed abstract class InjectInstances {
  implicit def catsReflexiveInjectInstance[F[_]]: Inject[F, F] =
    new Inject[F, F] {
      val inj = new (F ~> F) {
        override def apply[A](fa: F[A]): F[A] = fa
      }
    }

  implicit def catsLeftInjectInstance[F[_], G[_]]: Inject[F, ({type λ[α] = Coproduct[F, G, α]})#λ] =
    new Inject[F, ({type λ[α] = Coproduct[F, G, α]})#λ] {
      val inj = new (F ~> ({type λ[α] = Coproduct[F, G, α]})#λ) {
        override def apply[A](fa: F[A]): Coproduct[F, G, A] = Coproduct(Left(fa))
      }
    }

  implicit def catsRightInjectKInstance[F[_], G[_], H[_]](implicit I: Inject[F, G]): Inject[F, ({type λ[α] = Coproduct[H, G, α]})#λ] =
    new Inject[F, ({type λ[α] = Coproduct[H, G, α]})#λ] {
      val inj =  new (G ~> ({type λ[α] = Coproduct[H, G, α]})#λ) {
        override def apply[A](ga: G[A]): Coproduct[H, G, A] = Coproduct(Right(ga))
      } compose I.inj
    }
}

object Inject extends InjectInstances {
  def apply[F[_], G[_]](implicit I: Inject[F, G]): Inject[F, G] = I
}