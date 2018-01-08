package categorytheory.datatypes

import categorytheory.core.{Functor, Monad}

case class MaybeT[M[_], A](value: M[Maybe[A]])

object MaybeT {

  import categorytheory.core.ops._

  implicit def maybeT[M[_] : Monad] = new Monad[({type λ[α] = MaybeT[M, α]})#λ] {
    override def flatMap[A, B](fa: MaybeT[M, A], f: A => MaybeT[M, B]): MaybeT[M, B] = MaybeT(fa.value.flatMap {
      case Just(a) => f(a).value
      case Nothing => implicitly[Monad[M]].pure(Nothing)
    })

    override def pure[A](a: A) = MaybeT(implicitly[Monad[M]].pure(Just(a)))
  }

  object transform {
    def <~[M[_], A](target: M[Maybe[A]]): MaybeT[M, A] = MaybeT(target)

    def <~[F[_] : Functor, A](target: F[A]): MaybeT[F, A] = MaybeT(target.map(a => Just(a)))
  }

}