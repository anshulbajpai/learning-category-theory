package categorytheory.datatypes

import categorytheory.core.{Functor, Monad}

case class MaybeT[M[_], A](value: M[Maybe[A]])

object MaybeT {

  import categorytheory.core.ops._

  private type MF[A[_]] = Monad[A] with Functor[A]

  implicit def maybeT[M[_] : MF] = new Monad[({type λ[α] = MaybeT[M, α]})#λ] with Functor[({type λ[α] = MaybeT[M, α]})#λ] {
    override def flatMap[A, B](fa: MaybeT[M, A], f: A => MaybeT[M, B]): MaybeT[M, B] = MaybeT(fa.value.flatMap {
      case Just(a) => f(a).value
      case Nothing => implicitly[Monad[M]].pure(Nothing)
    })

    override def pure[A](a: A) = MaybeT(implicitly[Monad[M]].pure(Just(a)))

    override def map[A, B](fa: MaybeT[M, A], f: A => B): MaybeT[M, B] = MaybeT(fa.value.map(_.map(f)))
  }

  def pure[F[_] : Functor, A](ma: F[A]): MaybeT[F, A] = MaybeT(ma.map(a => Just(a)))

  object transform {
    def <~[M[_], A](target: M[Maybe[A]]): MaybeT[M, A] = MaybeT(target)

    def <~[F[_] : Functor, A](target: F[A]): MaybeT[F, A] = MaybeT(target.map(a => Just(a)))
  }

}