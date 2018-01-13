package categorytheory.core

import categorytheory.core.ops._

private[core] trait Implicits {

  implicit class Function1Ops[A, B](target: (A) => B) {
    def lift[F[_] : Functor] = Functor[F].lift(target)
  }

  implicit class MonadComposer[A, B, C, M[_] : Monad](target: A => M[B]) {
    def <==<(f: B => M[C]): A => M[C] = target(_) flatMap f
  }

  implicit def eitherImplicit[L] = new Monad[({type λ[α] = Either[L, α]})#λ] {

    override def flatMap[A, B](fa: Either[L, A], f: A => Either[L, B]): Either[L, B] = fa match {
      case Right(r) => f(r)
      case Left(l) => Left(l)
    }

    override def pure[A](a: A): Either[L, A] = Right(a)
  }

  implicit val id: Monad[Id] = new Monad[Id] {
    override def flatMap[A, B](fa: Id[A], f: A => Id[B]): Id[B] = f(fa)

    override def pure[A](a: A): Id[A] = a
  }

}