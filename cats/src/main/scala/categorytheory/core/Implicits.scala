package categorytheory.core

import ops._

trait Implicits {

  implicit class Function1Ops[A,B](target: (A) => B){
    def lift[F[_]: Functor] = Functor[F].lift(target)
  }

  implicit class MonadComposer[A, B, C, M[_]: Monad](target: A => M[B]) {
    def <==<(f: B => M[C]): A => M[C] = target(_) flatMap f
  }
}