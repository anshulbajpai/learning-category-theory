package categorytheory.datatypes

import categorytheory.core.Monad

case class Reader[R, V](run: R => V)

object Reader {
  implicit def readMonad[R] = new Monad[({type λ[α] = Reader[R, α]})#λ] {
    override def flatMap[A, B](r: Reader[R, A], f: A => Reader[R, B]): Reader[R, B] = Reader(x => f(r.run(x)).run(x))

    override def pure[A](a: A) = Reader(_ => a)
  }

  def value[R, V](v: V): Reader[R,V] = Reader(_ => v)
}