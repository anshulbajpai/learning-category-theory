package categorytheory.datatypes

import categorytheory.core.{Id, Monad}

object Id {
  implicit val id: Monad[Id] = new Monad[Id] {
    override def flatMap[A, B](fa: Id[A], f: A => Id[B]): Id[B] = f(fa)

    override def pure[A](a: A): Id[A] = a
  }
}