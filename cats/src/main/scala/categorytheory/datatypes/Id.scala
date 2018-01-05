package categorytheory.datatypes

import categorytheory.core.{Functor, Id, Monad}

object Id {
  implicit val id: Functor[Id] with Monad[Id] = new Functor[Id] with Monad[Id] {
    override def map[A, B](fa: Id[A], f: A => B): Id[B] = f(fa)

    override def flatMap[A, B](fa: Id[A], f: A => Id[B]): Id[B] = f(fa)

    override def pure[A](a: A): Id[A] = a
  }
}