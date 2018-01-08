package categorytheory.core

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](fa: F[A], f: A => F[B]): F[B]
  override def map[A, B](fa: F[A], f: A => B): F[B] = flatMap(fa, (a: A) => pure(f(a)))
  override def ap[A, B](fa: F[A], Ff: F[A => B]): F[B] = flatMap(fa, (a: A) => map(Ff, (f: A => B) => f(a)))
}

object Monad {
  def apply[F[_]](implicit instance: Monad[F]): Monad[F] = instance

  private[core] trait Ops[F[_], A] {
    def typeClassInstance: Monad[F]
    def self: F[A]
    def flatMap[B](f: A => F[B]): F[B] = typeClassInstance.flatMap(self, f)
  }

  private[core] trait ToMonadOps {
    implicit def toMonadOps[F[_], A](target: F[A])(implicit instance: Monad[F]): Ops[F, A] = new Ops[F, A] {
      override val typeClassInstance = instance
      override val self = target
    }
  }
}