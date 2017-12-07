package categorytheory.core

trait Monad[F[_]] {
  def flatMap[A, B](fa: F[A], f: A => F[B]): F[B]
  def pure[A](a: A): F[A]
}

object Monad {
  def apply[F[_]](implicit instance: Monad[F]): Monad[F] = instance

  private [core] trait Ops[F[_], A] {
    def typeClassInstance: Monad[F]
    def self: F[A]
    def flatMap[B](f: A => F[B]): F[B] = typeClassInstance.flatMap(self, f)
  }

  private [core] trait ToMonadOps {
    implicit def toMonadOps[F[_], A](target: F[A])(implicit instance: Monad[F]): Ops[F, A] = new Ops[F, A] {
      override val typeClassInstance = instance
      override val self = target
    }
  }
}