package categorytheory.core

trait Applicative[F[_]] extends Functor[F] {
  def ap[A, B](fa: F[A], Ff: F[A => B]): F[B]
  def pure[A](a: A): F[A]
  override def map[A, B](fa: F[A], f: A => B): F[B] = ap(fa, pure(f))
}

object Applicative {
  def apply[F[_]](implicit instance: Applicative[F]): Applicative[F] = instance

  private [core] trait Ops[F[_], A] {
    def typeClassInstance: Applicative[F]
    def self: F[A]
    def ap[B](Ff: F[A => B]): F[B] = typeClassInstance.ap(self, Ff)
  }

  private [core] trait ToApplicativeOps {
    implicit def toApplicativeOps[F[_], A](target: F[A])(implicit instance: Applicative[F]): Ops[F, A] = new Ops[F, A] {
      override val typeClassInstance = instance
      override val self = target
    }
  }
}

private [core] trait ApplicativeImplicits {
  implicit def function1Applicative[In] = new Applicative[({type λ[α] = In => α})#λ] {
    override def ap[A, B](fa: In => A, Ff: In => A => B) = { in =>
      Ff(in)(fa(in))
    }
    override def pure[A](a: A): In => A = _ => a
  }
}