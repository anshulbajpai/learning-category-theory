package categorytheory.core

trait Functor[F[_]] {
  def map[A, B](fa: F[A], f: A => B): F[B]
  def lift[A,B](f: A => B): F[A] => F[B] = map(_, f)
}

object Functor {
  def apply[F[_]](implicit instance: Functor[F]): Functor[F] = instance

  trait Ops[F[_], A] {
    def typeClassInstance: Functor[F]
    def self: F[A]
    def map[B](f: A => B): F[B] = typeClassInstance.map(self, f)
  }

  trait ToFunctorOps {
    implicit def toFunctorOps[F[_], A](target: F[A])(implicit instance: Functor[F]): Ops[F, A] = new Ops[F, A] {
      override val typeClassInstance = instance
      override val self = target
    }
  }
}

trait FunctorImplicits {
  implicit def function1Functor[In] = new Functor[({type λ[α] = In => α})#λ] {
    override def map[A, B](fa: In => A, f: A => B) = fa andThen f
  }
}
