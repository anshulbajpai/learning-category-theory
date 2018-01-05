package categorytheory.core

trait Cartesian[C[_]] {
  def |@|[A, B](ca: C[A], cb: C[B]): C[(A, B)]
}

object Cartesian {
  def apply[C[_]](implicit instance: Cartesian[C]): Cartesian[C] = instance

  private [core] trait Ops[C[_], A] {
    def typeClassInstance: Cartesian[C]
    def self: C[A]
    def |@|[B](cb: C[B]): C[(A,B)] = typeClassInstance.|@|(self, cb)
  }

  private [core] trait ToCartesianOps {
    implicit def toCartesianOps[C[_], A](target: C[A])(implicit instance: Cartesian[C]): Ops[C, A] = new Ops[C, A] {
      override val typeClassInstance = instance
      override val self = target
    }
  }
}