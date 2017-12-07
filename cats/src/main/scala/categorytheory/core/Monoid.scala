package categorytheory.core

trait Monoid[A] {
  def combine(x: A, y: A): A
  def pure: A
}

object Monoid {
  def apply[A](implicit instance: Monoid[A]): Monoid[A] = instance

  trait Ops[A] {
    def typeClassInstance: Monoid[A]
    def self: A
    def combine(other: A): A = typeClassInstance.combine(self, other)
  }

  object ops {
    implicit def toMonoidOps[A](target: A)(implicit instance: Monoid[A]): Ops[A] = new Ops[A] {
      override val typeClassInstance = instance
      override val self = target
    }
  }
}

trait MonoidImplicits {
  implicit def listMonoid[A] = new Monoid[List[A]] {
    override def combine(x: List[A], y: List[A]) = x ++ y
    override val pure = List.empty[A]
  }
}

object MonoidImplicits extends MonoidImplicits


