package categorytheory.core

trait Traverse[T[_]] {
  def traverse[U[_]: Applicative, A, B](fu: T[A], f: A => U[B]): U[T[B]]
}

object Traverse {
  def apply[T[_]](implicit instance: Traverse[T]): Traverse[T] = instance

  private [core] trait Ops[T[_], A] {
    private type AF[X[_]] = Applicative[X] with Functor[X]
    def typeClassInstance: Traverse[T]
    def self: T[A]
    def traverse[U[_]: AF, B](f: A => U[B]): U[T[B]] = typeClassInstance.traverse(self, f)
  }

  private [core] trait ToTraverseOps {
    implicit def toTraverseOps[T[_], A](target: T[A])(implicit instance: Traverse[T]): Ops[T, A] = new Ops[T, A] {
      override val typeClassInstance = instance
      override val self = target
    }
  }
}

private [core] trait TraverseImplicits {

  import categorytheory.core.ops._

  implicit def listTraverse: Traverse[List] = new Traverse[List] {
    override def traverse[U[_] : Applicative, A, B](ls: List[A], f: A => U[B]): U[List[B]] =
      ls.foldLeft(implicitly[Applicative[U]].pure(List.empty[B])) { (acc, a) =>
      acc.ap(f(a).map((b: B) => (ls: List[B]) => b :: ls))
    }
  }
}
