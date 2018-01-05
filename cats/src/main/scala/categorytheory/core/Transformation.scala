package categorytheory.core

trait Transformation[F[_], G[_]] {
  def apply[A](fa: F[A]): G[A]
}
