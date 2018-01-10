package categorytheory.core

import categorytheory.datatypes.Coproduct

trait Transformation[F[_], G[_]] {
  self =>
  def apply[A](fa: F[A]): G[A]

  def or[H[_]](h: Transformation[H, G]): Transformation[({type λ[α] = Coproduct[F, H, α]})#λ, G] =
    new Transformation[({type λ[α] = Coproduct[F, H, α]})#λ, G] {
      override def apply[A](fa: Coproduct[F, H, A]): G[A] = fa.fold(self, h)
    }

  def compose[E[_]](f: E ~ F): E ~ G = new (E ~ G) {
    override def apply[A](ea: E[A]): G[A] = self(f(ea))
  }
}
