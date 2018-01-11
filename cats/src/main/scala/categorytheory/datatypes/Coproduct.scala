package categorytheory.datatypes

import categorytheory.core.~

case class Coproduct[F[_], G[_], A](run: Either[F[A], G[A]]) {
  def fold[H[_]](f: F ~ H, g: G ~ H): H[A] = run.fold(f.apply, g.apply)
}