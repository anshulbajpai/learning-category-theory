package categorytheory.datatypes

import categorytheory.core.{Functor, Monad, Monoid}

case class Writer[L: Monoid, V](run: (L, V))

object Writer {
  import Monoid.ops._
  implicit def writeMonad[L: Monoid] = new Monad[({type λ[α] = Writer[L,α]})#λ] with Functor[({type λ[α] = Writer[L,α]})#λ] {
    override def flatMap[A, B](w: Writer[L, A], f: A => Writer[L, B]) = {
      val newW = f(w.run._2)
      Writer(w.run._1.combine(newW.run._1), newW.run._2)
    }

    override def map[A, B](w: Writer[L, A], f: A => B) = Writer(w.run._1, f(w.run._2))

    override def pure[A](a: A) = value(a)
  }

  def value[L: Monoid, V](v: V) = Writer(Monoid[L].pure, v)

  def tell[L: Monoid](l: L) = Writer(l, ())
}
