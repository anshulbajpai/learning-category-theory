package categorytheory.datatypes

import categorytheory.core.{Applicative, Cartesian}

sealed trait FreeAp[F[_], A]
case class PureAp[F[_], A](a: A) extends FreeAp[F, A]
case class Ap[F[_], A, X](fx: F[X], freeF: FreeAp[F, X => A]) extends FreeAp[F, A]

object FreeAp {

  implicit def free[F[_]] = new Applicative[({type λ[α] = FreeAp[F, α]})#λ] with Cartesian[({type λ[α] = FreeAp[F, α]})#λ] {
    override def ap[A, B](freeA: FreeAp[F, A], Ff: FreeAp[F, A => B]): FreeAp[F, B] = Ff match {
      case PureAp(aToB) => map(freeA, aToB)
      case Ap(fx, freeXToAToB) => Ap(fx, ap(freeA, map(freeXToAToB, (f: Any => A => B) => (a: A) => (x: Any) => f(x)(a))))
    }

    override def map[A, B](fa: FreeAp[F, A], f: A => B): FreeAp[F, B] = fa match {
      case PureAp(a) => PureAp(f(a))
      case Ap(fx, freeXToA) => Ap(fx, map(freeXToA, (g: Any => A) => (x: Any) => f(g(x))))
    }

    override def pure[A](a: A): FreeAp[F, A] = PureAp(a)

    override def |@|[A, B](ca: FreeAp[F, A], cb: FreeAp[F, B]): FreeAp[F, (A, B)] = (ca, cb) match {
      case (PureAp(a), PureAp(b)) => pure((a, b))
      case (PureAp(a), Ap(fx, freeXToB)) => Ap(fx, map(freeXToB, (f: Any => B) => (x: Any) => (a, f(x))))
      case (Ap(fx, freeXToA), PureAp(b)) => Ap(fx, map(freeXToA, (f: Any => A) => (x: Any) => (f(x), b)))
      case (Ap(fx, freeXToA), Ap(fy, freeYToB)) =>
        Ap(fx, Ap(fy, ap(freeXToA, map(freeYToB, (f: Any => B) => (g: Any => A) => (y: Any) => (x: Any) => (g(x), f(y))))))
    }
  }

  def lift[F[_], A](fa: F[A]): FreeAp[F, A] = Ap(fa, PureAp((a: A) => a))
}
