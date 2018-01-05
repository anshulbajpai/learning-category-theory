package categorytheory.datatypes

import categorytheory.core.{Applicative, Cartesian, Functor, Monoid}

import scala.util.{Failure, Success, Try}

sealed trait Validated[+I, +V]

case class Valid[V](valid: V) extends Validated[Nothing, V]

case class Invalid[I: Monoid](invalid: I) extends Validated[I, Nothing]

object Validated {
  implicit def validated[I: Monoid] = new Functor[({type λ[α] = Validated[I, α]})#λ] with Applicative[({type λ[α] = Validated[I, α]})#λ] with Cartesian[({type λ[α] = Validated[I, α]})#λ] {
    override def map[A, B](validated: Validated[I, A], f: A => B): Validated[I, B] = validated match {
      case Valid(valid) => Valid(f(valid))
      case Invalid(invalid) => Invalid(invalid)
    }

    override def ap[A, B](validated: Validated[I, A], validatedF: Validated[I, A => B]): Validated[I, B] = (validated, validatedF) match {
      case (Valid(validA), Valid(f)) => Valid(f(validA))
      case (Invalid(invalidA), Invalid(invalidB)) => Invalid(implicitly[Monoid[I]].combine(invalidB, invalidA))
      case (_, invalid@Invalid(_)) => invalid
      case (invalid@Invalid(_), _) => invalid
    }

    override def |@|[A, B](ca: Validated[I, A], cb: Validated[I, B]): Validated[I, (A, B)] = {
      val f: A => B => (A, B) = a => b => (a, b)
      ap(cb, map(ca, f))
    }
  }

  def condition[I: Monoid, V](condition: => Boolean, `false`: => I, `true`: => V): Validated[I, V] =
    if (condition) Valid(`true`) else Invalid(`false`)

  implicit class TryOps[T](`try`: Try[T]) {
    def toValidated[I: Monoid](exceptionConvertor: Throwable => I) = `try` match {
      case Success(t) => Valid(t)
      case Failure(exception) => Invalid(exceptionConvertor(exception))
    }
  }
}

