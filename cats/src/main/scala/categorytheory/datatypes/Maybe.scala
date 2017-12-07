package categorytheory.datatypes

import categorytheory.core.{Applicative, Functor, Monad}

sealed trait Maybe[+A] {
  def getOrElse[B >: A](defaultValue: => B): B
}
case class Just[A](value: A) extends Maybe[A] {
  override def getOrElse[B >: A](defaultValue: => B) = value
}
case object Nothing extends Maybe[Nothing] {
  override def getOrElse[B >: Nothing](defaultValue: => B) = defaultValue
}

object Maybe {

  implicit val maybe = new Functor[Maybe] with Applicative[Maybe] with Monad[Maybe]  {

    override def map[A, B](fa: Maybe[A], f: A => B) = fa match {
      case Just(a) => Just(f(a))
      case Nothing => Nothing
    }

    override def ap[A, B](fa: Maybe[A], Ff: Maybe[A => B]) = (fa, Ff) match {
      case (Just(a), Just(f)) => Just(f(a))
      case _ => Nothing
    }

    override def flatMap[A, B](fa: Maybe[A], f: A => Maybe[B]) = fa match {
      case Just(a) => f(a)
      case Nothing => Nothing
    }

    override def pure[A](a: A) = Just(a)
  }

  def just[A](a: A): Maybe[A] = Just(a)
}

