package categorytheory.datatypes

import categorytheory.core.Monad

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

  implicit val maybe = new Monad[Maybe]  {

    override def flatMap[A, B](fa: Maybe[A], f: A => Maybe[B]) = fa match {
      case Just(a) => f(a)
      case Nothing => Nothing
    }

    override def pure[A](a: A) = Just(a)
  }

  def just[A](a: A): Maybe[A] = Just(a)
}

