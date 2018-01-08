package categorytheory.datatypes

import categorytheory.core.Monad

case class State[S, A](run: S => (S, A))

object State {
  implicit def stateMonad[S] = new Monad[({type λ[α] = State[S, α]})#λ] {
    override def flatMap[A, B](sa: State[S, A], f: A => State[S, B]): State[S, B] = State { s =>
      val (newState, a) = sa.run(s)
      f(a).run(newState)
    }

    override def pure[A](a: A) = State(s => (s, a))
  }

  def get[S]: State[S, S] = State[S, S](s => (s, s))

  def init[S]: State[S, S] = get[S]

  def gets[S, A](f: S => A): State[S, A] = State(s => (s, f(s)))

  def put[S](s: S): State[S, Unit] = State(_ => (s, ()))


  def modify[S](ss: S => S): State[S, Unit] = State { s =>
    (ss(s), ())
  }
}
