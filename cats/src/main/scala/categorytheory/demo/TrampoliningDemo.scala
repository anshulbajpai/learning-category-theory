package categorytheory.demo

object TrampoliningDemo extends App {

  def even(n: Int): Boolean = if (n == 0) true else odd(n - 1)

  def odd(n: Int): Boolean = if (n == 0) false else even(n - 1)

  println(even(500))
  println(even(5001))
  println(even(5010))

  //  println(even(500001)) // Stack over flow

  sealed trait Bounce[A]
  case class Done[A](result: A) extends Bounce[A]
  case class Call[A](thunk: () => Bounce[A]) extends Bounce[A]

  object Bounce {
    def trampoline[A](bounce: Bounce[A]): A = bounce match {
      case Done(a) => a
      case Call(thunk) => trampoline(thunk())
    }
  }

  def evenT(n: Int): Bounce[Boolean] = if (n == 0) Done(true) else Call(() => oddT(n - 1))

  def oddT(n: Int): Bounce[Boolean] = if (n == 0) Done(false) else Call(() => evenT(n - 1))

  import Bounce._

  println(trampoline(evenT(500)))
  println(trampoline(evenT(5001)))
  println(trampoline(evenT(5010)))
  println(trampoline(evenT(50101)))


}
