package categorytheory.demo

import categorytheory.core.{Functor, Monad}
import categorytheory.datatypes._
import categorytheory.demo.Application.Machine.{lockedMachine, unlockedMachine}
import Functor.ops._
import Monad.ops._
import categorytheory.core.Applicative.ops._
import categorytheory.core.FunctorImplicits._
import Maybe.just

object Application extends App with Functions with Implicits with UserDomain with StateMachine {

  // Functor

  println(just("Hello") map toLength)

  val quarter = half map half

  println(quarter(16))

  // Applicative

  println(just(5.0) ap (just(3.0) map sum))
  println(just(5.0) ap (just(3.0) map sum map (_ map half)))
  println(just(5.0) ap sum.lift[Maybe].apply(just(3.0)))


  println(just(4.0) map sqrt map (_ map half))
  println(just(4.0) map sqrt map half.lift)
  println(Nothing map sqrt)

  // Monad

  println(just(100.0) flatMap log flatMap sqrt)
  println((log <==< sqrt)(100.0))
  println((log <==< sqrt)(-100.0))


  // Writer

  import categorytheory.core.MonoidImplicits._

  val result: Writer[List[String], Int] = for {
    x <- moduloNumber(5)
    _ <- Writer.tell(List("Blah blah blah"))
    y <- moduloNumber(-7)
    z <- Writer.value[List[String],Int](2)
  } yield x * y * z

  println(result.run)


  // Reader


  def friendsName(userId: String): Reader[UserRepository, List[String]] = for {
    user <- findUser(userId)
    friends <- user.map(findFriends).getOrElse(Reader.value(List.empty[User]))
  } yield friends.map(_.name)

  object UserRepository extends UserRepository
  println(friendsName("user1").run(UserRepository))
  println(friendsName("user2").run(UserRepository))
  println(friendsName("user3").run(UserRepository))
  println(friendsName("user4").run(UserRepository))
  println(friendsName("user5").run(UserRepository))

  // State

  val Coin = State[Machine, Unit] {
    case Machine(Locked, candies, coins) if candies > 0 => (unlockedMachine(candies, coins + 1), ())
    case machine => (machine, ())
  }

  val Turn = State[Machine, Unit] {
    case Machine(Unlocked, candies, coins) => (lockedMachine(candies - 1, coins), ())
    case machine => (machine, ())
  }

  val defaultMachine = lockedMachine(candies = 1, coins = 0)

  // style 1

  val candyMachineProgram: State[Machine, Unit] = for {
    _ <- Turn
    _ <- Coin
    _ <- Coin
    _ <- Turn
    _ <- Turn
  } yield ()


  val finalMachine: Machine = candyMachineProgram.run(defaultMachine)._1
  println(s"candies in machine = ${finalMachine.candies}")
  println(s"coins in machine = ${finalMachine.coins}")

  // style 2

  private val list = List(Turn, Coin, Coin, Turn, Turn)

  val machineFinal = runMachine(defaultMachine, list)
  println(s"candies in machine = ${machineFinal.candies}")
  println(s"coins in machine = ${machineFinal.coins}")

  def runMachine(initialMachine: Machine, steps: List[State[Machine, Unit]]): Machine = {
    val program = steps.reduce [State[Machine, Unit]]{
      case (first, second) => first.flatMap(_ => second)
    }
    program.run(initialMachine)._1
  }

  // Monad transformer

  import MaybeT.ops._

  def friendsNameSimpler(userId: String) = for {
    user <- transform <~ findUser(userId)
    friends <- transform <~ findFriends(user)
  } yield friends.map(_.name)

  println(friendsNameSimpler("user1").run.run(UserRepository))
  println(friendsNameSimpler("user2").run.run(UserRepository))
  println(friendsNameSimpler("user3").run.run(UserRepository))
  println(friendsNameSimpler("user4").run.run(UserRepository))
  println(friendsNameSimpler("user5").run.run(UserRepository))
}
