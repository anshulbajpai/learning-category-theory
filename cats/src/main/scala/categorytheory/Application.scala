package categorytheory

import categorytheory.Application.Machine.{lockedMachine, unlockedMachine}
import categorytheory.core.{Functor, Monad}
import categorytheory.datatypes._

object Application extends App {

  import categorytheory.core.Applicative.ops._
  import Functor.ops._
  import categorytheory.core.FunctorImplicits._
  import Monad.ops._

  // Functor

  private def just[A](a: A): Maybe[A] = Just(a)

  val toLength: String => Int = _.length

  val half: Double => Double = _ / 2

  println(just("Hello") map toLength)

  val quarter = half map half

  println(quarter(16))


  // Applicative

  val sum: Double => Double => Double = { x => y => x + y }

  println(just(5.0) ap (just(3.0) map sum))
  println(just(5.0) ap (just(3.0) map sum map (_ map half)))
  println(just(5.0) ap sum.lift[Maybe].apply(just(3.0)))

  val sqrt : Double => Maybe[Double] = x => if(x >= 0) Just(Math.sqrt(x)) else Nothing

  println(just(4.0) map sqrt map (_ map half))
  println(just(4.0) map sqrt map half.lift)
  println(Nothing map sqrt)

  // Monad

  val log: Double => Maybe[Double] = x => if(x >= 0) Just(Math.log(x)) else Nothing

  println(just(100.0) flatMap log flatMap sqrt)
  println((log <==< sqrt)(100.0))
  println((log <==< sqrt)(-100.0))

  implicit class Function1Ops[A,B](target: (A) => B){
    def lift[F[_]: Functor] = Functor[F].lift(target)
  }

  implicit class MonadComposer[A, B, C, M[_]: Monad](target: A => M[B]) {
    def <==<(f: B => M[C]): A => M[C] = target(_) flatMap f
  }


  // Writer

  import categorytheory.core.MonoidImplicits._

  def moduloNumber(x: Int) : Writer[List[String], Int] = Writer((List(s"moduling $x"), Math.abs(x)))

  val result: Writer[List[String], Int] = for {
    x <- moduloNumber(5)
    _ <- Writer.tell(List("Blah blah blah"))
    y <- moduloNumber(-7)
    z <- Writer.value[List[String],Int](2)
  } yield x * y * z

  println(result.run)


  // Reader

  trait UserRepository {
    private val users = Map(
      "user1" -> User("user1", "user 1"),
      "user2" -> User("user2", "user 2"),
      "user3" -> User("user3", "user 3"),
      "user4" -> User("user4", "user 4")
    )
    private val friends = Map(
      users("user1") -> List(users("user2"), users("user3")),
      users("user2") -> List(users("user1")),
      users("user3") -> List(users("user1"))
    )

    def findUser(userId: String): Maybe[User] = users.get(userId).fold[Maybe[User]](Nothing)(Just(_))
    def findFriends(user: User): List[User] = friends.getOrElse(user, List.empty)
  }

  case class User(id: String, name: String)

  def findUser(userId: String): Reader[UserRepository, Maybe[User]] = Reader(_.findUser(userId))
  def findFriends(user: User): Reader[UserRepository, List[User]] = Reader(_.findFriends(user))

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

  sealed trait MachineState
  case object Locked extends MachineState
  case object Unlocked extends MachineState

  type Candies = Int
  type Coins = Int

  case class Machine(locked: MachineState, candies: Candies, coins: Coins)

  object Machine {
    def lockedMachine(candies: Candies, coins: Coins) = Machine(Locked, candies, coins)
    def unlockedMachine(candies: Candies, coins: Coins) = Machine(Unlocked, candies, coins)
  }

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
