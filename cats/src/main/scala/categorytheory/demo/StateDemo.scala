package categorytheory.demo

import categorytheory.datatypes.State
import categorytheory.demo.support.StateMachine
import categorytheory.core.ops._

object StateDemo extends StateMachine with App {

  import Machine._

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

}
