package categorytheory.demo.support

trait StateMachine {

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

}
