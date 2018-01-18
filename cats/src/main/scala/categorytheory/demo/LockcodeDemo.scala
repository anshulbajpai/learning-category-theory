package categorytheory.demo

import categorytheory.core.implicits._
import categorytheory.core.ops._
import categorytheory.core.{Id, ~>}
import categorytheory.datatypes.FreeMonad

object LockcodeDemo extends App {

  type Code = (Int, Int, Int)
  type AllowedDigits = Set[Int]

  sealed trait Hint[A]
  case class WellPlacedCodeHint(code: Code, correct: Int) extends Hint[Set[Code]]
  case class WrongPlacedCodeHint(code: Code, correct: Int) extends Hint[Set[Code]]
  case class AllWrongCodeHint(code: Code) extends Hint[Set[Code]]

  type FreeCombinations = FreeMonad[Hint, Set[Code]]

  def wellPlacedCodeHint(code: Code, correct: Int): FreeCombinations = FreeMonad.liftF(WellPlacedCodeHint(code, correct))
  def wrongPlacedCodeHint(code: Code, correct: Int): FreeCombinations = FreeMonad.liftF(WrongPlacedCodeHint(code, correct))
  def allWrongCodeHint(code: Code): FreeCombinations = FreeMonad.liftF(AllWrongCodeHint(code))

  def hintInterpeter: Hint ~> Id = new (Hint ~> Id) {

    private def generateCodes(hundrethValues: Set[Int], tenthValues: Set[Int], unitValues: Set[Int]): Set[Code] = for {
      hundreth <- hundrethValues
      tenth <- tenthValues
      unit <- unitValues
    } yield (hundreth, tenth, unit)

    val allDigits = (0 to 9).toSet

    override def apply[A](hint: Hint[A]): Id[A] = hint match {
      case WellPlacedCodeHint((hundreth, tenth, unit), 1) =>
        generateCodes(Set(hundreth), allDigits -- Set(hundreth, tenth), allDigits -- Set(hundreth, unit)) ++
        generateCodes(allDigits -- Set(hundreth, tenth), Set(tenth), allDigits -- Set(unit, tenth)) ++
        generateCodes(allDigits -- Set(hundreth, unit), allDigits -- Set(tenth, unit), Set(unit))

      case WellPlacedCodeHint((hundreth, tenth, unit), 2) =>
        val remainingDigits = allDigits -- Set(hundreth, tenth, unit)
        generateCodes(Set(hundreth), Set(tenth), remainingDigits) ++
        generateCodes(remainingDigits, Set(tenth), Set(unit)) ++
        generateCodes(Set(hundreth), remainingDigits, Set(unit))

      case WellPlacedCodeHint((hundreth, tenth, unit), 3) =>
        generateCodes(Set(hundreth), Set(tenth), Set(unit))

      case WrongPlacedCodeHint((hundreth, tenth, unit), 1) =>
        val remainingDigits = allDigits -- Set(hundreth, tenth, unit)
        generateCodes(remainingDigits, Set(hundreth), remainingDigits) ++
          generateCodes(remainingDigits, remainingDigits, Set(hundreth)) ++
        generateCodes(Set(tenth), remainingDigits, remainingDigits) ++
          generateCodes(remainingDigits, remainingDigits, Set(tenth)) ++
        generateCodes(Set(unit), remainingDigits, remainingDigits) ++
          generateCodes(remainingDigits, Set(unit), remainingDigits)

      case WrongPlacedCodeHint((hundreth, tenth, unit), 2) =>
        val remainingDigits = allDigits -- Set(hundreth, tenth, unit)
        generateCodes(remainingDigits, Set(hundreth), Set(tenth)) ++
          generateCodes(Set(tenth), Set(hundreth), remainingDigits) ++
        generateCodes(Set(tenth), remainingDigits, Set(hundreth)) ++
        generateCodes(Set(unit), Set(hundreth), remainingDigits) ++
          generateCodes(Set(unit), remainingDigits, Set(hundreth)) ++
          generateCodes(remainingDigits, Set(unit), Set(hundreth)) ++
        generateCodes(Set(tenth), Set(unit), remainingDigits) ++
          generateCodes(Set(unit), remainingDigits, Set(tenth)) ++
          generateCodes(remainingDigits, Set(unit), Set(tenth))

      case WrongPlacedCodeHint((hundreth, tenth, unit), 3) =>
        generateCodes(Set(unit), Set(hundreth), Set(tenth)) ++
          generateCodes(Set(tenth), Set(unit), Set(hundreth))

      case AllWrongCodeHint((hundreth, tenth, unit)) =>
        val remainingDigits = allDigits -- Set(hundreth, tenth, unit)
        generateCodes(remainingDigits, remainingDigits, remainingDigits)
    }
  }

  val program: FreeCombinations = for {
    hint1Codes <- wellPlacedCodeHint((6, 8, 2), correct = 1)
    hint2Codes <- wrongPlacedCodeHint((6, 1, 4), correct = 1).map(_.intersect(hint1Codes))
    hint3Codes <- wrongPlacedCodeHint((2, 0, 6), correct = 2).map(_.intersect(hint2Codes))
    hint4Codes <- allWrongCodeHint((7, 3, 8)).map(_.intersect(hint3Codes))
    hint5Codes <- wrongPlacedCodeHint((7, 8, 0), correct = 1).map(_.intersect(hint4Codes))
  } yield hint5Codes

  println(program.foldMap(hintInterpeter))

}
