package categorytheory.demo.support

import categorytheory.core.implicits._
import categorytheory.datatypes.{Just, Maybe, Nothing, Writer}

trait Functions {

  val toLength: String => Int = _.length

  val half: Double => Double = _ / 2

  val sum: Double => Double => Double = { x => y => x + y }

  val sqrt : Double => Maybe[Double] = x => if(x >= 0) Just(Math.sqrt(x)) else Nothing

  val log: Double => Maybe[Double] = x => if(x >= 0) Just(Math.log(x)) else Nothing

  def moduloNumber(x: Int) : Writer[List[String], Int] = Writer((List(s"moduling $x"), Math.abs(x)))
}
