package categorytheory.demo

import categorytheory.core.ops._
import categorytheory.core.implicits._
import categorytheory.datatypes.Maybe.just
import categorytheory.demo.support.Functions

object FunctorDemo extends Functions with App {

  println(just("Hello") map toLength)

  val quarter = half map half

  println(quarter(16))
}

