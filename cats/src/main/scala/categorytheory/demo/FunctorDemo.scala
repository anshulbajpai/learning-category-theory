package categorytheory.demo

import categorytheory.core.implicits._
import categorytheory.core.ops._
import categorytheory.datatypes.Maybe.just
import categorytheory.datatypes.Nothing
import categorytheory.demo.support.Functions

object FunctorDemo extends Functions with App {

  println(just("Hello") map toLength)

  val quarter = half map half

  println(quarter(16))

  println(just(4.0) map sqrt map (_ map half))
  println(just(4.0) map sqrt map half.lift)
  println(Nothing map sqrt)

}

