package categorytheory.demo

import categorytheory.core.ops._
import categorytheory.core.implicits._
import categorytheory.datatypes.Maybe.just
import categorytheory.datatypes.{Maybe, Nothing}
import categorytheory.demo.support.Functions

object ApplicativeDemo extends Functions with App {

  println(just(5.0) ap (just(3.0) map sum))
  println(just(5.0) ap (just(3.0) map sum map (_ map half)))
  println(just(5.0) ap sum.lift[Maybe].apply(just(3.0)))

  println(just(4.0) map sqrt map (_ map half))
  println(just(4.0) map sqrt map half.lift)
  println(Nothing map sqrt)
}
