package categorytheory.demo

import categorytheory.core.implicits._
import categorytheory.core.ops._
import categorytheory.datatypes.Maybe
import categorytheory.datatypes.Maybe.just
import categorytheory.demo.support.Functions

object ApplicativeDemo extends Functions with App {

  println(just(5.0) ap (just(3.0) map sum))
  println(just(5.0) ap (just(3.0) map sum map (_ map half)))
  println(just(5.0) ap sum.lift[Maybe].apply(just(3.0)))

}
