package categorytheory.demo

import categorytheory.core.implicits._
import categorytheory.core.ops._
import categorytheory.datatypes.Maybe.just
import categorytheory.demo.support.Functions

object MonadDemo extends Functions with App {
  println(just(100.0) flatMap log flatMap sqrt)
  println((log <==< sqrt)(100.0))
  println((log <==< sqrt)(-100.0))
}
