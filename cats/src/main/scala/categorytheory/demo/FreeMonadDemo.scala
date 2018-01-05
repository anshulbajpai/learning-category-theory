package categorytheory.demo

import categorytheory.core.ops._
import categorytheory.datatypes.{Free, Id}
import categorytheory.demo.support.OrdersLanguage

object FreeMonadDemo extends App with OrdersLanguage {

  import categorytheory.datatypes.Free.FreeOps

  val smartTrade: Free[Order, Response] = for {
    _ <- buy("APPLE", 50)
    _ <- buy("GOOGLE", 100)
    rsp <- sell("APPLE", 20)
  } yield rsp

  import Id.id

  smartTrade.foldMap(orderPrinter)

}
