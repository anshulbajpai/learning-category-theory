package categorytheory.demo

import categorytheory.core.implicits._
import categorytheory.core.ops._
import categorytheory.datatypes.{Coproduct, Free, Id}
import categorytheory.demo.support.{LoggingLanguage, OrdersLanguage}

object FreeMonadDemo extends App with OrdersLanguage with LoggingLanguage {

  import categorytheory.datatypes.Free.FreeOps

  val smartTrade: Free[Order, Response] = for {
    _ <- buy("APPLE", 50)
    _ <- buy("GOOGLE", 100)
    rsp <- sell("APPLE", 20)
  } yield rsp

  import Id.id

  println(smartTrade.foldMap(orderPrinter))
  println(smartTrade.foldMap(betterOrderPrinter))

  val smartTradeWithList: Free[Order, Response] = for {
    stocks <- listStocks()
    _ <- stocks.traverse(stock => buy(stock, 100))
    rsp <- sell("GOOGLE", 100)
  } yield rsp


  println(smartTradeWithList.foldMap(orderPrinter))

  type TradeApp[A] = Coproduct[Order, Log, A]

  def smartTradeWithLogs(implicit O: OrderI[TradeApp], L: LogI[TradeApp]): Free[TradeApp, Response] = {
    import L._
    import O._

    for {
      _ <- infoI("Going to buy apple stocks")
      _ <- buyI("APPLE", 50)
      _ <- infoI("Going to buy google stocks")
      _ <- buyI("GOOGLE", 100)
      rsp <- sellI("APPLE", 20)
      _ <- errorI("Why error?")
    } yield rsp
  }

  println(smartTradeWithLogs.foldMap(orderPrinter or logPrinter))

}
