package categorytheory.demo

import categorytheory.core.implicits._
import categorytheory.core.ops._
import categorytheory.datatypes.{Coproduct, FreeMonad}
import categorytheory.demo.support.{AuditLanguage, LoggingLanguage, MessagingLanguage, OrdersLanguage}

object FreeMonadDemo extends App with OrdersLanguage with LoggingLanguage with AuditLanguage with MessagingLanguage {

  import categorytheory.datatypes.FreeMonad.FreeOps

  val smartTrade: FreeMonad[Order, Response] = for {
    _ <- buy("APPLE", 50)
    _ <- buy("GOOGLE", 100)
    rsp <- sell("APPLE", 20)
  } yield rsp

  println(smartTrade.foldMap(orderPrinter))
  println(smartTrade.foldMap(betterOrderPrinter))

  val smartTradeWithList: FreeMonad[Order, Response] = for {
    stocks <- listStocks()
    _ <- stocks.traverse(stock => buy(stock, 100))
    rsp <- sell("GOOGLE", 100)
  } yield rsp


  println(smartTradeWithList.foldMap(orderPrinter))

  type TradeApp[A] = Coproduct[Order, Log, A]

  def smartTradeWithLogs(implicit O: OrderI[TradeApp], L: LogI[TradeApp]): FreeMonad[TradeApp, Response] = {
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

  type AuditableTradeApp[A] = Coproduct[Audit, TradeApp, A]

  def smartTradeWithAuditsAndLogs(implicit O: OrderI[AuditableTradeApp],
                                  L: LogI[AuditableTradeApp],
                                  A: AuditI[AuditableTradeApp]
                                 ): FreeMonad[AuditableTradeApp, Response] = {
    import A._
    import L._
    import O._

    for {
      _ <- infoI("I'm going to trade smartly")
      _ <- userAction("ID102", "buy", List("APPL", "100"))
      _ <- buyI("APPL", 200)
      _ <- infoI("I'm going to trade even more smartly")
      _ <- userAction("ID102", "buy", List("MSFT", "100"))
      _ <- buyI("MSFT", 100)
      _ <- userAction("ID102", "sell", List("GOOG", "100"))
      rsp <- sellI("GOOG", 300)
      _ <- systemAction("BACKOFFICE", "tradesCheck", List("ID102", "lastTrades"))
      _ <- errorI("Wait, what?!")
    } yield rsp
  }

  println(smartTradeWithAuditsAndLogs.foldMap(auditPrinter or (orderPrinter or logPrinter)))

  println(smartTrade.foldMap(ordersToIdViaMessaging))

  println(smartTradeWithAuditsAndLogs.foldMap(auditPrinter or (ordersToIdViaMessaging or logPrinter)))
}
