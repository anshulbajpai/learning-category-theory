package categorytheory.demo.support

import java.util.UUID.randomUUID

import categorytheory.core.{Id, Inject, ~>}
import categorytheory.datatypes.Free
import categorytheory.datatypes.Free.liftF
import categorytheory.core.ops._

trait OrdersLanguage  {

  self: MessagingLanguage =>

  sealed trait Order[A]

  type Symbol = String

  case class Buy(stock: Symbol, quantity: Int) extends Order[Response]

  case class Sell(stock: Symbol, quantity: Int) extends Order[Response]

  case class ListStocks() extends Order[List[Symbol]]

  type OrdersF[A] = Free[Order, A]

  def buy(stock: Symbol, quantity: Int): OrdersF[Response] = liftF[Order, Response](Buy(stock, quantity))
  def sell(stock: Symbol, quantity: Int): OrdersF[Response] = liftF[Order, Response](Sell(stock, quantity))
  def listStocks(): OrdersF[List[Symbol]] = liftF[Order, List[Symbol]](ListStocks())

  class OrderI[F[_]](implicit I: Inject[Order, F]) {
    def buyI(stock: Symbol, quantity: Int): Free[F, Response] = Free.inject[Order, F](Buy(stock, quantity))
    def sellI(stock: Symbol, quantity: Int): Free[F, Response] = Free.inject[Order, F](Sell(stock, quantity))
    def listStocksI(): Free[F, List[Symbol]] = Free.inject[Order, F](ListStocks())
  }

  implicit def orderI[F[_]](implicit I: Inject[Order, F]): OrderI[F] = new OrderI[F]

  val orderPrinter: Order ~> Id = new (Order ~> Id) {
    override def apply[A](fa: Order[A]): Id[A] = fa match {
      case Buy(stock, quantity) =>
        println(s"Buying $quantity of $stock")
        "ok"
      case Sell(stock, quantity) =>
        println(s"Selling $quantity of $stock")
        "ok"
      case ListStocks() =>
        println("Listing stocks - APPLE, GOOGLE")
        List("APPLE", "GOOGLE")
    }
  }

  type ErrorOr[A] = Either[String, A]

  val betterOrderPrinter: Order ~> ErrorOr = new (Order ~> ErrorOr) {
    override def apply[A](fa: Order[A]): ErrorOr[A] = fa match {
      case Buy(stock, quantity) => Right(s"Buy - $stock - $quantity")
      case Sell(stock, quantity) => Left("Why are you selling that?")
      case ListStocks() => Right(List("APPLE", "GOOGLE"))
    }
  }

  val orderToMessagePrinter: Order ~> MessagingF = new (Order ~> MessagingF) {
    override def apply[A](fa: Order[A]): MessagingF[A] = fa match {
      case ListStocks() => for {
        _ <- publish("001", "Orders", randomUUID(), "Get stocks list")
        payload <- subscribe("001", "*")
      } yield List(payload)
      case Buy(stock, quantity) => publish("001", "Orders", randomUUID(), s"Buy $stock $quantity")
      case Sell(stock, quantity) => publish("001", "Orders", randomUUID(), s"Sell $stock $quantity")
    }
  }

  def ordersToIdViaMessaging: Order ~> Id = orderToMessagePrinter andThen messagingToId
}


