package categorytheory.demo.support

import categorytheory.core.{Id, ~}
import categorytheory.datatypes.Free
import categorytheory.datatypes.Free.liftF

trait OrdersLanguage {

  sealed trait Order[A]

  type Response = String
  type Symbol = String

  case class Buy(stock: Symbol, quantity: Int) extends Order[Response]

  case class Sell(stock: Symbol, quantity: Int) extends Order[Response]

  type OrdersF[A] = Free[Order, A]

  def buy(stock: Symbol, quantity: Int): OrdersF[Response] = liftF[Order, Response](Buy(stock, quantity))
  def sell(stock: Symbol, quantity: Int): OrdersF[Response] = liftF[Order, Response](Sell(stock, quantity))


  val orderPrinter: Order ~ Id = new (Order ~ Id) {
    override def apply[A](fa: Order[A]): Id[A] = fa match {
      case Buy(stock, quantity) =>
        println(s"Buying $quantity of $stock")
        "ok"
      case Sell(stock, quantity) =>
        println(s"Selling $quantity of $stock")
        "ok"
    }
  }
}


