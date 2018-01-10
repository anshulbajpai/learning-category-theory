package categorytheory.demo.support

import categorytheory.core.{Id, Inject, ~}
import categorytheory.datatypes.Free
import categorytheory.datatypes.Free.{inject, liftF}

trait LoggingLanguage {

  sealed trait Log[A]

  case class Info(msg: String) extends Log[Unit]
  case class Error(msg: String) extends Log[Unit]

  type LogF[A] = Free[Log, A]

  def info(msg: String) = liftF(Info(msg))
  def error(msg: String) = liftF(Error(msg))

  class LogI[F[_]](implicit I: Inject[Log, F]) {
    def infoI(msg: String): Free[F, Unit] = inject[Log, F](Info(msg))
    def errorI(msg: String): Free[F, Unit] = inject[Log, F](Error(msg))
  }

  implicit def logI[F[_]](implicit I: Inject[Log, F]): LogI[F] = new LogI[F]

  def logPrinter = new (Log ~ Id) {
    override def apply[A](fa: Log[A]): Id[A] = fa match {
      case Info(msg) => println(s"[Info] -  $msg")
      case Error(msg) => println(s"[Error] -  $msg")
    }
  }
}
