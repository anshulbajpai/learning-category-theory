package categorytheory.demo.support

import categorytheory.core.{Id, Inject, ~}
import categorytheory.datatypes.Free

trait AuditLanguage {

  sealed trait Audit[A]

  type UserId = String
  type JobId = String
  type Values = String

  case class UserAction(user: UserId, action: String, values: List[Values]) extends Audit[Unit]

  case class SystemAction(job: JobId, action: String, values: List[Values]) extends Audit[Unit]

  class AuditI[F[_]](implicit inject: Inject[Audit, F]) {
    def userAction(user: UserId, action: String, values: List[Values]) = Free.inject(UserAction(user, action, values))
    def systemAction(job: UserId, action: String, values: List[Values]) = Free.inject(SystemAction(job, action, values))
  }

  implicit def auditI[F[_]](implicit inject: Inject[Audit, F]): AuditI[F] = new AuditI[F]()

  val auditPrinter: Audit ~ Id = new (Audit ~ Id) {
    override def apply[A](fa: Audit[A]): Id[A] = fa match {
      case UserAction(user, action, values) => println(s"[USER Action] - user $user called $action with values $values")
      case SystemAction(job, action, values) => println(s"[SYSTEM Action] - $job called $action with values $values")
    }
  }

}

