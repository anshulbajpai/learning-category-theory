package categorytheory.demo.support

import java.time.LocalDate
import java.util.UUID

import categorytheory.core.implicits._
import categorytheory.datatypes.Validated

import scala.util.Try

trait UserValidation {

  import Validated._
  import categorytheory.core.ops._

  val isValidFirstName: String => Validated[List[String], String] = value =>
    Validated.condition(value.length <= 10, List("firstName should be less than 10 characters"), value)

  val isValidLastName: String => Validated[List[String], String] = value =>
    Validated.condition(value.length <= 10, List("lastName should be less than 10 characters"), value)

  val isValidDoB: LocalDate => Validated[List[String], LocalDate] = date =>
    Validated.condition(date.isBefore(LocalDate.now()), List("date should be in past"), date)

  val isValidId: String => Validated[List[String], UUID] = id =>
    Try(UUID.fromString(id)).toValidated(exception => List(exception.getMessage))

  case class User(firstName: String, lastName: String, dob: LocalDate, id: UUID)
}
