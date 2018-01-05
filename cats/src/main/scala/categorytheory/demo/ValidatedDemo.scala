package categorytheory.demo

import java.time.LocalDate
import java.util.UUID

import categorytheory.core.implicits._
import categorytheory.core.ops._
import categorytheory.demo.support.UserValidation

object ValidatedDemo extends App with UserValidation {

  def validatedUser(firstName: String, lastName: String, dob: LocalDate, id: String) =
    (isValidFirstName(firstName) |@|
      isValidLastName(lastName) |@|
      isValidDoB(dob) |@|
      isValidId(id))
      .map {
        case (((firstName, lastName), dob), id) => User(firstName, lastName, dob, id)
      }



  println(validatedUser("J" * 11, "D" * 11, LocalDate.now().plusDays(10), "foo"))
  println(validatedUser("John", "Doe", LocalDate.now().minusDays(10), UUID.randomUUID().toString))

}
