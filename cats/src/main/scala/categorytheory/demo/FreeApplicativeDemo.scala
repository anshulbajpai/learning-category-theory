package categorytheory.demo

import categorytheory.datatypes.FreeAp
import categorytheory.core.ops._

object FreeApplicativeDemo extends App {

  sealed trait Config[A]

  case class ConfigInt[A](field: String, value: Int => A) extends Config[A]
  case class ConfigString[A](field: String, value: String => A) extends Config[A]
  case class ConfigBoolean[A](field: String, value: Boolean => A) extends Config[A]


  def int(field: String): FreeAp[Config, Int] = FreeAp.lift(ConfigInt(field, identity))
  def string(field: String): FreeAp[Config, String] = FreeAp.lift(ConfigString(field, identity))
  def boolean(field: String): FreeAp[Config, Boolean] = FreeAp.lift(ConfigBoolean(field, identity))

  case class ServerConfig(host: String, port: Int)

  val port: FreeAp[Config, Int] = int("port")
  val host: FreeAp[Config, String] = string("host")

  val hostPort: FreeAp[Config, (Int, String)] = port |@| host

//  val serverConfig = hostPort((x: Int, y: String) => ???)




}
