package categorytheory.demo.support

import java.util.UUID

import categorytheory.core.{Id, ~>}
import categorytheory.datatypes.FreeMonad
import categorytheory.datatypes.FreeMonad.liftF
import categorytheory.core.implicits._

trait MessagingLanguage {

  type ChannelId = String
  type SourceId = String
  type MessageId = UUID
  type Payload = String
  type Response = String
  type Condition = String

  sealed trait Messaging[A]

  case class Publish(channelId: ChannelId, source: SourceId, messageId: MessageId, payload: Payload) extends Messaging[Response]

  case class Subscribe(channelId: ChannelId, filterBy: Condition) extends Messaging[Payload]

  type MessagingF[A] = FreeMonad[Messaging, A]

  def publish(channelId: ChannelId, source: SourceId, messageId: MessageId, payload: Payload): MessagingF[Response] =
    liftF[Messaging, Response](Publish(channelId, source, messageId, payload))

  def subscribe(channelId: ChannelId, filterBy: Condition): MessagingF[Payload] =
    liftF[Messaging, Payload](Subscribe(channelId, filterBy))

  val messagingPrinter: Messaging ~> Id = new (Messaging ~> Id) {
    def apply[A](fa: Messaging[A]): Id[A] =
      fa match {
        case Publish(channelId, source, messageId, payload) =>
          println(s"Publish [$channelId] From: [$source] Id: [$messageId] Payload: [$payload]")
          "ok"
        case Subscribe(channelId, filterBy) =>
          val payload = "Event fired"
          println(s"Received message from [$channelId](filter: [$filterBy]): [$payload]")
          payload
      }
  }

  val messagingToId: MessagingF ~> Id = new (MessagingF ~> Id) {
    override def apply[A](fa: MessagingF[A]): Id[A] = fa.foldMap(messagingPrinter)
  }
}
