package net.girkin.twiliosecretsanta

import com.twilio.`type`.PhoneNumber
import com.twilio.http.TwilioRestClient
import com.twilio.rest.api.v2010.account.Message
import net.girkin.twiliosecretsanta.TwilioApi.SendResult
import scalaz.zio.Task
import scalaz.zio.interop.catz._

object TwilioApi {
  type SendResult = Either[SendError, String]

  def apply(accountSid: String, accountToken: String): TwilioApi[Task] = {
    val client = new TwilioRestClient.Builder(accountSid, accountToken).build()

    new TwilioApiImpl(client)
  }
}

case class MessageData(
  from: PhoneNumber,
  to: PhoneNumber,
  text: String
)

case class SendError(message: MessageData)

trait TwilioApi[F[_]] {
  def sendMessage(textMessage: MessageData): F[SendResult]
  def sendSeveral(messages: List[MessageData]): F[List[SendResult]]
}

private class TwilioApiImpl(
  client: TwilioRestClient
) extends TwilioApi[Task] with Logging {

  private def sendOneTask(textMessage: MessageData) = Task {
    Message.creator(textMessage.to, textMessage.from, textMessage.text)
      .create(client)
      .getSid
  }

  override def sendMessage(textMessage: MessageData): Task[SendResult] = {
    val sendAction: Task[String] = for {
      _ <- debug[Task](s"Sending ${textMessage.from} -> ${textMessage.to}")
      sid <- sendOneTask(textMessage)
      _ <- info[Task](s"Success sending message to ${textMessage.to}")
    } yield sid

    sendAction.refineOrDie {
      case _ => SendError(textMessage)
    }.either
  }

  override def sendSeveral(messages: List[MessageData]): Task[List[SendResult]] = {
    Task.collectAllPar(
      messages.map(sendMessage)
    )
  }
}