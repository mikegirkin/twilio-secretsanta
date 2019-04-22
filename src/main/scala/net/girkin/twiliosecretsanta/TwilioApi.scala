package net.girkin.twiliosecretsanta

import com.twilio.`type`.PhoneNumber
import com.twilio.http.TwilioRestClient
import com.twilio.rest.api.v2010.account.Message
import scalaz.zio.{Task}
import scalaz.zio.interop.catz._

object TwilioApi {
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

trait TwilioApi[F[_]] {
  def sendMessage(textMessage: MessageData): F[String]
  def sendSeveral(messages: List[MessageData]): F[List[String]]
}

private class TwilioApiImpl(
  client: TwilioRestClient
) extends TwilioApi[Task] with Logging {
  override def sendMessage(textMessage: MessageData): Task[String] = {
    val sendAction = for {
      _ <- debug(s"Sending ${textMessage.from} -> ${textMessage.to}")
      sid <- Task {
        Message.creator(textMessage.to, textMessage.from, textMessage.text)
          .create(client)
          .getSid
      }
      _ <- info(s"Success sending message to ${textMessage.to}")
    } yield sid

    sendAction.catchAll {
      err => error(s"Failed sending to ${textMessage.to}", err)
        .flatMap {
          _ => Task.fail(err)
        }
    }
  }

  override def sendSeveral(messages: List[MessageData]): Task[List[String]] = {
    Task.collectAllPar(
      messages.map(sendMessage)
    )
  }
}