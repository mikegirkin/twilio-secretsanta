package net.girkin.twiliosecretsanta

import cats.Parallel
import cats.effect.IO
import cats.implicits._
import com.twilio.`type`.PhoneNumber
import com.twilio.http.TwilioRestClient
import com.twilio.rest.api.v2010.account.Message

object TwilioApi {
  def apply(accountSid: String, accountToken: String)(implicit p: Parallel[IO, IO.Par]): TwilioApi[IO] = {
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
) (
  implicit p: Parallel[IO, IO.Par]
) extends TwilioApi[IO] with Logging[IO] {
  override def sendMessage(textMessage: MessageData): IO[String] = {
    val sendAction = for {
      _ <- IO { logger.debug(s"Sending ${textMessage.from} -> ${textMessage.to}") }
      sid <- IO {
        Message.creator(textMessage.to, textMessage.from, textMessage.text)
          .create(client)
          .getSid
      }
      _ <- IO { logger.info(s"Success sending message to ${textMessage.to}") }
    } yield sid

    sendAction.handleErrorWith { err =>
      IO { logger.error(s"Failed sending to ${textMessage.to}", err) }
      IO.raiseError(err)
    }
  }

  override def sendSeveral(messages: List[MessageData]): IO[List[String]] = {
    messages.parTraverse((m: MessageData) => sendMessage(m))
  }
}