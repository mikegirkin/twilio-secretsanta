package net.girkin.twiliosecretsanta

import cats.Parallel
import cats.effect.Effect
import cats.implicits._
import com.twilio.`type`.PhoneNumber
import com.twilio.http.TwilioRestClient
import com.twilio.rest.api.v2010.account.Message

object TwilioApi {
  def apply[F[_]: Effect](accountSid: String, accountToken: String): TwilioApi[F] = {
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
  def sendSeveral[M[_]](messages: List[MessageData])(implicit p: Parallel[F, M]): F[List[String]]
}

private class TwilioApiImpl[F[_]: Effect](client: TwilioRestClient) extends TwilioApi[F] with Logging[F] {
  override def sendMessage(textMessage: MessageData): F[String] = {
    val sendAction = for {
      _ <- Effect[F].delay{ logger.debug(s"Sending ${textMessage.from} -> ${textMessage.to}") }
      sid <- Effect[F].delay {
        Message.creator(textMessage.from, textMessage.to, textMessage.text)
          .create(client)
          .getSid
      }
      _ <- Effect[F].delay { logger.info(s"Success sending message to ${textMessage.to}") }
    } yield sid

    sendAction.handleErrorWith { err =>
      Effect[F].delay { logger.error(s"Failed sending to ${textMessage.to}", err) }
      Effect[F].raiseError(err)
    }
  }

  override def sendSeveral[M[_]](messages: List[MessageData])(implicit p: Parallel[F, M]): F[List[String]] = {
    messages.parTraverse((m: MessageData) => sendMessage(m))
  }
}