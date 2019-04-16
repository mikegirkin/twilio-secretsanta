package net.girkin.twiliosecretsanta

import cats.Parallel
import cats.implicits._
import cats.effect.Sync
import com.twilio.`type`.PhoneNumber
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import org.http4s.dsl._
import org.http4s.{Request, Response}
import org.http4s.circe.CirceEntityDecoder._
import org.http4s.circe._

case class SendMessageRequestItem(
  to: PhoneNumber,
  text: String
)

case class SendMessageRequest(
  messages: List[SendMessageRequestItem]
)

case class SendMessageResponse(
  sids: List[String]
)

object JsonCodecs {
  implicit val phoneNumberDecoder: Decoder[PhoneNumber] = Decoder.decodeString.map {
    str => new PhoneNumber(str)
  }
  implicit val phoneNumberEncoder: Encoder[PhoneNumber] = Encoder.instance { ph => Json.fromString(ph.getEndpoint) }
  implicit val messageDataDecoder: Decoder[SendMessageRequestItem] = deriveDecoder[SendMessageRequestItem]
  implicit val messageDataEncoder: Encoder[SendMessageRequestItem] = deriveEncoder[SendMessageRequestItem]
  implicit val sendMessageRequestDecoder: Decoder[SendMessageRequest] = deriveDecoder[SendMessageRequest]
  implicit val sendMessageRequestEncoder: Encoder[SendMessageRequest] = deriveEncoder[SendMessageRequest]
  implicit val sendMessageResponseEncoder: Encoder[SendMessageResponse] = deriveEncoder[SendMessageResponse]
}

trait MessagingService[F[_]] {
  def post(request: Request[F]): F[Response[F]]
}

class TwilioService[F[_]: Sync](
  fromNumber: PhoneNumber,
  twilioApi: TwilioApi[F]
) extends MessagingService[F]
  with Http4sDsl[F] {

  import JsonCodecs._

  override def post(request: Request[F]): F[Response[F]] = {
    for {
      body <- request.as[SendMessageRequest]
      messages = body.messages.map {
        item => MessageData(fromNumber, item.to, item.text)
      }
      result <- twilioApi.sendSeveral(messages)
      response <- Ok(result.asJson)
    } yield {
      response
    }
  }
}
