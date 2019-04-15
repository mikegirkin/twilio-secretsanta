package net.girkin.twiliosecretsanta

import cats.Parallel
import cats.effect.Sync
import cats.implicits._
import com.twilio.`type`.PhoneNumber
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.HttpRoutes
import org.http4s.circe.CirceEntityDecoder._

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
  implicit val messageDataDecoder: Decoder[SendMessageRequestItem] = deriveDecoder[SendMessageRequestItem]
  implicit val sendMessageRequestDecoder: Decoder[SendMessageRequest] = deriveDecoder[SendMessageRequest]
  implicit val sendMessageResponseEncoder: Encoder[SendMessageResponse] = deriveEncoder[SendMessageResponse]
}

object Routes {
  import JsonCodecs._

  def messageRoutes[F[_]: Sync, M[_]](
    config: TwilioConfig,
    twilioApi: TwilioApi[F]
  ) (
    implicit p: Parallel[F, M]
  ): HttpRoutes[F] = {

    val dsl = new Http4sDsl[F]{}
    import dsl._

    HttpRoutes.of[F] {
      case body @ POST -> Root / "message" => {
        for {
          request <- body.as[SendMessageRequest]
          messages = request.messages.map {
            item => MessageData(config.fromNumber, item.to, item.text)
          }
          result <- twilioApi.sendSeveral(messages)
          response <- Ok(result.asJson)
        } yield {
          response
        }
      }
    }
  }

}