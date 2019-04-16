package net.girkin.twiliosecretsanta

import cats.implicits._
import cats.effect.{Effect, IO, Sync}
import com.twilio.`type`.PhoneNumber
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import org.http4s.dsl._
import org.http4s.{Request, Response}
import org.http4s.circe.CirceEntityDecoder._
import org.http4s.circe._

import scala.util.Random

case class SecretSantaParticipant(
  phone: PhoneNumber,
  name: String
)

case class SecretSantaAssignment(
  sendTo: PhoneNumber,
  assignedName: String
)

case class SecretSantaRequest(
  participants: List[SecretSantaParticipant]
)

case class SendMessageResponse(
  sids: List[String]
)

object JsonCodecs {
  implicit val phoneNumberDecoder: Decoder[PhoneNumber] = Decoder.decodeString.map {
    str => new PhoneNumber(str)
  }
  implicit val phoneNumberEncoder: Encoder[PhoneNumber] = Encoder.instance { ph => Json.fromString(ph.getEndpoint) }
  implicit val messageDataDecoder: Decoder[SecretSantaParticipant] = deriveDecoder[SecretSantaParticipant]
  implicit val messageDataEncoder: Encoder[SecretSantaParticipant] = deriveEncoder[SecretSantaParticipant]
  implicit val sendMessageRequestDecoder: Decoder[SecretSantaRequest] = deriveDecoder[SecretSantaRequest]
  implicit val sendMessageRequestEncoder: Encoder[SecretSantaRequest] = deriveEncoder[SecretSantaRequest]
  implicit val sendMessageResponseEncoder: Encoder[SendMessageResponse] = deriveEncoder[SendMessageResponse]
}

trait SecretSantaService[F[_]] {
  def post(request: Request[F]): F[Response[F]]
}

class TwilioSecretSantaService[F[_]: Sync](
  fromNumber: PhoneNumber,
  twilioApi: TwilioApi[F]
) extends SecretSantaService[F]
  with Http4sDsl[F] {

  import JsonCodecs._

  private def randomizeRecipients(participants: List[SecretSantaParticipant]): F[List[SecretSantaAssignment]] = {
    val names = participants.map(_.name)
    val numbers = participants.map(_.phone)
    Sync[F].delay {
      numbers.zip(Random.shuffle(names)).map(SecretSantaAssignment.tupled)
    }
  }

  override def post(request: Request[F]): F[Response[F]] = {
    for {
      body <- request.as[SecretSantaRequest]
      recipients <- randomizeRecipients(body.participants)
      messages = recipients.map {
        item => {
          val text = s"Your secret santa assignment is: ${item.assignedName}"
          MessageData(fromNumber, item.sendTo, text)
        }
      }
      result <- twilioApi.sendSeveral(messages)
      response <- Ok(result.asJson)
    } yield {
      response
    }
  }
}
