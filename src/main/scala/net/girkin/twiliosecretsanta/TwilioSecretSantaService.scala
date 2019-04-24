package net.girkin.twiliosecretsanta

import cats.effect.Sync
import cats.implicits._
import com.twilio.`type`.PhoneNumber
import io.circe.syntax._
import org.http4s.circe._
import org.http4s.circe.CirceEntityDecoder._
import org.http4s.dsl.Http4sDsl
import org.http4s.{Request, Response}
import JsonCodecs._

class MikeException(msg: String) extends Exception

class TwilioSecretSantaService[F[_]: Sync](
  fromNumber: PhoneNumber,
  twilioApi: TwilioApi[F]
) extends SecretSantaService[F]
  with Http4sDsl[F]
  with Logging {

  override def post(request: Request[F]): F[Response[F]] = {
    val action = for {
      body <- request.attemptAs[SecretSantaRequest].value
        .flatMap[SecretSantaRequest] {
          case Left(err) => Sync[F].raiseError(err)
          case Right(req) => Sync[F].delay(req)
        }
      recipients <- SecretSantaService.randomizeRecipients(body.participants.toVector)
      messages = recipients.map {
        item => {
          val text = s"This is Secret Santa time! Your assignment is: ${item.assignedName}"
          MessageData(fromNumber, item.sendTo, text)
        }
      }
      result <- twilioApi.sendSeveral(messages.toList)
      response <- Ok(result.asJson)
    } yield {
      response
    }

    action.handleErrorWith {
      case org.http4s.MalformedMessageBodyFailure(details, _) =>
        BadRequest(details)
      case e =>
        error("There was an error processing request", e).flatMap {
          _ => Sync[F].raiseError(e)
        }
    }
  }
}
