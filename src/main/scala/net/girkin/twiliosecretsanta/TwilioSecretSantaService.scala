package net.girkin.twiliosecretsanta

import cats.data.EitherT
import cats.effect.Sync
import cats.implicits._
import com.twilio.`type`.PhoneNumber
import io.circe.syntax._
import net.girkin.twiliosecretsanta.JsonCodecs._
import org.http4s.circe.CirceEntityDecoder._
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.{Request, Response}

class TwilioSecretSantaService[F[_]: Sync](
  fromNumber: PhoneNumber,
  twilioApi: TwilioApi[F]
) extends SecretSantaService[F]
  with Http4sDsl[F]
  with Logging {

  override def post(request: Request[F]): F[Response[F]] = {
    val action = for {
      body <- request.attemptAs[SecretSantaRequest]
        .leftSemiflatMap { failure => BadRequest(failure.message) }
      _ <- SecretSantaService.validate(body).toEitherT[F]
        .leftSemiflatMap { error => PreconditionFailed(error.asJson) }
      recipients <- EitherT.right[Response[F]] { SecretSantaService.randomizeRecipients(body.participants.toVector) }
      messages = recipients.map {
        item => {
          val text = s"This is Secret Santa time! Your assignment is: ${item.assignedName}"
          MessageData(fromNumber, item.sendTo, text)
        }
      }
      result <- EitherT.right[Response[F]] { twilioApi.sendSeveral(messages.toList) }
    } yield {
      result.map {
        case Right(sid) => sid
        case Left(SendError(message)) => s"Error sending message to ${message.to}"
      }.asJson
    }

    action.semiflatMap(json => Ok(json))
      .fold(identity, identity)
      .handleErrorWith {
        e => error("There was an error processing request", e).flatMap {
          _ => Sync[F].raiseError(e)
        }
      }
  }
}