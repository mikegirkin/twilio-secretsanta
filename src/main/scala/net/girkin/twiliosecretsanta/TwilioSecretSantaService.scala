package net.girkin.twiliosecretsanta

import cats.data.{EitherT, NonEmptyList}
import cats.effect.Sync
import cats.implicits._
import com.twilio.`type`.PhoneNumber
import io.circe.syntax._
import net.girkin.twiliosecretsanta.JsonCodecs._
import org.http4s.circe.CirceEntityDecoder._
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.{DecodeFailure, Request, Response}
import scalaz.zio.IO

trait Error

class TwilioSecretSantaService[F[_]: Sync](
  fromNumber: PhoneNumber,
  twilioApi: TwilioApi[F]
) extends SecretSantaService[F]
  with Http4sDsl[F]
  with Logging {

  override def post(request: Request[F]): IO[Error, Response[F]] = {
    val action = for {
      body <- parseRequest(request)
      _ <- validateRequest(body)
      recipients <- EitherT.right[Response[F]] { SecretSantaService.randomizeRecipients(body.participants.toVector) }
      messages = recipients.map {
        item => {
          val text = s"This is Secret Santa time! Your assignment is: ${item.assignedName}"
          MessageData(fromNumber, item.sendTo, text)
        }
      }
      result <- EitherT.right[Response[F]] { twilioApi.sendSeveral(messages.toList) }
      response <- EitherT.right[Response[F]] { Ok(result.asJson) }
    } yield {
      response
    }

    action.fold(identity, identity).handleErrorWith {
      e => error("There was an error processing request", e).flatMap {
        _ => Sync[F].raiseError(e)
      }
    }
  }

  def parseRequest(request: Request[F]): EitherT[F, Response[F], SecretSantaRequest] = {
    request.attemptAs[SecretSantaRequest]
      .leftSemiflatMap { failure => BadRequest(failure.message) }
  }

  def validateRequest(request: SecretSantaRequest): EitherT[F, Response[F], SecretSantaRequest] = {
    SecretSantaService.validate(request)
      .toEitherT[F]
      .leftSemiflatMap(
        errors => PreconditionFailed(errors.asJson
      )
    )
  }

  def generateSecretSantaMessages(request: SecretSantaRequest) = {
    SecretSantaService.randomizeRecipients(request.participants.toVector)
      .map {
        _.map { item =>
          val text = s"This is Secret Santa time! Your assignment is: ${item.assignedName}"
          MessageData(fromNumber, item.sendTo, text)
        }
      }
  }
}