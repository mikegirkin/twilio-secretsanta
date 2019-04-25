package net.girkin.twiliosecretsanta

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.effect.Sync
import cats.syntax.reducible._
import cats.data.NonEmptyList._
import com.twilio.`type`.PhoneNumber
import org.http4s.{Request, Response}

import scala.collection.immutable.SortedSet
import scala.util.Random

sealed trait RequestValidationError extends Product with Serializable {
  def msg: String
}

object RequestValidationError {
  def field(name: String, msg: String): RequestValidationError = FieldValidationError(name, msg)
  def global(msg: String): RequestValidationError = GlobalValidationError(msg)
}

final case class FieldValidationError(
  field: String,
  msg: String
) extends RequestValidationError

final case class GlobalValidationError(
  msg: String
) extends RequestValidationError



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

trait SecretSantaService[F[_]] {
  def post(request: Request[F]): F[Response[F]]
}

object SecretSantaService {
  def randomizeRecipients[F[_]: Sync](participants: Vector[SecretSantaParticipant]): F[Vector[SecretSantaAssignment]] = {
    val indicesSet = SortedSet(participants.indices:_*)

    Sync[F].delay {
      val assignments = participants.zipWithIndex.foldLeft((Vector.empty[(SecretSantaParticipant, SecretSantaParticipant)], indicesSet)) {
        case ((assignment, availableIndices), (giver, giverIndex)) => {
          val selection = availableIndices - giverIndex
          if (selection.nonEmpty) {
            val selected = Random.nextInt(selection.size)
            val takerIndex = selection.drop(selected).head
            (assignment :+ (giver, participants(takerIndex)), availableIndices - takerIndex)
          } else {
            // If the number of participants is odd, there might be situation where the last standing item could only be assigned to self
            // If the situation like this is detected - we need to swap assignments of that last standing with the another one
            val swapWith = Random.nextInt(assignment.length)
            val anotherAssignment@(anotherGiver, anotherTaker) = assignment(swapWith)
            val lastStanding = giver
            (assignment.filter(_ != anotherAssignment) :+ (lastStanding, anotherTaker) :+ (anotherGiver, lastStanding), selection)
          }

        }
      }

      assignments._1.map {
        case (giver, taker) => SecretSantaAssignment(giver.phone, taker.name)
      }
    }
  }

  def validate(secretSantaRequest: SecretSantaRequest): Either[NonEmptyList[RequestValidationError], SecretSantaRequest] = {
    val restrictions = NonEmptyList.of(
      "participants" -> NonEmptyList.of[(SecretSantaRequest => Boolean, String)](
        (ssr => ssr.participants.size >= 2, "There must be 2 or more participants"),
        (ssr => ssr.participants.map(_.name).distinct.size == ssr.participants.size,
          "All the participants must have unique names"),
        (ssr => ssr.participants.map(_.phone).distinct.size == ssr.participants.size,
          "All the participants must have unique phone numbers")
      )
    )

    val validated: ValidatedNel[RequestValidationError, SecretSantaRequest] = restrictions.reduceMapK {
      case(field, validator) => validator.reduceMapK {
        case (check, errorText) => Validated.cond(
          check(secretSantaRequest),
          secretSantaRequest,
          RequestValidationError.field(field, errorText)
        ).toValidatedNel
      }
    }

    validated.toEither
  }
}


