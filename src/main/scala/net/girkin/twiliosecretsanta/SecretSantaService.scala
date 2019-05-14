package net.girkin.twiliosecretsanta

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.effect.Sync
import cats.syntax.reducible._
import cats.syntax.functor._
import cats.data.NonEmptyList._
import com.twilio.`type`.PhoneNumber
import org.http4s.{Request, Response}

import scala.annotation.tailrec
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
  def pickRandomElement[A](indexedSeq: Set[A]): A = {
    indexedSeq.drop(Random.nextInt(indexedSeq.size)).head
  }

  def randomizeIndicesPreventSamePosition[F[_]: Sync](length: Int): F[Vector[Int]] = {
    @tailrec
    def randomizeIndicesPreventSamePositionIntrnl(partiallyDone: Vector[Int], remaining: SortedSet[Int]): Vector[Int] = {
      if(remaining.size == 1) {
        if(remaining.head == partiallyDone.size) {
          val index = Random.nextInt(partiallyDone.size)
          partiallyDone.updated(index, remaining.head) :+ partiallyDone(index)
        } else {
          partiallyDone :+ remaining.head
        }
      } else {
        val next = pickRandomElement(remaining - partiallyDone.size)
        randomizeIndicesPreventSamePositionIntrnl(partiallyDone :+ next, remaining - next)
      }
    }

    Sync[F].delay {
      randomizeIndicesPreventSamePositionIntrnl(Vector.empty, SortedSet(Range(0, length):_*))
    }
  }

  def randomizeRecipients[F[_]: Sync](participants: Vector[SecretSantaParticipant]): F[Vector[SecretSantaAssignment]] = {
    for {
      randomizedIndices <- randomizeIndicesPreventSamePosition(participants.length)
    } yield {
      participants.zip(randomizedIndices).foldLeft(Vector.empty[SecretSantaAssignment]) {
        case (acc, (giver, takerIndex)) => {
          acc :+ SecretSantaAssignment(giver.phone, participants(takerIndex).name)
        }
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


