package net.girkin.twiliosecretsanta

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

import scala.collection.immutable.SortedSet
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
}

class TwilioSecretSantaService[F[_]: Sync](
  fromNumber: PhoneNumber,
  twilioApi: TwilioApi[F]
) extends SecretSantaService[F]
  with Http4sDsl[F] {

  import JsonCodecs._

  override def post(request: Request[F]): F[Response[F]] = {
    for {
      body <- request.as[SecretSantaRequest]
      recipients <- SecretSantaService.randomizeRecipients(body.participants.toVector)
      messages = recipients.map {
        item => {
          val text = s"Your secret santa assignment is: ${item.assignedName}"
          MessageData(fromNumber, item.sendTo, text)
        }
      }
      result <- twilioApi.sendSeveral(messages.toList)
      response <- Ok(result.asJson)
    } yield {
      response
    }
  }
}
