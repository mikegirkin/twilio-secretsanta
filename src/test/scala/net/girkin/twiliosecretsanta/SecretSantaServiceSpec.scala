package net.girkin.twiliosecretsanta

import cats.effect.IO
import com.twilio.`type`.PhoneNumber
import org.http4s.{Method, Request, Status, Uri}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import org.http4s.circe.CirceEntityCodec._
import org.http4s.implicits._

import scala.concurrent.ExecutionContext
import JsonCodecs._

class SecretSantaServiceSpec extends WordSpec
  with MockFactory
  with Matchers {

  val twilioMock = mock[TwilioApi[IO]]
  implicit val cs = IO.contextShift(ExecutionContext.global)
  implicit val par = IO.ioParallel

  val fromNumber = new PhoneNumber("from")
  val messagingService = new TwilioSecretSantaService[IO](fromNumber, twilioMock)
  val service = Routes.messageRoutes(messagingService).orNotFound

  "Messaging service" should {
    "Send messages when requested" in {
      val requestData = SecretSantaRequest(List(
        SecretSantaParticipant(new PhoneNumber("t1"), "name1"),
        SecretSantaParticipant(new PhoneNumber("t2"), "name2"),
        SecretSantaParticipant(new PhoneNumber("t3"), "name3")
      ))

      (twilioMock.sendSeveral _).expects( where { items: List[MessageData] =>
        items.map(_.to).diff(requestData.participants.map(_.phone)).isEmpty &&
        items.map(_.from).forall(ph => ph == fromNumber)
      }).returning(
        IO { List("sid1", "sid2", "sid3") }
      )

      val request = Request[IO](Method.POST, Uri.unsafeFromString("/messages")).withEntity(
        requestData
      )

      val result = service.run(request).unsafeRunSync()

      result.status shouldBe Status.Ok
    }
  }

  "Randomizing recipients" should {
    "not assign a person to itself" in {
      val participants = Vector(
        SecretSantaParticipant(new PhoneNumber("t1"), "name1"),
        SecretSantaParticipant(new PhoneNumber("t2"), "name2"),
        SecretSantaParticipant(new PhoneNumber("t3"), "name3")
      )

      for {
        _ <- 0 to 100
      } {
        val result = SecretSantaService.randomizeRecipients[IO](participants).unsafeRunSync()
        for {
          p <- participants
        } {
          result.find(_.assignedName == p.name).get.sendTo should not be p.phone
          result.map(_.assignedName).diff(participants.map(_.name)) shouldBe empty
          result.map(_.sendTo).diff(participants.map(_.phone)) shouldBe empty
        }
      }

    }
  }
}