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

class MessagingServiceSpec extends WordSpec
  with MockFactory
  with Matchers {

  val twilioMock = mock[TwilioApi[IO]]
  implicit val cs = IO.contextShift(ExecutionContext.global)
  implicit val par = IO.ioParallel

  val fromNumber = new PhoneNumber("from")
  val messagingService = new TwilioService[IO](fromNumber, twilioMock)
  val service = Routes.messageRoutes(messagingService).orNotFound

  "Messaging service" should {
    "Send messages when requested" in {
      val requestData = SendMessageRequest(List(
        SendMessageRequestItem(new PhoneNumber("to1"), "text1"),
        SendMessageRequestItem(new PhoneNumber("to2"), "text2")
      ))

      (twilioMock.sendSeveral _).expects( where { items: List[MessageData] =>
        items.map(_.text).diff(requestData.messages.map(_.text)).isEmpty &&
        items.map(_.to).diff(requestData.messages.map(_.to)).isEmpty &&
        items.map(_.from).forall(ph => ph == fromNumber)
      }).returning(
        IO { List("sid1", "sid2") }
      )

      val request = Request[IO](Method.POST, Uri.unsafeFromString("/messages")).withEntity(
        requestData
      )

      val result = service.run(request).unsafeRunSync()

      result.status shouldBe Status.Ok
    }
  }
}