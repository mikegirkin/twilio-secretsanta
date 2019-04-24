package net.girkin.twiliosecretsanta

import com.twilio.`type`.PhoneNumber
import org.http4s.{Method, Request, Status, Uri}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import org.http4s.implicits._
import scalaz.zio.Task
import scalaz.zio.interop.catz._
import scalaz.zio._

class TwilioSecretSantaSpec extends WordSpec with MockFactory with Matchers {
  val twilioMock = mock[TwilioApi[Task]]

  val fromNumber = new PhoneNumber("from")
  val messagingService = new TwilioSecretSantaService[Task](fromNumber, twilioMock)
  val service = Routes.messageRoutes(messagingService).orNotFound
  val runtime = new DefaultRuntime {}

  "Twilio service" should {
    "return 400 when get bad json" in {
      val request = Request[Task](Method.POST, Uri.unsafeFromString("/messages")).withBodyStream(
        fs2.Stream.fromIterator[Task, Byte]("blablabla".getBytes().toIterator)
      )

      val result = runtime.unsafeRun(
        service.run(request)
      )

      result.status shouldBe Status.BadRequest
    }
  }
}
