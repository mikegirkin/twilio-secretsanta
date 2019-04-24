package net.girkin.twiliosecretsanta

import cats.effect.ExitCode
import fs2.Stream
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.Logger
import scalaz.zio.{Task, _}
import scalaz.zio.interop.catz._
import scalaz.zio.interop.catz.implicits._

object Server {

  def stream[R](
    config: TwilioConfig,
  )(
    implicit runtime: Runtime[R]
  ): Stream[Task, ExitCode] = {
    val twilioApi = TwilioApi(config.accountSid, config.accountToken)
    val twilioService = new TwilioSecretSantaService[IO[Throwable, ?]](config.fromNumber, twilioApi)

    val httpApp = (
      Routes.messageRoutes[Task](twilioService)
      ).orNotFound

    val finalHttpApp = Logger.httpApp(true, true)(httpApp)

    //implicit val runtime = new DefaultRuntime {}

    BlazeServerBuilder[Task]
      .bindHttp(8080, "0.0.0.0")
      .withHttpApp(finalHttpApp)
      .serve
  }
}

