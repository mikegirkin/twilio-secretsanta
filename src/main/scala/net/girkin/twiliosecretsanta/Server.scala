package net.girkin.twiliosecretsanta

import cats.Parallel
import cats.effect.{ContextShift, ExitCode, IO, Timer}
import fs2.Stream
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.Logger

object Server {

  def stream(
    config: TwilioConfig
  )(
    implicit T: Timer[IO],
    C: ContextShift[IO],
    P: Parallel[IO, IO.Par]
  ): Stream[IO, ExitCode] = {
    val twilioApi = TwilioApi(config.accountSid, config.accountToken)
    val twilioService = new TwilioService[IO](config.fromNumber, twilioApi)

    val httpApp = (
      Routes.messageRoutes[IO](twilioService)
      ).orNotFound

    val finalHttpApp = Logger.httpApp(true, true)(httpApp)

    BlazeServerBuilder[IO]
      .bindHttp(8080, "0.0.0.0")
      .withHttpApp(finalHttpApp)
      .serve
  }
}

