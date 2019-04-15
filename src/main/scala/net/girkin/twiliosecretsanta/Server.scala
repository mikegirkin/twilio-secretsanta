package net.girkin.twiliosecretsanta

import cats.Parallel
import cats.effect.{ConcurrentEffect, ContextShift, Timer}
import fs2.Stream
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.Logger

import scala.concurrent.ExecutionContext.global

object Server {

  def stream[F[_]: ConcurrentEffect, M[_]](
    config: TwilioConfig
  )(
    implicit T: Timer[F], C: ContextShift[F], P: Parallel[F, M]
  ): Stream[F, Nothing] = {

    for {
      client <- BlazeClientBuilder[F](global).stream
      twilioApi = TwilioApi(config.accountSid, config.accountToken)

      // Combine Service Routes into an HttpApp.
      // Can also be done via a Router if you
      // want to extract a segments not checked
      // in the underlying routes.
      httpApp = (
        Routes.messageRoutes[F, M](config, twilioApi)
      ).orNotFound

      // With Middlewares in place
      finalHttpApp = Logger.httpApp(true, true)(httpApp)

      exitCode <- BlazeServerBuilder[F]
        .bindHttp(8080, "0.0.0.0")
        .withHttpApp(finalHttpApp)
        .serve
    } yield exitCode
  }.drain


}

