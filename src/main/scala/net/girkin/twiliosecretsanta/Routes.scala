package net.girkin.twiliosecretsanta

import cats.effect.Sync
import org.http4s._
import org.http4s.dsl.Http4sDsl

object Routes {
  def messageRoutes[F[_]: Sync](
    messagingService: MessagingService[F]
  ) (
  ): HttpRoutes[F] = {

    val dsl = new Http4sDsl[F]{}
    import dsl._

    HttpRoutes.of[F] {
      case request @ POST -> Root / "messages" => messagingService.post(request)
      case _ => Ok()
    }
  }
}
