package net.girkin.twiliosecretsanta

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import com.twilio.`type`.PhoneNumber

case class TwilioConfig(
  accountSid: String,
  accountToken: String,
  fromNumber: PhoneNumber
)

object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    Server.stream[IO, IO.Par](
      TwilioConfig("", "", new PhoneNumber(""))
    ).compile.drain.as(ExitCode.Success)
}