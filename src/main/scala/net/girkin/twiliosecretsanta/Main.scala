package net.girkin.twiliosecretsanta

import cats.data.ValidatedNec
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import com.twilio.`type`.PhoneNumber

case class TwilioConfig(
  accountSid: String,
  accountToken: String,
  fromNumber: PhoneNumber
)

case class ConfigurationException(msg: String) extends Exception
case class ParameterRequired(name: String)

object Main extends IOApp {
  val config = TwilioConfig("", "", new PhoneNumber(""))

  private def readEnvRequired(name: String): IO[ValidatedNec[ParameterRequired, String]] = IO {
    val value = System.getenv(name)
    if(value == null) ParameterRequired(name).invalidNec
    else value.validNec
  }

  def readConfig: IO[TwilioConfig] = {
    (
      readEnvRequired("ACCOUNT_SID"),
      readEnvRequired("ACCOUNT_TOKEN"),
      readEnvRequired("PHONE_NUMBER")
    ).tupled.flatMap {
      _.mapN {
        case (accountSid, accountToken, fromNumber) => TwilioConfig(accountSid, accountToken, new PhoneNumber(fromNumber))
      }.fold(
        err => IO.raiseError[TwilioConfig](
          ConfigurationException(s"Parameters required but not provided: ${err.toList.map(_.name).mkString("[", ", ", "]")}")
        ),
        c => IO.pure(c)
      )
    }
  }

  def run(args: List[String]): IO[ExitCode] =
    (for {
      config <- readConfig
      exitcode <- Server.stream(config).compile.drain.as(ExitCode.Success)
    } yield {
      exitcode
    }).recoverWith {
      case ConfigurationException(msg) => IO {
        println(s"ERROR: $msg")
        ExitCode.Error
      }
    }
}