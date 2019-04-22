package net.girkin.twiliosecretsanta

import cats.data.ValidatedNec
import cats.implicits._
import com.twilio.`type`.PhoneNumber
import scalaz.zio._
import scalaz.zio.interop.catz._

case class TwilioConfig(
  accountSid: String,
  accountToken: String,
  fromNumber: PhoneNumber
)

case class ConfigurationException(msg: String) extends Exception
case class ParameterRequired(name: String)

object Main {
  private def readEnvRequired(name: String): Task[ValidatedNec[ParameterRequired, String]] = Task {
    val value = System.getenv(name)
    if(value == null) ParameterRequired(name).invalidNec
    else value.validNec
  }

  def readConfig: Task[TwilioConfig] = {
    (
      readEnvRequired("ACCOUNT_SID"),
      readEnvRequired("ACCOUNT_TOKEN"),
      readEnvRequired("PHONE_NUMBER")
    ).tupled.flatMap {
      _.mapN {
        case (accountSid, accountToken, fromNumber) => TwilioConfig(accountSid, accountToken, new PhoneNumber(fromNumber))
      }.fold(
        err => Task.fail(
          ConfigurationException(s"Parameters required but not provided: ${err.toList.map(_.name).mkString("[", ", ", "]")}")
        ),
        c => Task.succeed(c)
      )
    }
  }

  def main(args: Array[String]): Unit = {
    val runtime = new DefaultRuntime {}
    runtime.unsafeRun {
      (for {
        config <- readConfig
        exitcode <- Server.stream(config).compile.drain.as(0)
      } yield {
        exitcode
      }).recoverWith {
        case ConfigurationException(msg) => Task {
          println(s"ERROR: $msg")
          -1
        }
      }
    }
  }
}