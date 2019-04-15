package net.girkin.twiliosecretsanta

import org.slf4j.LoggerFactory

trait Logging[F[_]] {
  val logger = LoggerFactory.getLogger(this.getClass)
}
