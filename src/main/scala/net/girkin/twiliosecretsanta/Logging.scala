package net.girkin.twiliosecretsanta

import cats.effect.Sync
import org.slf4j.LoggerFactory

trait Logging {
  private val logger = LoggerFactory.getLogger(this.getClass)

  def trace[F[_]: Sync](msg: String): F[Unit] = Sync[F].delay {
    logger.trace(msg)
  }

  def trace[F[_]: Sync](msg: String, error: Throwable): F[Unit] = Sync[F].delay {
    logger.trace(msg, error)
  }

  def debug[F[_]: Sync](msg: String): F[Unit] = Sync[F].delay {
    logger.debug(msg)
  }

  def debug[F[_]: Sync](msg: String, error: Throwable): F[Unit] = Sync[F].delay {
    logger.debug(msg, error)
  }

  def info[F[_]: Sync](msg: String): F[Unit] = Sync[F].delay {
    logger.info(msg)
  }

  def info[F[_]: Sync](msg: String, error: Throwable): F[Unit] = Sync[F].delay {
    logger.info(msg, error)
  }

  def warn[F[_]: Sync](msg: String): F[Unit] = Sync[F].delay {
    logger.warn(msg)
  }

  def warn[F[_]: Sync](msg: String, error: Throwable): F[Unit] = Sync[F].delay {
    logger.warn(msg, error)
  }

  def error[F[_]: Sync](msg: String): F[Unit] = Sync[F].delay {
    logger.error(msg)
  }

  def error[F[_]: Sync](msg: String, error: Throwable): F[Unit] = Sync[F].delay {
    logger.error(msg, error)
  }

}
