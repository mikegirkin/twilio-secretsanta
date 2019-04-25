package net.girkin.twiliosecretsanta

import com.twilio.`type`.PhoneNumber
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder, Json}

object JsonCodecs {
  implicit val phoneNumberDecoder: Decoder[PhoneNumber] = Decoder.decodeString.map {
    str => new PhoneNumber(str)
  }
  implicit val phoneNumberEncoder: Encoder[PhoneNumber] = Encoder.instance { ph => Json.fromString(ph.getEndpoint) }
  implicit val messageDataDecoder: Decoder[SecretSantaParticipant] = deriveDecoder[SecretSantaParticipant]
  implicit val messageDataEncoder: Encoder[SecretSantaParticipant] = deriveEncoder[SecretSantaParticipant]
  implicit val sendMessageRequestDecoder: Decoder[SecretSantaRequest] = deriveDecoder[SecretSantaRequest]
  implicit val sendMessageRequestEncoder: Encoder[SecretSantaRequest] = deriveEncoder[SecretSantaRequest]
  implicit val sendMessageResponseEncoder: Encoder[SendMessageResponse] = deriveEncoder[SendMessageResponse]

  implicit val requestValidationErrorEncoder: Encoder[RequestValidationError] = deriveEncoder[RequestValidationError]
  implicit val requestValidationErrorDencoder: Decoder[RequestValidationError] = deriveDecoder[RequestValidationError]

}
