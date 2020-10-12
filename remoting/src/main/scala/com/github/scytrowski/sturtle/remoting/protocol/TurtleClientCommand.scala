package com.github.scytrowski.sturtle.remoting.protocol

import com.github.scytrowski.sturtle.core.TurtleQueryAnswer
import scodec.bits.BitVector
import scodec.{Attempt, Codec, Decoder, Encoder}
import shapeless.HNil

private[remoting] abstract class TurtleClientCommand

private[remoting] object TurtleClientCommand extends CoreCodecs {
  import scodec.codecs._

  val codec: Codec[TurtleClientCommand] = Codec(encoder, decoder)

  final case class JobScheduled(jobId: String) extends TurtleClientCommand
  final case class TurtleSelected(jobId: String) extends TurtleClientCommand
  final case class TurtleReleased(jobId: String) extends TurtleClientCommand
  final case class CommandRan(jobId: String) extends TurtleClientCommand
  final case class QueryExecuted(jobId: String, answer: TurtleQueryAnswer) extends TurtleClientCommand
  final case class NoTurtleSelected(jobId: String) extends TurtleClientCommand

  private def encoder: Encoder[TurtleClientCommand] = Encoder(encoderF)

  private def decoder: Decoder[TurtleClientCommand] =
    uint8.flatMap {
      case 1 => ascii32.as[JobScheduled]
      case 2 => ascii32.as[TurtleSelected]
      case 3 => ascii32.as[TurtleReleased]
      case 4 => ascii32.as[CommandRan]
      case 5 => (ascii32 :: queryAnswer).as[QueryExecuted]
      case 6 => ascii32.as[NoTurtleSelected]
    }

  private def encoderF: TurtleClientCommand => Attempt[BitVector] = {
    case JobScheduled(jobId) => (uint8 :: ascii32).encode(1 :: jobId :: HNil)
    case TurtleSelected(jobId) => (uint8 :: ascii32).encode(2 :: jobId :: HNil)
    case TurtleReleased(jobId) => (uint8 :: ascii32).encode(3 :: jobId :: HNil)
    case CommandRan(jobId) => (uint8 :: ascii32).encode(4 :: jobId :: HNil)
    case QueryExecuted(jobId, answer) => (uint8 :: ascii32 :: queryAnswer).encode(5 :: jobId :: answer :: HNil)
    case NoTurtleSelected(jobId) => (uint8 :: ascii32).encode(6 :: jobId :: HNil)
  }
}
