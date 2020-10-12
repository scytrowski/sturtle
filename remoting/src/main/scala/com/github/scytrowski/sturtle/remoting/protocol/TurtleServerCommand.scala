package com.github.scytrowski.sturtle.remoting.protocol

import com.github.scytrowski.sturtle.core.{TurtleCommand, TurtleQuery}
import scodec.bits.BitVector
import scodec.{Attempt, Codec, Decoder, Encoder}
import shapeless.HNil

private[remoting] sealed abstract class TurtleServerCommand

private[remoting] object TurtleServerCommand extends CoreCodecs {
  import scodec.codecs._

  val codec: Codec[TurtleServerCommand] = Codec(encoder, decoder)

  final case class SelectTurtle(turtleId: String) extends TurtleServerCommand
  case object ReleaseTurtle extends TurtleServerCommand
  final case class RunCommand(command: TurtleCommand) extends TurtleServerCommand
  final case class ExecuteQuery(query: TurtleQuery) extends TurtleServerCommand

  private def encoder: Encoder[TurtleServerCommand] = Encoder(encoderF)

  private def decoder: Decoder[TurtleServerCommand] =
    uint8.flatMap {
      case 1 => ascii32.as[SelectTurtle]
      case 2 => Decoder.point(ReleaseTurtle)
      case 3 => command.as[RunCommand]
      case 4 => query.as[ExecuteQuery]
    }

  private def encoderF: TurtleServerCommand => Attempt[BitVector] = {
    case SelectTurtle(turtleId) => (uint8 :: ascii32).encode(1 :: turtleId :: HNil)
    case ReleaseTurtle => uint8.encode(2)
    case RunCommand(c) => (uint8 :: command).encode(3 :: c :: HNil)
    case ExecuteQuery(q) => (uint8 :: query).encode(4 :: q :: HNil)
  }
}
