package com.github.scytrowski.sturtle.remoting.protocol

import com.github.scytrowski.sturtle.core.TurtleCommand._
import com.github.scytrowski.sturtle.core.TurtleQuery._
import com.github.scytrowski.sturtle.core.TurtleQueryAnswer._
import com.github.scytrowski.sturtle.core.{PenState, TurtleCommand, TurtleQuery, TurtleQueryAnswer}
import com.github.scytrowski.sturtle.geometry.{Angle, Path, Point, Vector}
import com.github.scytrowski.sturtle.graphics.Color
import scodec.bits.BitVector
import scodec.{Attempt, Codec, Decoder, Encoder}
import shapeless.{::, HNil}

private[protocol] trait CoreCodecs {
  import scodec.codecs._

  protected def command: Codec[TurtleCommand] = Codec(commandEncoder, commandDecoder)

  protected def query: Codec[TurtleQuery] = uint8.xmapc(intToTurtleQuery)(turtleQueryToInt)

  protected def queryAnswer: Codec[TurtleQueryAnswer] = Codec(queryAnswerEncoder, queryAnswerDecoder)

  protected def penState: Codec[PenState] = bool.xmapc(boolToPenState)(penStateToBool)

  protected def point: Codec[Point] = (double :: double)
    .xmapc { case x :: y :: HNil => Point.cartesian(x, y) } (p => p.x :: p.y :: HNil)

  protected def vector: Codec[Vector] = (double :: double)
    .xmapc { case dx :: dy :: HNil => Vector.cartesian(dx, dy) } (v => v.dx :: v.dy :: HNil)

  protected def angle: Codec[Angle] = double.xmapc(Angle.radians)(_.value)

  protected def path: Codec[Path] = list(point)
    .xmapc(_.foldLeft(Path.empty)(_ ~> _))(_.points)

  protected def color: Codec[Color] = (short16 :: short16 :: short16)
    .xmapc { case r :: g :: b :: HNil => Color.rgb(r, g, b) } (c => c.red :: c.green :: c.blue :: HNil)

  private def commandEncoder: Encoder[TurtleCommand] = Encoder(commandEncoderF)

  private def commandDecoder: Decoder[TurtleCommand] =
    uint8.flatMap {
      case 1 => point.as[MoveTo]
      case 2 => vector.as[MoveBy]
      case 3 => double.as[MoveForward]
      case 4 => double.as[MoveBackward]
      case 5 => angle.as[RotateTo]
      case 6 => angle.as[RotateLeftBy]
      case 7 => angle.as[RotateRightBy]
      case 8 => Decoder.point(Fill)
      case 9 => Decoder.point(ClearPath)
      case 10 => Decoder.point(PenDown)
      case 11 => Decoder.point(PenUp)
      case 12 => color.as[SetPenColor]
      case 13 => color.as[SetFillColor]
    }

  private def queryAnswerEncoder: Encoder[TurtleQueryAnswer] = Encoder(queryAnswerEncoderF)

  private def queryAnswerDecoder: Decoder[TurtleQueryAnswer] =
    uint8.flatMap {
      case 1 => point.as[PositionAnswer]
      case 2 => angle.as[AngleAnswer]
      case 3 => path.as[PathAnswer]
      case 4 => penState.as[PenStateAnswer]
      case 5 => color.as[PenColorAnswer]
      case 6 => color.as[FillColorAnswer]
    }

  private def commandEncoderF: TurtleCommand => Attempt[BitVector] = {
    case MoveTo(position) => (uint8 :: point).encode(1 :: position :: HNil)
    case MoveBy(v)   => (uint8 :: vector).encode(2 :: v :: HNil)
    case MoveForward(radius) => (uint8 :: double).encode(3 :: radius :: HNil)
    case MoveBackward(radius) => (uint8 :: double).encode(4 :: radius :: HNil)
    case RotateTo(a) => (uint8 :: angle).encode(5 :: a :: HNil)
    case RotateLeftBy(a) => (uint8 :: angle).encode(6 :: a :: HNil)
    case RotateRightBy(a) => (uint8 :: angle).encode(7 :: a :: HNil)
    case Fill => uint8.encode(8)
    case ClearPath => uint8.encode(9)
    case PenDown => uint8.encode(10)
    case PenUp => uint8.encode(11)
    case SetPenColor(c) => (uint8 :: color).encode(12 :: c :: HNil)
    case SetFillColor(c) => (uint8 :: color).encode(13 :: c :: HNil)
  }

  private def queryAnswerEncoderF: TurtleQueryAnswer => Attempt[BitVector] = {
    case PositionAnswer(position) => (uint8 :: point).encode(1 :: position :: HNil)
    case AngleAnswer(a) => (uint8 :: angle).encode(2 :: a :: HNil)
    case PathAnswer(p) => (uint8 :: path).encode(3 :: p :: HNil)
    case PenStateAnswer(ps) => (uint8 :: penState).encode(4 :: ps :: HNil)
    case PenColorAnswer(c) => (uint8 :: color).encode(5 :: c :: HNil)
    case FillColorAnswer(c) => (uint8 :: color).encode(6 :: c :: HNil)
  }

  private def turtleQueryToInt(query: TurtleQuery): Int =
    query match {
      case GetPosition => 1
      case GetAngle => 2
      case GetPath => 3
      case GetPenState => 4
      case GetPenColor => 5
      case GetFillColor => 6
    }

  private def intToTurtleQuery(v: Int): TurtleQuery =
    v match {
      case 1 => GetPosition
      case 2 => GetAngle
      case 3 => GetPath
      case 4 => GetPenState
      case 5 => GetPenColor
      case 6 => GetFillColor
    }

  private def penStateToBool(penState: PenState): Boolean =
    penState match {
      case PenState.Down => true
      case PenState.Up   => false
    }

  private def boolToPenState(b: Boolean): PenState =
    if (b) PenState.Down else PenState.Up
}
