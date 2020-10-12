package com.github.scytrowski.sturtle.remoting.net

import java.net.InetSocketAddress

import cats.data.OptionT
import cats.effect.Sync
import cats.syntax.flatMap._
import cats.syntax.functor._
import fs2.io.tcp.Socket
import fs2.{Chunk, Stream}
import io.chrisdavenport.log4cats.StructuredLogger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import scodec.bits.{BitVector, ByteVector}
import scodec.{Codec, Decoder, Encoder}

private[remoting] trait TurtleClient[F[_], In, Out] {
  def id: String

  def address: InetSocketAddress

  final def receives: Stream[F, In] =
    Stream
      .repeatEval(receive)
      .collectWhile { case Some(command) => command }

  def receive: F[Option[In]]

  def send(command: Out): F[Unit]
}

private[remoting] final class TcpTurtleClient[F[_]: Sync, In, Out](val id: String,
                                                 socket: Socket[F],
                                                 val address: InetSocketAddress,
                                                 decoder: Decoder[In],
                                                 encoder: Encoder[Out]) extends TurtleClient[F, In, Out] {
  def receive: F[Option[In]] =
    receiveExact(2)
      .semiflatTap(bytes => logger.debug(mdc)(s"Input command length bytes: ${bytes.toHex}"))
      .semiflatMap(decode[Int](commandLengthCodec, _))
      .semiflatTap(commandLength => logger.debug(mdc)(s"Input command length: $commandLength"))
      .flatMap(receiveExact)
      .semiflatTap(bytes => logger.debug(mdc)(s"Input command bytes: ${bytes.toHex}"))
      .semiflatMap(decode[In](decoder, _))
      .semiflatTap(command => logger.debug(mdc)(s"Received input command: $command"))
      .value

  def send(out: Out): F[Unit] =
    for {
      _ <- logger.debug(mdc)(s"Sending output command: $out")
      commandBytes <- encode(encoder, out)
      commandLengthBytes <- encode(commandLengthCodec, commandBytes.length.toInt)
      _ <- logger.debug(mdc)(s"Output command length bytes: ${commandLengthBytes.toHex}")
      _ <- logger.debug(mdc)(s"Output command bytes: ${commandBytes.toHex}")
      _ <- sendBytes(commandLengthBytes)
      _ <- sendBytes(commandBytes)
      _ <- logger.debug(mdc)(s"Output command $out has been sent")
    } yield ()

  private def receiveExact(count: Int): OptionT[F, BitVector] =
    OptionT(socket.readN(count))
      .map(_.toBitVector)

  private def sendBytes(bytes: ByteVector): F[Unit] =
    socket.write(Chunk.byteVector(bytes))

  private def decode[A](decoder: Decoder[A], bits: BitVector): F[A] =
    Sync[F].fromTry(decoder.decode(bits).toTry.map(_.value))

  private def encode[A](encoder: Encoder[A], a: A): F[ByteVector] =
    Sync[F].fromTry(encoder.encode(a).toTry.map(_.toByteVector))

  private val mdc: Map[String, String] = Map("clientId" -> id)

  private val commandLengthCodec: Codec[Int] = scodec.codecs.int16

  private val logger: StructuredLogger[F] = Slf4jLogger.getLogger
}
