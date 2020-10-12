package com.github.scytrowski.sturtle.remoting.mock

import java.net.InetSocketAddress

import cats.effect.IO
import cats.effect.concurrent.Ref
import com.github.scytrowski.sturtle.remoting.net.TurtleClient
import com.github.scytrowski.sturtle.remoting.protocol.{TurtleClientCommand, TurtleServerCommand}

final class TestTurtleClientSideClient(data: Ref[IO, TestTurtleClientSideClient.Data]) extends TurtleClient[IO, TurtleClientCommand, TurtleServerCommand] {
  override val id: String = "client"

  override val address: InetSocketAddress = new InetSocketAddress("127.0.0.1", 0)

  override def receive: IO[Option[TurtleClientCommand]] =
    data.modify { d =>
      d.in match {
        case head :: tail => d.copy(in = tail) -> Some(head)
        case Nil => d -> None
      }
    }

  override def send(command: TurtleServerCommand): IO[Unit] =
    data.update(d => d.copy(out = d.out :+ command))
}

object TestTurtleClientSideClient {
  final case class Data(in: List[TurtleClientCommand] = Nil,
                        out: List[TurtleServerCommand] = Nil)
}
