package com.github.scytrowski.sturtle.remoting.mock

import java.net.InetSocketAddress

import cats.effect.IO
import cats.effect.concurrent.Ref
import com.github.scytrowski.sturtle.remoting.mock.TestTurtleServerSideClient.Data
import com.github.scytrowski.sturtle.remoting.net.TurtleClient
import com.github.scytrowski.sturtle.remoting.protocol.{TurtleClientCommand, TurtleServerCommand}

final class TestTurtleServerSideClient(data: Ref[IO, Data]) extends TurtleClient[IO, TurtleServerCommand, TurtleClientCommand] {
  override val id: String = "client"

  override val address: InetSocketAddress = new InetSocketAddress("127.0.0.1", 0)

  override def receive: IO[Option[TurtleServerCommand]] =
    data.modify { d =>
      d.in match {
        case head :: tail => d.copy(in = tail) -> Some(head)
        case Nil => d -> None
      }
    }

  override def send(command: TurtleClientCommand): IO[Unit] =
    data.update(d => d.copy(out = d.out :+ command))
}

object TestTurtleServerSideClient {
  final case class Data(in: List[TurtleServerCommand] = Nil,
                        out: List[TurtleClientCommand] = Nil)
}
