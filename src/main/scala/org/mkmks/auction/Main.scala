package org.mkmks.auction

import cats.effect.{Blocker, ExitCode, IO, IOApp, Resource}
import cats.implicits._
import fs2.{io, text, Stream}
import java.nio.file.Paths
import spire.math.Natural
import Vickrey.auctionLittle

object Main extends IOApp {
  def bids(input: String): Stream[IO, Unit] = Stream.resource(Blocker[IO]).flatMap { blocker =>
    io.file.readAll[IO](Paths.get(input), blocker, 4096)
      .through(text.utf8Decode)
      .through(text.lines)
      .map((x: String) => x.split(",").map((y: String) => Natural(y.toInt)))
  }

  def run(args: List[String]): IO[ExitCode] =
    if (args.length != 1) {
      println("Usage: sbt \"run inputfile\"")
      IO.pure(ExitCode.Error)
    } else {
      bids(args(0)).compile.drain.as(ExitCode.Success)
    }
}
