package org.mkmks.auction

import cats.effect.{Blocker, ExitCode, IO, IOApp, Resource}
import cats.implicits._
import fs2.{io, text, Stream}
import java.nio.file.Paths
import spire.math.Natural
import Vickrey._
import Vickrey.AuctionState._

object Main extends IOApp {

  // reimplements `Vickrey.pricesToBidList` because the application reads streams not lists
  def bids(input: String): Stream[IO, Bid] = Stream.resource(Blocker[IO]).flatMap { blocker =>
    io.file.readAll[IO](Paths.get(input), blocker, 4096)
      .through(text.utf8Decode)
      .through(text.lines)
      .map((x: String) => x.split(",").filter(! _.equals("")).map((y: String) => Natural(y.toInt)))
      .zipWithIndex
      .flatMap(bs => Stream.emits(bs._1.map(Bid(_, bs._2))))
  }

  // FIXME: validate that reservePrice parses into a natural and inputFile into a path
  def run(args: List[String]): IO[ExitCode] =
    if (args.length != 2) {
      println("Usage: sbt \"run reservePrice inputFile\"")
      IO.pure(ExitCode.Error)
    } else {
      bids(args(1))
        .fold(NoBids: AuctionState)(updateAuctionState)
        .map(x => interpretAuctionState(x, Natural(args(0).toInt))
          .fold(println("There was no winner."))
          (y => println("Bidder " + y._2 + " buys the item at the price " + y._1))
        )
        .compile.drain.as(ExitCode.Success)
    }
}
