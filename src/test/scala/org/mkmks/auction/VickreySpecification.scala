package org.mkmks.auction

import org.scalacheck.Properties
import spire.math.Natural
import Vickrey.pricesToBidList
import Vickrey.auctionNaive
import Vickrey.auctionSeasoned

object VickreySpecification extends Properties("Vickrey") {

  val providedExampleBids = List(
    List(Natural(110), Natural(130)),               // A
    List(),                                         // B
    List(Natural(125)),                             // C
    List(Natural(105), Natural(115), Natural(90)),  // D
    List(Natural(132), Natural(135), Natural(140))) // E

  val providedExampleOutcome = (Natural(130), 4)

  property("providedExampleNaive") =
    (auctionNaive compose pricesToBidList)(providedExampleBids).equals(providedExampleOutcome)

  property("providedExampleSeasoned") =
    (auctionSeasoned compose pricesToBidList)(providedExampleBids).equals(providedExampleOutcome)
}
