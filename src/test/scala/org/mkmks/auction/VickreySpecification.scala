package org.mkmks.auction

import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, exists, propBoolean}
import org.scalacheck.Gen
import org.scalacheck.Gen.listOf
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

import spire.math.Natural
import Vickrey.{Bid, pricesToBidList, auctionNaive, auctionSeasoned}

object VickreySpecification extends Properties("Vickrey") {

  /* The provided test case serves as a basic sanity check. */

  val providedExampleBids = List(
    List(Natural(110), Natural(130)),               // A
    List(),                                         // B
    List(Natural(125)),                             // C
    List(Natural(105), Natural(115), Natural(90)),  // D
    List(Natural(132), Natural(135), Natural(140))) // E

  val providedExampleOutcome = (Natural(130), 4)

  property("providedExampleNaive") =
    (auctionNaive compose pricesToBidList)(providedExampleBids).head
      .equals(providedExampleOutcome)

  property("providedExampleSeasoned") =
    (auctionSeasoned compose pricesToBidList)(providedExampleBids).head
      .equals(providedExampleOutcome)

  /* At least two distinct bidders are needed to run a second-bid auction. If
   * there are fewer than two bidders, the auction must fail. Note that the list
   * generator takes care of the no-bidders case by generating empty lists. */

  val genBidTooFewBidders = for {
    price <- arbitrary[Natural]
  } yield Bid(price, 0)

  property("notEnoughBidsNaive") = forAll (listOf(genBidTooFewBidders))  {
    bids: List[Bid] => auctionNaive(bids).isEmpty
  }

  property("notEnoughBidsSeasoned") = forAll (listOf(genBidTooFewBidders))  {
    bids: List[Bid] => auctionSeasoned(bids).isEmpty
  }

  implicit lazy val arbNat: Arbitrary[Natural] = Arbitrary(for {
    n <- arbitrary[Int].suchThat (_ >= 0)
  } yield Natural(n))

  /* When identical prices are bid, the auction behaviour is not specified. It
   * isn't known then which of the two (or more) bidders who proposed the same
   * price wins. When it's not the case, two different implementations of the
   * auction should behave identically. */

  val genBid = (numBidders: Int) =>  for {
    price <- arbitrary[Natural]
    bidderId <- Gen.choose(0, numBidders)
  } yield Bid(price, bidderId)

  val distinctPrices = (bids: List[Bid]) => {
    val prices = bids.map(_.price)
    prices == prices.distinct
  }

  property("naiveSeasonedEquivDistinctBids") = forAll(arbitrary[Int].suchThat (_ >= 2)) {
    numBidders: Int => forAll (listOf(genBid(numBidders)) suchThat distinctPrices) {
      bids: List[Bid] => auctionNaive(bids).equals(auctionSeasoned(bids))
    }
  }

}
