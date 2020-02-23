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

  val genBid = for {
    price <- arbitrary[Natural]
    numBidders <- arbitrary[Int].suchThat (_ >= 0)
    bidderId <- Gen.choose(0, numBidders)
  } yield Bid(price, bidderId)

  property("naiveSeasonedEquiv") = forAll (listOf(genBid)) { bids: List[Bid] =>
      auctionNaive(bids).equals(auctionSeasoned(bids))
  }

  property("shuffleSeasoned") = exists(listOf(genBid)) { bids: List[Bid] =>
    auctionSeasoned(bids) != auctionSeasoned(shuffle(bids))
  }

}
