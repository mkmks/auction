package org.mkmks.auction

import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, exists, propBoolean}
import org.scalacheck.Gen
import org.scalacheck.Gen.listOf
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

import spire.math.Natural
import fs2.Stream.emits
import Vickrey.{Bid, pricesToBidList, auctionLittle, auctionSeasoned, auctionReasoned}

object VickreySpecification extends Properties("Vickrey") {

  /* The provided test case serves as a basic sanity check. */

  val providedExampleBids = List(
    List(Natural(110), Natural(130)),               // A
    List(),                                         // B
    List(Natural(125)),                             // C
    List(Natural(105), Natural(115), Natural(90)),  // D
    List(Natural(132), Natural(135), Natural(140))) // E

  val providedReservePrice = Natural(100)

  val providedExampleOutcome = (Natural(130), 4)

  property("providedExampleLittle") =
    auctionLittle(providedReservePrice, pricesToBidList(providedExampleBids)).head
      .equals(providedExampleOutcome)

  property("providedExampleSeasoned") =
    auctionSeasoned(providedReservePrice, pricesToBidList(providedExampleBids)).head
      .equals(providedExampleOutcome)

  /* At least two distinct bidders are needed to run a second-bid auction. If
   * there are fewer than two bidders, the auction must fail. Note that the list
   * generator takes care of the no-bidders case by generating empty lists. */

  implicit lazy val arbNat: Arbitrary[Natural] = Arbitrary(for {
    n <- arbitrary[Int].suchThat (_ >= 0)
  } yield Natural(n))

  val genBidTooFewBidders = for {
    price <- arbitrary[Natural]
  } yield Bid(price, 0)

  property("notEnoughBidsLittle") = forAll { reservePrice: Natural =>
    forAll (listOf(genBidTooFewBidders))  {
      bids: List[Bid] => auctionLittle(reservePrice, bids).isEmpty
    }
  }

  property("notEnoughBidsSeasoned") = forAll { reservePrice: Natural =>
    forAll (listOf(genBidTooFewBidders))  {
      bids: List[Bid] => auctionSeasoned(reservePrice, bids).isEmpty
    }
  }

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

  property("naiveSeasonedEquivDistinctBids") = forAll { reservePrice: Natural =>
    forAll(arbitrary[Int].suchThat (_ >= 2)) {
      numBidders: Int => forAll (listOf(genBid(numBidders)) suchThat distinctPrices) {
        bids: List[Bid] => auctionLittle(reservePrice, bids)
          .equals(auctionSeasoned(reservePrice, bids))
      }
    }
  }

  property("naiveReasonedEquivDistinctBids") = forAll { reservePrice: Natural =>
    forAll(arbitrary[Int].suchThat (_ >= 2)) {
      numBidders: Int => forAll (listOf(genBid(numBidders)) suchThat distinctPrices) {
        bids: List[Bid] => auctionLittle(reservePrice, bids)
          .equals(auctionReasoned(reservePrice, emits(bids)))
      }
    }
  }

}
