package org.mkmks.auction

import org.scalacheck.Properties
import org.scalacheck.Prop._
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

import spire.math.Natural
import fs2.Stream.emits
import Vickrey._
import Vickrey.AuctionState._

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

  property("providedExampleReasoned") =
    auctionReasoned(providedReservePrice, emits(pricesToBidList(providedExampleBids))).head
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

  val genBidders = nonEmptyListOf(arbitrary[Int]).map(_.distinct)

  val genBid = (bidders: List[Int]) => for {
    price <- arbitrary[Natural]
    bidderId <- oneOf(bidders)
  } yield Bid(price, bidderId)

  val genDistinctBids = (bidders: List[Int]) =>
    listOf(genBid(bidders)).map(_.distinctBy(_.price))

  property("littleSeasonedEquivDistinctBids") = forAll { reservePrice: Natural =>
    forAll (genBidders) { bidders: List[Int] =>
      forAll (genDistinctBids(bidders)) {
        bids: List[Bid] => auctionLittle(reservePrice, bids)
          .equals(auctionSeasoned(reservePrice, bids))
      }
    }
  }

  property("littleReasonedEquivDistinctBids") = forAll { reservePrice: Natural =>
    forAllNoShrink (genBidders) { bidders: List[Int] =>
      forAll (genDistinctBids(bidders)) {
        bids: List[Bid] => auctionLittle(reservePrice, bids)
          .equals(auctionReasoned(reservePrice, emits(bids)))
      }
    }
  }

  val genNoBids = const(NoBids)

  val genOneBid = (bidders: List[Int]) => for {
    bid <- genBid(bidders)
  } yield OneBid(bid)

  val genTwoOrMoreBids = (bidders: List[Int]) => for {
    bid1 <- genBid(bidders)
    bid2 <- genBid(bidders)
  } yield TwoOrMoreBids(bid1, bid2)

  def genAuctionState(bidders: List[Int]): Gen[AuctionState] =
    oneOf(genNoBids, genOneBid(bidders), genTwoOrMoreBids(bidders))

  // invariant 1
  property("auctionStateNoBids") =
    forAll (genBidders) { bidders: List[Int] =>
      forAll (genBid(bidders)) { bid: Bid =>
        updateAuctionState(NoBids, bid) match {
          case OneBid(_) => true
          case _ => false
        }
      }
    }

  // invariant 2
  property("auctionStateOnceTwoBidsAlwaysTwoBids") =
    forAllNoShrink (genBidders) { bidders: List[Int] =>
      forAll (genTwoOrMoreBids(bidders)) { ac: AuctionState =>
        forAll (genBid(bidders)) { (bid: Bid) =>
          updateAuctionState(ac, bid) match {
            case TwoOrMoreBids(_, _) => true
            case _ => false
          }
        }
      }
    }

  // invariant 3
  property("auctionStateBidsInPriceOrder") =
    forAll (genBidders) { bidders: List[Int] =>
      forAll (listOf(genBid(bidders))) { bids: List[Bid] =>
        bids.foldLeft(NoBids: AuctionState)(updateAuctionState) match {
          case TwoOrMoreBids(bid1, bid2) => bid1.price >= bid2.price
          case _ => true
        }
      }
    }

  // invariant 4
  property("auctionStateDistinctBidderNames") =
    forAll (genBidders) { bidders: List[Int] =>
      forAll (listOf(genBid(bidders))) { bids: List[Bid] =>
        bids.foldLeft(NoBids: AuctionState)(updateAuctionState) match {
          case TwoOrMoreBids(bid1, bid2) => bid1.bidderId != bid2.bidderId
          case _ => true
        }
      }
    }

  // needed to run folds in parallel, exposes "non-determinism"
  property("updateAuctionStateAssociative") =
    forAll (genBidders) { bidders: List[Int] =>
      forAll (genAuctionState(bidders))_ { ac: AuctionState =>
        forAll ((genBid(bidders), genBid(bidders))) { (bid1: Bid, bid2: Bid) =>
          updateAuctionState(updateAuctionState(ac, bid1), bid2)
          == updateAuctionState(updateAuctionState(ac, bid2), bid1)
        }
      }
    }
  
}
