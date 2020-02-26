package org.mkmks.auction

import org.scalacheck.Properties
import org.scalacheck.Prop._
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

import scala.util.Random
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

  implicit lazy val arbNat: Arbitrary[Natural] = Arbitrary(for {
    n <- arbitrary[Int].suchThat (_ >= 0)
  } yield Natural(n))

  val genBidders = nonEmptyListOf(arbitrary[Int]).map(_.distinct)

  val genBid = (bidders: List[Int]) => for {
    price <- arbitrary[Natural]
    bidderId <- oneOf(bidders)
  } yield Bid(price, bidderId)

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

  // required to run folds in parallel, shows that winning price is known
  // deterministically while the winner is not
  property("updateAuctionStateAssociative") =
    forAllNoShrink (genBidders) { bidders: List[Int] =>
      forAll (genAuctionState(bidders)) { ac: AuctionState =>
        forAll (genBid(bidders), genBid(bidders)) { (bid1: Bid, bid2: Bid) =>
          val outcome1 = updateAuctionState(updateAuctionState(ac, bid1), bid2)
          val outcome2 = updateAuctionState(updateAuctionState(ac, bid2), bid1)
            (outcome1, outcome2) match {
            case (TwoOrMoreBids(bid11, bid12), TwoOrMoreBids(bid21, bid22)) =>
              bid12.price == bid22.price
            case (OneBid(bid1), OneBid(bid2)) =>
              bid1.price == bid2.price
            case (NoBids, NoBids) => true
            case _ => false
          }
        }
      }
    }

  // when all bid prices are distinct, the winner is known deterministically as well

  val genDistinctBids = (bidders: List[Int]) =>
    listOf(genBid(bidders)).map(_.distinctBy(_.price))

  property("allBidPricesDistinct") =
    forAll { reservePrice: Natural =>
      forAllNoShrink (genBidders) { bidders: List[Int] =>
        forAll (genDistinctBids(bidders)) { bids: List[Bid] =>
          auctionReasoned(reservePrice, emits(bids))
            .equals(auctionReasoned(reservePrice, emits(Random.shuffle(bids))))
        }
      }
    }
}
