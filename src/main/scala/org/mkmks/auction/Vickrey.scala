package org.mkmks.auction

import spire.math.Natural
import spire.compat.ordering
import collection.mutable.PriorityQueue
import fs2.{Stream, Pure}

object Vickrey {

  /* Here, we implement a sealed-bid, second-price auction, or a Vickrey auction,
   * in several attempts, clarifying our reasoning in steps.

   An auction is a decision procedure defined on bids. First, we must clarify
   what a "bid" is.
   
   The provided problem statement represents bids as lists of prices given by
   bidders but such a representation isn't granular enough: we'll need to
   compare prices that are bid by different bidders.

   An individual bid is no more than a product of a price (that can't go below
   zero, therefore it's a natural number) and a name of a bidder who put
   it forward.

   There can be many ways to represent names. One operation on names that we'll
   need is equality. Representation of names as integers has that operation, and
   we don't care yet how those integers are assigned. We only care if they're
   different for different bidders.
   */

  case class Bid(price: Natural, bidderId: Int);

  implicit val BidOrdering = Ordering.by[Bid, Natural](_.price)

  /* Having agreed that the auction will work with collections of individual Bids,
   * we'll need a way to go from lists of lists of prices, as exemplified in the
   * problem statement, to lists of Bids. The function we implement for this
   * need must take care of choosing bidders' names. Again, they will be
   * integers, and we don't care yet for more than their distinctness. */

  val pricesToBidList = (bids: List[List[Natural]]) => bids
    .zipWithIndex.map(bs => bs._1.map(Bid(_, bs._2))).flatten

  /* Having chosen a list of Bids as the data representation, it's only natural to
   * apply some freshman-year functional programming to it.

   The outcome of a Vickrey auction is a pair of things: the second-largest bid
   price and the name of a bidder who bid the largest bid price. At the first
   glance, sorting the list of Bids in the price-descending order will let us
   find both: the sorted list's head will have the hame of the top bidder, and
   after popping enough top bidder bids from the list we'll get to the runner-up
   bidder and his second largest bid price.

   The time and space complexities of this implementation is dominated by choice
   of the sorting procedure, which often will be O(n*log(n)). Scala uses Timsort
   as the default sort on its collections. Its worst- and average case time
   complexities are O(n*log(n)), going down to O(n) in the best case. As Scala
   builds arrays from collections in its standard library collection sort, the
   space complexity is O(n).

   All other parts of this implementation, such as transforming lists of lists
   of prices to lists of Bids, have time complexity of O(n) or better.

   Before we move on to explore if there can be a more efficient implementation,
   there are some corner cases to take care of.

   */

  val auctionLittle = (reservePrice: Natural, bids: List[Bid]) => {
    bids match {
      case _ :: _ => {
        val ranking = bids.sortBy(_.price).reverse
        val winner = ranking(0).bidderId
        ranking.dropWhile(_.bidderId == winner) match {
          case x :: _ => Some (if (x.price > reservePrice) x.price else reservePrice, winner)
          case _ => None
        }
      }
      case _ => None
    }
  }

  val auctionSeasoned = (reservePrice: Natural, bids: List[Bid]) => {
    if (bids.nonEmpty) {
      val queue = new PriorityQueue() ++ bids
      val winner = queue.max.bidderId
      while (queue.nonEmpty && queue.max.bidderId == winner) {
        queue.dequeue
      }
      if (queue.nonEmpty)
        Some (if (queue.max.price > reservePrice) queue.max.price else reservePrice, winner)
      else None
    }
    else None
  }

  /* We only care about the two top bids made. We aim to represent no more and no
   * less, so we introduce a convenient data structure. It must maintain an
   * invariant that if it stores two bids, they're ordered by price. */

  sealed trait AuctionState

  object AuctionState {
    final case class NoBids() extends AuctionState
    final case class OneBid(bid: Bid) extends AuctionState
    final case class TwoOrMoreBids(bid1: Bid, bid2: Bid) extends AuctionState
  }

  import AuctionState._

  // FIXME: use the Ordering implicit defined above to do price comparison

  val updateAuctionState = (acc: AuctionState, newbid: Bid) => acc match {
    case NoBids() => AuctionState.OneBid(newbid)
    case OneBid(oldbid: Bid) =>
      if (oldbid.price < newbid.price)
        TwoOrMoreBids(newbid, oldbid)
      else
        TwoOrMoreBids(oldbid, newbid)
    case TwoOrMoreBids(bid1, bid2) =>
      if (bid1.price < newbid.price && bid2.price < newbid.price)
        TwoOrMoreBids(newbid, bid1)
      else if (bid1.price > newbid.price && bid2.price < newbid.price)
        TwoOrMoreBids(bid1, newbid)
      else acc
  }

  val auctionReasoned = (reservePrice: Natural, bids: Stream[Pure, Bid]) =>
    bids.fold(NoBids(): AuctionState)(updateAuctionState).toList.head match {
      case NoBids() => None
      case OneBid(bid) =>
        Some(if (bid.price > reservePrice) bid.price else reservePrice, bid.bidderId)
      case TwoOrMoreBids(bid1, bid2) =>
        Some((if (bid2.price > reservePrice) bid2.price else reservePrice,
          bid1.bidderId))
    }

}
