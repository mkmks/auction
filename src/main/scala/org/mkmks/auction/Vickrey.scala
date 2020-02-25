package org.mkmks.auction

import spire.math.Natural
import spire.compat.ordering
import collection.mutable.PriorityQueue
import fs2.{Stream, Pure}

  /** Here, we implement a sealed-bid, second-price auction, or a Vickrey auction,
    in several attempts, clarifying our reasoning in steps. */

object Vickrey {

   /* An auction is a decision procedure defined on bids. First, we must clarify
   what a "bid" is.
   
   The provided problem statement represents bids as lists of prices given by
   bidders but such a representation isn't granular enough: we'll need to
   compare prices that are bid by different bidders. */

   /** An individual bid is no more than a product of a price and a name of a
   bidder who put it forward.

     @constructor Creates a new bid with a price and a name of the bidder.
     @param price The bid price. It can't go below zero, therefore it's a
   natural number.
     @param bidderId There can be many ways to represent names. One operation on
   names that we'll need is equality. Representation of names as integers has
   that operation, and we don't care yet how those integers are assigned. We
   only care if they're different for different bidders.
   */

  final case class Bid(price: Natural, bidderId: Int);

  implicit val BidOrdering = Ordering.by[Bid, Natural](_.price)

  /** Takes lists of lists of prices to lists of [[Bid]]s.

    This function implicitly takes care of choosing bidders' names. They will be
integers, and we don't care yet for more than their distinctness.

    Time complexity: `O(n)`
    
    */

  val pricesToBidList = (bids: List[List[Natural]]) => bids
    .zipWithIndex.map(bs => bs._1.map(Bid(_, bs._2))).flatten

  /** Determines the auction outcome given a list of [[Bid]]s and a reserve price,
    * by sorting a list.

   Having chosen a list of [[Bid]]s as the data representation, it's only
   natural to apply some freshman-year functional programming to it.

   The outcome of a Vickrey auction is a pair of things: the second-largest bid
   price and the name of a bidder who bid the largest bid price. At the first
   glance, sorting the list of Bids in the price-descending order will let us
   find both: the sorted list's head will have the hame of the top bidder, and
   after popping enough top bidder bids from the list we'll get to the runner-up
   bidder and his second largest bid price.

   The time and space complexities of this implementation is dominated by choice
   of the sorting procedure, which often will be `O(n*log(n))`. Scala uses
   Timsort as the default sort on its collections. Its worst- and average case
   time complexities are `O(n*log(n))`, going down to `O(n)` in the best
   case. As Scala builds arrays from collections in its standard library
   collection sort, the space complexity is `O(n)`.

   @param reservePrice The minimum price for which the auction item can be sold.
   @param bids The list of [[Bid]]s to run the auction on.

   */

  def auctionLittle(reservePrice: Natural, bids: List[Bid]): Option[(Natural, Int)] =
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

  /** Determines the auction outcome given a list of [[Bid]]s and a reserve price,
    * by building a priority queue.

    The number of bids in an auction can be quite large, and collecting them all
    in a list for sorting feels suboptimal. The list might be larger than the
    available memory! With that in mind, it becomes natural to throw some
    sophomore-year data structures at the problem.

    The equivalence between sorting algorithms and priority queues is
    well-known. By going from sorting a list of [[Bid]]s to inserting them into
    a priority queue we can cut the time complexity down from `O(n*log(n))` to
    `O(n)`, thanks to insertion in `O(1)` amortized time that priority queues
    implemented, say, with a Fibonacci heap, have.

    @param reservePrice The minimum price for which the auction item can be sold.
    @param bids The list of [[Bid]]s to run the auction on.

   */

  def auctionSeasoned(reservePrice: Natural, bids: List[Bid]): Option[(Natural, Int)] =
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

  /** Keeps all the information to report the auction outcome if there weren't any
    * more bids than those already processed.

     We only care about the two top bids made. We aim to represent no more and
     no less, so we introduce a convenient data structure. It must maintain an
     invariant that if it stores two bids, they're ordered by price. */

  sealed abstract class AuctionState

  object AuctionState {
    final case object NoBids extends AuctionState
    final case class  OneBid(bid: Bid) extends AuctionState
    final case class  TwoOrMoreBids(bid1: Bid, bid2: Bid) extends AuctionState
  }

  import AuctionState._

  // FIXME: use the Ordering implicit defined above to do price comparison

  /** Given two top bids in a running auction, determines if an incoming bid
    * should deprive any of them of their place.

    Time complexity: O(1)

    */

  val updateAuctionState = (acc: AuctionState, newbid: Bid) => acc match {
    case NoBids => OneBid(newbid)
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

  /** Determines the auction outcome given a list of [[Bid]]s and a reserve price,
    * by folding a stream.

    @param reservePrice The minimum price for which the auction item can be sold.
    @param bids The list of [[Bid]]s to run the auction on.

   */

  def auctionReasoned(reservePrice: Natural,
                              bids: Stream[Pure, Bid]): Option[(Natural, Int)] =
    bids.fold(NoBids: AuctionState)(updateAuctionState).toList.head match {
      case NoBids => None
      case OneBid(bid) =>
        Some(if (bid.price > reservePrice) bid.price else reservePrice, bid.bidderId)
      case TwoOrMoreBids(bid1, bid2) =>
        Some(if (bid2.price > reservePrice) bid2.price else reservePrice,
          bid1.bidderId)
    }

}
