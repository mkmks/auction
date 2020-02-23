package org.mkmks.auction


/* using natural numbers instead of integers avoids the need to test prices for positivity */
import spire.math.Natural
import spire.compat.ordering
import collection.mutable.PriorityQueue

object Vickrey {

  case class Bid(price: Natural, bidderId: Int);

  implicit val BidOrdering = Ordering.by[Bid, Natural](_.price)

  val pricesToBidList = (bids: List[List[Natural]]) => bids
    .zipWithIndex.map(bs => bs._1.map(Bid(_, bs._2))).flatten

  val auctionNaive = (bids: List[Bid]) => {
    bids match {
      case _ :: _ => {
        val ranking = bids.sortBy(_.price).reverse
        val winner = ranking(0).bidderId
        ranking.dropWhile(_.bidderId == winner) match {
          case x :: _ => Some (x.price, winner)
          case _ => None
        }
      }
      case _ => None
    }
  }

  val auctionSeasoned = (bids: List[Bid]) => {
    if (bids.nonEmpty) {
      val queue = new PriorityQueue() ++ bids
      val winner = queue.max.bidderId
      while (queue.nonEmpty && queue.max.bidderId == winner) {
        queue.dequeue
      }
      if (queue.nonEmpty)
        Some (queue.max.price, winner)
      else None
    }
    else None
  }

}
