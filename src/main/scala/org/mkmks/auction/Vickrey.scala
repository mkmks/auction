package org.mkmks.auction


/* using natural numbers instead of integers avoids the need to test prices for positivity */
import spire.math.Natural
import spire.compat.ordering

object Vickrey {

  def auctionNaive(bids: List[List[Natural]]): (Natural, Int) = {
    val ranking = bids
      .zipWithIndex.map(bs => bs._1.map((_, bs._2))).flatten
      .sortBy(_._1).reverse
    val winner = ranking(0)._2
    (ranking.dropWhile(_._2 == winner).head._1, winner)
  }

}
