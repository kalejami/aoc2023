package de.mk.aoc.a1

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import de.mk.util.Util

import scala.annotation.tailrec

object Main extends IOApp {
  final case class Card(t: String, v: Int)
  val cards = (2 to 14).toList.map {
    case 14 => Card("A", 14)
    case 13 => Card("K", 13)
    case 12 => Card("Q", 12)
    case 11 => Card("J", 11)
    case 10 => Card("T", 10)
    case x  => Card(x.toString, x)
  }

  final case class Hand(s: String) {
    private val hand = s.map(c => cards.find(_.t == c.toString).get)

    private val groupByCards: Iterable[IndexedSeq[Card]] =
      hand.groupBy(identity).view.values.toList.sortBy(_.size).reverse

    private val matchTuple =
      (groupByCards.head.size, groupByCards.tail.headOption.fold(0)(_.size))

    def compare(h: Hand): Int = if (matchTuple == h.matchTuple) {
      compareEachCard(0, 0, h)
    } else matchTuple.compare(h.matchTuple)

    @tailrec
    private def compareEachCard(index: Int, compare: Int, oh: Hand): Int =
      if (index > hand.size - 1) compare
      else {
        val hc = hand(index)
        val ohc = oh.hand(index)
        val c = hc.v.compare(ohc.v)
        if (c == 0) compareEachCard(index + 1, c, oh) else c
      }
  }

  def parseLine(s: String) = {
    val split = s.split(" ")
    (Hand(split(0)), split(1).toLong)
  }

  override def run(args: List[String]): IO[ExitCode] = Util.getInput
    .use { bs =>
      val lines = bs.getLines()
      val handsAndBids =
        lines.toList.map(parseLine).sortWith((t1, t2) => t1._1.compare(t2._1) < 0)
      val ranked = handsAndBids.zipWithIndex
      val winnings = ranked.map { case ((_, b), r) =>
        b * (r + 1)
      }.sum
      IO.println(winnings)
    }
    .as(ExitCode.Success)
}
