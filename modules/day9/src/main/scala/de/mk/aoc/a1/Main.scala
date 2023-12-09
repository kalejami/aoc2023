package de.mk.aoc.a1

import cats.effect.{ExitCode, IO, IOApp}
import de.mk.util.Util

import scala.annotation.tailrec

object Main extends IOApp {
  final case class History(l: List[Long]) {
    def nextValue: Long = {
      val depths = interpolate(l, 1, List((0L, l)))
      depths.foldLeft(0L) { case (acc, (_, distances)) =>
        if (acc == 0L) distances.last
        else {
          acc + distances.last
        }
      }
    }

    @tailrec
    private def interpolate(
        current: List[Long],
        depth: Long,
        acc: List[(Long, List[Long])]
    ): List[(Long, List[Long])] = {
      val nextDistances = current
        .sliding(2)
        .foldLeft(List.empty[Long])((acc, xs) => (xs.last - xs.head) +: acc)
        .reverse
      val newAcc = (depth, nextDistances) +: acc
      if (nextDistances.distinct.sizeIs == 1) newAcc
      else interpolate(nextDistances, depth + 1, newAcc)
    }

  }

  def parseLine(l: String): History =
    History("-*\\d+".r.findAllMatchIn(l).toList.map(_.group(0).toLong))

  override def run(args: List[String]): IO[ExitCode] =
    Util.getInput
      .use { bs =>
        val lines = bs.getLines().toList
        val hs = lines.map(parseLine)
        IO.println(hs.map(_.nextValue).sum)
      }
      .as(ExitCode.Success)
}
