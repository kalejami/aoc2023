package de.mk.aoc.a2

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import de.mk.aoc.a1.Main.{parseLines, translate}
import de.mk.util.Util

object Main extends IOApp {

  def getSmallestLocations(l: List[(Long, List[List[Long]])]) = {
    val tail = l.tail
    l.head._2.flatten.sliding(2, 2).toList.parTraverse {
      case ::(start, ::(end, _)) => sLoop(start, end, tail)
      case _ => IO(Long.MaxValue)
    }
  }

  private def sLoop(
      start: Long,
      end: Long,
      tail: List[(Long, List[List[Long]])]
  ) = IO {
    var smallestLocation = Long.MaxValue
    var index = start
    while (index < start + end) {
      val location =
        tail.sortBy(_._1).foldLeft(index) { case (value, (_, mappings)) =>
          translate(value, mappings, value)
        }
      if (location < smallestLocation) smallestLocation = location
      index = index + 1
    }
    smallestLocation
  }

  override def run(args: List[String]): IO[ExitCode] = Util.getInput
    .use { bs =>
      val it = bs.getLines()
      val lists = parseLines(it, 0, (0 to 7).toList.map(i => (i, List.empty)))
      getSmallestLocations(lists).flatMap(l => IO.println(l.min))
    }
    .as(ExitCode.Success)
}
