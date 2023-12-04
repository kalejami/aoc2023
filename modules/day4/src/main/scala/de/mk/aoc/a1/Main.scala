package de.mk.aoc.a1

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import de.mk.util.Util

import scala.util.Try

object Main extends IOApp {

  def matchesPerLine(s: String): Int = {
    val relevant = s.substring(s.indexOf(":"))
    relevant.split("""\|""").toList match {
      case ::(head, ::(next, _)) =>
        val r = "\\d+".r

        val winningNums = r
          .findAllMatchIn(head)
          .toList
          .mapFilter(m => Try(m.group(0).toInt).toOption)

        val tips = r
          .findAllMatchIn(next)
          .toList
          .mapFilter(m => Try(m.group(0).toInt).toOption)

        val matches = winningNums.intersect(tips)

        val points =
          if (matches.isEmpty) 0
          else
            matches.zipWithIndex.map { case (_, index) =>
              if (index == 0) 1 else 2
            }.product
        points
      case _ => 0
    }
  }

  override def run(args: List[String]): IO[ExitCode] =
    Util.getInput
      .use { bs =>
        val lines = bs.getLines().toList
        val matches = lines.map(matchesPerLine)
        IO.print(matches.sum)
      }
      .as(ExitCode.Success)
}
