package de.mk.aoc.a2

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import de.mk.util.Util

import scala.annotation.tailrec

object Main extends IOApp {
  final case class Galaxy(y: Int, x: Int) {
    def distance(g: Galaxy): Long = Math.abs(y - g.y) + Math.abs(x - g.x)
  }

  private val millionMarker = '*'

  override def run(args: List[String]): IO[ExitCode] = Util.getInput
    .use { bs =>
      val lines = bs.getLines().toList
      val markedU = mark1M(lines)

      val galaxies = markedU.zipWithIndex.flatMap { case (line, y) =>
        line.zipWithIndex.toList.mapFilter { case (c, x) =>
          if (c == '#') {
            val offset = 1_000_000 - 1
            val mX = line.substring(0, x).count(_ == millionMarker)
            val mY = markedU.take(y).count(_.charAt(x) == millionMarker)
            Some(Galaxy(y + mY * offset, x + mX * offset))
          } else None
        }
      }

      val sum = galaxies.map { g =>
        galaxies.map { g2 =>
          g2.distance(g)
        }.sum
      }.sum

      IO.println(sum / 2)
    }
    .as(ExitCode.Success)

  private def mark1M(lines: List[String]) = {
    val expandLines =
      lines.map(l => if (l.forall(_ == '.')) l.replaceAll(".", "*") else l)

    @tailrec
    def expandLoop(i: Int, acc: List[String]): List[String] =
      if (i >= acc.head.length) acc
      else {
        if (acc.forall(s => s.charAt(i) == '.' || s.charAt(i) == millionMarker)) {
          val expanded = acc.map { s =>
            val (pre, post) = s.splitAt(i)
            pre + millionMarker + post.tail
          }
          expandLoop(i + 2, expanded)
        } else expandLoop(i + 1, acc)
      }

    expandLoop(0, expandLines)
  }
}
