package de.mk.aoc.a2

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import de.mk.util.Util

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Try

object Main extends IOApp {

  def matchesPerLine(s: String, i: Int): (Int, Int) = {
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
        (i, matches.size)
      case _ => (i, 0)
    }
  }

  @tailrec
  def loop(
      l: List[(Int, Int)],
      acc: ListBuffer[(Int, Int, Int)]
  ): ListBuffer[(Int, Int, Int)] = l match {
    case ::((currentIndex, wins), tail) =>
      val xs =
        (currentIndex + 1  to currentIndex + wins).toList.mapFilter { w =>
        acc.find(_._1 == w).map { case (i, w, n) =>
          (i, w, n + acc(currentIndex)._3)
        }
      }
      xs.foreach { case (i, w, n) => acc.update(i, (i, w, n)) }
      loop(tail, acc)
    case Nil => acc
  }

  override def run(args: List[String]): IO[ExitCode] = Util.getInput
    .use { bs =>
      val lines = bs.getLines().toList.zipWithIndex
      val matches = lines.map(t => matchesPerLine(t._1, t._2))
      val looped = loop(
        matches.sortBy(_._1),
        mutable.ListBuffer.from(matches.map(t => (t._1, t._2, 1))).sortBy(_._1)
      )
      IO.print(looped.map(_._3).sum)
    }
    .as(ExitCode.Success)
}
