package de.mk.aoc.a1

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.toTraverseOps
import de.mk.util.Util

import scala.annotation.tailrec

object Main extends IOApp {
  @tailrec
  def expandLoop(i: Int, acc: List[String]): List[String] =
    if(i >= acc.head.length) acc
    else {
      if(acc.forall(_.charAt(i) == '.')) {
        val expanded = acc.map { s =>
          val (pre, post) = s.splitAt(i)
          pre + "." + post
        }
        expandLoop(i + 2, expanded)
      } else expandLoop(i + 1, acc)
    }

  override def run(args: List[String]): IO[ExitCode] = Util.getInput
    .use { bs =>
      val lines = bs.getLines().toList
      val expandLines = lines.flatMap(l => if (l.forall(_ == '.')) List(l, l) else List(l))
      val fullyExpanded = expandLoop(0, expandLines)
      fullyExpanded.traverse(IO.println)
    }
    .as(ExitCode.Success)
}
