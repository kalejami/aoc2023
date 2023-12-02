package de.mk.aoc.a1

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.*
import de.mk.util.Util

import scala.annotation.tailrec
import scala.util.Try

object Main extends IOApp {

  private final case class NumF(value: Int, text: String)
  private final case class Pos(index: Int, value: Int)

  private val nums: List[NumF] = List(
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine"
  ).zipWithIndex.map { case (str, i) => NumF(i + 1, str) }

  @tailrec
  private def allIndicesForPattern(
                                    s: String,
                                    pattern: String,
                                    lastIndex: Int,
                                    acc: List[Int]
                                  ): List[Int] = {
    val index = s.indexOf(pattern, lastIndex)
    if (index == -1) acc.reverse
    else allIndicesForPattern(s, pattern, index + 1, index +: acc)
  }

  private def allIndicesForNumFs(s: String): List[Pos] = {
    val indices = nums.flatMap { nf =>
      // val isTxt = allIndicesForPattern(s, nf.text, 0, List.empty)
      val isNum = allIndicesForPattern(s, nf.value.toString, 0, List.empty)
      isNum.map(i => Pos(i, nf.value)).distinct
    }
    indices.sortBy(_.index)
  }

  private def transformToInt(l: List[Pos]): Option[Int] = for {
    first <- l.headOption
    last <- l.lastOption
    num <- Try(s"${first.value}${last.value}".toInt).toOption
  } yield num

  override def run(args: List[String]): IO[ExitCode] = Util.getInput
    .use { bs =>
      val result = bs
        .getLines()
        .toList
        .mapFilter { s =>
          val pos = allIndicesForNumFs(s)
          val n = transformToInt(pos)
          n
        }
        .sum
      IO.println(result)
    }
    .as(ExitCode.Success)
}

