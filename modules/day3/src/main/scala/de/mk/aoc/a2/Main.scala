package de.mk.aoc.a2

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import de.mk.util.Util

import scala.annotation.tailrec
import scala.util.Try

object Main extends IOApp {

  def checkLineForPartNumber(indexOfMatch: Int)(l: String): List[Int] = {
    val r = """\d+""".r
    val nums = r.findAllMatchIn(l).toList.mapFilter { m =>
      val realEnd = m.end - 1
      if (
        m.start == indexOfMatch ||
        realEnd == indexOfMatch ||
        m.start == indexOfMatch - 1 ||
        realEnd == indexOfMatch - 1 ||
        m.start == indexOfMatch + 1 ||
        realEnd == indexOfMatch + 1
      ) {
        Try(m.group(0).toInt).toOption
      } else None
    }
    nums
  }

  @tailrec
  def getAllRelevantPartNumbers(
      input: List[(String, Int)],
      lineAbove: Option[String],
      currentLine: Option[String],
      lineBelow: Option[String],
      acc: List[Int]
  ): List[Int] =
    currentLine match {
      case Some(line) =>
        val nums =
          getAllRelevantPartNumbersForLine(lineAbove, line, lineBelow)
        val indexCurrentLine = input.find(_._1 == line).map(_._2)
        val nextCurrentLine =
          input
            .find(t => indexCurrentLine.exists(i => t._2 == i + 1))
            .map(_._1)
        val nextLineBelow =
          input
            .find(t => indexCurrentLine.exists(i => t._2 == i + 2))
            .map(_._1)
        getAllRelevantPartNumbers(
          input,
          Some(line),
          nextCurrentLine,
          nextLineBelow,
          acc ++ nums
        )
      case None => acc
    }

  def getAllRelevantPartNumbersForLine(
      lineAbove: Option[String],
      line: String,
      lineBelow: Option[String]
  ): List[Int] = {
    val r = """\*""".r
    r.findAllMatchIn(line).toList.map { m =>
      val checkWithThisIndices = checkLineForPartNumber(m.start) _
      val result =
        checkWithThisIndices(line) ++
          lineAbove.traverse(checkWithThisIndices).flatten ++
          lineBelow.traverse(checkWithThisIndices).flatten
      if (result.sizeIs.==(2)) result.product else 0
    }
  }

  override def run(args: List[String]): IO[ExitCode] = Util.getInput
    .use { bs =>
      val lines = bs.getLines().toList
      val nums = getAllRelevantPartNumbers(
        lines.zipWithIndex,
        None,
        lines.headOption,
        lines.tail.headOption,
        List.empty
      )
      IO.println(nums.sum)
    }
    .as(ExitCode.Success)
}
