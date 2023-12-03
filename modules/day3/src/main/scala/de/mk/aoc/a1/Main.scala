package de.mk.aoc.a1

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import de.mk.util.Util

import scala.annotation.tailrec
import scala.util.Try

object Main extends IOApp {
  def getCheckRange(s: Int, e: Int, lineLength: Int): (Int, Int) = {
    val sc = if (s - 1 < 0) 0 else s - 1
    val ec = if (e + 1 > lineLength) lineLength else e + 1
    (sc, ec)
  }

  def checkForPartNumber(toCheck: String): Boolean =
    toCheck.replaceAll("""[\d|.]""", "").nonEmpty

  def checkLineForPartNumber(sc: Int, ec: Int)(l: String): Boolean = {
    val toCheck = l.substring(sc, ec)
    println(toCheck)
    checkForPartNumber(toCheck)
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
    val r = """\d+""".r

    r.findAllMatchIn(line).toList.mapFilter { m =>
      val start = m.start
      val end = m.end
      val (sc, ec) = getCheckRange(start, end, line.length)
      val checkWithThisIndices = checkLineForPartNumber(sc, ec) _
      val numOpt = Try(m.group(0).toInt).toOption
      val result =
        if (checkWithThisIndices(line)) numOpt
        else if (lineAbove.exists(checkWithThisIndices)) numOpt
        else if (lineBelow.exists(checkWithThisIndices)) numOpt
        else None
      println(result)
      result
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
