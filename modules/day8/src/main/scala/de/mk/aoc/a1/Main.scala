package de.mk.aoc.a1

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import de.mk.util.Util

import scala.annotation.tailrec

object Main extends IOApp {
  final case class MapItem(v: String, i: Int, left: String, right: String)

  @tailrec
  def getMap(xs: List[String], index: Int, acc: List[MapItem]): List[MapItem] =
    xs match {
      case ::(head, next) =>
        val m = parseMap(head, index)
        val newAcc = m.fold(acc)(_ +: acc)
        getMap(next, index + 1, newAcc)
      case Nil => acc.sortBy(_.i)
    }

  def parseMap(l: String, i: Int): Option[MapItem] = if (l.contains("=")) {
    val split = l.split("=")
    val xs = split.tail.head.split(",")
    val left = xs.head.replaceAll("""\(""", "").trim
    val right = xs.last.replaceAll("""\)""", "").trim
    Some(MapItem(split.head.trim, i, left, right))
  } else None

  @tailrec
  def getRuns(
      instr: String,
      currentIndex: Int,
      steps: Int,
      runs: Int,
      l: List[MapItem],
      current: MapItem,
      last: MapItem
  ): Int = {
    if (currentIndex <= instr.length - 1) {
      val dir = instr.charAt(currentIndex)
      val nextStr =
        if (dir == 'L') current.left
        else if (dir == 'R') current.right
        else throw new Throwable("Unknown direction")
      if (nextStr == last.v) steps
      else {
        val next = l.find(_.v == nextStr)
        getRuns(instr, currentIndex + 1, steps + 1, runs, l, next.get, last)
      }
    } else getRuns(instr, 0, steps, runs + 1, l, current, last)
  }

  override def run(args: List[String]): IO[ExitCode] =
    Util.getInput
      .use { bs =>
        val lines = bs.getLines().toList
        val instr = lines.head
        val mapItems = getMap(lines.tail, 0, List.empty)
        val start = mapItems.find(_.v == "AAA")
        val end = mapItems.find(_.v == "ZZZ")
        IO.println(
          getRuns(instr, 0, 1, 1, mapItems, start.get, end.get)
        )
      }
      .as(ExitCode.Success)
}
