package de.mk.aoc.a1

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import de.mk.util.Util

import scala.annotation.tailrec
import scala.util.Try

object Main extends IOApp {

  @tailrec
  def parseLines(
      it: Iterator[String],
      index: Long,
      acc: List[(Long, List[List[Long]])]
  ): List[(Long, List[List[Long]])] = it.nextOption() match {
    case Some(s) =>
      if (s == "") parseLines(it, index + 1, acc)
      else {
        val list = s.split(' ').toList
        val newList = list.mapFilter(s => Try(s.trim.toLong).toOption)
        val newAcc = acc.map { case (i, value) =>
          val update = if (i == index) value :+ newList else value
          (i, update)
        }
        parseLines(it, index, newAcc)
      }
    case None => acc.filter(_._2.nonEmpty)
  }

  def getLocations(l: List[(Long, List[List[Long]])]) = {
    val seeds = l.head._2.flatten
    seeds.map { value =>
      l.tail.sortBy(_._1).foldLeft(value) { case (value, (_, mappings)) =>
        translate(value, mappings, value)
      }
    }
  }

  @tailrec
  def translate(value: Long, mapping: List[List[Long]], mappedValue: Long): Long = {
    if (mappedValue == value) {
      mapping match {
        case ::(head, next) =>
          head match {
            case ::(dest, ::(src, ::(range, _))) =>
              if (value >= src && value <= (src + range) - 1) {
                val distance = value - src
                translate(value, next, dest + distance)
              } else translate(value, next, value)
            case _ => translate(value, next, value)
          }
        case Nil => mappedValue
      }
    } else mappedValue
  }

  override def run(args: List[String]): IO[ExitCode] = Util.getInput
    .use { bs =>
      val it = bs.getLines()
      val lists = parseLines(it, 0, (0 to 7).toList.map(i => (i, List.empty)))
      val locations = getLocations(lists)
      IO.println(locations.min)
    }
    .as(ExitCode.Success)
}
