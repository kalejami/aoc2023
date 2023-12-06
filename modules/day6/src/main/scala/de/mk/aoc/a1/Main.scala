package de.mk.aoc.a1

import cats.effect.{ExitCode, IO, IOApp}
import de.mk.util.Util

import scala.annotation.tailrec

object Main extends IOApp {
  final case class Race(time: Long, distance: Long) {
    def possibleWins: Long = {
      @tailrec
      def loop(timeCharged: Long, acc: Long): Long = if (timeCharged < time) {
        val travelTime = time - timeCharged
        val distanceTravelled = travelTime * timeCharged
        val newAcc = if (distanceTravelled > distance) acc + 1 else acc
        loop(timeCharged + 1, newAcc)
      } else acc
      loop(1, 0)
    }
  }

  def parseInput(l: List[String]): List[Race] = l match {
    case ::(time, ::(distance, _)) =>
      val r = "\\d+".r
      val times = r.findAllMatchIn(time).toList.map(_.group(0).toLong)
      val distances = r.findAllMatchIn(distance).toList.map(_.group(0).toLong)
      times.zip(distances).map { case (t, d) => Race(t, d) }
    case _ => List.empty
  }

  override def run(args: List[String]): IO[ExitCode] = Util.getInput
    .use { bs =>
      val races = parseInput(bs.getLines().toList)
      IO.println(races.map(_.possibleWins).product)
    }
    .as(ExitCode.Success)
}
