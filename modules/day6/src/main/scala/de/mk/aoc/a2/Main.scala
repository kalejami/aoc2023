package de.mk.aoc.a2

import cats.effect.{ExitCode, IO, IOApp}
import de.mk.aoc.a1.Main.Race
import de.mk.util.Util

object Main extends IOApp {

  def parseInput(l: List[String]): List[Race] = l match {
    case ::(time, ::(distance, _)) =>
      val r = "\\d+".r
      val times = r.findAllMatchIn(time.replaceAll(" ", "")).toList.map(_.group(0).toLong)
      val distances = r.findAllMatchIn(distance.replaceAll(" ", "")).toList.map(_.group(0).toLong)
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
