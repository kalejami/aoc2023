package de.mk.aoc.a2

import cats.effect.{ExitCode, IO, IOApp}
import de.mk.aoc.a1.Main.parseLine
import de.mk.util.Util

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    Util.getInput
      .use { bs =>
        val lines = bs.getLines().toList
        val hs = lines.map(parseLine)
        IO.println(hs.map(_.previousValue).sum)
      }
      .as(ExitCode.Success)
}
