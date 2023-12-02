package de.mk.aoc.a2

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.*
import de.mk.util.Util
import de.mk.aoc.a1.Main
import de.mk.aoc.a1.Main.{Color, CubeSet, Game, parseLine}

object Main extends IOApp {
  final case class Fewest(r: CubeSet, g: CubeSet, b: CubeSet) {
    def power: Int = r.i * g.i * b.i
  }
  object Fewest {
    def fromGame(g: Game): Fewest = {
      val forThisGame = (c: Color) => fewestCubeSetFor(c)(g)
      Fewest(
        r = forThisGame(Color.Red).getOrElse(CubeSet(0, Color.Red)),
        g = forThisGame(Color.Green).getOrElse(CubeSet(0, Color.Green)),
        b = forThisGame(Color.Blue).getOrElse(CubeSet(0, Color.Blue))
      )
    }
    private def fewestCubeSetFor(c: Color)(g: Game): Option[CubeSet] =
      g.l.flatMap(_.l.filter(_.c === c)).maxByOption(_.i)
  }

  override def run(args: List[String]): IO[ExitCode] =
    Util.getInput
      .use { bs =>
        val lines = bs.getLines().toList
        val games = lines.mapFilter(parseLine)
        val fewest = games.map(Fewest.fromGame)
        IO.println(fewest.map(_.power).sum)
      }
      .as(ExitCode.Success)
}
