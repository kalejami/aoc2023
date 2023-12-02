package de.mk.aoc.a1

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import de.mk.util.Util

import scala.util.Try

object Main extends IOApp {
  final case class Game(i: Int, l: List[GameSet]) {
    def isGamePossible(gs: GameSet): Boolean = l.forall(_.isGameSetPossible(gs))
  }
  final case class GameSet(l: List[CubeSet]) {
    def isGameSetPossible(gameSet: GameSet): Boolean = gameSet.l.forall { cs =>
      l.find(_.c === cs.c) match {
        case Some(v) => v.i <= cs.i
        case None    => true
      }
    }
  }
  final case class CubeSet(i: Int, c: Color)
  sealed trait Color extends Product with Serializable {
    val txt: String
  }
  object Color {
    implicit val eq: cats.Eq[Color] = cats.Eq.fromUniversalEquals
    case object Blue extends Color { override val txt: String = "blue" }
    case object Green extends Color { override val txt: String = "green" }
    case object Red extends Color { override val txt: String = "red" }

    def fromString(s: String): Option[Color] = s match {
      case Blue.txt  => Some(Blue)
      case Green.txt => Some(Green)
      case Red.txt   => Some(Red)
      case _         => None
    }
  }

  def parseLine(s: String): Option[Game] = s.split(":").toList match {
    case ::(head, ::(next, _)) =>
      val gameNum = Try(head.replaceAll("Game ", "").trim.toInt).toOption
      val gameSets = next.split(";").toList.map { gameSetString =>
        val cubeSets = gameSetString.split(",").toList.mapFilter { cubeString =>
          createCubeSet(cubeString)
        }
        GameSet(cubeSets)
      }
      gameNum.map(n => Game.apply(n, gameSets))
    case _ => Option.empty
  }

  private def createCubeSet(cubeString: String): Option[CubeSet] = {
    cubeString.trim.split(" ").toList match {
      case ::(head, ::(next, _)) =>
        val cubeSetNum = Try(head.toInt).toOption
        val cubeColor = Color.fromString(next)
        (cubeSetNum, cubeColor).tupled.map(t => CubeSet.apply(t._1, t._2))
      case _ => None
    }
  }

  private val toValidateGameSet = GameSet(
    List(
      CubeSet(12, Color.Red),
      CubeSet(13, Color.Green),
      CubeSet(14, Color.Blue)
    )
  )

  override def run(args: List[String]): IO[ExitCode] =
    Util.getInput
      .use { bs =>
        val lines = bs.getLines().toList
        val games = lines.mapFilter(parseLine)
        val possibleGames =
          games.filter(g => g.isGamePossible(toValidateGameSet))
        val sumIds = possibleGames.map(_.i).sum
        IO(println(sumIds))
      }
      .as(ExitCode.Success)
}
