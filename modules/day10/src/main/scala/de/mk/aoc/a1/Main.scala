package de.mk.aoc.a1

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import de.mk.util.Util

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

object Main extends IOApp {
  private val norths = List('|', '7', 'F')
  private val wests = List('-', 'L', 'F')
  private val souths = List('|', 'L', 'J')
  private val easts = List('-', '7', 'J')

  final case class Pos(c: Char, visited: Long = 0, isStart: Boolean = false)

  def surroundings(
      m: ArrayBuffer[ArrayBuffer[Pos]]
  )(y: Int, x: Int): Iterable[(Int, Int)] = {
    val p = m(y)(x)

    def updateGraph(y: Int, x: Int)(validMoves: List[Char]) = {
      Try(m(y)(x)).toOption match {
        case Some(next) =>
          if (validMoves.contains(next.c) && next.visited == 0) {
            val newNext = next.copy(visited = p.visited + 1)
            m(y).update(x, newNext)
            Some((y, x))
          } else None
        case None => None
      }

    }

    updateGraph(y - 1, x)(norths) ++
      updateGraph(y + 1, x)(souths) ++
      updateGraph(y, x - 1)(wests) ++
      updateGraph(y, x + 1)(easts)
  }

  override def run(args: List[String]): IO[ExitCode] = Util.getInput
    .use { bs =>
      val lines = bs.getLines().zipWithIndex.toList
      val map = scala.collection.mutable.ArrayBuffer
        .tabulate[Pos](lines.size, lines.head._1.length) { (y, x) =>
          val (line, _) = lines(y)
          val c = line.charAt(x)
          if (c == 'S') Pos('7', isStart = true) else Pos(c)
        }

      val sForM = surroundings(map) _

      val (y, x) = lines.mapFilter { case (str, i) =>
        val x = str.indexOf('S')
        if (x != -1) Some((i, x)) else None
      }.head

      var s = sForM(y, x)
      while (s.nonEmpty) {
        s = s.toList.flatMap(t => sForM(t._1, t._2))
      }

      val max = map.map(_.maxBy(_.visited)).maxBy(_.visited)
      IO.println(max)
    }
    .as(ExitCode.Success)

}
