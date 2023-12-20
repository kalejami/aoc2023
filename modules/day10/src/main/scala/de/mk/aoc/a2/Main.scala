package de.mk.aoc.a2

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import de.mk.util.Util

import scala.annotation.tailrec

object Main extends IOApp {
  private val norths = List('|', '7', 'F')
  private val wests = List('-', 'L', 'F')
  private val souths = List('|', 'L', 'J')
  private val easts = List('-', '7', 'J')

  final case class Pos(
      y: Int,
      x: Int,
      c: Char,
      visited: Long = 0,
      isStart: Boolean = false
  )

  def surroundings(m: List[Pos])(p: Pos): Iterable[Pos] = {
    def updateGraph(y: Int, x: Int)(validMoves: List[Char]) =
      m.find(pt => pt.y == y && pt.x == x).flatMap { next =>
        if (validMoves.contains(next.c) && next.visited == 0) {
          val newNext = next.copy(visited = p.visited + 1)
          Some(newNext)
        } else None
      }

    updateGraph(p.y - 1, p.x)(norths) ++
      updateGraph(p.y + 1, p.x)(souths) ++
      updateGraph(p.y, p.x - 1)(wests) ++
      updateGraph(p.y, p.x + 1)(easts)
  }

  @tailrec
  def visitedLoop(m: List[Pos], toDo: List[Pos]): List[Pos] =
    if (toDo.isEmpty) m
    else {
      val nxs = toDo.flatMap(surroundings(m))
      val newMap =
        m.map(p => nxs.find(pt => pt.y == p.y && pt.x == p.x).getOrElse(p))
      visitedLoop(newMap, nxs)
    }

  @tailrec
  def polygonLoop(m: List[Pos], toDo: List[Pos], acc: List[Pos]): List[Pos] =
    if (toDo.isEmpty) acc
    else {
      val backtrack: Pos => List[Pos] = p =>
        m.filter { pt =>
          val dR =
            (pt.x == p.x && pt.y == p.y - 1 && souths.contains(p.c)) ||
              (pt.x == p.x && pt.y == p.y + 1 && norths.contains(p.c)) ||
              (pt.x == p.x + 1 && pt.y == p.y && wests.contains(p.c)) ||
              (pt.x == p.x - 1 && pt.y == p.y && easts.contains(p.c))
          dR && pt.visited == p.visited - 1
        }
      val newToDo = toDo.flatMap(backtrack).distinct
      polygonLoop(m, newToDo, acc ++ toDo)
    }

  override def run(args: List[String]): IO[ExitCode] = Util.getInput
    .use { bs =>
      val lines = bs.getLines().zipWithIndex.toList
      val map = lines.flatMap { case (line, y) =>
        line.zipWithIndex.map { case (c, x) =>
          if (c == 'S') Pos(y, x, '7', isStart = true) else Pos(y, x, c)
        }
      }

      val start = map.find(_.isStart).head
      val visited = visitedLoop(map, List(start))
      val max = visited.maxBy(_.visited)

      val polygon = polygonLoop(visited, List(max), List.empty)

      val size = polygon.size

      val roundtrip = start +: polygon
        .groupBy(_.visited)
        .toList
        .flatMap { case (distance, items) =>
          items match {
            case ::(head, Nil) => List(head.copy(visited = size - distance))
            case ::(head, ::(next, _)) => List(next, head.copy(visited = size - distance))
            case _ => List()
          }
        }
        .sortBy(_.visited)

      @tailrec
      def allConnects(l: List[Pos], acc: Boolean = true): Boolean =
        if (!acc) acc
        else
          l match {
            case ::(p1, ::(p2, tail)) =>
              allConnects(p2 +: tail, p1.x == p2.x || p1.y == p2.y)
            case _ => acc
          }

      assert(allConnects(roundtrip))

      // https://de.wikipedia.org/wiki/Gau%C3%9Fsche_Trapezformel
      @tailrec
      def shoelace(l: List[Pos], acc: Long): Long = l match {
        case ::(p1, ::(p2, next)) =>
          val prod = (p1.y + p2.y) * (p1.x - p2.x)
          shoelace(p2 +: next, acc + prod)
        case _ => acc
      }

      val A = shoelace(roundtrip :+ start, 0L) / 2d
      // https://de.wikipedia.org/wiki/Satz_von_Pick
      val I = A + 1 - max.visited

      IO.println(max) >> IO.println(I)
    }
    .as(ExitCode.Success)

}
