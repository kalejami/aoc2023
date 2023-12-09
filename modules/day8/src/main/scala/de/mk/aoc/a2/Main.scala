package de.mk.aoc.a2

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
      current: MapItem
  ): Int = {
    if (currentIndex <= instr.length - 1) {
      val dir = instr.charAt(currentIndex)
      val nextStr =
        if (dir == 'L') current.left
        else if (dir == 'R') current.right
        else throw new Throwable("Unknown direction")
      if (nextStr.endsWith("Z")) steps
      else {
        val next = l.find(_.v == nextStr)
        getRuns(instr, currentIndex + 1, steps + 1, runs, l, next.get)
      }
    } else getRuns(instr, 0, steps, runs + 1, l, current)
  }

  // was to lazy to calc, so copy&pasted
  val first100Primes = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43,
    47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127,
    131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199,
    211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283,
    293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383,
    389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467,
    479, 487, 491, 499, 503, 509, 521, 523, 541)

  def inPrimes(n: Int, i: Int, acc: List[Int]): List[Int] = if (
    i < first100Primes.length
  ) {
    val prim = first100Primes(i)
    val mod = n % prim
    if (mod == 0) inPrimes(n / prim, 0, prim +: acc)
    else inPrimes(n, i + 1, acc)
  } else acc

  override def run(args: List[String]): IO[ExitCode] =
    Util.getInput
      .use { bs =>
        val lines = bs.getLines().toList
        val instr = lines.head
        val mapItems = getMap(lines.tail, 0, List.empty)
        val starts = mapItems.filter(_.v.endsWith("A"))
        // after analyzing the data, the distance between the start and first "**Z" match repeats
        // so lcm probably should work
        val allDistances = starts.map(getRuns(instr, 0, 1, 1, mapItems, _))
        val allAsPrimes = allDistances.map(inPrimes(_, 0, List.empty))
        if(allAsPrimes.forall(_.forall(i => first100Primes.contains(i)))) {
          IO.println(allAsPrimes.flatten.distinct.map(_.toLong).product)
        } else IO.raiseError(new Throwable("have to do sth i am to lazy for..."))

      }
      .as(ExitCode.Success)
}
