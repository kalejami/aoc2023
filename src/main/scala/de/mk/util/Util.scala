package de.mk.util

import cats.effect.{IO, Resource}

import scala.io.BufferedSource

object Util {
  def getInput: Resource[IO, BufferedSource] =
    Resource.make(IO(scala.io.Source.fromResource("input.txt")))(r =>
      IO(r.close())
    )
}
