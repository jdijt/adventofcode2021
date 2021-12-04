package eu.derfniw.aoc2021

import scala.concurrent.duration.Duration

def printWithRuntime(b: => Any): Unit =
  val start   = System.nanoTime()
  val result  = b.toString
  val end     = System.nanoTime()
  val runtime = Duration.fromNanos(end - start)
  println(s"""
       |Result: $result
       |Runtime: ${runtime.toMillis} ms
       |""".stripMargin)
end printWithRuntime
