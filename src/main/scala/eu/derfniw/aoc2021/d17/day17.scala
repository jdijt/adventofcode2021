package eu.derfniw.aoc2021.d17

import eu.derfniw.aoc2021.printWithRuntime

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.math.*

import java.math.BigInteger

case class Velocity(dx: Int, dy: Int):
  def applyPhysics: Velocity =
    val newX = if dx > 0 then dx - 1 else if dx < 0 then dx + 1 else 0
    Velocity(newX, dy - 1)

case class Position(x: Int, y: Int):
  def move(velocity: Velocity): Position = Position(x + velocity.dx, y + velocity.dy)

case class Target(xMin: Int, xMax: Int, yMin: Int, yMax: Int):
  assert(xMin <= xMax, "xMin must be less or equal to xMax")
  assert(yMin <= yMax, "yMin must be less or equal to yMax")
  def contains(point: Position): Boolean =
    point.x >= xMin
      && point.x <= xMax
      && point.y >= yMin
      && point.y <= yMax

  def overShot(point: Position): Boolean = point.x > xMax || point.y < yMin

  def ySize: Int = yMax - yMin
  def xSize: Int = xMax - xMin
end Target

val triangularNumbers = LazyList.from(1).map(n => (n, (n * (n + 1)) / 2))

@tailrec
def simulate(t: Target, p: Position, v: Velocity, maxY: Int): Option[Int] =
  if t.overShot(p) then None
  else if t.contains(p) then Some(maxY)
  else
    val newPoint = p.move(v)
    simulate(t, newPoint, v.applyPhysics, math.max(p.y, maxY))

def exercise1(target: Target): Int =
  // This velocity brings it above the target, less than that won't work.
  val (dX, _) = triangularNumbers
    .takeWhile { case (_, num) => num <= target.xMax }
    .find { case (_, num) => num >= target.xMin }
    .get

  LazyList
    .from(500, -1)
    .flatMap(dY => simulate(target, Position(0, 0), Velocity(dX, dY), 0))
    .head
end exercise1

def exercise2(target: Target): Int =
  val (minDX, _) = triangularNumbers
    .takeWhile { case (_, num) => num <= target.xMax }
    .find { case (_, num) => num >= target.xMin }
    .get

  val velocities = for
    dx <- minDX to target.xMax
    dy <- target.yMin to exercise1(target)
  yield Velocity(dx, dy)

  velocities.flatMap(v => simulate(target, Position(0, 0), v, 0)).size
end exercise2

@main
def run_16_1(): Unit = printWithRuntime(exercise1(Target(230, 283, -107, -57)))

@main
def run_16_2(): Unit = printWithRuntime(exercise2(Target(230, 283, -107, -57)))
