package eu.derfniw.aoc2021.d5

import eu.derfniw.aoc2021.*

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

case class Point(x: Int, y: Int)
object Point:
  def tupled: ((Int, Int)) => Point = (Point.apply _).tupled

case class Line(p1: Point, p2: Point):
  def isHorizontal: Boolean = p1.y == p2.y
  def isVertical: Boolean   = p1.x == p2.x
  def maxX: Int             = math.max(p1.x, p2.x)
  def minX: Int             = math.min(p1.x, p2.x)
  def maxY: Int             = math.max(p1.y, p2.y)
  def minY: Int             = math.min(p1.y, p2.y)
  def pointsOnLine: Seq[Point] =
    if isHorizontal then (minX to maxX).map(x => Point(x, p1.y))
    else if isVertical then (minY to maxY).map(y => Point(p1.x, y))
    else
      val xRange = if p1.x > p2.x then p1.x to p2.x by -1 else p1.x to p2.x
      val yRange = if p1.y > p2.y then p1.y to p2.y by -1 else p1.y to p2.y
      xRange.zip(yRange).map(Point.tupled)
end Line

// (Max X, Max Y), lines
private def parseInput(in: Source): Seq[Line] =
  in.getLines()
    .map { case s"$x1,$y1 -> $x2,$y2" =>
      Line(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
    }
    .toSeq

def exercise1(in: Source): Int =
  val lines = parseInput(in)
  val maxX  = lines.map(_.maxX).max
  val maxY  = lines.map(_.maxY).max
  val space = ArrayBuffer.fill(maxX + 1)(ArrayBuffer.fill(maxY + 1)(0))
  lines
    .filter(l => l.isVertical || l.isHorizontal)
    .flatMap(_.pointsOnLine)
    .foreach(p => space(p.x).update(p.y, space(p.x)(p.y) + 1))
  space.flatten.count(_ >= 2)
end exercise1

def exercise2(in: Source): Int =
  val lines = parseInput(in)
  val maxX  = lines.map(_.maxX).max
  val maxY  = lines.map(_.maxY).max
  val space = ArrayBuffer.fill(maxX + 1)(ArrayBuffer.fill(maxY + 1)(0))
  lines
    .flatMap(_.pointsOnLine)
    .foreach(p => space(p.x).update(p.y, space(p.x)(p.y) + 1))
  space.flatten.count(_ >= 2)
end exercise2

private lazy val input: Source = Source.fromResource("exerciseInputs/input_d05.txt")

@main
def run_5_1(): Unit = printWithRuntime(exercise1(input))

@main
def run_5_2(): Unit = printWithRuntime(exercise2(input))
