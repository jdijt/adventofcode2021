package eu.derfniw.aoc2021.d15

import eu.derfniw.aoc2021.printWithRuntime

import scala.annotation.tailrec
import scala.io.Source

case class Point(x: Int, y: Int):
  def adjacentPoints: Set[Point] = Set(
    Point(x - 1, y),
    Point(x + 1, y),
    Point(x, y - 1),
    Point(x, y + 1)
  )

class RiskGrid private (grid: IndexedSeq[IndexedSeq[Int]]):
  val maxY: Int = grid.size
  val maxX: Int = if maxY > 0 then grid.head.size else 0

  // Risk per cell is max 9, going out of grid is more risky than traversing whole grid.
  val peakRisk: Int = maxY * maxX * 10

  def valueAt(p: Point): Int =
    if p.y >= 0 && p.y < maxY && p.x >= 0 && p.x < maxY then grid(p.y)(p.x)
    else peakRisk

  def minStepsToEnd(p: Point): Int = maxX - p.x + maxY - p.y
end RiskGrid

object RiskGrid:
  def fromRows(rows: Iterator[String]): RiskGrid =
    RiskGrid(rows.map(_.map(_.asDigit).toIndexedSeq).toIndexedSeq)

extension (source: Source) def parsed: RiskGrid = RiskGrid.fromRows(source.getLines())

def exercise1(source: Source): Int =
  val grid = source.parsed
  def aStar(
      goal: Point,
      open: Set[Point],
      fScores: Map[Point, Int],
      gScores: Map[Point, Int],
      cameFrom: Map[Point, Point]
  ): Int =
    if open.isEmpty then throw new RuntimeException("Goal not found!")
    else
      val current = open.minBy(p => grid.minStepsToEnd(p))
      if current == goal then ???
      else val (newOpen, newFScores, newGScores, newCameFrom) = point.adjacentPoints.foldLeft

end exercise1

def exercise2(source: Source): Int =
  ???
end exercise2

private lazy val input: Source = Source.fromResource("exerciseInputs/input_d14.txt")

@main
def run_15_1(): Unit = printWithRuntime(exercise1(input))

@main
def run_15_2(): Unit = printWithRuntime(exercise2(input))
