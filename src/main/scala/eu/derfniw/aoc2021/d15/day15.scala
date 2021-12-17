package eu.derfniw.aoc2021.d15

import eu.derfniw.aoc2021.printWithRuntime

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

import math.*

case class Point(x: Int, y: Int):
  def adjacentPoints: List[Point] = List(
    Point(x - 1, y),
    Point(x + 1, y),
    Point(x, y - 1),
    Point(x, y + 1)
  )

  def manhattanDist(other: Point): Int = math.abs(this.x - other.x) + math.abs(this.y - other.y)
end Point

extension (s: scala.collection.Seq[scala.collection.Seq[Int]])
  def valueAt(point: Point): Int = s(point.y)(point.x)

class RiskGrid private (grid: IndexedSeq[IndexedSeq[Int]]):
  private val maxY: Int     = grid.size - 1
  private val maxX: Int     = if maxY > 0 then grid.head.size - 1 else -1
  private val target: Point = Point(maxX, maxY)
  private val start: Point  = Point(0, 0)
  // The risk if traversing the whole cave, nodes go up to 9.
  private val peakRisk = maxX * maxY * 10

  private def inBounds(p: Point): Boolean =
    p.x >= 0 && p.x <= maxX && p.y >= 0 && p.y <= maxY

  def expand: RiskGrid =
    def increaseRisk(current: Int, amount: Int): Int =
      val v = current + amount
      if v > 9 then v - 9 else v

    val colsExpanded =
      grid.map(row => (0 until 5).flatMap(riskToAdd => row.map(r => increaseRisk(r, riskToAdd))))

    RiskGrid(
      (0 until 5).flatMap(riskToAdd => colsExpanded.map(_.map(r => increaseRisk(r, riskToAdd))))
    )
  end expand

  // A* search
  def minimumRiskCost: Int =
    val gScore = mutable.ArrayBuffer.fill(maxY + 1)(mutable.ArrayBuffer.fill(maxX + 1)(peakRisk))
    gScore(start.y).update(start.x, 0)

    type ScoredPoint = (Point, Int)
    given Ordering[ScoredPoint] with
      override def compare(x: ScoredPoint, y: ScoredPoint): Int = y._2 - x._2

    val openPoints = mutable.PriorityQueue[ScoredPoint]((start, start.manhattanDist(target)))

    while openPoints.nonEmpty do
      val (current, _) = openPoints.dequeue()
      if current == target then return gScore(current.y)(current.x)
      else
        current.adjacentPoints.filter(inBounds).foreach { p =>
          val score = grid.valueAt(p) + gScore.valueAt(current)
          if score < gScore.valueAt(p) then
            gScore(p.y).update(p.x, score)
            openPoints.addOne(p, score + p.manhattanDist(target))
        }
      end if
    end while

    throw new RuntimeException("Empty open points but goal not found")
  end minimumRiskCost

end RiskGrid

object RiskGrid:
  def fromRows(rows: Iterator[String]): RiskGrid =
    RiskGrid(rows.map(_.map(_.asDigit).toIndexedSeq).toIndexedSeq)

extension (source: Source) def parsed: RiskGrid = RiskGrid.fromRows(source.getLines())

def exercise1(source: Source): Int = source.parsed.minimumRiskCost

def exercise2(source: Source): Int =
  val expanded = source.parsed.expand
  expanded.minimumRiskCost

private lazy val input: Source = Source.fromResource("exerciseInputs/input_d15.txt")

@main
def run_15_1(): Unit = printWithRuntime(exercise1(input))

@main
def run_15_2(): Unit = printWithRuntime(exercise2(input))
