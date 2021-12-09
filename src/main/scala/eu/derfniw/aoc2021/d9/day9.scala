package eu.derfniw.aoc2021.d9

import eu.derfniw.aoc2021.printWithRuntime

import scala.io.Source

case class Point(x: Int, y: Int)

extension (grid: Seq[Seq[Int]])
  def findLowPoints: Seq[Point] =
    val maxY = grid.size
    val maxX = if maxY > 0 then grid.head.size else 0
    grid.zipWithIndex.flatMap { (row, yi) =>
      row.indices.flatMap { xi =>
        if grid.adjacentValues(Point(xi, yi)).forall(_ > grid(yi)(xi)) then
          Some(Point(x = xi, y = yi))
        else None
      }
    }
  end findLowPoints

  def adjacentValues(p: Point): Seq[Int] =
    grid.adjacentPoints(p).map(p => grid(p.y)(p.x))

  def adjacentPoints(p: Point): Seq[Point] =
    val maxY = grid.size
    val maxX = if maxY > 0 then grid.head.size else 0
    List((p.x - 1, p.y), (p.x + 1, p.y), (p.x, p.y + 1), (p.x, p.y - 1))
      .filterNot { case (x, y) => x < 0 || y < 0 || y >= maxY || x >= maxX }
      .map { case (x, y) => Point(x, y) }

  def basinSize(p: Point): Int =
    ???
end extension

def parseInput(source: Source): Seq[Seq[Int]] = source.getLines().map(_.map(_.asDigit).toSeq).toSeq

def exercise1(source: Source): Int =
  val grid = parseInput(source)
  grid.findLowPoints.map(p => grid(p.y)(p.x) + 1).sum

def exercise2(source: Source): Int =
  val grid      = parseInput(source)
  val lowPoints = grid.findLowPoints

private lazy val input: Source = Source.fromResource("exerciseInputs/input_d09.txt")

@main
def run_9_1(): Unit = printWithRuntime(exercise1(input))

@main
def run_9_2(): Unit = printWithRuntime(exercise2(input))
