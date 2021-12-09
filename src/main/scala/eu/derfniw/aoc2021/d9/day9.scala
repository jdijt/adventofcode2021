package eu.derfniw.aoc2021.d9

import eu.derfniw.aoc2021.printWithRuntime

import scala.annotation.tailrec
import scala.io.Source

case class Point(x: Int, y: Int)

extension (grid: Seq[Seq[Int]])
  def get(p: Point): Int = grid(p.y)(p.x)
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
    grid.adjacentPoints(p).map(grid.get)

  def adjacentPoints(p: Point): Seq[Point] =
    val maxY = grid.size
    val maxX = if maxY > 0 then grid.head.size else 0
    List(Point(p.x - 1, p.y), Point(p.x + 1, p.y), Point(p.x, p.y + 1), Point(p.x, p.y - 1))
      .filterNot { case Point(x, y) => x < 0 || y < 0 || y >= maxY || x >= maxX }

  def basinSize(p: Point): Int =
    @tailrec
    def bfs(pointsToCheck: Seq[Point], checkedPoints: Set[Point]): Int =
      pointsToCheck match
        case Seq() => checkedPoints.size
        case p +: ps =>
          val newChecked = checkedPoints + p
          val newToCheck = ps :++ grid
            .adjacentPoints(p)
            .filter(p => grid.get(p) < 9)
            .filterNot(checkedPoints)
          bfs(newToCheck, newChecked)

    bfs(Seq(p), Set.empty[Point])
  end basinSize

end extension

def parseInput(source: Source): Seq[Seq[Int]] = source.getLines().map(_.map(_.asDigit).toSeq).toSeq

def exercise1(source: Source): Int =
  val grid = parseInput(source)
  grid.findLowPoints.map(p => grid.get(p) + 1).sum

def exercise2(source: Source): Int =
  val grid = parseInput(source)
  grid.findLowPoints.map(p => grid.basinSize(p)).sorted.takeRight(3).product

private lazy val input: Source = Source.fromResource("exerciseInputs/input_d09.txt")

@main
def run_9_1(): Unit = printWithRuntime(exercise1(input))

@main
def run_9_2(): Unit = printWithRuntime(exercise2(input))
