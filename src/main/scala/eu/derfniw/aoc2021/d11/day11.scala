package eu.derfniw.aoc2021.d11

import eu.derfniw.aoc2021.printWithRuntime

import scala.annotation.tailrec
import scala.io.Source

type Grid = IndexedSeq[IndexedSeq[Int]]

extension (source: Source)
  def parse: Grid = source.getLines().map(_.map(_.asDigit).toIndexedSeq).toIndexedSeq

extension (grid: Grid)
  def maxY = grid.size - 1
  def maxX = if grid.maxY > 0 then grid.head.size - 1 else -1

  def adjacentPoints(x: Int, y: Int) =
    import math.*
    for
      x <- max(0, x - 1) to min(grid.maxY, x + 1)
      y <- max(0, y - 1) to min(grid.maxX, y + 1)
    yield (x, y)
  end adjacentPoints

  def zeroAt(x: Int, y: Int)      = grid.updated(y, grid(y).updated(x, 0))
  def incrementAt(x: Int, y: Int) = grid.updated(y, grid(y).updated(x, grid(y)(x) + 1))
end extension

case class OctopusGrid(grid: Grid):

  def step: (OctopusGrid, Int) =
    this.incrementEnergy.flashOctopuses

  // step 1
  private def incrementEnergy: OctopusGrid = OctopusGrid(grid.map(_.map(_ + 1)))

  // step 2
  private def flashOctopuses: (OctopusGrid, Int) =
    def findOctopusesToFlash(g: Grid, hasFlashed: ((Int, Int)) => Boolean) =
      for
        (row, y)   <- g.zipWithIndex
        (value, x) <- row.zipWithIndex
        point = (x, y)
        result <- if value > 9 && !hasFlashed(point) then Some(point) else None
      yield result

    @tailrec
    def flashStep(current: Grid, hasFlashed: Set[(Int, Int)]): (Grid, Set[(Int, Int)]) =
      findOctopusesToFlash(current, hasFlashed) match
        case Seq() => (current, hasFlashed)
        case toFlash =>
          val newGrid = toFlash.foldLeft(current) { case (grid, (fx, fy)) =>
            grid.adjacentPoints(fx, fy).foldLeft(grid) { case (g, (x, y)) =>
              g.incrementAt(x, y)
            }
          }
          flashStep(newGrid, hasFlashed ++ toFlash.toSet)
    end flashStep

    val (newGrid, flashes) = flashStep(grid, Set())
    val flashesZeroedGrid  = flashes.foldLeft(newGrid) { case (g, (x, y)) => g.zeroAt(x, y) }
    (OctopusGrid(flashesZeroedGrid), flashes.size)
  end flashOctopuses

end OctopusGrid

def exercise1(source: Source): Int =
  val grid = OctopusGrid(source.parse)

  val (_, flashCount) = (0 until 100)
    .foldLeft((grid, 0)) { case ((grid, count), _) =>
      val (newGrid, flashed) = grid.step
      (newGrid, count + flashed)
    }

  flashCount
end exercise1

def exercise2(source: Source): Int =
  val grid = OctopusGrid(source.parse)

  @tailrec
  def getAllFlashStep(grid: OctopusGrid, stepCount: Int): Int =
    val (newGrid, flashCount) = grid.step
    if flashCount == 100 then stepCount + 1
    else getAllFlashStep(newGrid, stepCount + 1)

  getAllFlashStep(grid, 0)
end exercise2

private lazy val input: Source = Source.fromResource("exerciseInputs/input_d11.txt")

@main
def run_11_1(): Unit = printWithRuntime(exercise1(input))

@main
def run_11_2(): Unit = printWithRuntime(exercise2(input))
