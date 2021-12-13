package eu.derfniw.aoc2021.d13

import eu.derfniw.aoc2021.printWithRuntime

import scala.annotation.tailrec
import scala.io.Source

enum Fold:
  case FoldX(index: Int)
  case FoldY(index: Int)

object Fold:
  def fromLine(line: String): Fold = line match
    case s"fold along y=$y" => FoldY(y.toInt)
    case s"fold along x=$x" => FoldX(x.toInt)
    case _                  => throw new RuntimeException("In valid fold line!")
end Fold

type Grid = IndexedSeq[IndexedSeq[Boolean]]

extension (grid: Grid)
  def updated(x: Int, y: Int, value: Boolean) = grid.updated(y, grid(y).updated(x, value))

  def applyFold(fold: Fold): Grid =
    val (base, toOverLay) = fold match
      case Fold.FoldX(xLine) =>
        val left  = grid.map(_.take(xLine))
        val right = grid.map(_.drop(xLine + 1).reverse)
        (left, right)
      case Fold.FoldY(yLine) =>
        val upper = grid.take(yLine)
        val lower = grid.drop(yLine + 1).reverse
        (upper, lower)
    base.zip(toOverLay).map { case (baseRow, overLayRow) =>
      baseRow.zip(overLayRow).map { case (b1, b2) => b1 || b2 }
    }
  end applyFold
end extension

private def parseInput(source: Source): (Grid, List[Fold]) =
  val lines = source.getLines()
  val dotGrid =
    val positions = lines.takeWhile(_.nonEmpty).map { case s"$x,$y" => (x.toInt, y.toInt) }.toSeq
    val maxY      = positions.maxBy(_._2)._2 + 1
    val maxX      = positions.maxBy(_._1)._1 + 1
    val grid      = IndexedSeq.fill(maxY)(IndexedSeq.fill(maxX)(false))
    positions.foldLeft(grid) { case (grid, (x, y)) => grid.updated(x, y, true) }

  // Map remainder into Fold instances
  // No need to skip the empty line as it is consumed by the takeWhile already..
  val folds = lines.map(Fold.fromLine).toList

  (dotGrid, folds)
end parseInput

def exercise1(source: Source): Int =
  val (grid, folds) = parseInput(source)
  val newGrid       = grid.applyFold(folds.head)
  newGrid.flatten.count(_ == true)
end exercise1

def exercise2(source: Source): String =
  val (grid, folds) = parseInput(source)
  val folded        = folds.foldLeft(grid)((grid, fold) => grid.applyFold(fold))
  folded
    .map(_.map(b => if b then '#' else ' ').mkString)
    .mkString("\n", "\n", "\n")
end exercise2

private lazy val input: Source = Source.fromResource("exerciseInputs/input_d13.txt")

@main
def run_13_1(): Unit = printWithRuntime(exercise1(input))

@main
def run_13_2(): Unit = printWithRuntime(exercise2(input))
