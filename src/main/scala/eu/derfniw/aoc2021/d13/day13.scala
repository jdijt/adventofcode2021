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

case class Point(x: Int, y: Int):
  def applyFold(f: Fold): Point = f match
    case Fold.FoldX(xi) if xi < x => Point(xi - (x - xi), y)
    case Fold.FoldY(yi) if yi < y => Point(x, yi - (y - yi))
    case _                        => this

type SparseGrid = Set[Point]

extension (grid: SparseGrid)
  def folded(fold: Fold): SparseGrid = grid.map(_.applyFold(fold))

  def render: String =
    val maxX = grid.map(_.x).max + 1
    val maxY = grid.map(_.y).max + 1
    (0 to maxY)
      .map(y => (0 to maxX).map(x => if grid(Point(x, y)) then '█' else ' ').mkString)
      .mkString("\n", "\n", "\n")
end extension

extension (source: Source)
  def parsed: (SparseGrid, List[Fold]) =
    val lines   = source.getLines()
    val dotGrid = lines.takeWhile(_.nonEmpty).map { case s"$x,$y" => Point(x.toInt, y.toInt) }.toSet
    // Map remainder into Fold instances
    // No need to skip the empty line as it is consumed by the takeWhile already..
    val folds = lines.map(Fold.fromLine).toList
    (dotGrid, folds)
  end parsed
end extension

def exercise1(source: Source): Int =
  val (grid, folds) = source.parsed
  grid.folded(folds.head).size

def exercise2(source: Source): String =
  val (grid, folds) = source.parsed
  folds.foldLeft(grid)((g, f) => g.folded(f)).render

private lazy val input: Source = Source.fromResource("exerciseInputs/input_d13.txt")

@main
def run_13_1(): Unit = printWithRuntime(exercise1(input))

@main
def run_13_2(): Unit = printWithRuntime(exercise2(input))
