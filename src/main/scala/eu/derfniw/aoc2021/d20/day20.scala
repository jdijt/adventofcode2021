package eu.derfniw.aoc2021.d20

import eu.derfniw.aoc2021.printWithRuntime

import scala.io.Source

extension (s: Source)
  def readAlgorithmAndImage: (String, Grid) =
    val lines     = s.getLines()
    val algorithm = lines.next()
    // Skip line
    lines.next()
    val grid = Grid(lines.toIndexedSeq)
    (algorithm, grid)

class Grid(g: IndexedSeq[String], defaultChar: Char = '.'):

  def getBitValue(xIndex: Int, yIndex: Int): Int =
    val neighbours = for y <- yIndex - 1 to yIndex + 1; x <- xIndex - 1 to xIndex + 1 yield (x, y)
    val bitString = neighbours
      .map(this.getValue.tupled)
      .map(c => if c == '#' then '1' else '0')
      .mkString
    Integer.parseInt(bitString, 2)

  def getValue(x: Int, y: Int): Char =
    if x >= 0 && x < g.head.length && y >= 0 && y < g.size then g(y)(x)
    else '.'

  def applyAlgorythm(algo: String): Grid =
    val newImage = (-2 to g.size + 1).map { y =>
      (-2 to g.head.length + 1).map { x =>
        algo(getBitValue(x, y))
      }.mkString
    }
    val defaultChar = algo(0)
    Grid(newImage, defaultChar)

  def countLightPixels: Int = g.flatten.count(_ == '#')
end Grid

def exercise1(source: Source): Int =
  val (algorithm, image) = source.readAlgorithmAndImage

  val appliedOnce  = image.applyAlgorythm(algorithm)
  val appliedTwice = appliedOnce.applyAlgorythm(algorithm)

  appliedTwice.countLightPixels

private lazy val input: Source = Source.fromResource("exerciseInputs/input_d20.txt")

@main
def run_20_1(): Unit = printWithRuntime(exercise1(input))

//@main
//def run_18_2(): Unit = printWithRuntime(exercise2(input))
