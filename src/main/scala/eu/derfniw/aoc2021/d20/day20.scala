package eu.derfniw.aoc2021.d20

import eu.derfniw.aoc2021.printWithRuntime

import scala.io.Source

extension (s: Source)
  def readAlgorithmAndImage: (String, Grid) =
    val lines     = s.getLines()
    val algorithm = lines.next()
    // Skip line
    lines.next()
    val grid = Grid(lines.map(_.toIndexedSeq).toIndexedSeq)
    (algorithm, grid)

class Grid(g: IndexedSeq[IndexedSeq[Char]], defaultChar: Char = '.'):

  private def getBitValue(xIndex: Int, yIndex: Int): Int =
    val neighbours = for y <- yIndex - 1 to yIndex + 1; x <- xIndex - 1 to xIndex + 1 yield (x, y)
    val bitString = neighbours
      .map(this.getValue.tupled)
      .map(c => if c == '#' then '1' else '0')
      .mkString
    Integer.parseInt(bitString, 2)

  private def getValue(x: Int, y: Int): Char =
    if x >= 0 && x < g.head.length && y >= 0 && y < g.size then g(y)(x)
    else defaultChar

  def applyAlgorithm(algo: String): Grid =
    val newImage = (-1 to g.size).map { y =>
      (-1 to g.head.length).map { x =>
        algo(getBitValue(x, y))
      }
    }
    val newDefault = if defaultChar == '#' then algo.last else algo.head
    Grid(newImage, newDefault)

  def countLightPixels: Int = g.flatten.count(_ == '#')
end Grid

def exercise1(source: Source): Int =
  val (algorithm, image) = source.readAlgorithmAndImage

  val appliedOnce  = image.applyAlgorithm(algorithm)
  val appliedTwice = appliedOnce.applyAlgorithm(algorithm)

  appliedTwice.countLightPixels

def exercise2(source: Source): Int =
  val (algo, image) = source.readAlgorithmAndImage

  (0 until 50).foldLeft(image)((i, _) => i.applyAlgorithm(algo)).countLightPixels

private lazy val input: Source = Source.fromResource("exerciseInputs/input_d20.txt")

@main
def run_20_1(): Unit = printWithRuntime(exercise1(input))

@main
def run_20_2(): Unit = printWithRuntime(exercise2(input))
