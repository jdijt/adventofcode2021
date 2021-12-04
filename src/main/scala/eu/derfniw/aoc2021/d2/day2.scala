package eu.derfniw.aoc2021.d2

import scala.io.Source

case class Vec(x: Int, y: Int):
  def +(that: Vec): Vec = Vec(this.x + that.x, this.y + that.y)

  def *(that: Vec): Vec = Vec(this.x * that.x, this.y * that.y)

private lazy val input: Source = Source.fromResource("exerciseInputs/input_d02.txt")

private def parseInput(in: Source): Iterator[Vec] =
  in.getLines().map {
    case s"forward $x" => Vec(x.toInt, 0)
    case s"down $y"    => Vec(0, y.toInt)
    case s"up $y"      => Vec(0, -y.toInt)
  }

def exercise1(in: Source): Int =
  val finalPosition = parseInput(in).fold(Vec(0, 0))(_ + _)
  finalPosition.x * finalPosition.y

def exercise2(in: Source): Int =
  val (finalPosition, _) = parseInput(in)
    .foldLeft((Vec(0, 0), Vec(1, 0))) {
      case ((pos, aim), v @ Vec(0, y)) => (pos, aim + v)
      case ((pos, aim), v @ Vec(x, 0)) => (pos + (Vec(x, x) * aim), aim)
    }
  finalPosition.x * finalPosition.y

@main
def run_2_1(): Unit = println(exercise1(input))

@main
def run_2_2(): Unit = println(exercise2(input))
