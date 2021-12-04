package eu.derfniw.aoc2021.d4

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

enum Field {
  case Marked extends Field
  case UnMarked(v: Int) extends Field
}

case class Board(values: Seq[Seq[Field]]) {

  import Field._

  def markValue(value: Int): Board = {
    val newValues = values.map { row =>
      row.map {
        case UnMarked(v) if v == value => Marked
        case other                     => other
      }
    }
    Board(newValues)
  }

  lazy val sumUnmarked: Int = values.flatten.collect { case UnMarked(v) => v }.sum

  lazy val isWinner: Boolean = {
    val hasWinningRow    = values.exists(_.forall(_ == Marked))
    val columnSeqs       = for { col <- 0 until 5 } yield (0 until 5).map(row => values(row)(col))
    val hasWinningColumn = columnSeqs.exists(_.forall(_ == Marked))

    hasWinningColumn || hasWinningRow
  }
}

// First line: Input, then newline seperated boards of 5x5 int
private def parseInput(in: Source): (List[Int], List[Board]) = {
  val lines     = in.getLines()
  val inputNums = lines.next().split(",").map(_.toInt).toList

  val boardsCollector = mutable.ArrayDeque[Board]()

  while (lines.hasNext) {
    lines.next() // Skip empty line
    val fields = (0 until 5).map { _ =>
      lines.next().strip().split("""\s+""").map(v => Field.UnMarked(v.toInt)).toSeq
    }
    boardsCollector.addOne(Board(fields))
  }

  (inputNums, boardsCollector.toList)
}

def exercise1(in: Source): Int = {
  val (inputNumbers, boards) = parseInput(in)

  @tailrec
  def playGame(input: List[Int], state: List[Board]): (Int, Board) = input match {
    case Nil => throw new RuntimeException("Reached end of input without winning board")
    case in :: rest =>
      val newState = state.map(_.markValue(in))
      newState.find(_.isWinner) match {
        case Some(winner) => (in, winner)
        case _            => playGame(rest, newState)
      }
  }

  val (winningNumber, winningBoard) = playGame(inputNumbers, boards)
  winningNumber * winningBoard.sumUnmarked
}

def exercise2(in: Source): Int = {
  val (inputNumbers, boards) = parseInput(in)

  @tailrec
  def playGame(input: List[Int], state: List[Board], winners: Seq[(Int, Board)]): (Int, Board) =
    if (state.isEmpty) winners.last
    else
      input match {
        case Nil => winners.last // Not all boards have to win!
        case in :: rest =>
          val newState   = state.map(_.markValue(in))
          val newWinners = winners.appendedAll(newState.filter(_.isWinner).map((in, _)))
          val losers     = newState.filterNot(_.isWinner)
          playGame(rest, losers, newWinners)
      }

  val (winningNumber, winningBoard) = playGame(inputNumbers, boards, List())
  winningNumber * winningBoard.sumUnmarked
}

private lazy val input: Source = Source.fromResource("exerciseInputs/input_d04.txt")

@main
def run_3_1(): Unit = println(exercise1(input))

@main
def run_3_2(): Unit = println(exercise2(input))
