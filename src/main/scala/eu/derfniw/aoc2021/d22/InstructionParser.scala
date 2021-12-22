package eu.derfniw.aoc2021.d22

import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

object InstructionParser extends RegexParsers:

  def number: Parser[Int]  = "-?[0-9]+".r ^^ { _.toInt }
  def range: Parser[Range] = number ~ ".." ~ number ^^ { case l ~ _ ~ r => l to r }
  def instr: Parser[Instruction] =
    "(on)|(off)".r ~ "x=" ~ range ~ ",y=" ~ range ~ ",z=" ~ range ^^ {
      case "on" ~ _ ~ xr ~ _ ~ yr ~ _ ~ zr  => On(xr, yr, zr)
      case "off" ~ _ ~ xr ~ _ ~ yr ~ _ ~ zr => Off(xr, yr, zr)
    }

  def parse(source: Source): IndexedSeq[Instruction] =
    source
      .getLines()
      .map(l => parse(instr, l).get)
      .toIndexedSeq

end InstructionParser
