package eu.derfniw.aoc2021.d16

import scala.io.Source
import scala.math.BigInt

extension (source: Source) def parsed: Packet = Packet.parseLine(source.getLines().next())

extension (c: Char)
  def toBits =
    c match
      case '0' => "0000"
      case '1' => "0001"
      case '2' => "0010"
      case '3' => "0011"
      case '4' => "0100"
      case '5' => "0101"
      case '6' => "0110"
      case '7' => "0111"
      case '8' => "1000"
      case '9' => "1001"
      case 'A' => "1010"
      case 'B' => "1011"
      case 'C' => "1100"
      case 'D' => "1101"
      case 'E' => "1110"
      case 'F' => "1111"
      case _   => throw new RuntimeException(s"Invalid hex char: $c")
end extension

extension (s: String)
  def toIntBase2: Int = Integer.parseInt(s, 2)

  def toBigIntBase2: BigInt = BigInt(s, 2)
end extension
