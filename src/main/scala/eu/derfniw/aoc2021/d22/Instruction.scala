package eu.derfniw.aoc2021.d22

case class Point(x: Int, y: Int, z: Int)

sealed trait Instruction:
  def xRange: Range
  def yRange: Range
  def zRange: Range

  def appliesTo(p: Point): Boolean =
    xRange.contains(p.x) && yRange.contains(p.y) && zRange.contains(p.z)

end Instruction

case class On(xRange: Range, yRange: Range, zRange: Range) extends Instruction

case class Off(xRange: Range, yRange: Range, zRange: Range) extends Instruction
