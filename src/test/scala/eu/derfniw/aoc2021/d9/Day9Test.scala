package eu.derfniw.aoc2021.d9

import scala.io.Source

class Day9Test extends munit.FunSuite:
  val input: String =
    """|2199943210
       |3987894921
       |9856789892
       |8767896789
       |9899965678""".stripMargin

  test("Part 1 works per example") {
    val result = exercise1(Source.fromString(input))
    assertEquals(result, 15)
  }

  test("Part 2 works per example") {
    val result = exercise2(Source.fromString(input))
    assertEquals(result, 1134)
  }
end Day9Test
