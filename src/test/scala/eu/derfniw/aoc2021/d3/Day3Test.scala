package eu.derfniw.aoc2021.d3

import scala.io.Source

class Day3Test extends munit.FunSuite:

  private val testInput =
    """|00100
       |11110
       |10110
       |10111
       |10101
       |01111
       |00111
       |11100
       |10000
       |11001
       |00010
       |01010""".stripMargin

  test("Part 1 is ok per example") {
    val result = exercise1(Source.fromString(testInput))

    assertEquals(result, 198)
  }

  test("Part 2 is ok per example") {
    val result = exercise2(Source.fromString(testInput))

    assertEquals(result, 230)
  }
end Day3Test
