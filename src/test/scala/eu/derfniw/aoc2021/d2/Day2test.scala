package eu.derfniw.aoc2021.d2

import scala.io.Source

class Day2test extends munit.FunSuite:

  private val input =
    """forward 5
      |down 5
      |forward 8
      |up 3
      |down 8
      |forward 2""".stripMargin

  test("Part 1 works per example") {
    val result = exercise1(Source.fromString(input))

    assertEquals(result, 150)
  }

  test("Part 2 works per example") {
    val result = exercise2(Source.fromString(input))

    assertEquals(result, 900)
  }
end Day2test
