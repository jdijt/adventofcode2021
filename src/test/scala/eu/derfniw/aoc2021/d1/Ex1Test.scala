package eu.derfniw.aoc2021.d1

import munit.FunSuite

import scala.io.Source

class Ex1Test extends FunSuite:

  private val testInput =
    """ 
      |199
      |200
      |208
      |210
      |200
      |207
      |240
      |269
      |260
      |263
      |""".stripMargin

  test("Part 1 is ok per example") {
    val result = exercise1(Source.fromString(testInput))

    assertEquals(result, 7)
  }

  test("Part 2 is ok per example") {
    val result = exercise2(Source.fromString(testInput))

    assertEquals(result, 5)
  }
