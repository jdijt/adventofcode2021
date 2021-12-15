package eu.derfniw.aoc2021.d15

import scala.io.Source

class Day15Test extends munit.FunSuite:
  val input: String =
    """|1163751742
       |1381373672
       |2136511328
       |3694931569
       |7463417111
       |1319128137
       |1359912421
       |3125421639
       |1293138521
       |2311944581""".stripMargin

  test("Part 1 works per example") {
    val result = exercise1(Source.fromString(input))
    assertEquals(result, 40)
  }

  test("Part 2 works per example") {
    val result = exercise2(Source.fromString(input))
    assertEquals(result, ???)
  }

end Day15Test
