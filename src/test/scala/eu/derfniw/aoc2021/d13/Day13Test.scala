package eu.derfniw.aoc2021.d13

import scala.io.Source

class Day13Test extends munit.FunSuite:
  val input: String =
    """|6,10
       |0,14
       |9,10
       |0,3
       |10,4
       |4,11
       |6,0
       |6,12
       |4,1
       |0,13
       |10,12
       |3,4
       |3,0
       |8,4
       |1,10
       |2,14
       |8,10
       |9,0
       |
       |fold along y=7
       |fold along x=5
       |""".stripMargin

  test("Part 1 works per example") {
    val result = exercise1(Source.fromString(input))
    assertEquals(result, 17)
  }

end Day13Test
