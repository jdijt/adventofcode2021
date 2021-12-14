package eu.derfniw.aoc2021.d14

import scala.io.Source

class Day14Test extends munit.FunSuite:
  val input: String =
    """|NNCB
       |
       |CH -> B
       |HH -> N
       |CB -> H
       |NH -> C
       |HB -> C
       |HC -> B
       |HN -> C
       |NN -> C
       |BH -> H
       |NC -> B
       |NB -> B
       |BN -> B
       |BB -> N
       |BC -> B
       |CC -> N
       |CN -> C""".stripMargin

  test("Part 1 works per example") {
    val result = exercise1(Source.fromString(input))
    assertEquals(result, 1588)
  }

end Day14Test
