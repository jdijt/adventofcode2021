package eu.derfniw.aoc2021.d12

import scala.io.Source

class Day12Test extends munit.FunSuite:

  def input1: Source = Source.fromResource("testin_d12_1.txt")
  def input2: Source = Source.fromResource("testin_d12_2.txt")
  def input3: Source = Source.fromResource("testin_d12_3.txt")

  test("Part 1 works per example 1") {
    val result = exercise1(input1)
    assertEquals(result, 10)
  }
  test("Part 1 works per example 2") {
    val result = exercise1(input2)
    assertEquals(result, 19)
  }
  test("Part 1 works per example 3") {
    val result = exercise1(input3)
    assertEquals(result, 226)
  }

  test("Part 2 works per example 1") {
    val result = exercise2(input1)
    assertEquals(result, 36)
  }
  test("Part 2 works per example 2") {
    val result = exercise2(input2)
    assertEquals(result, 103)
  }
  test("Part 2 works per example 3") {
    val result = exercise2(input3)
    assertEquals(result, 3509)
  }
end Day12Test
