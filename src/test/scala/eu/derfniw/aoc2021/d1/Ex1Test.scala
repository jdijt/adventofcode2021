package eu.derfniw.aoc2021.d1

import munit.FunSuite

class Ex1Test extends FunSuite {

  test("Greeting is ok"){
    assert(greeting.head.isUpper)
    assert(greeting.last == '!')
  }

}
