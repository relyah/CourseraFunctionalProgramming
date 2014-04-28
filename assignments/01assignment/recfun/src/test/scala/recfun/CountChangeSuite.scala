package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CountChangeSuite extends FunSuite {
  import Main.countChange

  test("countChange: money=2, coins=[1 2]") {
    assert(countChange(2, List(1, 2)) === 2)
  }

  test("countChange: money=5, coins=[1 2]") {
    assert(countChange(5, List(1, 2)) === 3)
  }

    test("countChange: money=5, coins=[1 2 5]") {
    assert(countChange(5, List(1, 2, 5)) === 4)
  }

  test("countChange: only one coin") {
    assert(countChange(4, List(1)) === 1)
  }

  test("countChange: impossible amount=5, coins=[2]") {
    assert(countChange(5, List(2)) === 0)
  }

  test("countChange: example given in instructions") {
    assert(countChange(4, List(1, 2)) === 3)
  }

  test("countChange: sorted CHF") {
    assert(countChange(300, List(5, 10, 20, 50, 100, 200, 500)) === 1022)
  }

  test("countChange: no pennies") {
    assert(countChange(301, List(5, 10, 20, 50, 100, 200, 500)) === 0)
  }

  test("countChange: unsorted CHF") {
    assert(countChange(300, List(500, 5, 50, 100, 20, 200, 10)) === 1022)
  }

  test("countChange: empty List") {
    assert(countChange(300, List()) === 0)
  }

}
