package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BalanceSuite extends FunSuite {
  import Main.balance

  test("balance: '(if (zero? x) max (/ 1 x))' is balanced") {
    assert(balance("(if (zero? x) max (/ 1 x))".toList))
  }

  test("balance: 'I told him ...' is balanced") {
    assert(balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList))
  }

  test("balance: ':-)' is unbalanced") {
    assert(!balance(":-)".toList))
  }

  test("balance: counting is not enough") {
    assert(!balance("())(".toList))
  }

  test("balance: empty list") {
    assert(balance(List()))
  }

  test("balance: empty string") {
    assert(balance("".toList))
  }

  test("balance: ()") {
    assert(balance("()".toList))
  }

  test("balance: (3 + 4)(4 + 7)") {
    assert(balance("(3 + 4)(4 + 7)".toList))
  }

  test("balance: ((3 + 4))(4 + 7)") {
    assert(balance("((3 + 4))(4 + 7)".toList))
  }

  test("balance: (3 + 4)((4 + 7))") {
    assert(balance("(3 + 4)((4 + 7))".toList))
  }

  test("balance: ((3 + 4)(4 + 7))") {
    assert(balance("((3 + 4)(4 + 7))".toList))
  }

  test("balance: ()(3 + 4)(4 + 7)") {
    assert(balance("()(3 + 4)(4 + 7)".toList))
  }

  test("balance: no brackets") {
    assert(balance("no brackets here".toList))
  }
}
