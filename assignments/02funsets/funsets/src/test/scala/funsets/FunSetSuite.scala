package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  import FunSets._

  trait TestSetContains {
    def allIntegers(x: Int) = true
    def negIntegers(x: Int) = x < 0
  }

  test("contains is implemented") {
    new TestSetContains {
      assert(contains(allIntegers, 100))
    }
  }

  test("contains: set of negative integers, positive element") {
    new TestSetContains {
      assert(!contains(negIntegers, 5))
    }
  }

  test("contains: set of negative integers, negative element") {
    new TestSetContains {
      assert(contains(negIntegers, -5))
    }
  }

  test("contains: set of negative integers, zero element") {
    new TestSetContains {
      assert(!contains(negIntegers, 0))
    }
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    def s123: Set = x => x >= 1 && x <= 3
    def s234: Set = x => x >= 2 && x <= 4

    def filter23: Int => Boolean = x => x == 2 || x == 3

  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")

    }
  }

  test("singletonSet(1) does not contain 2") {
    new TestSets {
      assert(!contains(s1, 2), "singletonSet(1) does not contain 2")
    }
  }

  test("singletonSet(2) contains 2") {
    new TestSets {
      assert(contains(s2, 2), "singletonSet(2) contains 2")
    }
  }

  test("singletonSet(2) does not contain 1") {
    new TestSets {
      assert(!contains(s2, 1), "singletonSet(2) does not contain 1")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersection contains all common elements") {
    new TestSets {
      val s = intersect(s1, s2)
      assert(!contains(s, 1), "Intersect 1")
      assert(!contains(s, 2), "Intersect 2")

      var s23 = intersect(s123, s234)
      assert(contains(s23, 2), "Intersect s23 contains 2")
      assert(contains(s23, 3), "Intersect s23 contains 3")
      assert(!contains(s23, 1), "Intersect s23 does not contain 1")
      assert(!contains(s23, 4), "Intersect s23 does not contain 4")

    }
  }

  test("difference contains all elements in s but not in t") {
    new TestSets {
      val s = diff(s1, s2)
      assert(contains(s, 1), "Diff s1 s2 contains 1")
      assert(!contains(s, 2), "Diff s1 s2 does not contain 2")
      assert(!contains(s, 3), "Diff s1 s2 does not contain 3")

      var s1diff = diff(s123, s234)
      assert(contains(s1diff, 1), "Diff s123 s234 contains 1")
      assert(!contains(s1diff, 0), "Diff s123 s234 does not contain 0")
      assert(!contains(s1diff, 2), "Diff s123 s234 does not contain 2")
      assert(!contains(s1diff, 3), "Diff s123 s234 does not contain 3")
      assert(!contains(s1diff, 4), "Diff s123 s234 does not contain 4")
      assert(!contains(s1diff, 5), "Diff s123 s234 does not contain 5")

    }
  }

  test("filter") {
    new TestSets {
      assert(contains(filter(s123, filter23), 2), "s123 filtered by f23 contains 2")
      assert(contains(filter(s123, filter23), 3), "s123 filtered by f23 contains 3")
      assert(!contains(filter(s123, filter23), 0), "s123 filtered by f23 does not contain 0")
      assert(!contains(filter(s123, filter23), 1), "s123 filtered by f23 does not contain 1")
      assert(!contains(filter(s123, filter23), 4), "s123 filtered by f23 does not contain 4")
    }
  }

  test("forall") {
    new TestSets {
      assert(forall(s1, x => x < 2), "s1 forall x<2")
      assert(!forall(s1, x => x > 2), "s1 forall x>2")

      assert(forall(s123, x => x > 0 && x < 4), "s123 forall 0<x<4")
      assert(!forall(s123, x => x <= 0), "s123 forall x<=0")
      assert(!forall(s123, x => x >= 4), "s123 forall x>=4")
    }
  }

  test("exists") {
    new TestSets {
      assert(exists(s1, x => x == 1), "s1 exists 1")
      assert(!exists(s1, x => x == 2), "s1 does not exist 2")

      assert(exists(s123, x => x == 1), "s123 exists 1")
      assert(exists(s123, x => x == 2), "s123 exists 2")
      assert(exists(s123, x => x == 3), "s123 exists 3")
      assert(!exists(s123, x => x == 0), "s123 does not exist 0")

      assert(exists(s234, x => x >= 3), "s234 3,4 exists x>=3")
      assert(exists(s234, x => x >= 4), "s234 4 exists x>=4")
      assert(!exists(s234, x => x >= 5), "s234 does not exist x>=5")
      assert(!exists(s234, x => x <= 0), "s234 does not exist x<=0")

    }
  }

  test("map") {
    new TestSets {
      val map_s1 = map(s1, x => x + 1)
      assert(contains(map_s1, 2), "map f(x)=x+1, contains 2")
      assert(!contains(map_s1, 1), "map f(x)=x+1, does not contain 1")

      val map_s123 = map(s123, x => x - 1)
      assert(!contains(map_s123, -1), "map f(x)=x-1, does not contain -1")
      assert(contains(map_s123, 0), "map f(x)=x-1, contains 0")
      assert(contains(map_s123, 1), "map f(x)=x-1, contains 1")
      assert(contains(map_s123, 2), "map f(x)=x-1, contains 2")
      assert(!contains(map_s123, 3), "map f(x)=x-1, does not contain 3")
      assert(!contains(map_s123, 4), "map f(x)=x-1, does not contain 4")
    }
  }
}
