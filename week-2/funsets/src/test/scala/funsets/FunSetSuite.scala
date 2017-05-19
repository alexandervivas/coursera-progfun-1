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
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
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

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains all elements present on both sets at the time") {
    new TestSets {
      val s12 = union(s1, s2)
      val s13 = union(s1, s3)
      val s = intersect(s12, s13)
      assert(contains(s, 1), "Union 1")
      assert(!contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("diff contains all elements present in a set that are not present in another set") {
    new TestSets {
      val s12 = union(s1, s2)
      val s13 = union(s1, s3)
      val s = diff(s12, s13)
      assert(!contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("filter the elements in a set by some predicate") {
    new TestSets {
      val s = union(union(s1, s2), s3)
      val even = (n: Int) => n % 2 == 0
      val odd = (n: Int) => !even(n)
      val isEven = filter(s, even)
      val isOdd = filter(s, odd)
      assert(!isEven(1), "Even")
      assert(isEven(2), "Even")
      assert(!isEven(3), "Even")
      assert(isOdd(1), "Odd")
      assert(!isOdd(2), "Odd")
      assert(isOdd(3), "Odd")
    }
  }

  test("the predicate is true for all elements in set") {
    new TestSets {
      val s = union(union(s1, s2), s3)
      val so = union(s1, s3)
      val even = (n: Int) => n % 2 == 0
      val odd = (n: Int) => !even(n)
      val isEven = filter(s, even)
      val isOdd = filter(s, odd)
      assert(!forall(s, even), "All-Even")
      assert(forall(s2, even), "All-Even")
      assert(!forall(s, odd), "All-Odd")
      assert(forall(so, odd), "All-Odd")
    }
  }

  test("the predicate is true for at least one elements in set") {
    new TestSets {
      val s = union(union(s1, s2), s3)
      val even = (n: Int) => n % 2 == 0
      val isEven = filter(s, even)
      assert(exists(s, even), "At least one even number")
    }
  }

    test("the set is transformed given a function") {
      new TestSets {
        val s = union(union(s1, s2), s3)
        val f = (n: Int) => n * 2
        assert(FunSets.toString(map(s, f)) === "{2,4,6}", "Set transformed")
      }
  }

}
