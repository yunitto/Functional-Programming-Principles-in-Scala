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
    val s4 = singletonSet(4)
    val s5 = singletonSet(5)
    val s6 = singletonSet(6)
    val u = union(s1, s2)

    
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
      assert(contains(s2, 2), "Singleton")
      assert(contains(s2, 2), "Singleton")
      assert(!contains(s2, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      assert(contains(u, 1), "Union 1")
      assert(contains(u, 2), "Union 2")
      assert(!contains(u, 3), "Union 3")
    }
  }

  test("intersect well implemented") {
    new TestSets {
      assert(contains(intersect(u, s1), 1), "Intersection 1")
      assert(contains(intersect(u, s2), 2), "Intersection 2")
      assert(!contains(intersect(u, s1), 2), "Intersection 3")
    }
  }

  test("diff well implemented") {
    new TestSets {
      assert(contains(diff(u, s1), 2), "Diff 1")
      assert(contains(diff(u, s2), 1), "Diff 2")
      assert(contains(diff(s3, u), 3), "Diff 3")
    }
  }

  test("filter well implemented") {
    new TestSets {
      assert(contains(filter(u, x => x == 1), 1), "Filter 1")
      assert(!contains(filter(u, x => x == 1), 2), "Filter 2")
    }
  }

  test("forall") {
    new TestSets {
      assert(forall(x => x > 0, x => x > -10), "Forall 1")
      assert(!forall(x => x > 0, x => x > 10), "Forall 2")
    }
  }

  test("exists") {
    new TestSets {
      assert(exists(x => x % 2 == 0, x => x % 4 == 0), "Exists 1")
      assert(exists(x => x > 0, x => x > 10), "Exists 2")
      assert(exists(x => x > 10, x => x > 0), "Exists 3")
      assert(!exists(x => x % 2 == 0, x => x % 2 != 0), "Exists 3")
      assert(exists(union(x => x % 2 == 1, s3), x => x % 2 == 1), "Exists 4")
      assert(exists(union(union(s1, s2), s3), x => x % 2 == 0), "***I GOT THIS WRONG***")
    }
  }

  test("map") {
    new TestSets {
      assert(forall(map(x => x > 0, x => x*2), x => x%2 == 0), "Map 1")
    }
  }

}
