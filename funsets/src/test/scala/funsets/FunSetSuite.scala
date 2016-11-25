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

  test("Intersection tests") {
    new TestSets {
      val s = intersect(s1, s2)
      assert(!contains(s, 1), "Intersect 1")
      assert(!contains(s, 2), "Intersect 2")
      assert(!contains(s, 3), "Intersect 3")
    }
  }

  test("Filter tests") {
    new TestSets {
      val s = union(union(s1, s2), s3)
      assert(contains(s, 1), "Intersect 1")
      assert(contains(s, 2), "Intersect 2")
      assert(contains(s, 3), "Intersect 3")


      val filtered = filter(s, (v: Int) => v >= 2)
      assert(!contains(filtered, 1), "filtered 1")
      assert(contains(filtered, 2), "filtered 2")
    }
  }

  test("For all tests") {
    new TestSets {
      //forall: {1,3,4,5,7,1000} x$5.<(5)
      val s = union(union(s1, s2), singletonSet(3))
      val s4 = union(s, singletonSet(4))
      val s5 = union(s4, singletonSet(5))
      val s7 = union(s5, singletonSet(7))
      val s1000 = union(s7, singletonSet(1000))
      assert(contains(s1000, 1), "forall 1")
      assert(contains(s1000, 1000), "forall 3")

      var result = forall(s1000, (v: Int) => v <= 1500)
      assert(result, " v <= 1500")

      var r2 = forall(s1000, (v: Int) => v >= 2000)
      assert(!r2, " v >= 2000")

      var r3 = forall(s1000, (v: Int) => v < 5)
      assert(!r3, "v < 5")
    }
  }

  test("Exists tests") {
    new TestSets {
      //forall: {1,3,4,5,7,1000} x$5.<(5)
      val s = union(union(s1, s2), singletonSet(3))
      val s4 = union(s, singletonSet(4))
      val s5 = union(s4, singletonSet(5))
      val s7 = union(s5, singletonSet(7))
      val s1000 = union(s7, singletonSet(1000))
      assert(contains(s1000, 1), "forall 1")
      assert(contains(s1000, 1000), "forall 3")

      var r2 = exists(s1000, (v: Int) => v >= 2000)
      assert(!r2, " v >= 2000")

      var r3 = exists(s1000, (v: Int) => v < 5)
      assert(r3, "v < 5")

      var r4 = exists(s1000, (v: Int) => v == 4)
      assert(r4, "v == 4")
    }
  }

  test("Map tests") {
    new TestSets {
      //forall: {1,3,4,5,7,100} x$5.<(5)
      val s = union(union(s1, s2), singletonSet(3))
      val s4 = union(s, singletonSet(4))
      val s5 = union(s4, singletonSet(5))
      val s7 = union(s5, singletonSet(7))
      val s10 = union(s7, singletonSet(10))
      val mapped = map(s10, (x: Int) => x * x)
      println("mapped = "+ mapped)
      var r2 = exists(mapped, (x: Int) => x == 16)
      assert(r2, " x == 16")

      var r3 = exists(mapped, (x: Int) => x == 15)
      assert(!r3, " x == 15")

      var r4 = exists(mapped, (x: Int) => x == 49)
      assert(r4, " x == 49")
    }
  }

}
