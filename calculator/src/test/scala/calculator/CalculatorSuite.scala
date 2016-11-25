package calculator

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    println("hello")
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == TweetLength.MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == TweetLength.MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == TweetLength.MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("delta with a constant signal") {
    println("testing delta")
    val result = Polynomial.computeDelta(Signal(2.0d), Signal(2.0d), Signal(2.0d))
    assert(result() == -12)
  }

  test("delta with all NaN") {
    println("testing delta")
    val result = Polynomial.computeDelta(Signal(Double.NaN), Signal(Double.NaN), Signal(Double.NaN))
    assert(result().isNaN)
  }

//  test("delta with c= NaN") {
//    println("testing delta")
//    val result = Polynomial.computeDelta(Signal(1), Signal(1), Signal(Double.NaN))
//    assert(result() == -3)
//  }

  test("calc") {
    println("testing calc")
    val a = Literal(3)
    val b = Literal(4)
    val c = Plus(a, b)

    val references = Map[String, Signal[Expr]](
      ("a", Signal(a)),
      ("b", Signal(b)),
      ("c", Signal(c))
    )

    val res = Calculator.getReferenceExpr("a", references)
    println(res)
    assert(Literal(3) == res)

    val resC = Calculator.getReferenceExpr("c", references)
    println(resC)
    assert(Plus(Literal(3.0),Literal(4.0)) == resC)
  }

  test("calc eval") {
    val a = Literal(3)
    val b = Literal(4)
    val c = Plus(a, b)

    val references = Map[String, Signal[Expr]](
      ("a", Signal(a)),
      ("b", Signal(b)),
      ("c", Signal(c))
    )

    assert(3 == Calculator.eval(a, references))
    assert(7 == Calculator.eval(c, references))
  }

  test("is cyclic") {
    //a = b + 1 and b = 2 * a.
    val a = Plus(Ref("b"), Literal(1))
    val b = Plus(Ref("a"), Literal(1))

    val references = Map[String, Signal[Expr]](
      ("a", Signal(a)),
      ("b", Signal(b))
    )

    assert(Calculator.isCyclic("a", a, references))
    assert(Calculator.isCyclic("b", b, references))
    assert(!Calculator.isCyclic("c", Literal(9), references))
  }

}
