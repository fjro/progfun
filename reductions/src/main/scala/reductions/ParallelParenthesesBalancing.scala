package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    //val seqtime = 100
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    var c = 0
    var i = 0
    while(i < chars.length) {
      if (c < 0) i == chars.length
      else if (chars(i) == '(') c = c + 1
      else if (chars(i) == ')') c = c - 1
      i = i + 1
    }
    c == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {
    //println("parBalance: len = " + chars.length + ", threshold = " + threshold)
    def traverse(idx: Int, until: Int, open: Int, close: Int) : (Int, Int) = {
//      println("traverse: idx = " + idx + ", until = " + until + ", open = " + open + ", close = " + close)
      if(until - idx < threshold) {
        var i = idx
        var o = open
        var c = close
        while(i < until) {
          if (chars(i) == '(') o = o + 1
          else if (chars(i) == ')') c = c + 1
          i = i + 1
        }
        (o, c)
      }
      else {
        val mid = idx + (until - idx)/2
        parallel(reduce(idx, mid),
                reduce(mid, until))

      }
    }

    def reduce(from: Int, until: Int) : Int = {
      val oc = traverse(from, until, 0, 0)
      oc._1 - oc._2
    }

    reduce(0, chars.length) == 0

  }
}
