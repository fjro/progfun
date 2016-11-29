package scalashop

import org.scalameter._
import common._

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /** Blurs the columns of the source image `src` into the destination image
   *  `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each column, `blur` traverses the pixels by going from top to
   *  bottom.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    //println("blur: f = " + from + "; end = " + end)
    if (radius > 0) {
      val y = 0 until src.height
      val x = from until end
      val p = for {
        xi <- x
        yi <- y
      } yield (xi, yi)

      p map(t => dst update(t._1, t._2, boxBlurKernel(src, t._1, t._2, radius)))
    }
  }


  def bound(value: Int, max: Int): Int = {
    if(value <= max) value
    else max
  }

/** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  column0 until src.width by numTaskss.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val strips = 0 to src.width by numTasks map(i => (i, bound(i + numTasks, src.width)))
    val tasks = for {
      strip <- strips
    } yield task(blur(src, dst, strip._1, strip._2, radius))

    for {
      t <- tasks
    } yield t.join()
  }

}
