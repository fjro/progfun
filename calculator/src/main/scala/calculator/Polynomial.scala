package calculator

object Polynomial {

  /**
    * Δ = b² - 4ac
    * @param a
    * @param b
    * @param c
    * @return
    */
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
   // val bv = b()
    //if(c() == 0 || c().isNaN) Signal((b() * b()) - (4 * a()))
    //else
    Signal((b() * b()) - (4 * a() * c()))
  }

  // (-b ± √Δ) / 2a
  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {

    println("computeSolutions: a = " + a() + ", b = " + b() + ", c = " + c() + ", delta = " + computeDelta(a, b, c)())
    //val first = (-b() + scala.math.sqrt(computeDelta (a, b, c) ()))/(2 * a())
    //val second = (-b() + scala.math.sqrt(computeDelta (a, b, c) ()))/(2 * a())

//    if (first == Double.NaN && second == Double.NaN) Signal(Set[Double]())
//    else if (first == Double.NaN && second != Double.NaN) Signal(Set[Double](second))
//    else if (first != Double.NaN && second == Double.NaN) Signal(Set[Double](first))
//    else Signal(Set[Double](first, second))
    Signal(Set[Double](
      (-b() + scala.math.sqrt(computeDelta (a, b, c) ()))/(2 * a()),
      (-b() - scala.math.sqrt(computeDelta (a, b, c) ()))/(2 * a())
    ))
  }
}
