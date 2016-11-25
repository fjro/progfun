package funsets

object Main extends App {
  import FunSets._
  println("hello...")
  println(contains(singletonSet(2), 1))
  val s1 = singletonSet(1)
  val s2 = singletonSet(2)
  val s3 = union(s1, s2)
  println("4 = " + contains(s3, 4))
  println("1 = " + contains(s3, 1))
  println("2 = " + contains(s3, 2))
  println("s1 = " + s1)
  println("s3 = " + s3)


  def f2(p: Int): Boolean = p > 8
  println("f2 = " + f2(9))

  val s = union(union(s1, s2), singletonSet(3))
  val s4 = union(s, singletonSet(4))
  val s5 = union(s4, singletonSet(5))
  val s7 = union(s5, singletonSet(7))
  val s1000 = union(s7, singletonSet(12))

  println("Printing...")
  printSet(s1000)

  var result = forall(s1000, (v: Int) => v <= 13)
  println(result, " v <= 12")

  var result2 = exists(s1000, (v: Int) => v == 4)
  println(result2, " v == 4")

  printSet(s1000)
  val mapped = map(s1000, (v: Int) => v + v)
  println("Printing mapped...")
  printSet(mapped)

  println("contains 1 = "+ contains(mapped, 1))
  println("contains 12 = "+ contains(mapped, 12))
  println("contains 16 = "+ contains(mapped, 16))

  def t2(x: Int): Int = x * x
  println("t2 = " + t2(4))
}
