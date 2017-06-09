val test = List[Int](1,2,3,4)
test.foldLeft(0)(_+_)
test.foldLeft(0)((r, l) => r + l)

18f * 123f
(18f * 123f + 0)/123f

4f to 8f by 2

val x = List(1,2,3,4)
val y = List(5,6,7,8)

var rt = for {
  xi <- x
  yi <- y
} yield(xi, yi)

rt.minBy(f => f._1)