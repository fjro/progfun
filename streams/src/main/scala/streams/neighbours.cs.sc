def nno(neighbors: Stream[(Char, List[Int])],
                     explored: Set[Char]): Stream[(Char, List[Int])] = {

  val res = for {
    neighbour <- neighbors
    if !(explored contains(neighbour._1))
  } yield neighbour
  res
}

val exp = Set('a', 'b', 'c')
val neg = List[(Char, List[Int])](('J', List(1))).toStream
nno(neg, exp).take(10).toList

val neg3 = List[(Char, List[Int])](
  ('a', List(1)),
  ('j', List(1, 2)),
  ('f', List(3, 2)),
  ('b', List(3, 2))).toStream
nno(neg3, exp).take(10).toList


val s1 = (1 to 5).toStream
val s2 =  List[Char]('a', 'b', 'c').toStream

val io = for {
  v2 <- s2
  v1 <- s1
} yield (v2, v1)

io.take(7).toList

io.toList

