import forcomp.Anagrams

def enumerateOcurrence(pair: (Char, Int)): Seq[(Char, Int)] = {
  for {
    i <- 1 until pair._2 + 1
  } yield (pair._1, i)
}

val t2 = List(('a', 2), ('b', 2))
val subs = t2 flatMap (p => enumerateOcurrence(p))


def eo(pair: (Char, Int), others: List[(Char, Int)]): List[List[(Char, Int)]] = {
  //println("pair: " + pair + ": others = " + others)
  if(others.isEmpty) Nil
  else List(pair, others.head)::eo(pair, others.tail)
}

//eo(('a', 1), List( ('b', 1), ('b', 2), ('c', 2)))

//List(('a', 1), ('a', 2)) map(p => eo(p, List( ('b', 1), ('b', 2), ('c', 2))))

//val t4 = List(('a', 1),('a', 2), ('b', 1), ('b', 2))
//t4 filter(p => p._1 != 'a')

val res = subs flatMap(p => eo(p, subs filter(pp => pp._1 != p._1)))
val ttt = res map(p => p.toSet[(Char, Int)]) distinct

val tttt = ttt map(p => p.toList)
tttt::List(List())


def combinations(occurrences: List[(Char, Int)]): List[List[(Char, Int)]] = {
  val r1 = occurrences flatMap (p => enumerateOcurrence(p))
  val r2 = r1 flatMap(p => eo(p, r1 filter(pp => pp._1 != p._1)))
  val r3 = r2 map(p => p.toSet[(Char, Int)]) distinct
  val r4 = r3 map(p => p.toList)
  //List(List()):::r1 map(p => List(p)):::r4
  val r5 = r1 map(p => List(p))
  r5:::r4:::List(List())
}

val res3 = combinations(t2)
//val res4 = subs map(p => List(p))
//res3:::res4:::List(List())
//res4:::res3:::List(List())

//un map(p => p.toList[(Char, Int)])

//val groups = t4 groupBy(p => p._1)
//groups.keys
//groups.keys.head
//groups.values.toList
//
//
//for {
//  i <- groups.keys
//  j <- groups(i)
//} yield eo(j, t4 filter(p => p._1 != i))