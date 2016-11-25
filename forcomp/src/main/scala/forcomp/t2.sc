val t4 =  List(('a', 1),('a', 2), ('b', 1), ('b', 2))

val res = t4.combinations(3).toList



def fil(o : List[(Char, Int)], results : List[(Char, Int)]): List[(Char, Int)] = {
  //println(o)
  if(o.isEmpty) results sortBy(p => p._1)
  else if (results exists(p => p._1 == o.head._1)) fil(o.tail, results)
  else fil(o.tail, o.head::results)
}
res(2)
fil(res(2), List())

res map(p => fil(p, List()))
//val groups = res(0) groupBy(p => p._1)
//res(0) filter(p => groups.contains(p._1))



//for (p <- t2 if p._2 > 1) yield p


//t2 map (p => for (i until p._2) yield())

def enumerateOcurrence(pair: (Char, Int)): Seq[(Char, Int)] = {
  for {
    i <- 1 until pair._2 + 1
  } yield (pair._1, i)
}



val t2 = List(('a', 2), ('b', 2))
val subs = t2 flatMap (p => enumerateOcurrence(p))
for(p <- subs) yield p

val bm = 0 to 2 flatMap(i => List('a','b').combinations(i).toList)

t2 groupBy(p => p._1)


for {
  i <- 1 to 2
  j <- 1 to 2
} yield List(('a', i), ('b', j))

//def combinations(occurrences: Occurrences): List[Occurrences]
//def f2(o : List[(Char, Int)], results : List[(Char, Int)]): List[(Char, Int)] = {
//  println(o)
//  if(o.isEmpty) results sortBy(p => p._1)
//  else {
//    val diffs = o filter(p => p._1 != o.head._1)
//  }
//
//  //else if (results exists(p => p._1 == o.head._1)) fil(o.tail, results)
//  //else fil(o.tail, o.head::results)
//}
//
//t4 filter(p => p._1 != 'a')
//f2(t4, List())



//val t5 = List(('b', 1), ('b', 2))
//
//val groups = t4 groupBy(p => p._1)
//
//for {
//  i <- groups.keys
//  j <- groups(i)
//} yield j