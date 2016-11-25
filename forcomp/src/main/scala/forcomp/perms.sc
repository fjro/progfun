import forcomp.Anagrams

List(('a', 1),('a', 2), ('b', 1), ('b', 2)).toSet[(Char, Int)].subsets.map(_.toList).toList

val r2 = List('a', 'b').toSet[Char].subsets.toList


//for {
//  i <- 1 until 7
//  j <- 1 until i
//  if (i > 4)
//} yield (i, j)

val t2 = List(('a', 2), ('b', 2))

//for (p <- t2 if p._2 > 1) yield p

val t3 = ('a', 3)


//t2 map (p => for (i until p._2) yield())

def enumerateOcurrence(pair: (Char, Int)): Seq[(Char, Int)] = {
  for {
    i <- 1 until pair._2 + 1
  } yield (pair._1, i)
}

val subs = t2 flatMap (p => enumerateOcurrence(p))
for(p <- subs) yield p

def outer(occurences: List[(Char, Int)]): List[List[(Char, Int)]] = {
  if(occurences.isEmpty) Nil
  else List(inner(occurences.head, occurences.tail)):::outer(occurences.tail)
}

def inner(h: (Char, Int), occurences: List[(Char, Int)]): List[(Char, Int)] = {
  println("h = " + h + ": occurences = " + occurences)
  if(occurences.isEmpty) Nil
  //else if(h._1 == occurences.head._1) inner(occurences.head, occurences.tail)
  else occurences.head :: inner(occurences.head, occurences.tail)
}

def i2(occurences: List[(Char, Int)]): List[List[(Char, Int)]] = {
  println("occurences = " + occurences)
  if(occurences.isEmpty) Nil
  else occurences::i2(occurences.tail)
}

def o2(occurences: List[(Char, Int)]): List[List[(Char, Int)]] = {
  if(occurences.isEmpty) Nil
  else i2(occurences):::outer(occurences.tail)
}

println("hello")
i2(subs)
//o2(subs)

//subs.combinations(3).toList

val res = List(('a', 1),('a', 2), ('b', 1), ('b', 2)).combinations(3).toList

val t4 =  List(('a', 1),('a', 2), ('b', 1), ('b', 2))

////t4.distinct(_ => _2)
//
////
def fil(o : List[(Char, Int)], results : List[(Char, Int)]): List[(Char, Int)] = {
  //println(o)
  if(o.isEmpty) results sortBy(p => p._1)
  else if (results exists(p => p._1 == o.head._1)) fil(o.tail, results)
  else fil(o.tail, o.head::results)
}
res(0)
fil(res(0), List())

res(0) filter(p => p._1 == 'a')
//
//res map(e => fil(e, List()))

//t4 exists(p => p._1 == 'a')
//res.foldLeft(Nil: List[(Char, Int)]) {
//  (acc, next) => if (acc contains next) acc
//  else next :: acc }.reverse

//for {
//  i <- 1 until t4.size
//} yield t4.combinations(i).toList
//
//1 until t4.size flatMap(i => t4.combinations(i).toList)


//t4 groupBy(p => p._1)