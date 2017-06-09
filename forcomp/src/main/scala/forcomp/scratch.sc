import forcomp.Anagrams

//val word = "myteststring"
//val wo = word.trim.toLowerCase groupBy(c => c)  map(p => (p._1, p._2.length))
//wo.toList sortBy (c => c._1)
//
//Anagrams.wordOccurrences(word)
//
//val sentence = List[String]("A", "valid", "sentence")
////sentence.foldRight((x: String, y: String) => x.concat(y))
//val ss = sentence mkString(" ")
//Anagrams.wordOccurrences(ss)
//
////val sample = List("ate", "eat", "tea") ///Anagrams.dictionary.take(3)
//val sample = Anagrams.dictionary.take(3)
////lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = ???
//sample map (w => (Anagrams.wordOccurrences(w), w))
//
//val res = sample map (w => (Anagrams.wordOccurrences(w), w)) groupBy(p => p._1)
//
//res map { case (k,v) => (k,v.map(_._2))}
//
//object pairs {
//  val n = 7
//  (1 until n) map (i =>
//    (1 until i) map (j => (i, j)))
//}
//pairs
//
//(1 until 7) flatMap (i =>
//  (1 until i) map (j => (i,j))) //ilter(p -> p._)

//for (s) yield e

for {
  i <- 1 until 7
  j <- 1 until i
  if (i > 4)
} yield (i, j)

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

def outer(occurences: List[(Char, Int)]): List[(Char, Int)] = {
  if(occurences.isEmpty) Nil
  else occurences.head :: inner(occurences.tail)
}

def inner(occurences: List[(Char, Int)]): List[(Char, Int)] = {
  if(occurences.isEmpty) Nil
  else occurences.head :: inner(occurences.tail)
}

inner(subs)

//subs.combinations(3).toList

