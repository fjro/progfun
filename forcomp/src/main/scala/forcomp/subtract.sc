val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
val r = List(('r', 1))
val lad = List(('a', 1), ('d', 1), ('l', 1))

val grouped = lard groupBy(p => p._1)

grouped.keys.exists(p => p == 'r')



grouped.values.toList

val res = lard.filter(p => p._1 == 'r' && p._2 >= 1)
lard filter(p => r.exists(pp=> pp._1 == p._1 && pp._2 >= p._2))

val s1 = lard.toSet[(Char, Int)]
val s2 = r.toSet[(Char, Int)]
s1.diff(s2).toList

//grouped foldLeft{ case ('j'==k) => (k , v) }

//grouped('r')
//grouped

def subtract(x: List[(Char, Int)], y: List[(Char, Int)]): List[(Char, Int)] = {
  val xs = x.toSet[(Char, Int)]
  val inter = x filter(p => y.exists(pp=> pp._1 == p._1 && pp._2 >= p._2))
  val ys = inter.toSet[(Char, Int)]

  xs.diff(ys).toList
}

subtract(lard, r)

//def subtract(terms: Map[Char, List[(Char, Int)]], term : (Char, Int)): Map[Char, List[(Char, Int)]] = {
////  ter
//}