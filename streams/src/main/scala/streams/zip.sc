val res = List((1,"a"), (3, "b"), (4, "d")).unzip
res._2

val sr = res._1.toStream
val r5 = 4 #:: sr

sr.take(5).toList

val more = for {
  p <- r5
} yield p

more

sr ++ Stream(5)

(Stream(5) ++ sr).take(3)

sr.take(2)


def from(paths: Set[Int]): Stream[Set[Int]] = {
 if (paths.isEmpty) Stream.Empty
 else {
   val more = for {
     //path <- paths
     next <- paths map(p => p * p)
   } yield next
   paths #:: from(more)
 }
}

val sett = Set(2, 3, 4)
from(sett).take(9).toList

sett + 5