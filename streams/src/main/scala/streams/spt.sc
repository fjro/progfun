
import streams.Bloxorz._

val test = Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('o', 'd'))
val elem = Vector('S', 'I')

//for(p <- elem) if(p =='-') false
//val p = terrain(Pos(0,0))
elem contains('S')

test map(e => e contains('d')) exists(e => e == true)

test.indexWhere(_.indexOf('-') >= 0) != -1

val res = test find(e => e contains('S'))

//res match {
//  case None => true
//  case Some9) => false
//}

def isValid(level : Vector[Char]): Boolean = {
  level contains('-')
}

isValid(elem)

for {
  i <- 1 to test.size
  j <- 1 to 2
  if(i == 1 && j == 2)
} yield (i, j)

test(1)
test(0).indexOf('o')
//for (b <- books; a <- b.authors if a startsWith ”Bird,”)

val r2 = for {
  i <- 0 until test.size
} yield(i, test(i).indexOf('o'))

val r3 = r2 find(i => i._2 == -1)
r3.head

val t2 = Vector((0,-1), (1,-1), (2,-1))
t2

//for {
//  i <- 1 to test.size
//  j <- test(i).indexOf('o')
//} yield (i, j)

for (i <- 1 to 2) {
  for (j <- 1 to 2)
    yield (i, j)
}

val test2 = Vector(Vector('S', 'T'), Vector('d', 'o'), Vector('o', 'd'))
test2.indexWhere(_.indexOf('d') >= 0)
test(2).indexOf('d')
