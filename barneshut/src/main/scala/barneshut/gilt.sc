val test = List[Int](1,2,3,4)
test.permutations.mkString("\n")

val constraint = (5, 'M')
val n = 5
val finishes = Set[Char]('M', 'G')

for {
  i <- 1 to n
  c  <- finishes
} yield (i, c)


val a = Seq[(Int, Char)]((1, 'M'), (3,'G'), (5,'G'))
val b = Seq[(Int, Char)]((2, 'G'), (3,'G'), (4,'G'))
val c = Seq[(Int, Char)]((5,'M'))

val d = Seq[(Int, Char)]((1,'M'))
val e = Seq[(Int, Char)]((1,'G'))

val customers = Seq[Seq[(Int, Char)]](a, b, c)
val cust2 = Seq[Seq[(Int, Char)]](d, e)

for(v <- customers) yield v

//val c2= Set[Tuple2[Int, Char]]
println("hello")
//customers mapValues(c => c.length) mkString("\n")

//customers.values.flatten.toList

//id any hard constraints
for {
  v <- customers
  if (v.length == 1)
} yield v(0)

for {
  v <- customers
  if (v.length == 1)
} yield v(0)

for {
  i <- 1 to 5
  if (i != 3)
} yield i

val ana = List[Char]('G', 'G', 'G', 'G', 'M')
val a2 = Set[(Int, Char)]((1,'G'), (2,'G'), (3,'G'), (4,'G'), (5,'M'))
ana.permutations mkString("\n")

a.exists( v => a2.contains(v))

def isValid(perm: Seq[Char], cust: Seq[(Int, Char)]): Boolean = {
  val res = for {
    i <- 1 to perm.length
    col <- cust
    if i == col._1 && ana(i - 1) == col._2
  } yield i

  if (res.length > 0) true
  else false
}

isValid(ana, a)
println("hello")
isValid(ana, b)
b.exists( v => a2.contains(v))
c.exists( v => a2.contains(v))

def buildConstraints(constraint: (Int, Char), constraints: Set[(Int, Char)]): Set[(Int, Char)] = {
  if (constraints exists(c => c._1 == constraint._1
    && c._2 != constraint._2)) throw new Exception("Mutually exclusive constraint found")
  else constraints + constraint
}

def bc(cust: Seq[Seq[(Int, Char)]], constraints: Set[(Int, Char)]): Set[(Int, Char)] = {
  if (cust.isEmpty) constraints
  else {
    if (cust.head.length == 1) bc(cust.tail, buildConstraints(cust.head.head, constraints))
    else bc(cust.tail, constraints)
  }
}

def bc2(cust: Seq[Seq[(Int, Char)]], constraints: Set[(Int, Char)]): Set[(Int, Char)] = cust match {
  case Nil => constraints
  case x::tail =>
    if (cust.head.length == 1) bc(cust.tail, buildConstraints(cust.head.head, constraints))
    else bc(cust.tail, constraints)
}



buildConstraints((5, 'M'), Set[(Int, Char)]())
buildConstraints((5, 'M'), Set[(Int, Char)]((5, 'M')))
//buildConstraints((5, 'M'), Set[(Int, Char)]((5, 'G')))

val constraints = Set[(Int, Char)]()
val res = bc2(customers, constraints)
val r2 = bc2(cust2, constraints)

for {
  v <- customers
  if v.length == 1
} yield buildConstraints(v(0), constraints)

