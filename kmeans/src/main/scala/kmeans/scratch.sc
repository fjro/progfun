val means = List(1,2,3,4,5)
val pairs = means map(m => (m, 0))
means groupBy(m => m)
pairs groupBy(_._1) map { case (k,v) => (k,v.map(_._2))}
//means  map { case (k,v) => (k,v.map(_._2))}

