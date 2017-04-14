(0xff000000 & 1) >>> 24
1 >>> 2

val r = 1
val y = r to 20 by (2*2+1)
val x = r to 10 by (r*2+1)
val z = x zip y
z map(p => p._1 + p._2)

for {
  yi <- y
  xi <- x
} yield(xi,yi)

def bound(value: Int, max: Int): Int = {
  if(value <= max) value
  else max
}

1 to 8
0 to 20 by 5 map(i => (i, bound(i + 4, 20)))
1 to 21 by 5
21/4

0 to 32 by 32



