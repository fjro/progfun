import streams.Bloxorz._
import streams.GameDef

case class Pos(x: Int, y: Int) {
  /** The position obtained by changing the `x` coordinate by `d` */
  def dx(d: Int) = copy(x = x + d)

  /** The position obtained by changing the `y` coordinate by `d` */
  def dy(d: Int) = copy(y = y + d)
}
def terrainFunction(levelVector: Vector[Vector[Char]]): Pos => Boolean = {
  def isValid(pos: Pos): Boolean = levelVector map(e => e contains('-')) exists(e => e == true)
  isValid
}

def findChar(c: Char, levelVector: Vector[Vector[Char]]): Pos = {
  val r2 = for {
    i <- 0 until levelVector.size
  } yield(i, levelVector(i).indexOf(c))

  val r3 = r2 find(i => i._2 != -1)
  Pos(r3.head._1, r3.head._2)
}

val test = Vector(Vector('S', 'T'), Vector('d', 'o'), Vector('o', 'd'))
val elem = Vector('S', 'I')

test
findChar('d', test)