import barneshut.Fork
import common._
import barneshut.conctrees._

package object barneshut {

  class Boundaries {
    var minX = Float.MaxValue

    var minY = Float.MaxValue

    var maxX = Float.MinValue

    var maxY = Float.MinValue

    def width = maxX - minX

    def height = maxY - minY

    def size = math.max(width, height)

    def centerX = minX + width / 2

    def centerY = minY + height / 2

    override def toString = s"Boundaries($minX, $minY, $maxX, $maxY)"
  }

  sealed abstract class Quad {
    def massX: Float

    def massY: Float

    def mass: Float

    def centerX: Float

    def centerY: Float

    def size: Float

    def total: Int

    def insert(b: Body): Quad


  }

  case class Empty(centerX: Float, centerY: Float, size: Float) extends Quad {
    def massX: Float = centerX
    def massY: Float = centerY
    def mass: Float = 0
    def total: Int = 0
    def insert(b: Body): Quad = Leaf(centerX, centerY, size, Seq(b))

    override def toString = s"Empty($mass, $centerX, $centerY, $size)"
  }

  case class Fork(nw: Quad, ne: Quad, sw: Quad, se: Quad
  ) extends Quad {
    //println("Fork: " + nw  + ", "+ ne + ", " +  sw + ", " + se)
    val centerX: Float = se.centerX - se.size/2
    val centerY: Float = se.centerY - se.size/2
    val size: Float = se.size * 2
    val mass: Float = nw.mass + ne.mass + sw.mass + se.mass

    val massX: Float = {
      if (mass > 0) ((nw.massX * nw.mass) + (ne.massX * ne.mass) + (sw.massX * sw.mass) + (se.massX * se.mass))/mass
      else centerX
    }

    val massY: Float = {
      if (mass > 0)(nw.massY * nw.mass + ne.massY * ne.mass + sw.massY * sw.mass + se.massY * se.mass)/mass
      else centerY
    }
    val total: Int = nw.total + ne.total + sw.total + se.total

    def insert(b: Body): Fork = {
      if (bounds(nw, b)) Fork(nw.insert(b), ne, sw, se)
      else if(bounds(ne, b)) Fork(nw, ne.insert(b), sw, se)
      else if(bounds(sw, b)) Fork(nw, ne, sw.insert(b), se)
      else Fork(nw, ne, sw, se.insert(b))
    }
//    override def toString = s"Fork($mass, $centerX, $centerY, $size, $nw, $ne, $sw, $se)"
      override def toString = s"Fork($mass, $centerX, $centerY, $size)"
  }

  case class Leaf(centerX: Float, centerY: Float, size: Float, bodies: Seq[Body])
  extends Quad {
    val bodyMass  = bodies map(b => b.mass) sum
    val (mass, massX, massY) = (bodyMass: Float,
      (bodies map(b => b.mass * b.x) sum)/bodyMass : Float,
      (bodies map(b => b.mass * b.y) sum)/bodyMass : Float)
    val total: Int = bodies.length

    /**
      * If the size of a Leaf is greater than a predefined constant minimumSize,
      * inserting an additonal body into that Leaf quadtree creates a Fork quadtree
      * with empty children, and adds all the bodies into that Fork (including the
      * new body).
      * Otherwise, inserting creates another Leaf with all the existing bodies and the new one.
      * @param b
      * @return
      */
    def insert(b: Body): Quad = {
      //println("Leaf.insert: this " + this + "; b = " + b + ", size = " + size + ", minimumSize = " + minimumSize)
      if (size > minimumSize) {
        val newSize = size/2
        val offset = size/4
        var fork = Fork(
          Empty(centerX - offset, centerY - offset, newSize),
          Empty(centerX + offset, centerY - offset, newSize),
          Empty(centerX - offset, centerY + offset, newSize),
          Empty(centerX + offset, centerY + offset, newSize))

        var i = 0
        while (i < bodies.length) {
          fork = fork.insert(bodies(i))
          i = i + 1
        }

        fork.insert(b)
      }
      else {
        val leaf = Leaf(centerX, centerY, size, bodies:+ b)
      //  println("insert: leaf = " + leaf)
        leaf
      }
    }

    override def toString = s"Leaf($mass, $centerX, $centerY, $size, $bodies)"
  }

  def bounds(quad: Quad, body: Body): Boolean = {

    val hs = quad.size/2f
    if(body.x >= quad.centerX - hs &&
        body.x <= quad.centerX + hs &&
        body.y >= quad.centerY - hs &&
        body.y <= quad.centerY + hs) {
      true
    }
    else false
  }

  def minimumSize = 0.00001f

  def gee: Float = 100.0f

  def delta: Float = 0.01f

  def theta = 0.5f

  def eliminationThreshold = 0.5f

  def force(m1: Float, m2: Float, dist: Float): Float = gee * m1 * m2 / (dist * dist)

  def distance(x0: Float, y0: Float, x1: Float, y1: Float): Float = {
    val dist = math.sqrt((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)).toFloat
    println("dist = " + dist)
    dist
  }

  class Body(val mass: Float, val x: Float, val y: Float, val xspeed: Float, val yspeed: Float) {

    def updated(quad: Quad): Body = {
      println("updated: body = " + this + ": quad = " + quad)
      var netforcex = 0.0f
      var netforcey = 0.0f

      def addForce(thatMass: Float, thatMassX: Float, thatMassY: Float): Unit = {
       println("addForce: thatMass = " + thatMass + ", thatMassX = " + thatMassX + ", thatMassY = " + thatMassY)
        ////println("pre: netforcex = " + netforcex)
        val dist = distance(thatMassX, thatMassY, x, y)
        /* If the distance is smaller than 1f, we enter the realm of close
         * body interactions. Since we do not model them in this simplistic
         * implementation, bodies at extreme proximities get a huge acceleration,
         * and are catapulted from each other's gravitational pull at extreme
         * velocities (something like this:
         * http://en.wikipedia.org/wiki/Interplanetary_spaceflight#Gravitational_slingshot).
         * To decrease the effect of this gravitational slingshot, as a very
         * simple approximation, we ignore gravity at extreme proximities.
         */
        if (dist > 1f) {
          val dforce = force(mass, thatMass, dist)
          val xn = (thatMassX - x) / dist
          val yn = (thatMassY - y) / dist
          val dforcex = dforce * xn
          val dforcey = dforce * yn
          netforcex += dforcex
          netforcey += dforcey
        }
        println("post: netforcex = " + netforcex)

      }


      def traverse(quad: Quad): Unit = (quad: Quad) match {
        case Empty(_, _, _) =>
          // no force
        case Leaf(_, _, _, bodies) => {
          // add force contribution of each body by calling addForce
          println("leaf: bodies = " + bodies)
//          println("mm")
          val m = bodies.map(b => b.mass).foldLeft(0f)((l , r) => l + r)
          val mx = bodies.map(b => b.mass * b.x).foldLeft(0f)((l , r) => l + r)
          val my = bodies.map(b => b.mass * b.y).foldLeft(0f)((l , r) => l + r)

//          println("m = " + m + ", mx = " + mx + ", my = " + my)
//          println("qm = " + quad.mass + ", qmx = " + quad.massX + ", qmy = " + quad.massY)
//          println("bm = " + mass + ", x = " + x + ", y = " + y)
//          println("bm = " + mass + ", mx = " + (x*mass) + ", my = " + (y*mass))
          //addForce(m, mx, my)
          //          addForce(bodies map(b => b.mass) sum,
          //            bodies map(b => b.mass * b.x) sum,
          //            bodies map(b => b.mass * b.y) sum)
          bodies map(b => addForce(b.mass, b.mass * b.x, b.mass * b.y))
          //bodies map(b => addForce(mass, mass * x, mass * y))
          //addForce(quad.mass + mass, quad.massX + (mass * x), quad.massY + (mass * y))
          //addForce(quad.mass, quad.massX, quad.massY)
         // addForce(quad.mass + mass, quad.massX + (mass * x), quad.massY + (mass * y))
         // addForce(quad.mass + m, quad.massX + mx, quad.massY + my)
        }
        case Fork(nw, ne, sw, se) =>
          val dist = distance(quad.centerX, quad.centerY, x, y)
          println("quad.size / dist < theta = " + (quad.size / dist < theta))
          if(quad.size / dist < theta) addForce(quad.mass, quad.massX, quad.massY)
          else {
            traverse(nw)
            traverse(ne)
            traverse(sw)
            traverse(se)
          }
          // see if node is far enough from the body,
          // or recursion is needed
      }

      traverse(quad)

      val nx = x + xspeed * delta
      val ny = y + yspeed * delta
      val nxspeed = xspeed + netforcex / mass * delta
      val nyspeed = yspeed + netforcey / mass * delta

//      println("nxspeed = " + nxspeed)
//      println("xspeend = " + xspeed)
//      println("netforcex = " + netforcex)
//      println("mass = " + mass)
//      println("delta = " + delta)
      new Body(mass, nx, ny, nxspeed, nyspeed)
    }
    override def toString = s"Body($mass, $x, $y)"
  }

  val SECTOR_PRECISION = 8

  class SectorMatrix(val boundaries: Boundaries, val sectorPrecision: Int) {
    val sectorSize = boundaries.size / sectorPrecision
    val matrix = new Array[ConcBuffer[Body]](sectorPrecision * sectorPrecision)
    for (i <- 0 until matrix.length) matrix(i) = new ConcBuffer

    def +=(b: Body): SectorMatrix = {
      ???
      this
    }

    def apply(x: Int, y: Int) = matrix(y * sectorPrecision + x)

    def combine(that: SectorMatrix): SectorMatrix = {
      ???
    }

    def toQuad(parallelism: Int): Quad = {
      def BALANCING_FACTOR = 4
      def quad(x: Int, y: Int, span: Int, achievedParallelism: Int): Quad = {
        if (span == 1) {
          val sectorSize = boundaries.size / sectorPrecision
          val centerX = boundaries.minX + x * sectorSize + sectorSize / 2
          val centerY = boundaries.minY + y * sectorSize + sectorSize / 2
          var emptyQuad: Quad = Empty(centerX, centerY, sectorSize)
          val sectorBodies = this(x, y)
          sectorBodies.foldLeft(emptyQuad)(_ insert _)
        } else {
          val nspan = span / 2
          val nAchievedParallelism = achievedParallelism * 4
          val (nw, ne, sw, se) =
            if (parallelism > 1 && achievedParallelism < parallelism * BALANCING_FACTOR) parallel(
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            ) else (
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            )
          Fork(nw, ne, sw, se)
        }
      }

      quad(0, 0, sectorPrecision, 1)
    }

    override def toString = s"SectorMatrix(#bodies: ${matrix.map(_.size).sum})"
  }

  class TimeStatistics {
    private val timeMap = collection.mutable.Map[String, (Double, Int)]()

    def clear() = timeMap.clear()

    def timed[T](title: String)(body: =>T): T = {
      var res: T = null.asInstanceOf[T]
      val totalTime = /*measure*/ {
        val startTime = System.currentTimeMillis()
        res = body
        (System.currentTimeMillis() - startTime)
      }

      timeMap.get(title) match {
        case Some((total, num)) => timeMap(title) = (total + totalTime, num + 1)
        case None => timeMap(title) = (0.0, 0)
      }

      println(s"$title: ${totalTime} ms; avg: ${timeMap(title)._1 / timeMap(title)._2}")
      res
    }

    override def toString = {
      timeMap map {
        case (k, (total, num)) => k + ": " + (total / num * 100).toInt / 100.0 + " ms"
      } mkString("\n")
    }
  }
}
