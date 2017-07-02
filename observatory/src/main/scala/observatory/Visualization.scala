package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import Math._

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  /**
    * Converts a location to radians
    * @param loc The location
    * @return The conversion
    */
  def toRadians(loc: Location): Location = Location(math.toRadians(loc.lat), math.toRadians(loc.lon))

  /**
    * Computes the great circle distance between two points.
    * @param loc1 Location 1
    * @param loc2 Location 2
    * @param radius Defaults to the earth's radius in km.
    * @return The distance.
    */
  def greatCircleDistance(loc1: Location, loc2: Location, radius: Double = 6371d): Double = {
    val rloc1 = toRadians(loc1)
    val rloc2 = toRadians(loc2)
    acos((sin(rloc1.lat) * sin(rloc2.lat)) + (cos(rloc1.lat) * cos(rloc2.lat) * cos(abs(rloc1.lon - rloc2.lon)))) * radius
  }

  /**
    * Computes the inverse distance weight.
    *
    * @param distance The distance.
    * @param p The power.
    * @return The idw.
    */
  def idw(distance: Double, p: Int = 2): Double = 1/pow(distance, p)

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    val distances = temperatures.map(t => (greatCircleDistance(location, t._1), t._2))
    val zeros = distances.count(d => d._1 == 0d)

    //TODO: slow, multiple passes, <= 1 will do
    if (zeros == 0) distances.map(d => idw(d._1) * d._2).reduce(_+_) / distances.map(d => idw(d._1)).reduce(_+_)
    else distances.find(d => d._1 == 0).get._2
  }

  /**
    * Linear interpolation between two double values.
    * @param v0
    * @param v1
    * @param t The alpha
    * @return
    */
  def lerp(v0: Double, v1: Double, t: Double): Double = (1 - t) * v0 + t * v1

  /**
    * Linear interpolation between two Int values.
    * @param v0
    * @param v1
    * @param t The alpha
    * @return
  */
  def lerpInt(v0: Int, v1: Int, t: Double): Int = math.round(lerp(v0, v1, t)).toInt

  /**
    * Linear interpolation between two Colors
    * @param c0
    * @param c1
    * @param t The alpha
    * @return The interpolated Color.
    */
  def lerpColor(c0: Color, c1: Color, t: Double): Color = {
    if (c0 == c1) c0
    else Color(
      lerpInt(c0.red, c1.red, t),
      lerpInt(c0.green, c1.green, t),
      lerpInt(c0.blue, c1.blue, t))
  }

  def bounds(list: List[(Double, Color)], d: Double): Option[((Double, Color), (Double, Color))] = {
    val r = list.find(p => p._1 == d)
    if (r.isDefined) Some((r.get, r.get))
    else {
      val res = for {
        i <- 0 until list.length -1
        if list(i)._1 < d && list(i + 1)._1 > d

      } yield (list(i), list(i + 1))

      if (res.size == 1) Some(res(0))
      else None
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    val sorted = points.toList.sortWith(_._1 < _._1)
    val closest = bounds(sorted, value).get

    //TODO: calculate alpha
    val alpha = 0.5
    lerpColor(closest._1._2, closest._2._2, alpha)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    ???
  }

}

