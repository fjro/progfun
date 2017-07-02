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
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    ???
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

