package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Visualization._

import scala.math._

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile,
    *         as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location =
    Location(
      toDegrees(atan(sinh(Pi * (1.0 - 2.0 * y.toDouble / (1 << zoom))))),
      x.toDouble / (1 << zoom) * 360.0 - 180.0)

  //TODO: bug
  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
    val topLeft = tileLocation(zoom, x, y)
    val zoomedLocations = locs(topLeft, zoom)
    val predictedTemps = zoomedLocations.map(l => predictTemperature(temperatures, l))
    val alpha = 127 // [0,255]
    val pixels = predictedTemps
      .map(t => interpolateColor(colors, t))
      .map(c => Pixel.apply(c.red, c.green, c.blue, alpha)).toArray
    val image = Image.apply(imageSize, imageSize, pixels)
    image
  }

  // the image length or width
  val imageSize = 64

  /**
    * The tile width as latitude.
    * @param zoom The zoom level.
    * @return The width.
    */
  def width(zoom: Int) = (360d/pow(2, zoom))/imageSize

  /**
    * The tile height as longitude.
    * @param zoom The zoom level.
    * @return The height.
    */
  def height(zoom: Int) = (180d/pow(2, zoom))/imageSize

  /**
    * The latitudes for tiles.
    * @param tl The top left corner.
    * @param zoom The zoom level.
    * @return The latitudes
    */
  def latitudes(tl: Location, zoom: Int): Iterable[Double] = {
    val w = width(zoom)
    tl.lat until tl.lat + (w * imageSize) by w
  }

  /**
    * The longitudes for tiles.
    * @param tl The top left corner.
    * @param zoom The zoom level.
    * @return The longitudes
    */
  def longitudes(tl: Location, zoom: Int): Iterable[Double] = {
    val h = height(zoom)
    (tl.lon - (h * imageSize) to tl.lon by h).tail.reverse
  }

  /**
    * The tile locations.
    *
    * @param tl The top left corner.
    * @param zoom The zoom level.
    * @return The Locations
    */
  def locs(tl: Location, zoom: Int): Iterable[Location] = {
    val lats = latitudes(tl, zoom)
    val longs = longitudes(tl, zoom)

    for {
      lat <- lats
      lon <- longs
    } yield Location(lat, lon)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Int, Data)],
    generateImage: (Int, Int, Int, Int, Data) => Unit
  ): Unit = {
    for {
      (year, ad) <- yearlyData
      zoom <- 0 to 3
      x <- 0 until pow(2, zoom).toInt
      y <- 0 until pow(2, zoom).toInt
    } {
      generateImage(year, zoom, x, y, ad)
      //      val path = "target/temperatures/zoom/" + x + "-" + y + ".png"
      //      image.output(new java.io.File("target/map16.png"))
    }
  }

}
