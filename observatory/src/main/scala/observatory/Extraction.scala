package observatory

import java.time.LocalDate
import java.io
import scala.collection.mutable.Map
import scala.io.{BufferedSource, Source}

/**
  * 1st milestone: data extraction
  */
object Extraction {

  type stationID = (Int, Int)
  type station = (stationID, Location)
  type observation = (LocalDate, Double)
  type stationObservation = (stationID, observation)

  def parseStation(s: String): Option[station] = {
    try {
      val parts = s.split(",")
      if (parts.length == 4) {
        return Some(((parts(0).toInt, parts(1).toInt), Location(parts(2).toDouble, parts(3).toDouble)))
      }
      else
        return None
    }
    catch {
      case e: Exception => None
    }
  }

  def readStations(fileName: String): Map[stationID, Location] = {
    val stations = Map[stationID, Location]()
    val src: BufferedSource = Source.fromInputStream(getClass.getResourceAsStream(fileName))

    try {
      val it: Iterator[String] = src.getLines()
      while(it.hasNext) {
        val line = it.next()
        val station = parseStation(line)
        if (station.isDefined)
          stations +=  station.get._1 -> station.get._2
      }

      return stations
    }
    finally
      src.close
  }

  def parseTemp(s: String, year: Int): Option[stationObservation] = {
    try {
      val parts = s.split(",")
      if (parts.length == 5) {
        val si = (parts(0).toInt, parts(1).toInt)
        val obs = (LocalDate.of(year, parts(2).toInt, parts(3).toInt), parts(4).toDouble )
        return Some( (si, obs) )
      }
      else
        return None
    }
    catch {
      case e: Exception => None
    }
  }

  def readTemps(year: Int, fileName: String): Map[stationID, observation] = {
    val temps = Map[stationID, observation]()
    val src: BufferedSource = Source.fromInputStream(getClass.getResourceAsStream(fileName))

    try {
      val it: Iterator[String] = src.getLines
      while(it.hasNext) {
        val line = it.next()
        val temp = parseTemp(line, year)
        if (temp.isDefined)
          temps +=  temp.get._1 -> temp.get._2
      }

      return temps
    }
    finally
      src.close
  }

  def fahrenheitToCelsius(f: Double): Double =
    (f - 32.0) * (5.0/9.0)

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    val stations = readStations(stationsFile)
    val temps = readTemps(year, temperaturesFile)
    return stations.filter(s => temps.contains(s._1)).map(s => (temps(s._1)._1,  s._2, temps(s._1)._2))
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    ???
  }

}
