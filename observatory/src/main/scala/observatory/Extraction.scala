package observatory

import java.time.LocalDate
import java.io.File

import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}

import scala.collection.mutable.Map
import scala.io.{BufferedSource, Source}
import org.apache.log4j.{Level, Logger}

/**
  * 1st milestone: data extraction
  */
object Extraction {

  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)
  type stationID = (Int, Int)
  type station = (stationID, Location)
  type observation = (LocalDate, Double)
  type stationObservation = (stationID, observation)

  val conf: SparkConf = new SparkConf()
  val sc: SparkContext = new SparkContext("local", "observatory", conf)

  def parseStation(s: String): Option[station] = {
    try {
      val parts = s.split(",")
      if (parts.length == 4) {
        Some(((parts(0).toInt, parts(1).toInt), Location(parts(2).toDouble, parts(3).toDouble)))
      }
      else
        None
    }
    catch {
      case e: Exception => None
    }
  }

  def parseStation2(s: String): Option[Station] = {
      try {
        val parts = s.split(",")
        if (parts.length == 4)
          Some(Station(StationID(parts(0).toInt, parts(1).toInt), Location(parts(2).toDouble, parts(3).toDouble)))
        else
          None
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

      stations
    }
    finally
      src.close
  }

  def readStations2(fileName: String): Map[StationID, Location] = {
    val stations = Map[StationID, Location]()
    val src: BufferedSource = Source.fromInputStream(getClass.getResourceAsStream(fileName))

    try {
      val it: Iterator[String] = src.getLines()
      while(it.hasNext) {
        val line = it.next()
        val station = parseStation2(line)
        if (station.isDefined)
          stations +=  station.get.stationID -> station.get.location
      }

      stations
    }
    finally
      src.close
  }

  def parseTemp(s: String, year: Int): Option[stationObservation] = {
    try {
      val parts = s.split(",")
      if (parts.length == 5) {
        val si = (parts(0).toInt, parts(1).toInt)
        val obs = (LocalDate.of(year, parts(2).toInt, parts(3).toInt), fahrenheitToCelsius(parts(4).toDouble) )
        Some( (si, obs) )
      }
      else
        None
    }
    catch {
      case e: Exception => None
    }
  }

  def parseTemp2(year: Int, s: String): Option[Observation] = {
    try {
      val parts = s.split(",")
      if (parts.length == 5) {
        val si = StationID(parts(0).toInt, parts(1).toInt)
        val obs = Observation(si, LocalDate.of(year, parts(2).toInt, parts(3).toInt), getTemp(parts(4)) )
        Some( obs )
      }
      else
        None
    }
    catch {
      case e: Exception => None
    }
  }

  def getTemp(t: String): Double = {
    val temp = t.toDouble
    if (temp == 9999.9) Double.NaN
    else temp
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

      temps
    }
    finally
      src.close
  }

//  val stationRdd: RDD[Station] = {
//    val resource = getClass.getResource("/stations.csv")
//    val filePath = new File(resource.toURI).getPath
//    val rdd = sc.textFile(filePath)
//    rdd.map(s => parseStation2(s)).filter(o => o.isDefined).map(s => s.get)
//  }

  def tempRdd(year: Int, file: String): RDD[Observation] = {
    val resource = getClass.getResource(file)
    val filePath = new File(resource.toURI).getPath
    val rdd = sc.textFile(filePath)
    rdd.map(s => parseTemp2(year, s)).filter(o => o.isDefined).map(s => s.get)
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
    val stations = readStations2(stationsFile)
    val temps = tempRdd(year, temperaturesFile)
    temps.filter(t => stations.contains(t.stationID)).map(s => (s.date, stations(s.stationID), s.temp)).collect()
  }

  def locateTemperatures2(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    val stations = readStations(stationsFile)
    val temps = readTemps(year, temperaturesFile)
    stations.filter(s => temps.contains(s._1)).map(s => (temps(s._1)._1,  s._2, temps(s._1)._2))
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    records.groupBy(o => (o._2, o._1.getYear)).mapValues(v => (v.head._2, avg(v))).values
  }

  def avg(obs: Iterable[(LocalDate, Location, Double)]): Double = {
    val temps = obs.map(t => t._3).filter(t => !t.isNaN)
    val d = temps.reduce(_+_)/temps.size
    val c = fahrenheitToCelsius(d)
    BigDecimal(c).setScale(1, BigDecimal.RoundingMode.HALF_UP).toDouble
  }

}
