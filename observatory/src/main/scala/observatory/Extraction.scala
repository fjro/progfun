package observatory

import java.time.LocalDate
import java.io.File
import java.sql.Timestamp

import org.apache.spark.{SparkConf, SparkContext}
import org.apache.log4j.{Level, Logger}
import org.apache.spark.sql.{DataFrame, Row, SparkSession}
import org.apache.spark.sql.types._

/**
  * 1st milestone: data extraction
  */
object Extraction {

  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  val conf: SparkConf = new SparkConf()
  val sc: SparkContext = new SparkContext("local", "observatory", conf)
  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("observatory")
      .config("spark.master", "local")
      .getOrCreate()

  // For implicit conversions like converting RDDs to DataFrames
  import spark.implicits._

  /**
    * Creates the stations dataframe.
    * @param file The csv path
    * @return The dataframe.
    */
  def stationsDF(file: String): DataFrame = {
    val resource = getClass.getResource(file)
    val filePath = new File(resource.toURI).getPath
    val csv = sc.textFile(filePath)
    val rddOption = csv.map(line => parseStationRow(line))
    val rdd = rddOption.filter(r => r.isDefined).map(r => r.get)

    val schema = new StructType()
      .add(StructField("StnID", IntegerType, true))
      .add(StructField("WbanID", IntegerType, true))
      .add(StructField("Lat", DoubleType, true))
      .add(StructField("Long", DoubleType, true))

    spark.createDataFrame(rdd, schema)
  }

  /**
    * Creates a temperature dataframe.
    *
    * @param file The csv path.
    * @param year The year.
    * @return The dataframe
    */
  def tempDF(file: String, year: Int): DataFrame = {
    val resource = getClass.getResource(file)
    val filePath = new File(resource.toURI).getPath
    val csv = sc.textFile(filePath)
    val rddOption = csv.map(line => parseTempRow(line, year))
    val rdd = rddOption.filter(r => r.isDefined).map(r => r.get)

    val schema = new StructType()
      .add(StructField("StnID", IntegerType, true))
      .add(StructField("WbanID", IntegerType, true))
      .add(StructField("Date", TimestampType, true))
      .add(StructField("Temp", DoubleType, true))

    spark.createDataFrame(rdd, schema)
  }

  /**
    * Safely parses a String to Double.
    * @param s The value
    * @return The double value or NaN.
    */
  def parseDouble(s: String): Double = {
    s match {
      case null => Double.NaN
      case s => if (s.isEmpty) Double.NaN else s.toDouble
    }
  }

  def parseInt(s: String): Int = {
    s match {
      case null => Int.MinValue
      case s => if (s.isEmpty) Int.MinValue else s.toInt
    }
  }


  /**
    * Parses a station csv line into a Row.
    * @param line The csv line.
    * @return A row or None.
    */
  def parseStationRow(line: String): Option[Row] = {
    if (line == null || line.isEmpty) None
    else {
      val parts = line.trim.split(",")
      if (parts.length == 4) Some(Row(parseInt(parts(0)), parseInt(parts(1)), parseDouble(parts(2)), parseDouble(parts(3))))
      else None
    }
  }

  /**
    * Parses a temperature csv line into a Row.
    * @param line The csv line.
    * @return A row or None.
    */
  def parseTempRow(line: String, year: Int): Option[Row] = {
    if (line == null || line.isEmpty) None
    else {
      val parts = line.trim.split(",")
      if (parts.length == 5) {
        Some(
          Row(parseInt(parts(0)),
          parseInt(parts(1)),
          Timestamp.valueOf(LocalDate.of(year, parts(2).toInt, parts(3).toInt).atStartOfDay()),
          parseDouble(parts(4))))
      }
      else None
    }
  }

  /**
    * Converts fahrenheit to celcius.
    * @param f The Fahrenheit value.
    * @return The Celcius value
    */
  def fahrenheitToCelsius(f: Double): Double = (f - 32.0) * (5.0/9.0)

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    val stations = stationsDF(stationsFile).filter($"StnID" =!= Int.MinValue && $"WbanID" =!= Int.MinValue )
    val temps = tempDF(temperaturesFile, year).filter($"StnID" =!= Int.MinValue && $"WbanID" =!= Int.MinValue && $"Temp" =!= 9999.9)
    val j = stations.join(temps, stations("StnId") === temps("StnId") && stations("WbanID") === temps("WbanID"))
    val res = j.select("Date", "Lat", "Long", "Temp").rdd.map {
      case Row(date: Timestamp, lat: Double, longitude: Double, temp: Double) => (date.toLocalDateTime.toLocalDate, Location(lat, longitude), fahrenheitToCelsius(temp))
    }
    res.collect()
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    records.groupBy(o => (o._2, o._1.getYear)).mapValues(v => (v.head._2, avg(v))).values
  }

  /**
    * Caluculates the mean temperature ignoring NAs.
    * @param obs The triplet of observations.
    * @return The mean temperature to 1 dp.
    */
  def avg(obs: Iterable[(LocalDate, Location, Double)]): Double = {
    val temps = obs.map(t => t._3).filter(t => !t.isNaN)
    val d = temps.reduce(_+_)/temps.size
    round(d)
  }

  /**
    * Rounds a Double to the specified number of places.
    *
    * @param d The value to round.
    * @param dp The number of decimal places
    * @return The rounded value
    */
  def round(d: Double, dp: Int = 1): Double = BigDecimal(d).setScale(dp, BigDecimal.RoundingMode.HALF_UP).toDouble

}
