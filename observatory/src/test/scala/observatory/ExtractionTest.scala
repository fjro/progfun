package observatory

import java.io.File

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import Extraction._

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {

  test("fahrenheit conversion") {
    assert(fahrenheitToCelsius(100.0) === 37.77777777777778, "Temp conversion failed")
  }

  test("parseStation") {
    assert(parseStation("") === None)
    assert(parseStation(null) === None)
    assert(parseStation("11,") === None)
    assert(parseStation("1,2,+11.111,+222.222").get._2 === Location(11.111,222.222))
  }

  test("readStations") {
    val stations = readStations("/stations.csv")
    assert(stations.size === 2818)
  }

  test("readTemps") {
    val temps1975 = readTemps(1975, "/1975.csv")
    assert(temps1975.size === 721)

    val temps1976 = readTemps(1976, "/1976.csv")
    assert(temps1976.size === 722)
  }

  test("locateTemperatures") {
    val temps = locateTemperatures2(1975, "/stations.csv", "/1975.csv").toList
    assert(temps.length === 720)
  }

  test("locateTemperatures2") {
    val temps = locateTemperatures2(1975, "/stations.csv", "/1975.csv").toList
    assert(temps.length === 720)
  }

//  S

  test("Temp RDD") {
    assert(tempRdd(1975, "/1975.csv").count() === 250773)
  }
  
}