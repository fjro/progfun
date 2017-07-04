package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import Extraction._
import Visualization._

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {

  test("fahrenheit conversion") {
    assert(fahrenheitToCelsius(100.0) === 37.77777777777778, "Temp conversion failed")
  }

  test("Stations DF") {
    val df = stationsDF("/stations.csv")
    assert(df.count() === 28128)
  }

  test("Temp DF 1975") {
    val df = tempDF("/1975.csv", 1975)
    assert(df.count() === 2190974)
  }

  test("Temp DF 2000") {
    val df = tempDF("/2000.csv", 2000)
    assert(df.count() === 2546978)
  }

  test("locateTemperatures") {
    val temps = locateTemperatures(1975, "/stations.csv", "/1975.csv").toList
    assert(temps.length === 250434)
  }

}