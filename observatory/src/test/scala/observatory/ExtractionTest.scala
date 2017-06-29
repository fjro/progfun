package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import Extraction._

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {

  test("fahrenheit conversion") {
    assert(fahrenheitToCelsius(100.0) === 37.77777777777778, "Temp conversion failed")
  }
  
}