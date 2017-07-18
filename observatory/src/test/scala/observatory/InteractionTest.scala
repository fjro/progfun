package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers
import observatory.Interaction._
import observatory.Extraction._
import observatory.Visualization._

@RunWith(classOf[JUnitRunner])
class InteractionTest extends FunSuite with Checkers {

//  test("width") {
//    assert(22.5/16 === width(0))
//    assert(11.25/16 === width(1))
//    assert(5.625/16 === width(2))
//    assert(2.8125/16 === width(3))
//  }
//
//  test("height") {
//    assert(11.25/16 === height(0))
//    assert(5.625/16 === height(1))
//    assert(2.8125/16 === height(2))
//    assert(1.40625/16 === height(3))
//  }
//
//  test("locations") {
//    val l = locs(Location(-180, 90), 0)
//    assert(256 * 256 === l.size)
//  }

  test("tile") {
    val temps = locateTemperatures(1975, "/stations.csv", "/1975.csv").toList
    val means = locationYearlyAverageRecords(temps)
    tile(means, colors, 0, 0, 0)
  }

}
