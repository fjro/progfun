package observatory

import java.time.LocalDate

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers
import observatory.Visualization._
import observatory.Extraction._

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {

  test("lerp") {
    assert(lerp(2d, 4d, 0.5d) === 3)
    assert(lerp(2d, 5d, 0.5d) === 3.5)
    assert(lerp(255, 0, 0.25) === 191.25)
    assert(lerp(0, 255d, 0.25) === 63.75)
  }

  test("lerpColour") {
    assert(lerpColor(Color(255,0,0), Color(0,0,255), 0.5) === Color(128,0,128))
    assert(lerpColor(Color(255,0,0), Color(0,0,255), 0.25) === Color(191,0,64))
  }

  test("bounds with intermediate") {
    val c = Color(250, 0, 0)
    val values = List[(Double, Color)]((12d, c), (2d, c), (3d, c), (0, c), (1, c)).sortWith(_._1 < _._1)
    val closest = bounds(values, 2.1)
    assert(closest._1._1 === 2)
    assert(closest._2._1 === 3)
  }

  test("bounds with Max") {
    val c = Color(250, 0, 0)
    val values = List[(Double, Color)]((12d, c), (2d, c), (3d, c), (0, c), (1, c)).sortWith(_._1 < _._1)
    val closest = bounds(values, 22.1)
    assert(closest._1._1 === 12)
    assert(closest._2._1 === 12)
  }

  test("bounds with Min") {
    val c = Color(250, 0, 0)
    val values = List[(Double, Color)]((12d, c), (2d, c), (3d, c), (0, c), (1, c)).sortWith(_._1 < _._1)
    val closest = bounds(values, -22.1)
    assert(closest._1._1 === 0)
    assert(closest._2._1 === 0)
  }

  test("interpolateColor") {
    val actual = interpolateColor(List((0.0,Color(255,0,0)), (5.0,Color(0,0,255))), value = 1.25)
    assert(actual === Color(191,0,64))
    val actual1 = interpolateColor(List((-1.0,Color(255,0,0)), (0.0,Color(0,0,255))), value = -0.75)
    assert(actual1 === Color(191,0,64))
    val actual2 = interpolateColor(List((-72.70731499297264,Color(255,0,0)), (1.0,Color(0,0,255))), value = -82.70731499297264)
    assert(actual2 === Color(255,0,0))

  }

  test("visualize") {
    val temps = locateTemperatures(1975, "/stations.csv", "/1975.csv").toList
    val means = locationYearlyAverageRecords(temps)
    var image = visualize(means, colors)

  }



}
