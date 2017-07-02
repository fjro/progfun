package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers
import observatory.Visualization._

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

  test("interpolateColor") {
    val actual = interpolateColor(List((-1.0,Color(255,0,0)), (0.0,Color(0,0,255))), value = -0.75)
    assert(actual === Color(191,0,64))
  }
}
