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
  }

  test("lerpColour") {
    assert(lerpColor(Color(255,0,0), Color(0,0,255), 0.5) === Color(128,0,128))
  }

}
