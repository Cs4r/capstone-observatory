package observatory


import observatory.Visualization.interpolateColor
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {


  val white = Color(255, 255, 255)
  val black = Color(0, 0, 0)
  val red = Color(255, 0, 0)
  val green = Color(0, 255, 0)

  val sortedPoints: Iterable[(Double, Color)] = List((-60.0, black), (0.0, red), (5.0, green), (60.0, white))
  val unsortedPoints: Iterable[(Double, Color)] = List((60.0, white), (-60.0, black), (0.0, red))


  test("interpolateColor should return color for lowest temperature when temperature is lower than the lowest") {

    val color = interpolateColor(sortedPoints, -160.0)

    assert(color == black)
  }

  test("interpolateColor should return color for highest temperature when temperature is higher than the highest") {


    val color = interpolateColor(sortedPoints, 160.0)

    assert(color == white)
  }


  test("interpolateColor should return color for highest temperature when temperature is higher than the highest and the iterable is not sorted") {


    val color: Color = interpolateColor(unsortedPoints, 160)

    assert(color == white)
  }

  test("interpolateColor should return color for lowest temperature when temperature is lower than the lowest and the iterable is not sorted") {
    val color: Color = interpolateColor(unsortedPoints, -160)

    assert(color == black)
  }

  test("interpolateColor should return same color as reference point when temperature is equal to the temperature of one of the reference points") {
    val color = interpolateColor(sortedPoints, 5.0)

    assert(color == green)
  }

  test("interpolateColor should return interpolated color when temperature is not in the list of reference points") {
    val color = interpolateColor(sortedPoints, 2.5)

    assert(color == Color(128, 128, 0))
  }


  test("interpolateColor should return interpolated color when temperature is not in the list of reference points and there are more than 3 colors") {
    val points: Iterable[(Double, Color)] = List((60.0, white), (-60.0, black), (0.0, red), (4.0, green))
    val color = interpolateColor(points, 2)

    assert(color == Color(128, 128, 0))
  }

  test("interpolateColor should return first color when there is only one reference color") {
    val color = interpolateColor(List((60.0, white)), 2)
    assert(color == white)
  }

  test("interpolateColor should throw IllegalArgument if iterable is empty") {

    intercept[IllegalArgumentException] {
      interpolateColor(List(), 1)
    }
  }

  test("color interpolation") {
    val color = interpolateColor(List((-1.0, Color(255, 0, 0)), (0.0, Color(0, 0, 255))), -0.75)

    val expected = Color(191, 0, 64)

    assert(color == expected)
  }

  test("exceeding the greatest value of a color scale should return the color associated with the greatest value") {

    val color = interpolateColor(List((-85.46584346884845, Color(255, 0, 0)), (-77.57141733273376, Color(0, 0, 255))), -77.57141733273376)

    val expected = Color(0, 0, 255)

    assert(color == expected)

  }
}
