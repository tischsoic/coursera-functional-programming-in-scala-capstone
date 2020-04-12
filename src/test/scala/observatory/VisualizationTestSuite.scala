package observatory

import org.junit.Assert.assertEquals
import org.junit.Test

class VisualizationTestSuite extends VisualizationTest {


  @Test def `'predictTemperature'`: Unit = {
    val location1 = Location(0, 0)
    val location2 = Location(0, 1)
    val location3 = Location(0, -3)
    val location4 = Location(0, 0.007)
    val location5 = Location(0, 0.006)
    val (u1, u2) = (10d, 100d)
    val temperatures: List[(Location, Temperature)] = List(
      (location2, u1),
      (location3, u2)
    )
    val (distance12, distance13) = (111.2, 333.6)
    val p = 2
    val (w1, w2) = (1 / Math.pow(distance12, p), 1 / Math.pow(distance13, p))
    val predictedTemp = (w1 * u1 + w2 * u2) / (w1 + w2)
    val delta = 0.1
    val location1Temperature = (location1, -2d)
    val location4Temperature = (location4, -15d)
    val location5Temperature = (location5, -10d)

    assertEquals("same location",
      location1Temperature._2,
      Visualization.predictTemperature(location1Temperature :: temperatures, location1),
      delta)
    assertEquals("close location",
      location5Temperature._2,
      Visualization.predictTemperature(location4Temperature :: location5Temperature :: temperatures, location1),
      delta)
    assertEquals("far location",
      predictedTemp,
      Visualization.predictTemperature(temperatures, location1),
      delta)
  }

  @Test def `'inverseDistanceWeighting'`: Unit = {
    val distanceTemp: List[(Double, Temperature)] = List(
      (10, 10),
      (20, 100)
    )
//    val rdd = Visualization.sc.parallelize(distanceTemp)
    val (w1, w2) = ((1 / Math.pow(10, 2)), (1 / Math.pow(20, 2)))
    val (u1, u2) = (10, 100)
    val predictedTemp = (w1 * u1 + w2 * u2) / (w1 + w2)
    val delta = 0

    assertEquals("predicted temp", predictedTemp,  Visualization.inverseDistanceWeighting(distanceTemp)(2), delta)
  }

  @Test def `'computeDistance'`: Unit = {
    val delta = 1
    val delta2 = 6

    assertEquals("(10,10), (10,10): ", 0,  Visualization.computeDistance(10.toRadians, 10.toRadians, 10.toRadians, 10.toRadians), delta)
    assertEquals("(0,0), (0,10): ", 1112,  Visualization.computeDistance(0, 0, 0, 10.toRadians), delta)
    assertEquals("(0,0), (0,180): ", 20020,  Visualization.computeDistance(0, 0, 0, 180.toRadians), delta2)
  }


  @Test def `'interpolateColor'`: Unit = {
    val turquoise = Color(0, 255, 255)
    val yellow = Color(255, 255, 0)
    val red = Color(255, 0, 0)
    val white = Color(255, 255, 255)
    val temperatures: Iterable[(Temperature, Color)] = Seq(
      (-10, turquoise),
      (0, yellow),
      (10, red),
      (20, white)
    )

    assertEquals("-11: ", turquoise, Visualization.interpolateColor(temperatures, -11))
    assertEquals("-10: ", turquoise, Visualization.interpolateColor(temperatures, -10))
    assertEquals("0: ", yellow, Visualization.interpolateColor(temperatures, 0))
    assertEquals("3: ", Color(255, (255 * (7d / 10)).toInt, 0), Visualization.interpolateColor(temperatures, 3))
    assertEquals("22: ", white, Visualization.interpolateColor(temperatures, 22))
  }

  @Test def `'visualize'`: Unit = {
    val location2 = Location(0, 10)
    val location3 = Location(0, -30)
    val (u1, u2) = (10d, 100d)
    val temperatures: List[(Location, Temperature)] = List(
      (location2, u1),
      (location3, u2)
    )
    val colors: List[(Temperature, Color)] = List(
      (u1, Color(255, 0, 0)),
      (u2, Color(255, 255, 0))
    )
    val image = Visualization.visualize(temperatures, colors)

    image.output(new java.io.File("visualization.png"))
  }


}

