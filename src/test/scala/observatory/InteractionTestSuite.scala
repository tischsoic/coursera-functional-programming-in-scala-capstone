package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import org.junit.Test

class InteractionTestSuite extends InteractionTest {

  @Test def `'tile'`: Unit = {
    val t0 = System.currentTimeMillis()

    val year = 2015
    val zoom = 0
    val (tileX, tileY) = (0, 0)
    println("!!!!!!!!!!!!!!!! 1111")
    val a = Extraction.locateTemperatures(2015, "/stations.csv", s"/$year.csv")
    println("Elapsed time: " + (System.currentTimeMillis() - t0) / 1000 + "s")
    println("!!!!!!!!!!!!!!!! 2222")
    val temperatures = Extraction.locationYearlyAverageRecords(a)
    println("Elapsed time: " + (System.currentTimeMillis() - t0) / 1000 + "s")
    println("!!!!!!!!!!!!!!!! 3333")
    val colors: List[(Temperature, Color)] = List(
      (60, Color(255, 255, 255)),
      (32, Color(255, 0, 0)),
      (12, Color(255, 255, 0)),
      (0, Color(0, 255, 255)),
      (-15, Color(0, 0, 255)),
      (-27, Color(255, 0, 255)),
      (-50, Color(33, 0, 107)),
      (-60, Color(0, 0, 0))
    )
    for (a <- 0 to 1; b <- 0 to 1) {
      val zoom = 1
      val tileX = a
      val tileY = b
      val image = Interaction.tile(temperatures, colors, Tile(tileX, tileY, zoom))
      println("Elapsed time: " + (System.currentTimeMillis() - t0) / 1000 + "s")
      println("!!!!!!!!!!!!!!!! 444")
      //    val pix = new Array[Pixel](4)
      //    pix(0) = Pixel(100, 100, 100, 255)
      //    pix(1) = Pixel(100, 100, 100, 255)
      //    pix(2) = Pixel(255, 0, 0, 255)
      //    pix(3) = Pixel(100, 100, 100, 255)
      //    val image = Image(2, 2, pix)
      //    image.fit(4, 4)
      image.output(new java.io.File(s"target/temperatures/$year/$zoom/$tileX-$tileY.png"))
    }
  }

}

