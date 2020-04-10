package observatory

import java.time.LocalDate

import org.junit.Assert.assertEquals
import org.junit.Test

class ExtractionTestSuite extends ExtractionTest {

  @Test def `'locateTemperatures'`: Unit = {
    val testStationsFilePath = "/stations_test.csv"
    val test2015DataFilePath = "/2015_test.csv"
    val correctResult = List(
      (LocalDate.of(2015, 8, 11), Location(37.35, -78.433), 27.3),
      (LocalDate.of(2015, 12, 6), Location(37.358, -78.438), 0.0),
      (LocalDate.of(2015, 1, 29), Location(37.358, -78.438), 2.0)
    )

    def tupleToTuple(a: (LocalDate, Location, Double)) = a match {
      case (date, Location(lat, lon), temp) => (date.getYear, date.getMonth, date.getDayOfMonth, lat, lon, temp)
    }

    assertEquals(
      correctResult.sortBy(tupleToTuple),
      Extraction.locateTemperatures(2015, testStationsFilePath, test2015DataFilePath).toList.sortBy(tupleToTuple))
  }

  @Test def `'locationYearlyAverageRecords'`: Unit = {
    val records = Seq(
      (LocalDate.of(2015, 8, 11), Location(37.35, -78.433), 27.3),
      (LocalDate.of(2015, 12, 6), Location(37.358, -78.438), 0.0),
      (LocalDate.of(2015, 1, 29), Location(37.358, -78.438), 2.0)
    )
    val correctResult = List(
      (Location(37.35, -78.433), 27.3),
      (Location(37.358, -78.438), 1.0)
    )

    def tupleToTuple(a: (Location, Double)) = a match {
      case (Location(lat, lon), temp) => (lat, lon, temp)
    }

    assertEquals(
      correctResult.sortBy(tupleToTuple),
      Extraction.locationYearlyAverageRecords(records).toList.sortBy(tupleToTuple))
  }

}

