package observatory

import java.time.LocalDate

import scala.io.Source

/**
  * 1st milestone: data extraction
  */
object Extraction {

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    // ignore data coming from stations that have no GPS coordinates
    // The temperature field contains a decimal value (or 9999.9 if missing)

    val stationStream = getClass.getResourceAsStream(stationsFile)
    val stationToLocation = Source.fromInputStream(stationStream).getLines()
      .map(line => line.split(","))
      .filter(chunks => chunks.size >= 4)
      .map(chunks => {
        val location = Location(chunks(2).toDouble, chunks(3).toDouble)
        (Station(chunks(0), chunks(1)), location)
      })
      .toMap

    val temperaturesStream = getClass.getResourceAsStream(temperaturesFile)

    Source.fromInputStream(temperaturesStream).getLines()
      .map(line => line.split(","))
      .map(chunks => {
        val station = Station(chunks(0), chunks(1))
        val month = chunks(2).toInt
        val day = chunks(3).toInt
        val temperatureInFahrenheit = if (chunks(4).isEmpty) 9999.9 else chunks(4).toDouble
        val temperature = toCelsius(temperatureInFahrenheit)

        val date = LocalDate.of(year, month, day)
        val location = stationToLocation.get(station)
        
        if (location.isDefined) Some((date, location.get, temperature)) else None
      })
      .filter(_.isDefined)
      .map(_.get)
      .toIterable
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    records
      .groupBy(_._2)
      .mapValues(triplet => triplet.map(_._3).sum / triplet.size)

  }


  private def toCelsius(inFahrenheit: Double) = (inFahrenheit - 32) * 5 / 9

}
