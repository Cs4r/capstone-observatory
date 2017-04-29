package observatory

/**
  * 4th milestone: value-added information
  */
object Manipulation {

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Double)]): (Int, Int) => Double = {
    val grid =
      (-89 to 90).toParArray.flatMap(lat => {
          (-180 to 179).toParArray
            .map(lon =>
              (Location(lat, lon), Visualization.predictTemperature(temperatures, Location(lat, lon))))
        }).toMap


    (lat, lon) => grid(Location(lat, lon))
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Double)]]): (Int, Int) => Double = {
    val gridsForAllYears = temperaturess.toParArray.map(makeGrid)

    (lat, lon) => {
      val temperatures = gridsForAllYears.map(grid => grid(lat, lon))
      temperatures.sum / temperatures.size
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A sequence of grids containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Double)], normals: (Int, Int) => Double): (Int, Int) => Double = {
    val grid = makeGrid(temperatures)

    (lat, lon) => grid(lat, lon) - normals(lat, lon)
  }


}

