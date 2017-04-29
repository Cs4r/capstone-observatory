package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.math.pow
import scala.math.atan
import scala.math.sinh
import scala.math.Pi
import scala.math.toDegrees

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param zoom Zoom level
    * @param x    X coordinate
    * @param y    Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location = {
    val lon = (x / pow(2, zoom)) * 360 - 180

    val lat = atan(sinh(Pi - ((y / pow(2, zoom)) * 2 * Pi))) * 180 / Pi

    Location(lat, lon)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @param zoom         Zoom level
    * @param x            X coordinate
    * @param y            Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
    import Visualization._
    val width = 256
    val height = 256
    val alpha = 127

    val pixels = (0 until width * height)
      .par.map(pos => {

      val xPos = (pos % width).toDouble / width + x
      val yPos = (pos / height).toDouble / height + y

      val temperature = predictTemperature(
        temperatures,
        toLocation(xPos, yPos, zoom)
      )

      val color: Color = interpolateColor(colors, temperature)

      pos -> color.toPixel(alpha)
    })
      .seq
      .sortBy(_._1)
      .map(_._2).toArray

    Image(width, height, pixels)
  }

  def toLocation(x: Double, y: Double, zoom: Int): Location = {
    Location(
      lat = toDegrees(atan(sinh(Pi * (1.0 - 2.0 * y / (1 << zoom))))),
      lon = x / (1 << zoom) * 360.0 - 180.0)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    *
    * @param yearlyData    Sequence of (year, data), where `data` is some data associated with
    *                      `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
                           yearlyData: Iterable[(Int, Data)],
                           generateImage: (Int, Int, Int, Int, Data) => Unit
                         ): Unit = {
    for ((year, data) <- yearlyData) {
      for (zoom <- 0 to 3) {
        for (x <- 0 until 1 << zoom) {
          for (y <- 0 until 1 << zoom) {
            generateImage(year, zoom, x, y, data)
          }
        }
      }
    }
  }

}
