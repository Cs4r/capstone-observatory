package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.math._

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  private val imageWidth = 256
  private val imageHeight = 256
  private val alpha = 127

  /**
    * @param x X coordinate between 0 and 1
    * @param y Y coordinate between 0 and 1
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
                             x: Double,
                             y: Double,
                             d00: Double,
                             d01: Double,
                             d10: Double,
                             d11: Double
                           ): Double = d00 * (1 - x) * (1 - y) +  d10 * x * (1 - y) +  d01 * (1 - x) * y +  d11 * x * y


  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param zoom Zoom level of the tile to visualize
    * @param x X value of the tile to visualize
    * @param y Y value of the tile to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: (Int, Int) => Double,
    colors: Iterable[(Double, Color)],
    zoom: Int,
    x: Int,
    y: Int
  ): Image = {

    val locations = for {
      i <- (0 until imageWidth)
      j <- 0 until imageHeight
      xTile = i.toDouble / imageWidth + x
      yTile = j.toDouble / imageHeight + y
    } yield j * imageWidth + i -> Interaction.toLocation(xTile, yTile, zoom)

    val pixels = locations.map { case (i, loc) =>

      val latRange = List(floor(loc.lat).toInt, ceil(loc.lat).toInt)
      val lonRange = List(floor(loc.lon).toInt, ceil(loc.lon).toInt)

      val dx = loc.lon - lonRange.head
      val dy = latRange(1) - loc.lat

      val d = {
        for {
          xPos <- 0 to 1
          yPos <- 0 to 1
        } yield (xPos, yPos) -> grid(latRange(1 - yPos), lonRange(xPos))
      }.toMap


      val temperature = bilinearInterpolation(dx, dy, d((0, 0)), d((0, 1)), d((1, 0)), d((1, 1)))
      val color = Visualization.interpolateColor(colors, temperature)
      (i,color.toPixel(alpha))
    }

    Image(imageWidth, imageHeight, pixels.toArray.sortBy(_._1).map(_._2))
  }

}
