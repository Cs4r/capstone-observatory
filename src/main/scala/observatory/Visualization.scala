package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    var weightsSum = 0.0
    var weghtByTempSum = 0.0

    for (temp <- temperatures) {
      if (temp._1 == location) {
        return temp._2
      }

      val di: Double = greatCircleDistance(location, temp._1)

      if (di < 1.0) {
        return temp._2
      }

      val wi: Double = weight(di)
      weightsSum += wi
      weghtByTempSum += (wi * temp._2)
    }

    val result = weghtByTempSum / weightsSum
    result

  }


  private def weight(distance: Double): Double = {
    1.0 / (Math.pow(distance, 6))
  }

  private def greatCircleDistance(a: Location, b: Location): Double = {

    val pk = 180 / 3.14169

    val a1 = a.lat / pk
    val a2 = a.lon / pk
    val b1 = b.lat / pk
    val b2 = b.lon / pk

    val t1 = Math.cos(a1) * Math.cos(a2) * Math.cos(b1) * Math.cos(b2)
    val t2 = Math.cos(a1) * Math.sin(a2) * Math.cos(b1) * Math.sin(b2)
    val t3 = Math.sin(a1) * Math.sin(b1)
    val tt = Math.acos(t1 + t2 + t3)

    return 6371 * tt
  }


  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    val asSeq = points.toIndexedSeq.sortBy(_._1)
    if (asSeq.isEmpty) {
      throw new IllegalArgumentException
    }

    var left = 0
    for (i <- 1 until asSeq.size) {
      if (asSeq(i)._1 < value) {
        left = i
      }
    }

    var right = asSeq.size - 1

    for (i <- asSeq.size - 2 to 0 by -1) {
      if (asSeq(i)._1 > value) {
        right = i
      }
    }

    if (left + 1 == right - 1 && asSeq(left + 1)._1 == value) {
      return asSeq(left + 1)._2
    } else if (left == 0 && value <= asSeq(left)._1) {
      asSeq(left)._2
    } else if (right == asSeq.size - 1 && value >= asSeq(right)._1) {
      asSeq(right)._2
    } else {
      interpolateColors(asSeq(left), asSeq(right), value)
    }


  }

  private def interpolateColors(a: (Double, Color), b: (Double, Color), x: Double): Color = {
    val red = a._2.red + (x - a._1) * (b._2.red - a._2.red) / (b._1 - a._1)

    val green = a._2.green + (x - a._1) * (b._2.green - a._2.green) / (b._1 - a._1)

    val blue = a._2.blue + (x - a._1) * (b._2.blue - a._2.blue) / (b._1 - a._1)

    Color(round(red), round(green), round(blue))
  }

  private def round(d: Double) = math.round(d).toInt


  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    val height = 180
    val width = 360
    val rows = height
    val cols = width

    val pixels: Array[Pixel] = Array.ofDim(rows * cols)

    var lat = 90
    var lon = -180

    for (x <- 0 until rows) {
      lon = -180
      for (y <- 0 until cols) {

        val temperature = predictTemperature(temperatures, Location(lat, lon))

        val color: Color = interpolateColor(colors, temperature)

        pixels(x * cols + y) = Pixel(color.red, color.green, color.blue, 0)

        lon += 1
      }
      lat -= 1
    }

    Image(width, height, pixels)
  }

}

