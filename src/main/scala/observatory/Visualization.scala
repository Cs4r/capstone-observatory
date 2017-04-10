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
    ???
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

  def round(d: Double) = math.round(d).toInt


  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    ???
  }

}

