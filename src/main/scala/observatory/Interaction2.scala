package observatory

import observatory.Color.white
import observatory.Color.red
import observatory.Color.black
import observatory.Color.yellow
import observatory.Color.turquoise
import observatory.Color.blue
import observatory.Color.pink
import observatory.Color.purple
import observatory.LayerName.{Deviations, Temperatures}


/**
  * 6th (and last) milestone: user interface polishing
  */
object Interaction2 {

  /**
    * @return The available layers of the application
    */
  def availableLayers: Seq[Layer] = {
    List(
      Layer(
        Temperatures,
        Seq(
          (60.0, white),
          (32.0, red),
          (12.0, yellow),
          (0.0, turquoise),
          (-15.0, blue),
          (-27.0, pink),
          (-50.0, purple),
          (-60.0, black)
        ),
        1975 to 2015
      ),
      Layer(
        Deviations,
        Seq(
          (7.0, black),
          (4.0, red),
          (2.0, yellow),
          (0.0, white),
          (-2.0, turquoise),
          (-7.0, blue)
        ),
        1975 to 2015
      )
    )
  }

  /**
    * @param selectedLayer A signal carrying the layer selected by the user
    * @return A signal containing the year bounds corresponding to the selected layer
    */
  def yearBounds(selectedLayer: Signal[Layer]): Signal[Range] = {
    Signal(selectedLayer().bounds)
  }

  /**
    * @param selectedLayer The selected layer
    * @param sliderValue The value of the year slider
    * @return The value of the selected year, so that it never goes out of the layer bounds.
    *         If the value of `sliderValue` is out of the `selectedLayer` bounds,
    *         this method should return the closest value that is included
    *         in the `selectedLayer` bounds.
    */
  def yearSelection(selectedLayer: Signal[Layer], sliderValue: Signal[Int]): Signal[Int] =
  Signal.apply(
    sliderValue().max(yearBounds(selectedLayer)().min).min(yearBounds(selectedLayer)().max)
  )


  /**
    * @param selectedLayer The selected layer
    * @param selectedYear The selected year
    * @return The URL pattern to retrieve tiles
    */
  def layerUrlPattern(selectedLayer: Signal[Layer], selectedYear: Signal[Int]): Signal[String] = {
    Signal(s"generated/${selectedLayer().layerName.id}/${selectedYear()}/{z}/{x}/{y}.png")
  }

  /**
    * @param selectedLayer The selected layer
    * @param selectedYear The selected year
    * @return The caption to show
    */
  def caption(selectedLayer: Signal[Layer], selectedYear: Signal[Int]): Signal[String] = {
    Signal(s"${selectedLayer().layerName.id.capitalize} (${selectedYear()})")
  }

}

sealed abstract class LayerName(val id: String)
object LayerName {
  case object Temperatures extends LayerName("temperatures")
  case object Deviations extends LayerName("deviations")
}

/**
  * @param layerName Name of the layer
  * @param colorScale Color scale used by the layer
  * @param bounds Minimum and maximum year supported by the layer
  */
case class Layer(layerName: LayerName, colorScale: Seq[(Double, Color)], bounds: Range)

