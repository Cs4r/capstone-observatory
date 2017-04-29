package observatory

import com.sksamuel.scrimage.Pixel

case class Location(lat: Double, lon: Double)

case class Color(red: Int, green: Int, blue: Int){

  def toPixel(alpha: Int): Pixel = {
    Pixel(red, green, blue, alpha)
  }
}

case class Station(stn: String, wban: String)


object Color {

  val white: Color = Color(255, 255, 255)

  val red: Color = Color(255, 0, 0)

  val black: Color = Color(0, 0, 0)

  val yellow: Color = Color(255, 255, 0)

  val turquoise: Color = Color(0, 255, 255)

  val blue: Color = Color(0, 0, 255)

  val pink: Color = Color(255, 0, 255)

  val purple: Color = Color(33, 0, 107)
}