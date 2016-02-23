package io

object AnsiColor256 {
  private def rgb6Val(r:Int, g:Int, b:Int):Int = (r, g, b) match {
    case (r, g, b) if r > 5 => rgb6Val(5, g, b)
    case (r, g, b) if r < 0 => rgb6Val(0, g, b)
    case (r, g, b) if g > 5 => rgb6Val(r, 5, b)
    case (r, g, b) if g < 0 => rgb6Val(r, 0, b)
    case (r, g, b) if b > 5 => rgb6Val(r, g, 5)
    case (r, g, b) if b < 0 => rgb6Val(r, g, 0)
    case (r, g, b) => 16 + (36 * r) + (6 * g) + b
  }
  def rgb6(r:Int, g:Int, b:Int):String = s"\u001b[38;5;${rgb6Val(r,g,b)}m"
  def rgb6_b(r:Int, g:Int, b:Int):String = s"\u001b[48;5;${rgb6Val(r,g,b)}m"

  def gray24(g:Int):String = g match {
    case g if g < 0 => gray24(0)
    case g if g > 23 => gray24(23)
    case g => s"\u001b[38;5;${232 + g}m"
  }
  def gray24_b(g:Int):String = g match {
    case g if g < 0 => gray24_b(0)
    case g if g > 23 => gray24_b(23)
    case g => s"\u001b[48;5;${232 + g}m"
  }
  def gray(g: Double):String = gray24((g*24).toInt)
  def gray_b(g: Double):String = gray24_b((g*24).toInt)
  def gray(g: Int):String = gray(g / 256.0)
  def gray_b(g: Int):String = gray_b(g / 256.0)
  def rgb(r:Double, g:Double, b:Double):String = (r, g, b) match {
    case (r, g, b) if r == g && g == b => gray(g)
    case (r, g, b) => rgb6((r*6).toInt, (g*6).toInt, (b*6).toInt)
  }
  def rgb_b(r:Double, g:Double, b:Double):String = (r, g, b) match {
    case (r, g, b) if r == g && g == b => gray_b(g)
    case (r, g, b) => rgb6_b((r*6).toInt, (g*6).toInt, (b*6).toInt)
  }
  def rgb(r:Int, g:Int, b:Int):String = rgb(r / 256.0, g / 256.0, b / 256.0)
  def rgb_b(r:Int, g:Int, b:Int):String = rgb_b(r / 256.0, g / 256.0, b / 256.0)

  def rgb(v:(Int, Int, Int)):String = rgb(v._1, v._2, v._3)
  def rgb(v:Int):String = rgb(rgbDecode(v))
  def rgb_b(v:(Int, Int, Int)):String = rgb_b(v._1, v._2, v._3)
  def rgb_b(v:Int):String = rgb_b(rgbDecode(v))

  def rgbDecode(rgb:Int) =
    (
      (rgb>>16)&0xFF,
      (rgb>>8)&0xFF,
      rgb &0xFF
    )

  /** Reset ANSI styles */
  final val RESET      = "\u001b[0m"
  /** ANSI bold */
  final val BOLD       = "\u001b[1m"
  /** ANSI underlines */
  final val UNDERLINED = "\u001b[4m"
  /** ANSI blink */
  final val BLINK      = "\u001b[5m"
  /** ANSI reversed */
  final val REVERSED   = "\u001b[7m"
  /** ANSI invisible */
  final val INVISIBLE  = "\u001b[8m"
}