package pictbliz.enrich

import java.awt.Color

private[enrich] trait ToPackedColorInt {

  def argb(a: Int, r: Int, g: Int, b: Int) = ((0xff & a) << 24) | ((0xff & r) << 16) | ((0xff & g) << 8) | (0xff & b)

  def rgb(r: Int, g: Int, b: Int) = 0xff000000 | ((0xff & r) << 16) | ((0xff & g) << 8) | (0xff & b)

  def hsv(h: Int, s: Int, v: Int) = {
    if (s == 0)
      rgb(v, v, v)
    else {
      val i = math.floor(h/60.0) % 6
      val f = h/60.0 - math.floor(h/60.0)
      val p = math.round(v * (1.0 - s/255.0)).toInt
      val q = math.round(v * (1.0 - s/255.0 * f)).toInt
      val t = math.round(v * (1.0 - s/255.0 * (1.0 - f))).toInt
      val (r, g, b) = i match {
        case 0 => (v, t, p)
        case 1 => (q, v, p)
        case 2 => (p, v, t)
        case 3 => (p, q, v)
        case 4 => (t, p, v)
        case 5 => (v, p, q)
      }
      rgb(r, g, b)
    }
  }

  implicit def toPackedColorInt(self: Int): PackedColorInt = new PackedColorInt(self)
}

final class PackedColorInt(val self: Int) {

  @inline def a: Int = self >> 24 & 0xff
  @inline def r: Int = self >> 16 & 0xff
  @inline def g: Int = self >> 8 & 0xff
  @inline def b: Int = self & 0xff

  @inline def argb: (Int, Int, Int, Int) = (a, r, g, b)
  @inline def rgb: (Int, Int, Int) = (r, g, b)

  @inline def hsv: (Int, Int, Int) = {
    val max = math.max(r, math.max(g, b))
    val min = math.min(r, math.min(g, b))
    val s = if (max == 0) 0 else 255.0 * ((max - min) / max.toDouble)
    val div = (max - min).toDouble
    val h =
      if (max == min)
        0
      else if (max == r)
        (60 * (g - b) / div + 360) % 360
      else if (max == g)
        (60 * (b - r) / div) + 120
      else
        (60 * (r - g) / div) + 240

    (h.toInt, s.toInt, max)
  }

  @inline def asColor: Color = new Color(self, true)
}
