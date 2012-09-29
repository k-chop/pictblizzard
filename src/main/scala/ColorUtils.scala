package com.github.chuwb.pictbliz

import java.awt

object ColorUtils {

  def toColor(str: String): UColor = {
    sys.error("not implemented")
  }
}

object UColor {
  final val regex = """\#([0-9a-fA-F]{2,2}){3,4}""".r
  def code(s: String) = {
    if (!s.startsWith("#") && regex.findPrefixMatchOf(s).isEmpty)
      throw new IllegalArgumentException("invalid color code -> " + s)
    val (r: Int,g: Int,b: Int,a: Int) = if (s.length == 7) { // #ffffff(rgb)
      val xs = s.tail.sliding(2,2).map(byte => Integer.parseInt(byte, 16)).toArray
      (xs(0), xs(1), xs(2), 0)
    } else if (s.length == 9){ // #ffffffff(rgba)
      val xs = s.tail.sliding(2,2).map(byte => Integer.parseInt(byte, 16)).toArray
      (xs(0), xs(1), xs(2), xs(3))
    }
    new UColor(new awt.Color(r,g,b,a))
  }
  def hsv(h: Int, s: Int, v: Int, a: Int = 0) = {
    if (s == 0) new UColor(new awt.Color(v, v, v, a))
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
      new UColor(new awt.Color(r,g,b,a))
    }
  }
  def rgb(r: Int, g: Int, b: Int, a: Int = 0) = {
    new UColor(new awt.Color(r,g,b,a))
  }
}

/*
 * 誤差がヤバイ
 */
class UColor(val self: java.awt.Color) {
  case class HSV(h: Int, s:Int, v:Int)
  lazy val hsv = {
    val max = math.max(r, math.max(g, b))
    val min = math.min(r, math.min(g, b))
    val s = if (max == 0) 0 else (255.0 * ((max - min) / max.toDouble))
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
    HSV(h.toInt, s.toInt, max)
  }
  val r = self.getRed
  val g = self.getGreen
  val b = self.getBlue
  val a = self.getAlpha
  lazy val h = hsv.h
  lazy val s = hsv.s
  lazy val v = hsv.v
  def equals(c: UColor): Boolean = {
    self == c.self
  }
}
