package pictbliz.enrich


private[enrich] trait ToPackedColorInt {

  implicit def toPackedColorInt(self: Int): PackedColorInt = new PackedColorInt(self)
}

final class PackedColorInt(val self: Int) {

  def a: Int = self >> 24 & 0xff
  def r: Int = self >> 16 & 0xff
  def g: Int = self >> 8 & 0xff
  def b: Int = self & 0xff

  def argb: (Int, Int, Int, Int) = (a, r, g, b)
  def rgb: (Int, Int, Int) = (r, g, b)

}
