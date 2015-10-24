package pictbliz

import java.awt.image.BufferedImage


object RawIndexColorImage {

  def fromBufferedImage(buf: BufferedImage): RawIndexColorImage = ???
}

case class RawIndexColorImage(pixelIdx: Array[Int], palette: Array[Int])
