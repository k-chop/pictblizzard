package com.github.chuwb.pictbliz

import java.awt.image.{ BufferedImage }
import javax.imageio.{ ImageIO }
import java.io.{ File }

object PNGIO {

  def read(path: java.net.URI): BufferedImage = ImageIO.read(new File(path))

  def read(path: String): BufferedImage = read( (new File(path)).toURI )
  
  def write(img: BufferedImage, path: String) {
    ImageIO.write(img, "png", new File(path))
  }
  
}
