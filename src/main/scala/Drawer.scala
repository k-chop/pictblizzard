package com.github.whelmaze.pictbliz

import java.awt.{ Font, Color }
import java.awt.image.BufferedImage

import scriptops._
import scriptops.Attrs._

class Drawer(layout: LayoutUnit) {

  private[this] val ASize(sizeX, sizeY) = layout.env('size)
  private[this] val areamap: AreaMap = layout.areamap

  def draw(valuemap: ValueMap, context: Context): DrawableImage = {
    val img = new DrawableImage(ImageUtils.newImage(sizeX, sizeY))
    img.clear(Color.black)
    areamap foreach {
      case (name, unit) =>
        img.drawArea(name, unit, valuemap getOrElse (name, NullValue))
    }
    img
  }

}

class DrawableImage(img: BufferedImage) {
  
  def clear(c: Color) = {
    val g2d = img.createGraphics
    g2d.setPaint(c)
    g2d.fillRect(0, 0, img.getWidth, img.getHeight)
    g2d.dispose()
    this
  }

  def drawArea(areaname: Key, areaunit: AreaUnit, target: AValue) {
    logger.trace("drawing :" + areaname + " ...")
    val frontImage = ImageBuilder.build(target, areaunit.attrmap)

    val (fx, fy, exSize) = frontImage map {
      case ResultImage((x, y), tgt) => (x, y, ASize(tgt.getWidth, tgt.getHeight))
    } getOrElse((0, 0, ASize(1, 1)))

    val bgImage = AttrMap.findParam(areaunit.attrmap, 'tile, 'background, 'window) flatMap {
      case a: Drawable =>
        ImageBuilder.build(a, areaunit.attrmap += ('size -> exSize))
      case _ => None
    }

    val g = img.createGraphics
    bgImage foreach {
      case ResultImage(_, tgt) => g.drawImage(tgt, null, fx, fy)
    }
    frontImage foreach {
      case ResultImage((x, y), tgt) => g.drawImage(tgt, null, x, y)
    }
    g.dispose()
  }

  def result = img
  def write(ref: String) {
    ext.PNG.write(img, ref)
  }
  
}

