package com.github.whelmaze.pictbliz

import java.awt.{ Font, Color }
import java.awt.image.BufferedImage

import scriptops._
import scriptops.Attrs._

object Drawer {
  val DEFAULT_FONT = new Font("ＭＳ ゴシック", Font.PLAIN, 12)
}

class Drawer(layout: LayoutUnit) {

  private[this] val APoint(sizeX, sizeY) = layout.env('size)
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

  // 描画位置と描画するImageを持つ
  case class ResultImage(pos: (Int, Int), img: BufferedImage)
  
  def clear(c: Color) = {
    val g2d = img.createGraphics
    g2d.setPaint(c)
    g2d.fillRect(0, 0, img.getWidth, img.getHeight)
    g2d.dispose()
    this
  }

  def drawArea(areaname: Key, areaunit: AreaUnit, target: AValue) {
    logger.info("drawing :" + areaname + " ...")
    val frontImage = Option(target) map {
      case Icon(url) =>
        iconImage(url, areaunit.attrmap)
      case FaceGraphic(uri, no) =>
        faceImage(uri, no, areaunit.attrmap)
      case Str(s)  =>
        stringImage(s, areaunit.attrmap)
      case NullValue =>
        null // Option(null) => None
    }

    val (fx, fy, exSize) = frontImage map {
      case ResultImage((x, y), tgt) => (x, y, (tgt.getWidth, tgt.getHeight))
    } getOrElse((0, 0, (1, 1)))

    val bgkind = AttrMap.findParam(areaunit.attrmap, 'tile, 'background, 'window)
    val bgImage = bgkind map {
      case t: ATile       => tileImage(exSize, t)
      case t: ABackground => backgroundImage(exSize, t)
      case t: AWindow  => windowImage(exSize, t)
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

  def findBeginPoint(attrmap: AttrMap): (Int, Int) = {
    AttrMap.findParam(attrmap, 'point, 'rect) map {
      case APoint(x, y)      => (x, y)
      case ARect(x, y, _, _) => (x, y)
    } getOrElse (0, 0)
  }

  def backgroundImage(size: (Int, Int), attr: Attr): ResultImage = sys.error("not implemented")

  def tileImage(size: (Int, Int), attr: Attr): ResultImage = sys.error("not implemented")

  def windowImage(size: (Int, Int), attr: Attr): ResultImage = {
    val sysg = attr match {
      case AWindow(path) =>
        SystemGraphics.fromPath(path)
      case _ =>
        SystemGraphics.default
    }

    val buf = ImageUtils.newImage(size)

    val (sw, sh) = size
    val syswin = sysg.getSystemWindow(sw, sh, zoom=true)
    val g = buf.createGraphics
    g.drawImage(syswin, null, 0, 0)
    g.dispose()
    ResultImage((0, 0), buf)
  }

  def iconImage(path: java.net.URI, attrmap: AttrMap): ResultImage = {
    val icon: BufferedImage = ext.PNG.read(path)
    ResultImage(findBeginPoint(attrmap), icon)
  }

  def faceImage(path: java.net.URI, no: Int, attrmap: AttrMap): ResultImage = {
    sys.error("not implemented")
  }

  def stringImage(str: String, attrmap: AttrMap): ResultImage = {
    val strgraphics = getStrGraphics(str, attrmap)
    val result = strgraphics.processImage()
    strgraphics.dispose()
    ResultImage(findBeginPoint(attrmap), result)
  }

  def getStrGraphics(str: String, attrmap: AttrMap): StrGraphics = {
    val font = attrmap.get('font) map { attr =>
      val AFont(name, style, pt, _) = attr
      new Font(name, extractStyle(style), pt)
    } getOrElse {
      logger.warning("フォント設定が見つかりません。デフォルトフォントを使用します。")
      Drawer.DEFAULT_FONT
    }

    val g2d = img.createGraphics
    new StrGraphics(g2d, str, font, attrmap)
  }

  private def extractStyle(s: Symbol): Int = s match {
    case 'plain => Font.PLAIN
    case 'bold => Font.BOLD
    case 'italic => Font.ITALIC
    case 'bolditalic => Font.BOLD | Font.ITALIC
    case n => logger.warning("不明なフォントスタイルです: "+n+"\nデフォルトのスタイルを使用します."); Font.PLAIN
  }
  
  def result = img
  def write(ref: String) {
    ext.PNG.write(img, ref)
  }
  
}

