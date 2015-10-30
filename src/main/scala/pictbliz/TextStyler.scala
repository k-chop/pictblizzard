package pictbliz

import java.awt.Color
import java.awt.image.BufferedImage
import com.typesafe.scalalogging.LazyLogging

import util.Rect2DConversion
import enrich.all._

import scala.annotation.tailrec

class TextStyler(val origimg: BufferedImage,
                  val glyphvec: WrappedGlyphVector,
                  val params: Params,
                  val attrstr: AttributedText) extends LazyLogging
{
  var colors: Texturable = params.frontColor
  //var test = scala.collection.mutable.ArrayBuffer.empty[(Int, Int, Int, Int)]

  def process(): BufferedImage = {
    // TODO: 陰もAttrMap見てありなし決める
    val dest = ImageUtils.sameSizeRawImage(origimg)
    val shadow = shadowed(origimg)
    val body: RawIndexColorImage =
      params.hemming.fold(colored(origimg.toRaw)) { hem =>
        colored( hemmed(origimg, hem.color, hem.size) )
      }

    dest.drawImage(shadow, 1, 1) // shadow offset = 1
    dest.drawImage(body, 0, 0)

    // TODO: ふちどりした場合、サイズと描画位置が変更されるのでParamsの更新する
    //if (params.border) bordered(Color.white)

    dest.toBufferedImage()
  }
  
  // つける
  def shadowed(src: BufferedImage): RawIndexColorImage = {

    val maskImg = src.toRaw
    val targetImg = ImageUtils.newRawImage(maskImg.width, maskImg.height)
    val (px, py, pw, ph) = glyphvec.getFixedWholeLogicalBounds.xywh
    val paintTex = colors.getShadowTexture(pw, ph)
    targetImg.drawImage(paintTex, px, py + glyphvec.ascent.toInt)

    maskImg.synthesis(targetImg)
    maskImg
  }

  // 色つける
  def colored(src: RawIndexColorImage): RawIndexColorImage = {

    val that = ImageUtils.newRawImage(src.width, src.height)

    for (AttributeRange(begin, end, ctr) <- attrstr.iter) {
      val texIdx = ctr match {
        case CtrColor(idx) => idx
        case CtrNop => 0
      }

      @scala.annotation.tailrec
      def drawEachLine(b: Int, l: List[String]) {
        l match {
          case Nil =>
          case head :: rest =>
            val (px, py, pw, ph) = glyphvec.getFixedLogicalBounds(b, b + head.length).xywh
            //if (debug) test += ((px, py, pw, ph))
            val paintTex = colors.getTexture(pw, ph, texIdx)
            that.drawImage(paintTex, px, py + glyphvec.ascent.toInt)

            drawEachLine(b + head.length + 1, rest)
        }
      }
      val subs = attrstr.str.substring(begin, end)
      drawEachLine(begin, subs.split("\n").toList)
    }    

    src.synthesis(that)
    src
  }

  def hemmed(src: BufferedImage, hemColor: Int, hemSize: Int): RawIndexColorImage = {
    import ImageUtils.{neighbor, alpha}

    val dest2 = ImageUtils.extraSizeRawImage(src, hemSize)
    val da2 = ImageUtils.newRawImage(dest2.width, dest2.height)

    dest2.drawImage(src.toRaw, hemSize, hemSize)

    val w = dest2.width

    @tailrec def traverse(n: Int, lim: Int) {
      import enrich.packedcolor._

      if (n < lim) {
        val a = neighbor(dest2.pixels, n, w, default = 0)
        val alp = dest2.color(n).a
        var maxalpha = 0
        var i = 0; val len = a.length
        while(i < len) {
          val alphav = dest2.palette(a(i)).a
          if (alphav > maxalpha) maxalpha = alphav
          i += 1
        }

        if (alp == 0 && maxalpha != 0) da2.setColor(n, alpha(hemColor, maxalpha))
        if (0 < alp && alp < 255) {
          val s = alp / 255.0
          val newcolor = 0xFFFFFFFF//add(mul(dest(n), s), mul(hemcolor, 1-s))
          da2.setColor(n, alpha(newcolor, 0xFF))
        }
        if (alp == 255) da2.pixels(n) = dest2.pixels(n)
        traverse(n+1, lim)
      }
    }
    traverse(0, dest2.length)
    da2
  }
  
  def bordered(c: Color): BufferedImage = {
    val g2d = origimg.createGraphics
    g2d.setPaint(c)
    g2d.drawRect(0, 0, origimg.getWidth-1, origimg.getHeight-1)
    origimg
  }
}
