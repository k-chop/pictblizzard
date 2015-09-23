package pictbliz

import java.awt.Color
import java.awt.image.{DataBufferInt, BufferedImage}
import scala.annotation.tailrec

import scriptops._
import scriptops.Attrs._

class TextStyler(val origimg: BufferedImage,
                  val glyphvec: WrappedGlyphVector,
                  val params: Params,
                  val attrstr: AttributedText)
{
  var colors: Texturable = params.frontColor
  //var test = scala.collection.mutable.ArrayBuffer.empty[(Int, Int, Int, Int)]

  def process(): BufferedImage = {
    // TODO: 陰もAttrMap見てありなし決める
    val dest = ImageUtils.extraSizeImage(origimg, 0)
    val s = shadowed()
    val body: BufferedImage =
      params.hemming.fold(colored(origimg)) { hem =>
        colored( hemmed(origimg, hem.color, hem.size) )
      }

    val g = dest.createGraphics
    g.drawImage(s, null, 1, 1) // shadow offset = 1
    g.drawImage(body, null, 0, 0)
    g.dispose()

    // TODO: ふちどりした場合、サイズと描画位置が変更されるのでParamsの更新する

    if (params.border) bordered(Color.white)

    dest
  }
  
  // つける
  def shadowed(): BufferedImage = colors match {
    case s: SystemGraphics =>
      val maskimg = ImageUtils.copy(origimg)
      val targetimg = ImageUtils.newImage(maskimg.getWidth, maskimg.getHeight)
      val g = targetimg.createGraphics
      val Extractors.Rect2DALL(px, py, pw, ph) = glyphvec.getFixedWholeLogicalBounds
      val paintTex = colors.getTexture(pw, ph)(20)
      g.drawImage(paintTex, null, px, py + glyphvec.ascent.toInt)
      g.dispose()
      
      ImageUtils.synthesis(maskimg, targetimg)

    case _ => sys.error("not implemented")
  }
  // 色つける
  def colored(origimg: BufferedImage): BufferedImage = {
    val maskimg = ImageUtils.copy(origimg)
    val targetimg = ImageUtils.newImage(maskimg.getWidth, maskimg.getHeight)
    val g = targetimg.createGraphics
  
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
            val Extractors.Rect2DALL(px, py, pw, ph) = glyphvec.getFixedLogicalBounds(b, b + head.length)
            //if (debug) test += ((px, py, pw, ph))
            val paintTex = colors.getTexture(pw, ph)(texIdx)
            g.drawImage(paintTex, null, px, py + glyphvec.ascent.toInt)

            drawEachLine(b + head.length + 1, rest)
        }
      }
      val subs = attrstr.str.substring(begin, end)
      drawEachLine(begin, subs.split("\n").toList)
    }    
    g.dispose()
    
    ImageUtils.synthesis(maskimg, targetimg)
  }

  def hemmed(src: BufferedImage, hemcolor: UColor, hemSize: Int): BufferedImage = {
    import ImageUtils.{neighbor, alpha}


    val destimg = ImageUtils.extraSizeImage(src, hemSize)
    val d = ImageUtils.sameSizeImage(destimg)
    locally {
      val g = destimg.createGraphics()
      g.drawImage(src, null, hemSize, hemSize)
    }
    val da: Array[Int] = d.getRaster.getDataBuffer.asInstanceOf[DataBufferInt].getData
    val dest: Array[Int] = destimg.getRaster.getDataBuffer.asInstanceOf[DataBufferInt].getData

    val w = destimg.getWidth

    @tailrec def traverse(n: Int, lim: Int) {
      if (n < lim) {
        val a = neighbor(dest, n, w, default = 0)
        val alp = dest(n) & 0xFF000000 >>> 24
        var maxalpha = 0
        var i = 0; val len = a.length
        while(i < len) {
          val alphav = a(i) & 0xFF000000 >>> 24
          if (alphav > maxalpha) maxalpha = alphav
          i += 1
        }
        //val ct_n = n+destimg.getWidth+1+(n/src.getWidth)*2
        if (alp == 0 && maxalpha != 0) da(n) = alpha(hemcolor.rgb, maxalpha)
        if (0 < alp && alp < 255) {
          val s = alp / 255.0
          val newcolor = 0xFFFFFFFF//add(mul(dest(n), s), mul(hemcolor, 1-s))
          da(n) = alpha(newcolor, 0xFF)
        }
        if (alp == 255) da(n) = dest(n)
        traverse(n+1, lim)
      }
    }
    traverse(0, dest.length)
    d
  }
  
  def bordered(c: Color): BufferedImage = {
    val g2d = origimg.createGraphics
    g2d.setPaint(c)
    g2d.drawRect(0, 0, origimg.getWidth-1, origimg.getHeight-1)
    origimg
  }


}
