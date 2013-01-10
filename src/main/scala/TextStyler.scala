package com.github.whelmaze.pictbliz

import java.awt.Color
import java.awt.image.{DataBufferInt, BufferedImage}

import scriptops._
import scriptops.Attrs._
import com.github.whelmaze.pictbliz.ImageUtils.ARGB

class TextStyler(val origimg: BufferedImage,
                  val glyphvec: WrappedGlyphVector,
                  val attrmap: AttrMap,
                  val attrstr: AttributedText)
{
  var colors: Texturable = {
    AttrMap.findParam(attrmap, 'front_color) match {
      case Some(ASystemGraphics(path: String)) =>
        SystemGraphics.fromPath(path)
      case Some(ASingleColors(xs: Array[String])) =>
        new SingleColors(xs)
      case _ =>
        SystemGraphics.default
    }
  }
  var test = scala.collection.mutable.ArrayBuffer.empty[(Int, Int, Int, Int)]
  val debug = attrmap.contains('debug)
  
  def process(): BufferedImage = {
    // TODO: 陰もAttrMap見てありなし決める
    val dest = ImageUtils.extraSizeImage(origimg, 1)
    val s = shadowed()
    val c = colored()
    val h = hemmed(c)

    val g = dest.createGraphics
    g.drawImage(s, null, 1, 1) // shadow offset = 1
    g.drawImage(h, null, -1, -1)
    g.dispose()

    // TODO: ふちどりした場合、サイズと描画位置が変更されるのでAttrMapの更新しろ

    if (attrmap.contains('border)) bordered(Color.white)

    if (debug) {
      val b = dest.createGraphics()
      b.setPaint(Color.black)
      test foreach {
        case (x, y, w, h) =>
          b.drawRect(x, y + glyphvec.ascent.toInt, w, h)
      }
      b.dispose()
    }
    
    dest
  }
  
  // 影つける
  def shadowed(): BufferedImage = colors match {
    case s: SystemGraphics => {
      val maskimg = ImageUtils.copy(origimg)
      val targetimg = ImageUtils.newImage(maskimg.getWidth, maskimg.getHeight)
      val g = targetimg.createGraphics
      val Extractors.Rect2DALL(px, py, pw, ph) = glyphvec.getFixedWholeLogicalBounds
      val paintTex = colors.getTexture(pw, ph)(20)
      g.drawImage(paintTex, null, px, py + glyphvec.ascent.toInt)
      g.dispose()
      
      ImageUtils.synthesis(maskimg, targetimg)
    }
    case _ => sys.error("not implemented")
  }
  // 色つける
  def colored(): BufferedImage = {
    val maskimg = ImageUtils.copy(origimg)
    val targetimg = ImageUtils.newImage(maskimg.getWidth, maskimg.getHeight)
    val g = targetimg.createGraphics
  
    for (AttributeRange(begin, end, ctr) <- attrstr.iter) {
      val texIdx = ctr match {
        case CtrColor(idx) => idx
        case CtrNop => 0
      }

      @scala.annotation.tailrec
      def drawEachLine(b: Int, l: List[String]): Unit = l match {
        case Nil => return
        case head :: rest =>
          val Extractors.Rect2DALL(px, py, pw, ph) = glyphvec.getFixedLogicalBounds(b, b + head.length)
          if (debug) test += ((px, py, pw, ph))
          val paintTex = colors.getTexture(pw, ph)(texIdx)
          g.drawImage(paintTex, null, px, py + glyphvec.ascent.toInt)

          drawEachLine(b + head.length + 1, rest)
      }
      val subs = attrstr.str.substring(begin, end)
      drawEachLine(begin, subs.split("\n").toList)
    }    
    g.dispose()
    
    ImageUtils.synthesis(maskimg, targetimg)
    maskimg
  }

  def hemmed(src: BufferedImage): BufferedImage = {
    import ImageUtils.{neighbor, alpha}
    import scala.annotation.tailrec
    val ex = 1  // 余分にとるサイズは縁の幅によって変わるけど仮で1固定
    val destimg = ImageUtils.extraSizeImage(src, ex)
    val pix: Array[Int] = (src.getRaster.getDataBuffer).asInstanceOf[DataBufferInt].getData
    val dest: Array[Int] = (destimg.getRaster.getDataBuffer).asInstanceOf[DataBufferInt].getData

    // TODO: 縁取り色はAttrMapから読み込む。今は仮
    val hemcolor = 0xFFFF0000 // 赤
/*
    pix.indices foreach { i: Int =>
      val a = neighbor(pix, i)
      val ARGB(alp,_,_,_) = pix(i)
      val alphas = a.map(_ & 0xFF000000 >>> 24)
      if (alp == 0 && alphas.sum != 0) dest(i+1+destimg.getWidth) = alpha(hemcolor, alphas.max)
    }
  */

    import ImageUtils.{ add, mul }

    val w = src.getWidth

    @tailrec def traverse(n: Int, lim: Int) {
      if (n < lim) {
        val a = neighbor(pix, n, w)
        val alp = (pix(n) & 0xFF000000 >>> 24)
        var maxalpha = 0
        var i = 0; val len = a.length
        while(i < len) {
          val alphav = (a(i) & 0xFF000000 >>> 24)
          if (alphav > maxalpha) maxalpha = alphav
          i += 1
        }
        val ct_n = n+destimg.getWidth+1+(n/src.getWidth)*2
        if (alp == 0 && maxalpha != 0) dest(ct_n) = alpha(hemcolor, maxalpha)
        if (0 < alp && alp < 255) {
          val s = alp / 255.0
          val newcolor = add(mul(pix(n), s), mul(hemcolor, 1-s))
          dest(ct_n) = alpha(newcolor, 0xFF)
        }
        if (alp == 255) dest(ct_n) = pix(n)
        traverse(n+1, lim)
      } else return
    }
    traverse(0, pix.length)
    destimg
  }
  
  def bordered(c: Color): BufferedImage = {
    val g2d = origimg.createGraphics
    g2d.setPaint(c)
    g2d.drawRect(0, 0, origimg.getWidth-1, origimg.getHeight-1)
    origimg
  }

  
}
