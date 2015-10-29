package pictbliz

import java.awt.image._

import enrich.bufferedimage._

object ImageUtils {

  def alpha(color: Int, value: Int): Int = (color & 0x00FFFFFF) | (value << 24)

  /**
   * 与えられたArrayの8近傍を取り、新しいArrayに入れて返す。
   * 配列の範囲外は引数として与えたdefaultの値を取る。
   *
   * @param arr 8近傍を調べるArray(1次元)
   * @param i 調べる位置のインデックス
   * @param w Arrayの横幅
   * @param default 配列の範囲外を表す値
   * @return 8近傍の値が入ったArray
   */
  def neighbor(arr: Array[Int], i: Int, w: Int, default: Int): Array[Int] = {
    // nante hidoi code nanda...
    val a = Array(0, 0, 0, 0, 0, 0, 0, 0)
    val u = i < w
    val b = (arr.length - i) <= w
    val r = (i+1)%w == 0
    val l = i%w == 0
    a(0) = if (u || l) -1 else i-1-w
    a(1) = if (u) -1 else i-w
    a(2) = if (u || r) -1 else i+1-w
    a(3) = if (l) -1 else i-1
    a(4) = if (r) -1 else i+1
    a(5) = if (b || l) -1 else i-1+w
    a(6) = if (b) -1 else i+w
    a(7) = if (b || r) -1 else i+1+w

    var j = 0; val len = arr.length
    while(j < 8) {
      a(j) = if (0 <= a(j) && a(j) < len) arr(a(j)) else default
      j += 1
    }
    a
    /*a.map { i=>
      if (0 <= i && i < arr.length) arr(i) else 0
    }*/
  }

  def newImage(w: Int, h: Int, typ: Int = BufferedImage.TYPE_BYTE_INDEXED): BufferedImage = new BufferedImage(w, h, typ)
  def newImage(size: (Int, Int)): BufferedImage = newImage(size._1, size._2)

  def sameSizeImage(src: BufferedImage): BufferedImage = 
    new BufferedImage(src.getWidth, src.getHeight, src.getType)

  def extraSizeImage(src: BufferedImage, plus: Int): BufferedImage =
    new BufferedImage(src.getWidth+plus*2, src.getHeight+plus*2, src.getType)
  
  def copy(src: BufferedImage): BufferedImage = {
    val dest = sameSizeImage(src)
    dest.setData(src.getData)
    dest
  }

  def enableAlphaIndexColor(src: BufferedImage, paletteIdx: Int = 0): BufferedImage = {
    import enrich.packedcolor._

    val raw = src.toRaw

    if (raw.palette(paletteIdx).a == 0xff) {
      raw.palette(paletteIdx) = raw.palette(paletteIdx) & 0x00ffffff
    }
    raw.toBufferedImage()
  }

  def synthesisIndexColor(src: BufferedImage, target: BufferedImage, maskcolor: Int = 0xFFFFFFFF): BufferedImage = {
    require(src.getType == BufferedImage.TYPE_BYTE_INDEXED && target.getType == BufferedImage.TYPE_BYTE_INDEXED,
      "source & target's image type should be 'TYPE_BYTE_INDEXED'")

    val rawTarget = target.toRaw
    val dest = src.toRaw

    // synthesis
    dest.foreachWithIndex { i =>
      if (dest.color(i) == maskcolor) {
        val targetColor = rawTarget.color(i)
        dest.setColor(i, targetColor)
      }
    }

    dest.toBufferedImage()
  }

}
