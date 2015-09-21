package pictbliz.ext

import java.io.FileInputStream
import java.nio.channels.FileChannel
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.File
import pictbliz.{BinaryUtils, ImageUtils}

import scala.language.implicitConversions

class BufferedImageBuilder(ref: File) {
  private[this] var _argb = false
  private[this] var _transparent = false

  def build: BufferedImage = {
    var image = ImageIO.read(ref)
    if (_argb) image = ImageUtils.toARGBImage(image)
    if (_transparent) {
      val transp = PNG.transparentColor(ref)
      image = ImageUtils.enableAlpha(image, transp)
    }
    image
  }

  def asARGB = {
    _argb = true
    this
  }

  def transparent(b: Boolean) = {
    if (b) _argb = true
    _transparent = b

    this
  }

  def refresh = {
    _argb = false
    _transparent = false
    this
  }
}

object PNG {
  private[this] var counter = 0

  object refconvert {
    implicit def uri2File(uri: java.net.URI) = new File(uri)
    implicit def str2File(str: String) = new File(str)
  }

  object autobuild {
    implicit def builder2image(b: BufferedImageBuilder) = b.build
  }

  //def read(file: File): BufferedImage = ImageIO.read(file)
  def read(file: File): BufferedImageBuilder = new BufferedImageBuilder(file)

  /**
   * 指定したパスに書き出す。指定したディレクトリがない場合は生成する。
   * @param img ファイルに書きだすデータ
   * @param path 書き出し先のパス
   * @param name ファイル名
   */
  def write(img: BufferedImage, path: String, name: String) {
    val f = new File(s"$path/$name.png")
    println(s"write to ${f.getPath} ...")
    Option(f.getParentFile) foreach { _.mkdirs() }
    ImageIO.write(img, "png", f)
  }
  
  import BinaryUtils._

  private[this] val DWORD = new Array[Byte](4)
  private[this] val QWORD = new Array[Byte](8)

  def transparentColor(path: File): Int = {
    val cnl = new FileInputStream(path).getChannel
    try {
      val map = cnl.map(FileChannel.MapMode.READ_ONLY, 0, cnl.size())
      map.order(java.nio.ByteOrder.LITTLE_ENDIAN)
      map.get(QWORD)
      if (!(QWORD sameElements PNG_IDENTIFER)) {
        throw new IllegalArgumentException("ヘッダが不正です.")
      }
      var result: Int = 0x0
      while (map.hasRemaining) {
        map.get(DWORD)
        val len = DWORD2Long(DWORD)
        map.get(DWORD)
        val name = DWORD map { _.toChar } mkString ""
        if (name == "PLTE") {
          val r = map.get
          val g = map.get
          val b = map.get
          result = unsign(b) | (unsign(g) << 8) | (unsign(r) << 16) | 0xff000000 & 0xffffffff
          map.position(map.position - 3 + len.toInt + 4)
        } else {
          map.position(map.position + len.toInt + 4)
        }
      }
      result
    } finally {
      cnl.close()
    }

  }

  def transparentColor(path: String): Int = transparentColor(new File(path))

}
