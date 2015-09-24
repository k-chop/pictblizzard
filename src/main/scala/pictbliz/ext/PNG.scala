package pictbliz.ext

import java.io.FileInputStream
import java.nio.channels.FileChannel
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.File
import java.nio.file.Path

import com.typesafe.scalalogging.LazyLogging
import pictbliz.{BinaryUtils, ImageUtils}

import scala.language.implicitConversions

object PNG extends LazyLogging {
  import FilePath._

  def read[T: ToPath](path: T, argb: Boolean = false, transparent: Boolean = false): BufferedImage =
    buildBufferedImage(implicitly[ToPath[T]].toPath(path), argb, transparent)

  private[this] def buildBufferedImage(path: Path, argb: Boolean, transparent: Boolean): BufferedImage = {
    var image = ImageIO.read(path.toFile)
    if (argb) image = ImageUtils.toARGBImage(image)
    if (transparent) {
      val transp = PNG.transparentColor(path)
      image = ImageUtils.enableAlpha(image, transp)
    }
    image
  }

  /**
   * 指定したパスに書き出す。指定したディレクトリがない場合は生成する。
   * @param img ファイルに書きだすデータ
   * @param path 書き出し先のパス
   * @param name ファイル名
   */
  def write(img: BufferedImage, path: String, name: String) {
    val f = new File(s"$path/$name.png")
    logger.trace(s"write to ${f.getPath} ...")
    Option(f.getParentFile) foreach { _.mkdirs() }
    ImageIO.write(img, "png", f)
  }
  
  import BinaryUtils._

  private[this] val DWORD = new Array[Byte](4)
  private[this] val QWORD = new Array[Byte](8)

  def transparentColor(path: Path): Int = {
    val cnl = new FileInputStream(path.toFile).getChannel
    try {
      val map = cnl.map(FileChannel.MapMode.READ_ONLY, 0, cnl.size())
      map.order(java.nio.ByteOrder.LITTLE_ENDIAN)
      map.get(QWORD)
      if (!(QWORD sameElements PNG_IDENTIFER)) {
        throw new IllegalArgumentException(s"Invalid header: ${QWORD.deep}")
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

}
