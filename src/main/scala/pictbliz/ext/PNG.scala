package pictbliz.ext

import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.nio.file.{Files, Path}

import com.typesafe.scalalogging.LazyLogging

import scala.language.implicitConversions

object PNG extends LazyLogging {
  import FilePath._

  def read[T: ToPath](path: T, argb: Boolean = false, transparent: Boolean = false): BufferedImage =
    buildBufferedImage(implicitly[ToPath[T]].toPath(path), transparent)

  private[this] def buildBufferedImage(path: Path, transparent: Boolean): BufferedImage = {
    logger.debug(s"Build from $path, alpha: $transparent")

    ImageIO.read(path.toFile)
  }

  /**
   * 指定したパスに書き出す。指定したディレクトリがない場合は生成する。
   * @param img ファイルに書きだすデータ
   * @param path 書き出し先のパス
   * @param name ファイル名
   */
  def write[T: ToPath](img: BufferedImage, path: T, name: String) {
    val filePath = implicitly[ToPath[T]].toPath(path).resolve(s"$name.png")
    logger.trace(s"write to ${filePath.toString} ...")
    Option(filePath.getParent) foreach { Files.createDirectories(_) }
    ImageIO.write(img, "png", filePath.toFile)
  }

}
