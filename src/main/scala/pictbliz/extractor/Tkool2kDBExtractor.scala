package pictbliz
package extractor

import java.io.FileInputStream
import java.nio.ByteBuffer
import java.nio.channels.FileChannel

import Values._
import pictbliz.ext.FilePath.ToPath

import scala.annotation.tailrec

object Tkool2kDBExtractor {

  // cast unsigned int(int32_t)
  def unsigned(i: Int): Long = ((i >>> 1).toLong << 1) + (i & 1)

  // TODO: rewrite with nio.ByteBuffer!!
  @inline final def ber2int(arr: Array[Byte]): Int = {
    @tailrec def rec(idx: Int, acc: Long): Long = {
      val i = arr(idx) & 0xff
      if (i < 128) acc * 128 + i
      else rec(idx + 1, acc * 128 + (i - 128))
    }
    rec(0, 0L).toInt
  }

  // TODO: rewrite with nio.ByteBuffer!!
  @inline final def int2ber(i: Int): Array[Byte] = {
    val BUF_LEN = 8
    val buf = Array.ofDim[Byte](BUF_LEN)
    @tailrec def rec(c: Long, idx: Int): Int = {
      val d = c % 128
      if (idx == 0) buf(BUF_LEN - 1 - idx) = d.toByte
      else buf(BUF_LEN - 1 - idx) = (d + 128).toByte
      if (c / 128 == 0) idx+1 else rec(c/128, idx+1)
    }
    val len = rec(unsigned(i), 0)
    val ret = Array.ofDim[Byte](len)
    Array.copy(buf, BUF_LEN - len, ret, 0, len)
    ret
  }

  def asByteBuffer[T: ToPath](path: T): ByteBuffer = {
    val ch = new FileInputStream(implicitly[ToPath[T]].toPath(path).toFile).getChannel
    val size = ch.size()
    val buf = if (size < Int.MaxValue) {
      ch.map(FileChannel.MapMode.READ_ONLY, 0, size)
    } else sys.error(s"wow, too big file! filename: $path, size: $size")
    ch.close()
    buf
  }
}

class Tkool2kDBExtractor extends Extractor[Tkool2kDB, Tkool2kDBQuery] {

  override def execute(path: String, query: Tkool2kDBQuery): String = "not implemented"
}

case class Tkool2kDBQuery(category: String, args: Seq[String]) extends Query[Tkool2kDB]
