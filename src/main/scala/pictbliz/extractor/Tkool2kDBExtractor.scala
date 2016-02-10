package pictbliz
package extractor

import java.nio.ByteBuffer
import java.nio.charset.Charset

import Tkool2kDB.RichByteBuffer

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

  // consume Byte as BER integer and return converted int from ber.
  def nextBer(buf: ByteBuffer): Long = {
    @tailrec def rec(acc: Long): Long = {
      val i = buf.get(buf.position) & 0xff
      if (i < 128) {
        buf.forward(1)
        acc * 128 + i
      } else {
        buf.forward(1)
        rec(acc * 128 + (i - 128))
      }
    }
    rec(0L)
  }

  def nextStr(buf: ByteBuffer, length: Int): String = {
    val t = Array.ofDim[Byte](length)
    buf.get(t)
    new String(t, Charset.forName("Windows-31j"))
  }

  def nextStr(buf: ByteBuffer): String = {
    val len = nextBer(buf).toInt
    nextStr(buf, len)
  }

  def nextBerInt(buf: ByteBuffer): Int = nextBer(buf).toInt
}

class Tkool2kDBExtractor extends Extractor[Tkool2kDB, Tkool2kDBQuery] {

  override def execute(path: String, query: Tkool2kDBQuery): String = {

    val db = Tkool2kDB.fromFile(path)
    val args = query.args

    query.category match {
      case "skill" => // args = ["<array number>", "<keyword>", "<rest>"... ],
                      // if <keyword> has child, <rest> = repeat("<child array number>", "<keyword>")
        new SkillAccessor(db.skills).get(args.head.toInt, args(1), args.drop(2))

      case "vocabulary" => // args = ["<keyword>"]
        new VocabularyAccessor(db.vocabulary).get(args.head)

      case _ => ""
    }
  }
}

case class Tkool2kDBQuery(category: String, args: Seq[String]) extends Query[Tkool2kDB]
