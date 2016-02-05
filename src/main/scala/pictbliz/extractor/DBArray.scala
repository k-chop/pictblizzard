package pictbliz.extractor

import java.nio.ByteBuffer

import scala.annotation.tailrec
import scala.collection.mutable

private[extractor] case class QueryResult(byte: ByteBuffer, position: Int, isSafe: Boolean) {
  self =>
  import Tkool2kDB.RichByteBuffer

  def asString(): String = {
    byte.position(position)
    byte.nextStr()
  }
  def asInt(): Int = {
    byte.position(position)
    byte.nextBerInt()
  }
  def asArray1(): DBArray1 = new DBArray1(byte, position)
  def asArray2(): DBArray2 = new DBArray2(byte, position)

  // Return result wrapped with option.
  object opt {

    def asString(): Option[String] = if (isSafe) Some(self.asString()) else None
    def asInt(): Option[Int] = if (isSafe) Some(self.asInt()) else None
    def asArray1(): Option[DBArray1] = if (isSafe) Some(self.asArray1()) else None
    def asArray2(): Option[DBArray2] = if (isSafe) Some(self.asArray2()) else None

  }
}

sealed trait DBArray {
  import Tkool2kDB.RichByteBuffer

  val byteRef: ByteBuffer
  val position: Int
  val indices: mutable.LongMap[Int]

  def apply(index: Int) = QueryResult(byteRef, indices(index), indices.contains(index))

  def makeIndices1(section: DBArray): mutable.LongMap[Int] = makeIndices1(section.position)

  def makeIndices1(start: Int): mutable.LongMap[Int] = {
    val acc = mutable.LongMap.withDefault(_ => -1)
    byteRef.position(start)

    @tailrec def rec(len: Int = 0): Int = {
      val arrIdx = byteRef.nextBer()
      if (arrIdx == 0) len else {
        acc += (arrIdx, byteRef.position)
        val datLen = byteRef.nextBerInt()
        byteRef.forward(datLen)
        rec(len + 1)
      }
    }

    rec()
    acc
  }

  def makeIndices2(section: DBArray): mutable.LongMap[Int] = makeIndices2(section.position)

  def makeIndices2(start: Int): mutable.LongMap[Int] = {

    val acc = mutable.LongMap.withDefault(_ => -1)
    byteRef.position(start)
    val elementLength = byteRef.nextBer()

    @tailrec def rec(len: Int = 0): Int = {
      if (elementLength <= len) len else {
        val arrIdx = byteRef.nextBer()
        acc += (arrIdx, byteRef.position)

        while(byteRef.nextBer() != 0) { // index-0 is end of array.
        val childDatLen = byteRef.nextBerInt()
          byteRef.forward(childDatLen)
        }
        rec(len + 1)
      }
    }

    rec()
    acc
  }
}

case class DBArray2(byteRef: ByteBuffer, position: Int) extends DBArray {

  val indices = makeIndices2(this)
}

case class DBArray1(byteRef: ByteBuffer, position: Int) extends DBArray {

  val indices = makeIndices1(this)
}
