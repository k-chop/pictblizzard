package pictbliz.extractor

import java.nio.ByteBuffer

import scala.annotation.tailrec
import scala.collection.mutable

private[extractor] case class QueryResult(byte: ByteBuffer, position: Int, isSafe: Boolean, isChild: Boolean) {
  self =>
  import Tkool2kDB.RichByteBuffer

  def asString(): String = {
    byte.position(position)
    byte.nextStr()
  }
  def asInt(): Int = {
    byte.position(position)
    byte.nextBer() // skip data length
    byte.nextBerInt()
  }
  def asArray1(): DBArray1 = new DBArray1(byte, position, isChild)
  def asArray2(): DBArray2 = new DBArray2(byte, position)

  def asArray1At(index: Int): QueryResult = new DBArray1(byte, position, isChild).at(index)
  def asArray2At(index: Int): QueryResult = new DBArray2(byte, position).at(index)

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

  def apply(index: Int): QueryResult

  def at(index: Int): QueryResult = apply(index)

  def makeIndices1(section: DBArray, isChild: Boolean): mutable.LongMap[Int] = makeIndices1(section.position, isChild)

  def makeIndices1(start: Int, isChild: Boolean): mutable.LongMap[Int] = {

    val acc = mutable.LongMap.withDefault(_ => -1)
    byteRef.position(start)

    if (!isChild) { // when create indices as child of 2-dim array, it has no data length.
      byteRef.nextBer() // normally skip data length.
    }

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
    byteRef.nextBer() // skip data length
    val elementLength = byteRef.nextBer()

    @tailrec def rec(len: Int = 0): Int = {
      if (elementLength <= len) len else {

        val arrIdx = byteRef.nextBer()
        if (arrIdx == 0) len else {

          acc +=(arrIdx, byteRef.position)

          while (byteRef.nextBer() != 0) {
            // index-0 is end of array.
            val childDatLen = byteRef.nextBerInt()
            byteRef.forward(childDatLen)
          }
          rec(len + 1)
        }
      }
    }

    rec()
    acc
  }
}

case class DBArray2(byteRef: ByteBuffer, position: Int) extends DBArray {

  def apply(index: Int) = QueryResult(byteRef, indices(index), indices.contains(index), isChild = true)
  val indices = makeIndices2(this)
}

case class DBArray1(byteRef: ByteBuffer, position: Int, isChild: Boolean = false) extends DBArray {

  def apply(index: Int) = QueryResult(byteRef, indices(index), indices.contains(index), isChild = false)
  val indices = makeIndices1(this, isChild)
}
