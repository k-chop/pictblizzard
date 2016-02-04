package pictbliz.extractor

import java.nio.ByteBuffer

import scala.annotation.tailrec
import scala.collection.mutable

sealed trait DBArray {
  import Tkool2kDB.RichByteBuffer

  val byteRef: ByteBuffer
  val position: Int
  val indices: mutable.LongMap[Int]

  def asStringOption(index: Int): Option[String] =
    if (indices.contains(index)) Some(asString(index)) else None

  def asString(index: Int): String = {
    byteRef.position(indices(index))
    byteRef.nextStr()
  }

  def asIntOption(index: Int): Option[Int] =
    if (indices.contains(index)) Some(asInt(index)) else None

  def asInt(index: Int): Int = {
    byteRef.position(indices(index))
    byteRef.nextBerInt()
  }

  def asArray1(index: Int): DBArray1 = new DBArray1(byteRef, indices(index))

  def asArray2(index: Int): DBArray2 = new DBArray2(byteRef, indices(index))

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
