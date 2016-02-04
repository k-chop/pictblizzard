package pictbliz.extractor

import java.nio.ByteBuffer

import scala.annotation.tailrec
import scala.collection.mutable

sealed trait DBArray {
  import Tkool2kDB.RichByteBuffer

  val byteRef: ByteBuffer
  val position: Int
  val length: Int
  val indices: mutable.LongMap[Int]

  def asString(index: Int): Option[String] = {
    val pos = indices(index)
    if (pos != -1) {
      byteRef.position(pos)
      Some( byteRef.nextStr() )
    } else None
  }

  def asInt(index: Int): Option[Int] = {
    val pos = indices(index)
    if (pos != -1) {
      byteRef.position(pos)
      Some( byteRef.nextBerInt() )
    } else None
  }

  def asArray1(index: Int): DBArray1 = new DBArray1(byteRef, indices(index), -1)

  def asArray2(index: Int): DBArray2 = new DBArray2(byteRef, indices(index), -1)

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

case class DBArray2(byteRef: ByteBuffer, position: Int, length: Int) extends DBArray {

  val indices = makeIndices2(this)
}

case class DBArray1(byteRef: ByteBuffer, position: Int, length: Int) extends DBArray {

  val indices = makeIndices1(this)
}