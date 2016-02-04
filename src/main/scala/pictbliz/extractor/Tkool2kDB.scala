package pictbliz.extractor

import java.io.FileInputStream
import java.nio.{Buffer, ByteBuffer}
import java.nio.channels.FileChannel

import scala.annotation.tailrec
import scala.collection.immutable.LongMap
import scala.collection.mutable

import pictbliz.ext.FilePath.ToPath

sealed trait DBArray {
  val byteRef: ByteBuffer
  val position: Int
  val length: Int
  val indices: mutable.LongMap[Int]
}

case class DBArray2(byteRef: ByteBuffer, position: Int, length: Int) extends DBArray {

  val indices = makeIndices2()

  def makeIndices2(): mutable.LongMap[Int] = {
    import Tkool2kDB.RichByteBuffer

    val acc = mutable.LongMap.withDefault(_ => -1)
    byteRef.position(position)
    byteRef.nextBer() // skip element length
    while(byteRef.position < position + length) {
      val arrIdx = byteRef.nextBer()
      acc += (arrIdx, byteRef.position)

      while(byteRef.nextBer() != 0) { // index-0 is end of array.
      val childDatLen = byteRef.nextBerInt()
        byteRef.forward(childDatLen)
      }
    }

    acc
  }
}

case class DBArray1(byteRef: ByteBuffer, position: Int, length: Int) extends DBArray {

  val indices = makeIndices1()

  // calculate and return byte positions from indices.
  private[this] def makeIndices1(): mutable.LongMap[Int] = {
    import Tkool2kDB.RichByteBuffer

    val acc = mutable.LongMap.withDefault(_ => -1)
    byteRef.position(position)
    while(byteRef.position < position + length) {
      val arrIdx = byteRef.nextBer()
      acc += (arrIdx, byteRef.position)
      val datLen = byteRef.nextBerInt()
      byteRef.forward(datLen)
    }

    acc
  }
}

object Tkool2kDB {
  import Tkool2kDBExtractor._

  implicit class RichByteBuffer(val self: ByteBuffer) extends AnyVal {
    def forward(step: Int): Buffer = self.position(self.position + step)
    def back(step: Int): Buffer = forward(-step)
    def nextBer(): Long = Tkool2kDBExtractor.nextBer(self)
    def nextBerInt(): Int = Tkool2kDBExtractor.nextBerInt(self)
    def nextStr(): String = Tkool2kDBExtractor.nextStr(self)
  }

  def fromFile[T: ToPath](path: T): Tkool2kDB = {

    val buf = asByteBuffer(path)
    buf.forward(nextBerInt(buf)) // skip header
    val acc = mutable.ArrayBuilder.make[(Int, Int)]

    while(buf.position < buf.limit) {
      nextBerInt(buf) // skip ArrayNumber
      val length = nextBerInt(buf)
      val pos = buf.position
      acc += ((pos, length))
      buf.forward(length)
    }
    val a = acc.result()

    buf.position(0)

    def f2(a: (Int, Int)) = new DBArray2(buf, a._1, a._2)
    def f1(a: (Int, Int)) = new DBArray1(buf, a._1, a._2)

    Tkool2kDB(
      bytes = buf,
      heroes = f2(a(0)),
      skills = f2(a(1)),
      items = f2(a(2)),
      monsters = f2(a(3)),
      monsterGroups = f2(a(4)),
      terrains = f2(a(5)),
      attributes = f2(a(6)),
      conditions = f2(a(7)),
      animations = f2(a(8)),
      tileSets = f2(a(9)),
      vocabulary = f1(a(10)),
      systemSettings = f1(a(11)),
      switches = f2(a(12)),
      variables = f2(a(13)),
      commonEvents = f2(a(14))
    )
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

case class Tkool2kDB(
    bytes: ByteBuffer,
    heroes: DBArray2,
    skills: DBArray2,
    items: DBArray2,
    monsters: DBArray2,
    monsterGroups: DBArray2,
    terrains: DBArray2,
    attributes: DBArray2,
    conditions: DBArray2,
    animations: DBArray2,
    tileSets: DBArray2,
    vocabulary: DBArray1,
    systemSettings: DBArray1,
    switches: DBArray2,
    variables: DBArray2,
    commonEvents: DBArray2
) {
  import Tkool2kDB.RichByteBuffer

  // seek bytes to beginning of section
  def seek(section: DBArray): Tkool2kDB = seek(section.position)

  def seek(newPos: Int): Tkool2kDB = {
    bytes.position(newPos)
    this
  }

  // calculate and return byte positions from indices. (for DBArray1)
  def makeIndices1(section: DBArray): mutable.LongMap[Int] = {

    val acc = mutable.LongMap.withDefault(_ => -1)
    seek(section)
    while(bytes.position < section.position + section.length) {
      val arrIdx = nextBer()
      acc += (arrIdx, bytes.position)
      val datLen = nextBerInt()
      bytes.forward(datLen)
    }

    acc
  }

  def makeIndices1(start: Int): mutable.LongMap[Int] = {
    val acc = mutable.LongMap.withDefault(_ => -1)
    seek(start)

    @tailrec def rec(len: Int = 0): Int = {
      val arrIdx = nextBer()
      if (arrIdx == 0) len else {
        acc += (arrIdx, bytes.position)
        val datLen = nextBerInt()
        bytes.forward(datLen)
        rec(len + 1)
      }
    }

    rec()
    acc
  }

  def makeIndices2(section: DBArray): mutable.LongMap[Int] = {

    val acc = mutable.LongMap.withDefault(_ => -1)
    seek(section)
    nextBer() // skip element length
    while(bytes.position < section.position + section.length) {
      val arrIdx = nextBer()
      acc += (arrIdx, bytes.position)

      while(nextBer() != 0) { // index-0 is end of array.
        val childDatLen = nextBerInt()
        bytes.forward(childDatLen)
      }
    }

    acc
  }

  def nextBer(): Long = Tkool2kDBExtractor.nextBer(bytes)
  def nextBerInt(): Int = Tkool2kDBExtractor.nextBerInt(bytes)
  def nextStr(): String = Tkool2kDBExtractor.nextStr(bytes)
}

