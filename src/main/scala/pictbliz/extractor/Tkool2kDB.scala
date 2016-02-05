package pictbliz.extractor

import java.io.FileInputStream
import java.nio.{Buffer, ByteBuffer}
import java.nio.channels.FileChannel

import scala.collection.mutable

import pictbliz.ext.FilePath.ToPath


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
    val builder = Map.newBuilder[Int, Int]

    while(buf.position < buf.limit) {
      val rootArrayNumber = nextBerInt(buf)
      val length = nextBerInt(buf)
      val pos = buf.position
      builder += rootArrayNumber -> pos
      buf.forward(length)
    }
    val mappos = builder.result()

    buf.position(0)

    def f2(i: Int) = new DBArray2(buf, mappos(i))
    def f1(i: Int) = new DBArray1(buf, mappos(i))

    Tkool2kDB(
      bytes = buf,
      heroes         = f2(0x0B),
      skills         = f2(0x0C),
      items          = f2(0x0D),
      monsters       = f2(0x0E),
      monsterGroups  = f2(0x0F),
      terrains       = f2(0x10),
      attributes     = f2(0x11),
      conditions     = f2(0x12),
      animations     = f2(0x13),
      tileSets       = f2(0x14),
      vocabulary     = f1(0x15),
      systemSettings = f1(0x16),
      switches       = f2(0x17),
      variables      = f2(0x18),
      commonEvents   = f2(0x19)
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

}

