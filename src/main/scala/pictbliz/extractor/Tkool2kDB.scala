package pictbliz.extractor

import java.io.FileInputStream
import java.nio.ByteBuffer
import java.nio.channels.FileChannel

import pictbliz.ext.FilePath.ToPath

sealed trait DBArray {
  val position: Int
  val length: Int
}

case class DBArray2(position: Int, length: Int) extends DBArray
case class DBArray1(position: Int, length: Int) extends DBArray

object Tkool2kDB {

  def fromFile[T: ToPath](path: T): Tkool2kDB = {
    ??? //val buf = asByteBuffer(path)
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

