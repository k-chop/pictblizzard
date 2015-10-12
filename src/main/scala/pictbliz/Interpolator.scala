package pictbliz

import java.io.FileReader

import com.opencsv.CSVReader
import com.typesafe.scalalogging.LazyLogging

import Values._

import scala.collection.mutable

class Interpolator(target: Layouts.VMap, ids: Seq[Int] = Vector(0)) extends LazyLogging {

  private[this] val csvCache = new mutable.WeakHashMap[String, Array[Array[String]]]

  def csv(path: String): Array[Array[String]] = {
    val aas = if (csvCache.contains(path))  {
      csvCache(path)
    } else {
      val reader = new CSVReader(new FileReader(path))
      Stream.continually(reader.readNext()).takeWhile(_ != null).toArray
    }
    aas
  }

  def iterator = new Iterator[Layouts.VMap] {
    private[this] var idx = 0
    
    def next() = {
      val result = interpolate(ids(idx))
      idx += 1
      result
    }
    
    def hasNext = idx < ids.length
  }

  // Return interpolated VMap with 'id'.
  def interpolate(id: Int): Layouts.VMap = {

    target.map { case (key, value) =>
      key -> interpolateWithId(id, key, value)
    } + ("id" -> Text(id.toString))
  }

  final val intepRegex = """\#\{([\w\~]+)\}""".r
  final val idRegex = """id\~(\d+)""".r

  // Rule of convert value to string while interpolation.
  private[this] def value2String(v: Value): String = v match {
    case Text(s) => s
    case Icon(s) => s
    case FaceGraphic(s, _, _) => s
    case CharaGraphic(s, _, _) => s
    case BattleGraphic(s, _, _) => s
    case _ => ""
  }

  // Return next target value.
  // It has special cases that contains 'id'.
  private[this] def lookup(key: String, id: Int) = {
    if (key == "id") Text(id.toString)
    else if (key.startsWith("id~")) Text("")
    else target(key)
  }

  // Return interpolated Values.Value with 'id'.
  def interpolateWithId(id: Int, key: String, value: Values.Value, visited: Set[String] = Set.empty[String]): Values.Value = {

    def impl(self: Value, str: String, makeSelf: String => Value): Value = {
      val newTo = intepRegex.findFirstMatchIn(str).map { m =>
        val name = m.group(m.groupCount)
        val replaceTo = interpolateWithId(id, name, lookup(name, id), visited + key)
        val p = replaceTo match {
          case Text(rep) => rep
          case etc => value2String(etc)
        }
        makeSelf(str.replace(m.matched, p))
      }
      newTo.map(
        // continuing interpolate next #{}.
        interpolateWithId(id, key, _, visited)
      ) getOrElse self
    }

    if (visited(key)) {
      // if detect circular reference error, all values are convert to Values.Text with error msg.

      val msg = "Circular Reference Error Occurred."
      logger.error(msg + s" id: $id, key: $key, value: $value")
      Text(msg)
    } else if (key.startsWith("id~")) {
      // zerofill id

      val idMatch = idRegex.findFirstMatchIn(key)
      val result = idMatch map { m =>
        val n = m.group(m.groupCount).toInt // already matched (\d+), always success toInt.
        val zerofill = if (n == 0) 1 else n
        val formatter = s"%0${zerofill}d"
        formatter.format(id)
      } getOrElse id.toString
      Text(result)

    } else {
      // ugly...
      value match {
        case t @ Text(str) => impl(t, str, s => t.copy(str = s))
        case i @ Icon(str) => impl(i, str, s => i.copy(path = s))
        case f @ FaceGraphic(str, _, _) => impl(f, str, s => f.copy(path = s))
        case c @ CharaGraphic(str, _, _) => impl(c, str, s => c.copy(path = s))
        case b @ BattleGraphic(str, _, _) => impl(b, str, s => b.copy(path = s))
        case v @ CSV(path, column) => Text(csv(path)(id)(column))
        case etc => etc
      }
    }
  }
  
}
