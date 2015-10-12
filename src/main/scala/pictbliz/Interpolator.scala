package pictbliz

import com.typesafe.scalalogging.LazyLogging

import Values._

class Interpolator(target: Layouts.VMap, ids: Seq[Int] = Vector(0)) extends LazyLogging {
  
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

  final val reg = """\#\{(\w+)\}""".r

  // Rule of convert value to string while interpolation.
  private[this] def value2String(v: Value): String = v match {
    case Text(s) => s
    case Icon(s) => s
    case FaceGraphic(s, _, _) => s
    case CharaGraphic(s, _, _) => s
    case BattleGraphic(s, _, _) => s
    case _ => ""
  }

  // Return interpolated Values.Value with 'id'.
  def interpolateWithId(id: Int, key: String, value: Values.Value, visited: Set[String] = Set.empty[String]): Values.Value = {

    def impl(self: Value, str: String, makeSelf: String => Value): Value = {
      val newTo = reg.findFirstMatchIn(str).map { m =>
        val name = m.group(m.groupCount)
        val replaceTo = interpolateWithId(id, name, target(name), visited + key)
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

    // if detect circular reference error, all values are convert to Values.Text with error msg.
    if (visited(key)) {
      val msg = "Circular Reference Error Occurred."
      logger.error(msg + s" id: $id, key: $key, value: $value")
      Text(msg)
    } else {

      // ugly...
      value match {
        case t @ Text(str) => impl(t, str, s => t.copy(str = s))
        case i @ Icon(str) => impl(i, str, s => i.copy(path = s))
        case f @ FaceGraphic(str, _, _) => impl(f, str, s => f.copy(path = s))
        case c @ CharaGraphic(str, _, _) => impl(c, str, s => c.copy(path = s))
        case b @ BattleGraphic(str, _, _) => impl(b, str, s => b.copy(path = s))
        case etc => etc
      }
    }
  }
  
}
