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

  // Return interpolated Values.Value with 'id'.
  def interpolateWithId(id: Int, key: String, value: Values.Value, visited: Set[String] = Set.empty[String]): Values.Value = {
    if (visited(key)) {
      val msg = "Circular Reference Error Occurred."
      //logger.error(msg + s" id: $id, key: $key, value: $value, visited: $visited")
      Text(msg)
    } else {
      value match {
        case t @ Text(str) =>
          reg.findFirstMatchIn(str).map { m =>
            val name = m.group(m.groupCount)
            val replaceTo = interpolateWithId(id, name, target(name), visited + key)
            replaceTo match {
              case Text(rep) =>
                val newTo = Text(str.replace(m.matched, rep))
                // continuing interpolate next #{}.
                interpolateWithId(id, name, newTo, visited)
              case etc => etc
            }
          } getOrElse t
        case etc => etc
      }
    }
  }
  
}
