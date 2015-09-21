package pictbliz
package scriptops

import pictbliz.Resource
import scala.language.implicitConversions

object implicits {
  implicit def string2URI(s: String): java.net.URI =
    Resource.uri(s)
}