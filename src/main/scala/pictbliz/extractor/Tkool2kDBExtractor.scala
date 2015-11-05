package pictbliz
package extractor

import Values._

class Tkool2kDBExtractor extends Extractor[Tkool2kDB, Tkool2kDBQuery] {

  override def execute(path: String, query: Tkool2kDBQuery): String = "not implemented"
}

case class Tkool2kDBQuery(category: String, args: Seq[String]) extends Query[Tkool2kDB]
