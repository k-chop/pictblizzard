package pictbliz
package extractor

import java.io.FileReader

import com.opencsv.CSVReader

import Values._

import scala.collection.mutable

class CSVExtractor extends Extractor[CSV, CSVQuery] {

  private[this] val csvCache = new mutable.WeakHashMap[String, Array[Array[String]]]

  private[this] def csv(path: String): Array[Array[String]] = {
    val aas = if (csvCache.contains(path))  {
      csvCache(path)
    } else {
      val reader = new CSVReader(new FileReader(path))
      Stream.continually(reader.readNext()).takeWhile(_ != null).toArray
    }
    aas
  }

  def execute(path: String, query: CSVQuery): String =
    csv(path)(query.id)(query.column)
}

case class CSVQuery(id: Int, column: Int) extends Query[CSV]
