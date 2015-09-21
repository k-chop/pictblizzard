package com.github.whelmaze.pictbliz

import com.typesafe.scalalogging.LazyLogging

import collection.mutable
import collection.immutable.HashMap
import com.opencsv.CSVReader
import java.io.FileReader

import scriptops.Attrs
import scriptops.Attrs._

/**
 * 展開可能なExValueMapを、外部の値を読み込んだりしつつ展開する。
 * @param exs 展開するValueMap
 */
class ValueExpander(exs: ExValueMap) extends LazyLogging {

  case class KV(id: Int, k: Key)

  private[this] val checker = new mutable.HashSet[Key]

  private[this] val cache = new mutable.WeakHashMap[KV, AValue]

  private[this] val cvsCache = new mutable.WeakHashMap[String, Array[Array[String]]]

  private[this] lazy val range: Array[Int] = exs.get("id") match {
    case Some(ExRange(ids)) => ids
    case Some(Number(id)) => Array(id)
    case _ => Array.empty[Int]
  }

  def getCVSData(path: String): Array[Array[String]] = {
    val aas = if (cvsCache.contains(path))  {
      cvsCache(path)
    } else {
      val reader = new CSVReader(new FileReader(Resource.str(path)))
      Stream.continually(reader.readNext()).takeWhile(_ != null).toArray
    }
    aas
  }

  def expand(): Array[ValueMap] = {
    val acc = mutable.ArrayBuffer.empty[ValueMap]

    iterator.foreach( acc += _ )

    cache.clear()
    acc.toArray
  }

  def iterator: Iterator[Attrs.ValueMap] = new Iterator[ValueMap] {
    private var idx = 0
    def next(): ValueMap = {
      val res = expandAt(range(idx))
      idx += 1
      res
    }
    def hasNext = idx < range.length
  }

  def apply(id: Int): ValueMap = expandAt(id)

  def expandAt(id: Int): ValueMap = {
    val m = new mutable.MapBuilder[Key, AValue, HashMap[Key, AValue]](new HashMap[Key, AValue])
    m += ('id -> Str(id.toString))
    exs withFilter (_._1 != "id") foreach { kv =>
      checker.clear()
      kv match {
        case (k, v: ExValue) =>
          m += (Symbol(k) -> expandWithId(id, Symbol(k), v))
        case (k, v: AValue) =>
          m += (Symbol(k) -> v)
      }
    }
    m.result()
  }

  def expandWithId(id: Int, k: Key, exv: ExValue, zerofillDigit: Int = 0): AValue = {
    import scriptops.implicits.string2URI

    val c = cache.get(KV(id, k))
    if (k != 'id && c.isEmpty && checker.contains(k)) {
      logger.error(s"returning Circular Reference Error. id:$id, key:$k, cache:$checker")
      return Str("Circular Reference Error")
    } else {
      checker += k
    }

    c getOrElse {
      exv match {
        case ExStr(str) =>
          val ret = Str(extractSubStrs(id, str))
          cache += (KV(id, k) -> ret)
          ret
        case ExRange(_) =>
          if (zerofillDigit == 0) {
            Str(id.toString)
          } else {
            val ids = new mutable.StringBuilder(id.toString)
            var a: Int = zerofillDigit - ids.length
            while(0 < a) {
              ids.insert(0, "0")
              a -= 1
            }
            Str(ids.toString())
          }
        case ExCSV(path, col) =>
          val ret = Str(getCVSData(path)(id)(col))
          cache += (KV(id, k) -> ret)
          ret
        case ExIcon(str, zfD) =>
          val ret = Icon(extractSubStrs(id, str, zfD))
          cache += (KV(id, k) -> ret)
          ret
        case NullValue =>
          NullValue
      }
    }
  }

  def extractSubStrs(id: Int, _str: String, zerofillDigit: Int = 0): String = {

    val pf: PartialFunction[AValue, String] = {
      case Str(s) => s
      case Icon(p) => p.toString
      case Number(n) => n.toString
      case FaceGraphic(p, n, t) => p.toString + ":" + n.toString
      case NullValue => ""
    }

    var str = _str
    val reg = """\$\{(\w+)\}""".r
    reg.findAllIn(str).matchData foreach { m =>
      val name = m.group(m.groupCount)
      val replaceTo = exs(name) match {
        //case v: ExValue if name == "id" => id.toString
        case v: ExValue =>
          pf( expandWithId(id, Symbol(name), v, zerofillDigit) )
        case v: AValue =>
          pf (v)
      }
      str = str.replace(m.matched, replaceTo)
    }
    str
  }


}
