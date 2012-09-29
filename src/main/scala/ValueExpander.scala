package com.github.chuwb.pictbliz

import collection.mutable
import ScriptOps._
import collection.immutable.HashMap
import au.com.bytecode.opencsv.CSVReader
import java.io.FileReader

class ValueExpander(exs: ExValueMap) {
  case class KV(id: Int, k: Key)
  //val cache = new mutable.MapBuilder[KV, AValue, HashMap[KV, AValue]](new HashMap[KV, AValue])
  val cache = new mutable.WeakHashMap[KV, AValue]
  val cvsCache = new mutable.WeakHashMap[String, Array[Array[String]]]
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
    val range: Array[Int] = exs.get("id") match {
      case Some(ExRange(ids)) =>
        ids
      case None =>
        logger.error("idが見つからないのでExValueMapを展開できません.空のArrayを返します.", exs)
        Array()
    }

    val m = new mutable.MapBuilder[Key, AValue, HashMap[Key, AValue]](new HashMap[Key, AValue])
    range.foreach { id =>
      m += ('id -> Str(id.toString))
      exs withFilter (_ != "id") foreach { kv =>
        kv match {
          case (k, v: ExValue) =>
            m += (Symbol(k) -> expandWithId(id, Symbol(k), v))
          case (k, v: AValue) =>
            m += (Symbol(k) -> v)
        }
      }
      acc += m.result()
      m.clear()
    }

    cache.clear()
    acc.toArray
  }

  def expandWithId(id: Int, k: Key, exv: ExValue): AValue = {
    cache.get(KV(id, k)) getOrElse {
      exv match {
        case ExStr(str) =>
          val ret = Str(extractSubStrs(id, str))
          cache.+=((KV(id, k), ret))
          ret
        case ExRange(_) => Str(id.toString)
        case ExCSV(path, col) =>
          Str(getCVSData(path)(id)(col))
      }
    }
  }

  def extractSubStrs(id: Int, _str: String): String = {
    var str = _str
    val reg = """\$\{(\w+)\}""".r
    reg.findAllIn(str).matchData foreach { m =>
      val name = m.group(m.groupCount)
      val replaceTo = exs(name) match {
        //case v: ExValue if name == "id" => id.toString
        case v: ExValue =>
          expandWithId(id, Symbol(name), v) match {
            // other case
            case Str(s) => s
          }
        case v: AValue => v match {
          // other case
          case Str(s) => s
        }
      }
      str = str.replace(m.matched, replaceTo)
    }
    str
  }


}
