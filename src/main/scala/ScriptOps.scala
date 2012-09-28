package com.github.chuwb.pictbliz

import collection.mutable
import collection.immutable.{IntMap, TreeMap}

object ScriptOps {

  // つめこみすぎ
  
  object implicits {
    implicit def string2URI(s: String): java.net.URI =
      Resource.uri(s)
  }
  
  trait Attr
  type Key = Symbol
  
  type AttrMap = Map[Key, Attr]
  type ValueMap = Map[Key, AValue]

  object AttrMap {
    def empty() = Map.empty[Key, Attr]

    def findParam(am: AttrMap, ss: Symbol*): Option[Attr] = {
      @scala.annotation.tailrec
      def findEnableParamRec(am: AttrMap, sl: List[Symbol]): Option[Attr] = sl match {
        case s :: cdr => {
          am find { case(k,v) => k == s } match {
            case Some((k,v)) => Some(v)
            case None => findEnableParamRec(am, cdr)
          }
        }
        case Nil => None
      }
      findEnableParamRec(am, ss.toList)
    }

  }


  object AreaMap {
    def empty() = {
      ScriptOps.AreaMap(IntMap(), Map(), 0)
    }
    def fromSeq(kvs: (Key, AreaUnit)*) = {
      val _accId = mutable.HashMap.empty[Int, Key]
      val _self = mutable.HashMap.empty[Key, AreaUnit]
      var count: Int = 0

      kvs foreach { case (k, v) =>
        _accId += (count -> k)
        _self  += (k -> v)
        count += 1
      }

      ScriptOps.AreaMap(IntMap(_accId.toSeq: _*), _self.toMap, count)
    }
  }
  case class AreaMap(idmap: IntMap[Key], self: Map[Key, AreaUnit], nextId: Int = 0) {
    /*
     * 指定したIDに挿入
     * IDが埋まっていたらどうしよう
     */
    def addWithId(kv: (Key, AreaUnit), id: Int): AreaMap = {
      sys.error("not implemented")
    }
    /*
     * 頭から空きを探して挿入
     */
    def add(kv: (Key, AreaUnit)): AreaMap = {
      var chkId = nextId
      while (idmap.contains(chkId))
        chkId += 1

      AreaMap(idmap + (chkId -> kv._1), self + kv, chkId + 1)
    }
    def foreach(f: (Key, AreaUnit) => Unit) {
      idmap foreach ( kv => f(kv._2, self(kv._2)) )
    }
  }
//  type AreaMap = TreeMap[Key, AreaUnit]
  
  sealed trait AValue
  case class Str(s: String) extends AValue
  case class Icon(uri: java.net.URI) extends AValue
  case object NullValue extends AValue

  object LayoutUnit {
    def empty() = LayoutUnit.apply(AttrMap.empty(), AreaMap.empty())
  }
  case class LayoutUnit(env: AttrMap, areamap: AreaMap) 
  case class AreaUnit(attrmap: AttrMap) {


  }

  // attr_name
  // class difinition

  // point
  case class APoint(x: Int, y: Int) extends Attr
  // rect
  case class ARect(x: Int, y: Int, w: Int, h: Int) extends Attr
  // auto_expand
  case object AAutoExpand extends Attr
  // font
  case class AFont(name: String, style: Symbol, size: Int, lang: FontLang = Ja) extends Attr
  // interval
  case class AInterval(xparam: Int, yparam: Int) extends Attr
  // padding
  case class APadding(xparam: Int, yparam: Int) extends Attr
  // align
  case class AAlign(xparam: Symbol, yparam: Symbol) extends Attr
  // border
  case object ABorder extends Attr
  // nil
  case object ANil extends Attr
  // front_color or back_color
  case class ASystemGraphics(path: String) extends Attr
  // front_color or back_color
  case class ASingleColors(colors: Array[String]) extends Attr {
    def this(strs: String*) = {
      this(strs.toArray)
    }
  }


  case class ATile() extends Attr
  case class ABackground() extends Attr
  case class AWindow(systemGraphicsPath: String) extends Attr
  
  sealed trait FontLang
  case object Ja extends FontLang
  case object En extends FontLang
  
  trait Context
  class LexicalContext extends Context
  object NullContext extends Context
  
}

