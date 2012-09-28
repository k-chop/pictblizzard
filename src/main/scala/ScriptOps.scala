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

  object AreaMap {
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

    }
    /*
     * 頭から空きを探して挿入
     */
    def add(kv: (Key, AreaUnit)): AreaMap = {
      var chkId = nextId
      while (idmap.contains(chkId))
        chkId += 1

      AreaMap(idmap + (chkId -> kv._1), self + kv, chkId)
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
  
  case class LayoutUnit(env: AttrMap, areamap: AreaMap) 
  case class AreaUnit(attrmap: AttrMap) {


  }

  case class APoint(x: Int, y: Int) extends Attr
  case class ARect(x: Int, y: Int, w: Int, h: Int) extends Attr
  case object AAutoExpand extends Attr
  case class AFont(name: String, style: Symbol, size: Int, lang: FontLang = Ja) extends Attr
  case class AInterval(xparam: Int, yparam: Int) extends Attr
  case class APadding(xparam: Int, yparam: Int) extends Attr
  case class AAlign(xparam: Symbol, yparam: Symbol) extends Attr
  case object ABorder extends Attr
  case object ANil extends Attr
  
  case class ATile() extends Attr
  case class ABackground() extends Attr
  case class AWindow() extends Attr
  
  sealed trait FontLang
  case object Ja extends FontLang
  case object En extends FontLang
  
  trait Context
  class LexicalContext extends Context
  object NullContext extends Context
  
}

