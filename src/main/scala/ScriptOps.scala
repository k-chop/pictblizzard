package com.github.chuwb.pictbliz

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
  type AreaMap = Map[Key, AreaUnit]
  
  sealed trait AValue
  case class Str(s: String) extends AValue
  case class Icon(uri: java.net.URI) extends AValue
  case object NullValue extends AValue
  
  case class LayoutUnit(env: AttrMap, areamap: AreaMap) 
  case class AreaUnit(attrmap: AttrMap)

  case class APoint(x: Int, y: Int) extends Attr
  case class ARect(x: Int, y: Int, w: Int, h: Int) extends Attr
  case object AAutoExpand extends Attr
  case class AFont(name: String, style: Symbol, size: Int, lang: FontLang = Ja) extends Attr
  case class AInterval(xparam: Int, yparam: Int) extends Attr
  case class APadding(xparam: Int, yparam: Int) extends Attr
  case class AAlign(xparam: Symbol, yparam: Symbol) extends Attr
  case object ABorder extends Attr
  
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

