package com.github.whelmaze.pictbliz.scriptops

import collection.mutable
import Attrs._
import collection.generic.{CanBuildFrom, MutableMapFactory}
import language.implicitConversions

object AttrMap {
  implicit def map2AttrMap(from: Map[Key, Attr]): AttrMap = AttrMap(from.toSeq: _*)

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

  def apply(elems: (Key, Attr)*): AttrMap = {
    val buf = empty
    elems.foreach(buf += _)
    buf
  }

  def empty[Key, Attr]: AttrMap = new AttrMap

  implicit def canBuildFrom[A, B] = new CanBuildFrom[AttrMap, (Key, Attr), AttrMap] {
    def apply(from: AttrMap): mutable.Builder[(Key, Attr), AttrMap] = apply()
    def apply(): mutable.Builder[(Key, Attr), AttrMap] = new mutable.MapBuilder[Key, Attr, AttrMap](empty)
  }
}

// 変更不能で値を取得するだけのAttrMapのView
class AttrMapView(private[this] val self: AttrMap) {

  def apply(k: Key): Attr = self.apply(k)
  def get(k: Key): Option[Attr] = self.get(k)
  def find(p: ((Key, Attr)) => Boolean): Option[(Key, Attr)] = self.find(p)
  def contains(k: Key): Boolean = self.contains(k)

}

class AttrMap extends mutable.Map[Key, Attr] with mutable.MapLike[Key, Attr, AttrMap] {

  def const: AttrMapView = new AttrMapView(this)

  val self = mutable.Map.empty[Key, Attr]

  def newTo(from: Map[Key, Attr]): AttrMap = {
    val a = new AttrMap
    a.self ++= from
    a
  }

  override def empty = new AttrMap

  override def apply(k: Key) = self(k)

  def +=(kv: (Attrs.Key, Attr)): this.type = { self += kv; this }

  def +(kv: (Key, Attr)): this.type = { self + kv; this }

  def -=(key: Attrs.Key): this.type = { self -= key; this}

  def get(key: Attrs.Key): Option[Attr] = self.get(key)

  def iterator: Iterator[(Attrs.Key, Attr)] = self.iterator
}
