package com.github.whelmaze.pictbliz.scriptops

import collection.immutable.IntMap
import collection.mutable

import Attrs._

object AreaMap {
  def empty() = {
    AreaMap(IntMap(), Map(), 0)
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

    AreaMap(IntMap(_accId.toSeq: _*), _self.toMap, count)
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
  override def toString: String = {
    val s = new mutable.StringBuilder
    s append s"[AreaMap: nextId=$nextId, "
    idmap.valuesIterator foreach { v => s append s"${self(v).toString}, " }
    s append "]"
    s.toString()
  }
}
//  type AreaMap = TreeMap[Key, AreaUnit]