package com.github.whelmaze.pictbliz.scriptops

import Attrs._

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
