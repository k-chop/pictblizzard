package com.github.chuwb.pictbliz

import com.github.chuwb.pictbliz.ScriptOps.LayoutUnit
import collection.mutable

object LayoutRepository {
  def empty() = new LayoutRepository(Map())
}

class LayoutRepository(_container: Map[Symbol, LayoutUnit]) {
  val container = mutable.WeakHashMap.empty[Symbol, LayoutUnit]
  container ++= _container

  def add(name: Symbol, v: LayoutUnit): LayoutRepository = {
    container += name -> v
    this
  }
  def apply(name: Symbol): LayoutUnit = container(name)
  def get(name: Symbol): Option[LayoutUnit] = container.get(name)
}
