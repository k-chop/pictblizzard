package pictbliz

import collection.mutable

import scriptops.Attrs._

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
