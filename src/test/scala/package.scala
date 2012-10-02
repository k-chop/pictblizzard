package com.github.whelmaze.pictbliz.test

object `package` {

  def isSame[A, B](a: Map[A, B], b: Map[A, B]) = {
    val forElem = a forall { kv => b(kv._1) == kv._2 }
    val forSize = a.size == b.size
    val result = (forElem && forSize)
    if (result) result else {
      if (!forElem) println("%s has not SameElement %s" format (a,b))
      if (!forSize) println("invalid size => size A(%d) sizeB(%d)" format (a.size, b.size))
      result
    }
  }
}
