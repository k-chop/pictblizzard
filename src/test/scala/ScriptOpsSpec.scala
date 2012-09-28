package com.github.chuwb.pictbliz
package test

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import com.github.chuwb.pictbliz.ScriptOps.{Attr, ANil, AreaUnit, AreaMap}
import collection.immutable.IntMap

class ScriptOpsSpec extends WordSpec with ShouldMatchers {


  "ScriptOps.AreaMap" should {
    val kvA = 'a -> AreaUnit(Map('A -> ANil))
    val kvB = 'b -> AreaUnit(Map('B -> ANil))
    val kvC = 'c -> AreaUnit(Map('C -> ANil))

    val ans = AreaMap(
      IntMap(0 -> 'a, 1 -> 'b, 2 -> 'c),
      Map(kvA, kvB, kvC),
      3
    )

    def isSame[A, B](a: Map[A, B], b: Map[A, B]) = {
      val forElem = a forall { kv => b(kv._1) == kv._2 }
      val forSize = a.size == b.size
      forElem && forSize
    }

    "create valid AreaMap from fromSeq()" in {

      val res = AreaMap.fromSeq(kvA, kvB, kvC)
      ans.idmap should be (res.idmap)
/*      (ans.self.forall { case (k,v) =>
        res.self(k) == v
      }) should be (true) */
      isSame(ans.self, res.self) should be (true)
    }

    "create valid AreaMap from AreaMap.add()" in {
      val emptymap = AreaMap.empty()
      val res = emptymap.add(kvA).add(kvB).add(kvC)
      isSame(ans.self, res.self) should be (true)
    }

  }
}
