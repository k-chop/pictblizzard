package com.github.chuwb.pictbliz
package test

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import com.github.chuwb.pictbliz.ScriptOps.{Attr, ANil, AreaUnit, AreaMap}
import collection.immutable.IntMap

class ScriptOpsSpec extends WordSpec with ShouldMatchers {


  "ScriptOps.AreaMap" should {
    "create valid AreaMap from fromSeq()" in {

      val ans = AreaMap(
        IntMap(0 -> 'a, 1 -> 'b, 2 -> 'c),
        Map(
          'a -> AreaUnit(Map('A -> ANil)),
          'b -> AreaUnit(Map('B -> ANil)),
          'c -> AreaUnit(Map('C -> ANil))),
        3)
      val res = AreaMap.fromSeq(
        'a -> AreaUnit(Map('A -> ANil)),
        'b -> AreaUnit(Map('B -> ANil)),
        'c -> AreaUnit(Map('C -> ANil))
      )
      ans.idmap should be (res.idmap)
      (ans.self.forall { case (k,v) =>
        res.self(k) == v
      }) should be (true)

    }

  }
}
