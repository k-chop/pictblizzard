package com.github.whelmaze.pictbliz.test

import collection.immutable.IntMap

import com.github.whelmaze.pictbliz.scriptops.Attrs._
import com.github.whelmaze.pictbliz.scriptops._

class ScriptOpsSpec extends UnitSpec {

  "ScriptOps.AreaMap" should {

    val kvA = 'a -> AreaUnit(Map('A -> ANil))
    val kvB = 'b -> AreaUnit(Map('B -> ANil))
    val kvC = 'c -> AreaUnit(Map('C -> ANil))

    val ans = AreaMap(
      IntMap(0 -> 'a, 1 -> 'b, 2 -> 'c),
      Map(kvA, kvB, kvC),
      3
    )

    "create valid AreaMap from fromSeq()" in {

      val res = AreaMap.fromSeq(kvA, kvB, kvC)
      ans.idmap should be (res.idmap)
      ans.self shouldBe res.self
    }

    "create valid AreaMap from AreaMap.add()" in {
      val emptymap = AreaMap.empty()
      val res = emptymap.add(kvA).add(kvB).add(kvC)
      ans.self shouldBe res.self
    }

  }
}
