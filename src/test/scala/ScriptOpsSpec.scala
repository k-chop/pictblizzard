package com.github.whelmaze.pictbliz.test

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import com.github.whelmaze.pictbliz.ScriptOps.{ANil, AreaUnit, AreaMap}
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

    "create valid AreaMap from fromSeq()" in {

      val res = AreaMap.fromSeq(kvA, kvB, kvC)
      ans.idmap should be (res.idmap)
      isSame(ans.self, res.self) should be (true)
    }

    "create valid AreaMap from AreaMap.add()" in {
      val emptymap = AreaMap.empty()
      val res = emptymap.add(kvA).add(kvB).add(kvC)
      isSame(ans.self, res.self) should be (true)
    }

  }
}
