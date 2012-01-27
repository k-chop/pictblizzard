package com.github.chuwb.pictbliz
package test

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import scala.collection.mutable.Stack

class TestSpec extends WordSpec with ShouldMatchers {

  import ScriptOps.implicits.string2URI
  
  "Ops" should {
    
    import ScriptOps._

    val lay = LayoutUnit(
      Map('size -> APoint(320, 60)),
      Map(
      'name->AreaUnit(Map('rect->ARect(5,10,300,13))),
      'icon->AreaUnit(Map('rect->ARect(280,0,32,32))),
      'desc->AreaUnit(Map('point->APoint(8,25),
                                'x_interval-> AXInterval(2),
                                'border->ABorder,
                                'auto_expand->AAutoExpand)),
      'cost->AreaUnit(Map('rect->ARect(300,2,30,15),
                                'font->AFont("Verdana", 'plain, 10))))
    )

    val v1 = Map(
      'name->Str("エターナルフォースブリザード"),
      'icon->Icon("icon/icon1.png"),
      'desc->Str("一瞬で相手の周囲の大気ごと氷結させる\n相手は死ぬ"),
      'cost->Str("42"))

    val v2 = Map(
      'name->Str("サマーサンシャインバースト"),
      'icon->Icon("icon/icon2.png"),
      'desc->Str("一瞬で太陽を相手の頭上に発生させる\n相手は死ぬ"),
      'cost->Str("42"))

    val v3 = Map(
      'name->Str("\\c[1]インフェルノ\\c[0]・\\c[2]オブ\\c[0]・\\c[3]メサイア"),
      'icon->Icon("icon/icon3.png"),
      'desc->Str("冥界王ダーク・インフェルノを召喚士半径8kmの大地に\n無差別に\\c[2]種\\c[0]を撒き散らしそれはやがて実を結ぶ"),
      'cost->Str("42"))
    
    val v4 = Map(
      'name->Str("ヘルズボルケイノシュート"),
      'icon->Icon("icon/icon1.png"),
      'desc->Str("死の世界から呼び寄せた闇の火弾をマッハ2でぶつける\n相手は死ぬ"),
      'cost->Str("42"))

    val v5 = Map(
      'name->Str("アルティメットインフィニティサンデイ"),
      'icon->Icon("icon/icon2.png"),
      'desc->Str("毎日が日曜日。\n相手は死ぬ"),
      'cost->Str("42"))

    val v6 = Map(
      'name->Str("エンパイア・ステート・ビル"),
      'icon->Icon("icon/icon3.png"),
      'desc->Str("1931年に建てられた高さ443m、102階建てのビル。\n相手は死ぬ"),
      'cost->Str("42"))

    val vs = List(v1,v2,v3,v4,v5,v6)
    
    val d = new Drawer(lay)
    vs.map{
      d.draw(_, NullContext)
    }.zip{
      0 to vs.length
    }.foreach { case (res, idx) =>
      res.write("temp/skill0%d.png" format idx)
    }
    
    "a" in { val s = 1
      s should be (s) }
  }

  
  // "stack" should {
  //   val stack = new Stack[Int]
  //   "be empty" in {
  //     stack should be ('empty)
  //   }
  //   "complain when popped" in {
  //     evaluating { stack.pop() } should produce [NoSuchElementException]
  //   }
  // }

}
