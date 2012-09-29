package com.github.chuwb.pictbliz.sample

import com.github.chuwb.pictbliz._

class TestSpec {

  import ScriptOps.implicits.string2URI
  
  def run() {
    testReuseLayout()
    testOldSpec()
  }

  def testReuseLayout() {
    import ScriptOps._

    val fontsetting = 'font -> AFont("ＭＳ ゴシック", 'plain, 12)
    val repo = LayoutRepository.empty()

    val lay = LayoutUnit(
      Map('size -> APoint(320, 240)),
      AreaMap.fromSeq(
      'name->AreaUnit(Map('rect -> ARect(5,0,300,13),
                                fontsetting)),
      'icon->AreaUnit(Map('rect -> ARect(280,0,32,32),
                                fontsetting)),
      'desc->AreaUnit(Map('point -> ARect(0, 20, 12, 2),
                                'interval -> AInterval(0, 3),
                                'padding -> APadding(8, 10),
                                fontsetting,
                                'window -> AWindow("system6.png"),
                                'auto_expand -> AAutoExpand,
                                'front_color -> ASystemGraphics("system6.png")
                         )),
      'cost->AreaUnit(Map('rect -> ARect(300,2,30,15),
                                'font -> AFont("Verdana", 'plain, 10))))
    )

    repo.add('normalwindow, lay)

    val v1 = Map(
      'name->Str("エターナルフォースブリザード"),
      'icon->Icon("icon/icon1.png"),
      'desc->Str("一瞬で相手の周囲の大気ごと氷結させる\n相手は死ぬ"),
      'cost->Str("42"))

    val v2 = Map(
      'name->Str("\\c[2]サマーサンシャインバースト"),
      'icon->Icon("icon/icon2.png"),
      'desc->Str("一瞬で太陽を相手の頭上に発生させる\n相手は\\c[4]死ぬ\n\n\\c[0]どう考えても自分も\\c[4]死ぬ"),
      'cost->Str("42")) //

    val v3 = Map(
      'name->Str("\\c[1]インフェルノ\\c[0]・\\c[2]オブ\\c[0]・\\c[3]メサイア"),
      'icon->Icon("icon/icon3.png"),
      'desc->Str("冥界王ダーク・インフェルノを召喚し半径8kmの大地に\n無差別に\\c[2]種\\c[0]を撒き散らしそれはやがて実を結ぶ"),
      'cost->Str("42"))
    
    val v4 = Map(
      'name->Str("ヘルズボルケイノシュート"),
      'icon->Icon("icon/icon1.png"),
      'desc->Str("\\c[6]死の世界\\c[0]から呼び寄せた\\c[8]闇\\c[0]の\\c[2]火弾\\c[0]を\nマッハ2でぶつける\n相手は\\c[4]死ぬ"),
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

    val vs: List[ValueMap] = List(v1,v2,v3,v4,v5,v6)
    //val vs = List(v4)

    val ex1: ExValueMap = Map(

    )

    //val expanded1: Array[ValueMap] = ValueExpander.expand(ex1)

    val d = new Drawer(lay)
    (vs).map{
      d.draw(_, NullContext)
    }.zipWithIndex.foreach { case (res, idx) =>
      res.write("temp/skill0%d.png" format idx)
    }
  }

  def testOldSpec() {
    import ScriptOps._
    import ScriptOps.implicits.string2URI

    val layout = LayoutUnit(
      Map('size -> APoint(320,240)), //env
      AreaMap.fromSeq(  //layouts
        'icon1 -> AreaUnit(Map('point->APoint(0,0))),
        'icon2 -> AreaUnit(Map('point->APoint(50,0))),
        'icon3 -> AreaUnit(Map('point->APoint(100,0))),
        'str -> AreaUnit(Map(
          'rect->ARect(10,30,200,30),
          'align->AAlign('x_center, 'bottom),
          'border->ABorder,
          'font->AFont("Terminus-ja", 'plain, 12))),
        'str2->AreaUnit(Map('rect->ARect(10,60,200,120),
          'font->AFont("Terminus-ja", 'plain, 12),
          'align->AAlign('x_center, 'y_center)))
      ))

    val valuemap = Map(
      'icon1 -> Icon("icon/icon1.png"),
      'icon2 -> Icon("icon/icon2.png"),
      'icon3 -> Icon("icon/icon3.png"),
      'str -> Str("test"),
      'str2 -> Str("a\nb\ncedf\ngiaasdasd\near"))

    val d = new Drawer(layout)
    val result = d.draw(valuemap, NullContext)
    result.write(Resource.tempdir + "test.png")
  }


}
