package com.github.whelmaze.pictbliz.sample

import com.github.whelmaze.pictbliz._
import scriptops.Attrs._
import scriptops.{AttrMap => AMap, AreaMap}
import scala.language.postfixOps

class TestSpec {
  import scriptops.implicits.string2URI

  val strdef = AMap('interval -> AInterval(0, 3),
                   'padding -> APadding(8, 10))
  def stdstyle(font: String = "ＭＳ ゴシック", size: Int = 12, style: Symbol = 'plain, inWin: Boolean = false) = {
    (if (inWin) strdef else AMap.empty) + ('font -> AFont(font, style, size))
  }

  @inline def r(i: Int) = scala.util.Random.nextInt(i)

  def run() {
    testReuseLayout()
    testOldSpec()
    csvRead()
    faceSpec()
    charaSpec()
    battleSpec()
  }

  def csvRead() {
    val path = "item.csv"
    val vmap: Array[ValueMap] = new ValueExpander(Map(
      "id" -> ExRange(0 to 76 toArray),
      "itemno" -> ExCSV(path, 0),
      "name" -> ExCSV(path, 1),
      "win" -> ExStr(""),
      "t_price" -> ExCSV(path, 2), "t_desc" -> ExCSV(path, 3), "t_kind" -> ExCSV(path, 4), "t_atk" -> ExCSV(path, 6), "t_hit" -> ExCSV(path, 15),
      "price" -> ExStr("${t_price} \\c[4]G\\c[0]"),
      "desc" -> ExStr("\\c[2]${t_desc}\\c[0]"),
      "misc" -> ExStr("攻撃${t_atk}, 命中率:${t_hit}, ${t_kind}武器"),
      "filename" -> ExStr("${itemno}-${name}")
    )).expand()
    val layout = LayoutUnit(
      AMap('size -> ASize(320, 80)),
      AreaMap.fromSeq(
        'win -> AreaUnit(AMap(
          'rect -> ARect(0, 0, 320, 80),
          'window -> AWindow("no-v/system6.png"),
          'front_color -> ASystemGraphics("no-v/system6.png"))),
        'name -> AreaUnit(AMap(
          'point -> APoint(0, 0)
        ) ++= stdstyle(style='bold, inWin=true)),
        'price -> AreaUnit(AMap(
          'rect -> ARect(160, 0, 150, 30),
          'align -> AAlign('right, 'top),
          'hemming -> AHemming(UColor.code("#001300"), 1)
        ) ++= stdstyle(inWin=true)),
        'desc -> AreaUnit(AMap(
          'point -> APoint(10, 18)
        ) ++= stdstyle(inWin=true)),
        'misc -> AreaUnit(AMap(
          'rect -> ARect(10, 40, 310, 40),
          'align -> AAlign('right, 'bottom)
        ) ++= stdstyle(size=12, inWin=true))
      )
    )
    val d = new Drawer(layout)
    vmap map { vm =>
      d.draw(vm, NullContext)
    } foreach {
      _.write(Resource.tempdir + "items/")
    }
  }

  def faceSpec() {
    val layout = LayoutUnit(
      AMap('size -> ASize(320, 240)),
      AreaMap.fromSeq(
        'a -> AreaUnit(AMap('point -> APoint(0, 0))),
        'b -> AreaUnit(AMap('point -> APoint(50, 0))),
        'c -> AreaUnit(AMap('point -> APoint(100, 0))),
        'd -> AreaUnit(AMap('point -> APoint(0, 50))),
        'e -> AreaUnit(AMap('point -> APoint(50, 50))),
        'f -> AreaUnit(AMap('point -> APoint(100, 50)))
      )
    )
    val rs = Resource.uri("no-v/ds1.png")
    val vm = Map(
      'a -> FaceGraphic(rs, 0, transparent = false),
      'b -> FaceGraphic(rs, 1, transparent = false),
      'c -> FaceGraphic(rs, 2, transparent = false),
      'd -> FaceGraphic(rs, 3, transparent = false),
      'e -> FaceGraphic(rs, 4, transparent = false),
      'f -> FaceGraphic(rs, 5, transparent = false),
      'filename -> Str("facetest")
    )
    val d = new Drawer(layout)
    d.draw(vm, NullContext).write(Resource.tempdir)
  }

  def charaSpec() {
    val layout = LayoutUnit(
      AMap('size -> ASize(320, 240)),
      AreaMap.fromSeq(
        (for(c <- 1 to 200) yield {
          (Symbol(c.toString): Key) -> AreaUnit(AMap('point -> APoint(r(320), r(240))))
        }): _*
      )
    )
    val rs = Resource.uri("no-v/sl1.png")
    val _vm = (for(c <- 1 to 200) yield {
      (Symbol(c.toString): Key) -> CharaGraphic(rs, CharaProperty(r(8), r(4), r(3)))
    }).toMap
    val vm: Map[Key, AValue] = _vm + ('filename -> Str("charatest"))
    val d = new Drawer(layout)
    d.draw(vm, NullContext).write(Resource.tempdir)
  }

  def battleSpec() {
    val layout = LayoutUnit(
      AMap('size -> ASize(320, 240)),
      AreaMap.fromSeq(
        (for(c <- 1 to 20) yield {
          (Symbol(c.toString): Key) -> AreaUnit(AMap('point -> APoint(r(320), r(240))))
        }): _*
      )
    )
    val rs = Resource.uri("no-v/bs1.png")
    val _vm = (for(c <- 1 to 20) yield {
      (Symbol(c.toString): Key) -> BattleGraphic(rs, r(15), transparent = true)
    }).toMap
    val vm: Map[Key, AValue] = _vm + ('filename -> Str("battletest"))
    val d = new Drawer(layout)
    d.draw(vm, NullContext).write(Resource.tempdir)
  }


  def testReuseLayout() {

    val repo = LayoutRepository.empty()

    val lay = LayoutUnit(
      AMap('size -> ASize(320, 240)),
      AreaMap.fromSeq(
      'name->AreaUnit(AMap('rect -> ARect(5,5,300,20),
                                'auto_expand -> AAutoExpand
                                ) ++= stdstyle()),
      'icon->AreaUnit(AMap('rect -> ARect(280,0,32,32)
                                ) ++= stdstyle()),
      'desc->AreaUnit(AMap('point -> ARect(0, 20, 12, 2),
                                'window -> AWindow("no-v/system6.png"),
                                'auto_expand -> AAutoExpand,
                                'front_color -> ASystemGraphics("no-v/system6.png")
                         ) ++= stdstyle(inWin = true)),
      'cost->AreaUnit(AMap('rect -> ARect(300,2,30,15),
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

    val _vs: List[ValueMap] = List(v1,v2,v3,v4,v5,v6)
    //val vs = List(v4)

    val vs = _vs.zipWithIndex.map { case (s, i) =>
      s + ('filename -> Str(f"skill$i%2d"))
    }

    //val expanded1: Array[ValueMap] = ValueExpander.expand(ex1)

    val d = new Drawer(lay)
    (vs).map{
      d.draw(_, NullContext)
    }.zipWithIndex.foreach { case (res, idx) =>
      res.write(Resource.tempdir)
    }
  }

  def testOldSpec() {

    val layout = LayoutUnit(
      AMap('size -> ASize(320,240)), //env
      AreaMap.fromSeq(  //layouts
        'icon1 -> AreaUnit(AMap('point->APoint(0,0))),
        'icon2 -> AreaUnit(AMap('point->APoint(50,0))),
        'icon3 -> AreaUnit(AMap('point->APoint(100,0))),
        'str -> AreaUnit(AMap(
          'rect->ARect(10,30,200,30),
          'align->AAlign('x_center, 'bottom),
          'border->ABorder,
          'font->AFont("Terminus-ja", 'plain, 12))),
        'str2->AreaUnit(AMap('rect->ARect(10,60,200,120),
          'font->AFont("Terminus-ja", 'plain, 12),
          'align->AAlign('x_center, 'y_center)))
      ))

    val valuemap = Map(
      'icon1 -> Icon("icon/icon1.png"),
      'icon2 -> Icon("icon/icon2.png"),
      'icon3 -> Icon("icon/icon3.png"),
      'str -> Str("test"),
      'str2 -> Str("a\nb\ncedf\ngiaasdasd\near"),
      'filename -> Str("test"))

    val d = new Drawer(layout)
    val result = d.draw(valuemap, NullContext)
    result.write(Resource.tempdir)
  }


}
