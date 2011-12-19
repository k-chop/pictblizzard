package com.github.chuwb.pictbliz

import javax.swing._
import java.awt._
import java.awt.font._
import java.awt.image._
import javax.imageio._
import java.io._

object Test {

  val x = 0
  val y = 0
  
  def main(args: Array[String]) = {
    import ScriptOps._
    import ScriptOps.implicits._
    
    val text = "backhearts1234456778906[]!?"
    val pros = new Font("ProFontWindows", Font.PLAIN, 9)
    val msgo = new Font("ＭＳ ゴシック", Font.PLAIN, 13)

    val layout = LayoutUnit(
      Map('size -> APoint(320,240)), //env
      Map(  //laouts
      'icon1 -> AreaUnit(Map('point->APoint(0,0))),
      'icon2 -> AreaUnit(Map('point->APoint(50,0))),
      'icon3 -> AreaUnit(Map('point->APoint(100,0))),
      'str -> AreaUnit(Map(
        'rect->ARect(10,30,200,30),
                       'align->AAlign('x_center, 'y_center),
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
    
    val d = new Drawer(valuemap, layout ,NullContext)
    d.draw()
    d.write("./test.png")

    ()
  }  
}
