package com.github.whelmaze.pictbliz.scriptops

import NodeDefinition._
import Attrs._
import com.github.whelmaze.pictbliz.{UColor, Resource}
import collection.mutable


class Environment {

}

object BuilderFromAST {

  val vtable = mutable.HashMap.empty[String, Node]

  // ASTからプログラム内部で使う形に変換
  def build(n: List[Node]) = {
    // 変数をテーブルに登録
    n collect {
      case BindExpr(name, expr) => vtable += name -> expr
    }

  }

  def fetchValue(name: String, args: Seq[AnyValue]): AnyValue = {
    val len = args.size
    name match {
      case "icon" if len == 1 => // ExIconのときはどうすんの
        Icon(Resource.uri(args(0).toStr))
      case _ => NullValue
    }
  }

  def fetchAttr(name: String, args: Seq[AnyValue]): Attr = {
    val len = args.size
    name match {
      case "rect"  if len == 4 =>
        ARect(args(0).toInt, args(1).toInt, args(2).toInt, args(3).toInt)
      case "point" if len == 2 =>
        APoint(args(0).toInt, args(1).toInt)
      case "size" if len == 2 =>
        ASize(args(0).toInt, args(1).toInt)
      case "align" if len == 1 || len == 2 =>
        AAlign(Symbol(args(0).toStr), Symbol(args(1).toStr))
      case "window" if len == 1 =>
        AWindow(args(0).toStr)
      case "front_color" if len == 1 =>
        ASystemGraphics(args(0).toStr)
      case "hemming" if len == 2 =>
        AHemming(UColor.code(args(0).toStr), args(1).toInt)
      case "auto_expand" =>
        AAutoExpand
      case "on_center" =>
        AOnCenter
      case "interval" if len == 2 =>
        AInterval(args(0).toInt, args(1).toInt)
      case "padding" if len == 2 =>
        APadding(args(0).toInt, args(1).toInt)
      case "font" if len == 3 =>
        AFont(args(0).toStr, validateFontStyle(args(1).toStr), args(2).toInt)
      case _ => ANil
    }
  }

  private[scriptops] implicit class AnyValueInterpreter(val a: AnyValue) extends AnyVal {
    // 変換不能な値はログで出力しといた方がよさげ
    def toInt: Int = a match {
      case Number(n) => n
      case _ => 0
    }
    def toStr: String = a match {
      case Str(s) => s.replace("\\n", "\n")
      case ExStr(s) => s.replace("\\n", "\n")
      case _ => ""
    }
  }

  def validateFontStyle(s: String): Symbol = s match {
    case "plain" => 'plain
    case "bold" => 'bold
    case "italic" => 'italic
    case "bolditalic" => 'bolditalic
    case _ => 'plain // 不明な指定は全部標準書体に置き換える
  }
}
