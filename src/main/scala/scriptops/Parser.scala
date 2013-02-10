package com.github.whelmaze.pictbliz.scriptops

import com.github.whelmaze.pictbliz
import pictbliz.{UColor, Resource, ValueExpander, Drawer}
import pictbliz.scriptops.Attrs._

import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator._
import scala.collection.mutable

import util.Try
import java.net.URI
import java.io.File

trait ParserUtil {
  class Will[T](ex: => T) {
    def doit: T = ex
  }
  object Will {
    def apply[T](ex : => T) = new Will[T](ex)
  }

  def validateFontStyle(s: String): Symbol = s match {
    case "plain" => 'plain
    case "bold" => 'bold
    case "italic" => 'italic
    case "bolditalic" => 'bolditalic
    case _ => 'plain // 不明な指定は全部標準書体に置き換える
  }

  private[scriptops] implicit class StringConv(s: String) {
    def toURI: URI = (new File(s)).toURI
  }

}

object Parser extends StandardTokenParsers with ParserUtil {
  lexical.delimiters ++= List("(",")","{","}","+","-","*","/","=","$",".",",","@",":","..")
  lexical.reserved += ("val", "layout", "values", "value", "with", "generate")

  val layouts = mutable.Map.empty[String, LayoutUnit]
  val valuemaps = mutable.Map.empty[String, ExValueMap]

  lazy val all = rep1(top)
  lazy val top = bind | gen
  lazy val bind = "val" ~> ident ~ "=" ~ exp ^^ {
    case i ~ _ ~ ex => ex match {
      case lay: LayoutUnit => layouts += (i -> lay)
      case vva: ExValueMap => valuemaps += (i -> vva)
    }
  }

  lazy val gen: Parser[Will[Unit]] = "generate" ~> ident ~ "with" ~ ident ^^ {
    case lays ~ _ ~ vs => Will{
      val d = new Drawer(layouts(lays))
      val e = new ValueExpander(valuemaps(vs))
      var n: Option[String] = None
      e.iterator.map { vm =>
        n = Option( vm.get('filename) match { case Some(Str(s)) => s case _ => "" } )
        d.draw(vm, NullContext)
      }.zipWithIndex.foreach { case (res, idx) =>
        val name = Resource.tempdir + "sc/" + (n getOrElse idx) + ".png"
        println(name + " output.")
        res.write(name)
      }
    }
  }

  lazy val exp = evaluable | layoutOne | layoutDef | valuesOne | valuesDef
  lazy val evaluable: Parser[AnyValue] = range | literal

  lazy val range: Parser[ExRange] = numericLit ~ ".." ~ numericLit ^^ {
    case begin ~ _ ~ end => ExRange((begin.toInt to end.toInt).toArray)
  }

  lazy val literal: Parser[AnyValue] = {
    numericLit ^^ { case n => Number(n.toInt) } |
    stringLit ^^ {
      case s => ExStr(s.replace("\\n", "\n"))
    }
  }
//  lazy val values = value | "{" ~> repsep(value, ",") <~ "}" ^^ { case a => s"""{ ${a.mkString(",")} }"""}
  lazy val layoutOne: Parser[(Key, AreaUnit)] = {
    ident ~ ":" ~ layoutApplys ^^ { case i ~ _ ~ a => Symbol(i) -> AreaUnit(a.toMap)}
  }

  lazy val layoutApply: Parser[(Key, Attr)] = ident ~ "(" ~ repsep(evaluable, ",") <~ ")" ^^ {
    case i ~ _ ~ seq => Symbol(i) -> fetchAttr(i, seq)
  } | ident ^^ { case i => Symbol(i) -> fetchAttr(i, Nil) }

  lazy val layoutApplys: Parser[Seq[(Key, Attr)]] = {
    layoutApply ^^ { case a => List(a) } |
    "{" ~> repsep(layoutApply, ",") <~ "}" ^^ { case a => a }
  }

  lazy val symbol = ":" ~> ident ^^ { case i => s":$i"}

  // TODO: envの指定
  lazy val layoutDef: Parser[LayoutUnit] = "layout" ~> "(" ~> rep1(layoutApply) ~ ")" ~ "{" ~ rep1(layoutOne) <~ "}" ^^ {
    case env ~ _ ~ _ ~ as => LayoutUnit(AttrMap(env: _*), AreaMap.fromSeq(as: _*))
  }

  lazy val valuesDef: Parser[ExValueMap] = "values" ~> "{" ~> rep1(valuesOne) <~ "}" ^^ { case s => s.toMap }

  lazy val valuesOne: Parser[(String, AnyValue)] = {
    ident ~ ":" ~ evaluable ^^ { case i ~ _ ~ ev => (i, ev) } |
    ident ~ ":" ~ valuesApply ^^ { case i ~ _ ~ va => (i, va) }
  }

  lazy val valuesApply: Parser[AnyValue] = ident ~ "(" ~ repsep(evaluable, ",") <~ ")" ^^ {
    case i ~ _ ~ seq => fetchValue(i, seq)
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

  def parse(source: String) = {
    all(new lexical.Scanner(source)) match {
      case Success(strs, _) =>
        println(strs.mkString) // => とりあえず文字列で出力
        strs.foreach{
          case w: Will[_] => w.doit // 実行
          case _ =>
        }

      case Failure(msg, d) => println(msg); println(d.pos.longString); sys.error("")
      case Error(msg, _) => println(msg); sys.error("")
    }
  }

  private[scriptops] implicit class AnyValueInterpreter(val a: AnyValue) extends AnyVal {
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

}

