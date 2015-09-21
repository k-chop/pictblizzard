package pictbliz.scriptops

/**
 * Parser内で使うASTを表すcase class群を定義するobject.
 * 使う時は import scriptops.NodeDefinition._ で.
 */
object NodeDefinition {

  // ASTの基底クラス
  abstract class Node

  // 値を表すAST
  sealed trait NodeValue extends Node
  // 式を表すAST
  sealed trait NodeExpr extends Node

  case class NumberValue(v: Int) extends NodeValue

  case class StrValue(v: String) extends NodeValue

  case class RangeValue(start: Int, end: Int) extends NodeValue

  case class NumberSeqValue(v: Seq[Int]) extends NodeValue

  case class LayoutApply(name: String, args: List[Node]) extends NodeValue

  case class LayoutDef(env: List[Node], body: List[Node]) extends NodeExpr

  case class ValuesApply(name: String, args: List[Node]) extends NodeValue

  case class ValuesDef(body: List[Node]) extends NodeExpr

  // 紐付けられた名前, 評価するとNodeになる.
  case class Var(name: String) extends NodeExpr

  /** nameにexprを紐付ける. 関連付ける際にexprは評価する.
    * IdentNativeにはBindできない.
    * @param name 紐付ける名前
    * @param expr 紐付けられたNode
    */
  case class BindExpr(name: String, expr: Node) extends NodeExpr

  /** layoutでvaluesを生成する. NodeExprの評価結果がIdent, またはLayout,Valueになっていれば良い.
    * と思ったけど直接書くのはちょっとないので変数だけ受け付ける
    */
  case class GenerateExpr(layout: Var, values: List[Var]) extends NodeExpr

  // layoutの1つ, 評価結果がlayoutになれば良い
  case class LayoutOne(name: String, defs: List[Node]) extends NodeExpr
  case class LayoutOnes(ls: List[Node]) extends NodeExpr

  case class ValuesOne(name: String, defs: Node) extends NodeExpr
  case class ValuesOnes(ls: List[Node]) extends NodeExpr
}
