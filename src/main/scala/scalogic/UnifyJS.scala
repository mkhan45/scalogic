package scalogic.js

import scala.scalajs.js
import scala.scalajs.js.annotation.*

import scalogic.unify

@JSExportTopLevel("UnifyJS")
@JSExportAll
object JSExports {
  def Const(i: Int) = unify.Term.Const(i)
  def Var(name: String) = unify.Term.Var(name)
  def Tuple(ts: js.Array[unify.Term]) = unify.Term.Tuple(ts.toList)

  def Eq(t1: unify.Term, t2: unify.Term) = unify.Formula.Eq(t1, t2)
  def And(f1: unify.Formula, f2: unify.Formula) = unify.Formula.And(f1, f2)
  def Or(f1: unify.Formula, f2: unify.Formula) = unify.Formula.Or(f1, f2)
  def Not(f: unify.Formula) = unify.Formula.Not(f)
  def Fact(r: String, ts: js.Array[unify.Term]) = unify.Formula.Fact(r, ts.toList)
  def RelApp(r: String, ts: js.Array[unify.Term]) = unify.Formula.RelApp(r, ts.toList)
}
