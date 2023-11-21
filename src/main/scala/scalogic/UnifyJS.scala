package scalogic.js

import scala.scalajs.js
import scala.scalajs.js.annotation.*

import scalogic.unify
import scalogic.unify.{Term, Formula, Relation, fullEval}

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

  def Relation(argNames: js.Array[String], formula: unify.Formula) = unify.Relation(argNames.toList, formula)

  def solve(
    formula: Formula, facts: js.Array[Formula.Fact], relations: js.Map[String, Relation]
  ): SolveResult = {
    given factsList: Set[Formula.Fact] = facts.toList.toSet
    given relationsList: Map[String, Relation] = relations.toList.toMap
    println(s"f: $formula")
    println(s"facts: $factsList")
    println(s"relations: $relationsList")
    val result = formula.solve
    println(s"result: $result")
    SolveResult(result)
  }

  def termToString(term: Term): String = term.toString

  def termToJS(term: Term): js.Any = term match {
    case Term.Const(x) => x
    case Term.Var(s) => s
    case Term.Tuple(xs) => js.Array(xs.map(termToJS))
  }

  class SolveResult(result: Option[unify.Substs]) extends js.Object {
    def getVar(x: String): js.Any = {
      val xv = result.map(r => r.fullEval(Term.Var(x)))
      xv match {
        case Some(v) => termToJS(v)
        case None => x
      }
    }

    def display(): String = {
      result match {
        case Some(m) if m.isEmpty => "true."
        case Some(r) => r.toString
        case None => "false."
      }
    }
  }
}
