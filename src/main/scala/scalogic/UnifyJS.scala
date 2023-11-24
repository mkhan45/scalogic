package scalogic.js

import scala.scalajs.js
import scala.scalajs.js.annotation.*

import scalogic.reunify
import scalogic.reunify.{Term, Formula, Relation, eval, resolve}

@JSExportTopLevel("UnifyJS")
@JSExportAll
object JSExports {
  def Const(i: Int) = reunify.Term.Const(i)
  def Var(name: String) = reunify.Term.Var(name)
  def Tuple(ts: js.Array[reunify.Term]) = reunify.Term.Tuple(ts.toList)

  def Eq(t1: Term, t2: Term) = Formula.Eq(t1, t2)
  def And(f1: Formula, f2: Formula) = Formula.And(f1, f2)
  def Or(f1: Formula, f2: Formula) = Formula.Or(f1, f2)
  def Not(f: Formula) = Formula.Not(f)
  def Fact(r: String, ts: js.Array[reunify.Term]) = Formula.Fact(r, ts.toList)
  def RelApp(r: String, ts: js.Array[reunify.Term]) = Formula.RelApp(r, ts.toList)

  def freshVar(name: String) = reunify.FreshVar.freshVar(name)

  def Relation(argNames: js.Array[String], formula: reunify.Formula) = {
    val args = argNames.toList
    val newArgs = args.map(freshVar)
    val argSubsts = args.map(Term.Var(_)).zip(newArgs.map(Term.Var(_))).toMap[Term, Term]
    val freeVars = (formula.freeVars -- args.toSet).toList
    val newFreeVars = freeVars.map(freshVar)
    val freeVarSubsts = freeVars.map(Term.Var(_)).zip(newFreeVars.map(Term.Var(_))).toMap[Term, Term]
    reunify.Relation(newArgs, formula.withSubsts(argSubsts).withSubsts(freeVarSubsts))
  }

  def solve(
    formula: Formula, facts: js.Array[Formula.Fact], relations: js.Map[String, reunify.Relation]
  ): SolveResult = {
    reunify.FreshVar.varCounts.clear()
    given factsList: Set[Formula.Fact] = facts.toList.toSet
    given relationsList: Map[String, Relation] = relations.toList.toMap
    // println(s"f: $formula")
    // println(s"facts: $factsList")
    // println(s"relations: $relationsList")
    try {
      val result = formula.solve.map(_.resolve).flatten
      val fvs = formula.freeVars
      val fvSubsts = result.map(r => fvs.map(v => (Term.Var(v), r.eval(Term.Var(v)))).toMap)
      SolveResult(fvSubsts)
    } catch {
      case th: Throwable => {
        th.printStackTrace()
        throw th
      }
    }
  }

  def termToString(term: Term): String = term.toString

  def termToJS(term: Term): js.Any = term match {
    case Term.Const(x) => x
    case Term.Var(s) => s
    case Term.Tuple(xs) => js.Array(xs.map(termToJS))
  }

  class SolveResult(result: Option[reunify.Substs]) extends js.Object {
    def getVar(x: String): js.Any = {
      val xv = result.map(r => r.eval(Term.Var(x)))
      xv match {
        case Some(v) => termToJS(v)
        case None => x
      }
    }

    def length(): Int = {
      result match {
        case Some(m) => m.size
        case None => 1
      }
    }

    def display(): String = {
      result match {
        case Some(m) if m.isEmpty => "true."
        case Some(r) => r.toMap.toString
        case None => "false."
      }
    }
  }
}
