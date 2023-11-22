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

  def Eq(t1: Term, t2: Term) = Formula.Eq(t1, t2)
  def And(f1: Formula, f2: Formula) = Formula.And(f1, f2)
  def Or(f1: Formula, f2: Formula) = Formula.Or(f1, f2)
  def Not(f: Formula) = Formula.Not(f)
  def Fact(r: String, ts: js.Array[unify.Term]) = Formula.Fact(r, ts.toList)
  def RelApp(r: String, ts: js.Array[unify.Term]) = Formula.RelApp(r, ts.toList)

  val varCounts = scala.collection.mutable.Map.empty[String, Int]
  def freshVar(name: String): String = {
    varCounts.get(name) match {
      case Some(i) => {
        varCounts(name) = i + 1
        s"$name$i"
      }
      case None => {
        varCounts(name) = 0
        name
      }
    }
  }
  def Relation(argNames: js.Array[String], formula: unify.Formula) = {
    val args = argNames.toList
    val newArgs = args.map(freshVar)
    val argSubsts = args.map(Term.Var(_)).zip(newArgs.map(Term.Var(_))).toMap[Term, Term]
    val freeVars = (formula.freeVars -- args.toSet).toList
    val newFreeVars = freeVars.map(freshVar)
    val freeVarSubsts = freeVars.map(Term.Var(_)).zip(newFreeVars.map(Term.Var(_))).toMap[Term, Term]
    unify.Relation(newArgs, formula.withSubsts(argSubsts).withSubsts(freeVarSubsts))
  }

  def solve(
    formula: Formula, facts: js.Array[Formula.Fact], relations: js.Map[String, unify.Relation]
  ): SolveResult = {
    varCounts.clear()
    given factsList: Set[Formula.Fact] = facts.toList.toSet
    given relationsList: Map[String, Relation] = relations.toList.toMap
    // println(s"f: $formula")
    // println(s"facts: $factsList")
    // println(s"relations: $relationsList")
    try {
      val result = formula.solve
      SolveResult(result)
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
        case Some(r) => r.toMap.toString
        case None => "false."
      }
    }
  }
}
