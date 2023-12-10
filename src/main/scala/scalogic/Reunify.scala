package scalogic.reunify

enum Term {
  case Const(i: Int)
  case Var(name: String)
  case Tuple(ts: List[Term])

  def contains(t: Term): Boolean = t match
    case _ if t == this => true
    case Tuple(ts) => ts.exists(contains)
    case _ => false

  def freeVars: Set[String] = this match
    case Const(i) => Set.empty
    case Var(name) => Set(name)
    case Tuple(ts) => ts.flatMap(_.freeVars).toSet

  def withSubst(pat: Term, replacement: Term): Term = this match
    case `pat` => replacement
    case Tuple(ts) => Tuple(ts.map(_.withSubst(pat, replacement)))
    case _ => this

  def withSubsts(substs: Substs): Term = substs.foldLeft(this) {
    case (t, (pat, replacement)) => t.withSubst(pat, replacement)
  }

  override def toString: String = this match
    case Const(i) => i.toString
    case Var(name) => name
    case Tuple(ts) => s"(${ts.mkString(", ")})"
}
import Term._

type Substs = Map[Term, Term]
type PartialSubsts = List[(Term, Term) | Formula.Or]

extension (ps: PartialSubsts) {
  def resolve(using facts: Set[Fact], relations: Map[String, Relation]): Option[Substs] = resolveLoop(Map.empty)

  private def resolveLoop
    (substs: Substs)
    (using facts: Set[Fact], relations: Map[String, Relation]): Option[Substs] 
  = ps match
    case Nil => Some(substs)
    case (t1: Term, t2: Term) :: tail => for {
      newSubsts <- unify(t1, t2, substs)
      finalSubsts <- tail.resolveLoop(newSubsts)
    } yield finalSubsts
    case Or(l, r) :: tail =>
      // not sure how to interleave
      val s1 = l.solve
      val s2 = r.solve
      lazy val lp = s1.flatMap(s => (s ++ tail).resolveLoop(substs))
      lazy val rp = s2.flatMap(s => (s ++ tail).resolveLoop(substs))
      lp.orElse(rp)
}

extension (substs: Substs) {
  def eval(t: Term): Term = t match
    case Tuple(ts) => Tuple(ts.map(eval))
    case _ => substs.get(t).map(eval).getOrElse(t)
}

import Formula.*

def unify(t1: Term, t2: Term, s: Substs): Option[Substs] = (s.eval(t1), s.eval(t2)) match
  case (t1: Const, t2: Const) if t1 == t2 => Some(s)
  case (t1: Const, t2: Const) /* t1 != t2 */ => None
  case (t1: Var, t2: Var) if t1 == t2 => Some(s)
  case (t1: Var, t2) => Some(s + (t1 -> t2))
  case (t1, t2: Var) => Some(s + (t2 -> t1))
  case (t1: Term, t2: Term) if t1 == t2 => Some(s)
  case (t1: Term, t2: Term) if t1.contains(t2) || t2.contains(t1) => None
  case (Tuple(ts1), Tuple(ts2)) if ts1.length == ts2.length =>
    ts1.zip(ts2).foldLeft(Some(s): Option[Substs]) { case (substAcc, (l, r)) =>
      for {
        substs <- substAcc
        newSubsts <- unify(l, r, substs)
      } yield {
        substs ++ newSubsts
      }
    }
  case _ => None
end unify

case class Relation(argNames: List[String], body: Formula)

def conjunct(fs: Formula*): Formula.And = fs.length match
  case n if n >= 2 => fs.reduceRight(Formula.And(_, _)).asInstanceOf[Formula.And]
  case 1 => Formula.And(fs.head, fs.head)
  case 0 => ???

def disjunct(fs: Formula*): Formula.Or = fs.length match
  case n if n >= 2 => fs.reduceRight(Formula.Or(_, _)).asInstanceOf[Formula.Or]
  case 1 => Formula.Or(fs.head, fs.head)
  case 0 => ???

object FreshVar {
  val varCounts = scala.collection.mutable.Map.empty[String, Int]
  def freshVar(name: String): String = varCounts.get(name) match
    case Some(i) =>
      varCounts(name) = i + 1
      s"$name$i"
    case None =>
      varCounts(name) = 0
      name
}

enum Formula {
  case Eq(t1: Term, t2: Term)
  case And(f1: Formula, f2: Formula)
  case Or(f1: Formula, f2: Formula)
  case Not(f: Formula)
  case Fact(name: String, args: List[Term])
  case RelApp(name: String, args: List[Term])

  def freeVars: Set[String] = this match
    case Eq(t1, t2) => t1.freeVars ++ t2.freeVars
    case And(f1, f2) => f1.freeVars ++ f2.freeVars
    case Or(f1, f2) => f1.freeVars ++ f2.freeVars
    case Not(f) => f.freeVars
    case Fact(name, args) => args.flatMap(_.freeVars).toSet
    case RelApp(name, args) => args.flatMap(_.freeVars).toSet

  override def toString: String = this match
    case Eq(t1, t2) => s"$t1 == $t2"
    case And(f1, f2) => s"($f1 && $f2)"
    case Or(f1, f2) => s"($f1 || $f2)"
    case Not(f) => s"!($f)"
    case Fact(name, args) => s"$name(${args.mkString(", ")})"
    case RelApp(name, args) => s"$name(${args.mkString(", ")})"

  def withSubst(pat: Term, replacement: Term): Formula = this match
    case Eq(t1, t2) => Eq(t1.withSubst(pat, replacement), t2.withSubst(pat, replacement))
    case And(f1, f2) => And(f1.withSubst(pat, replacement), f2.withSubst(pat, replacement))
    case Or(f1, f2) => Or(f1.withSubst(pat, replacement), f2.withSubst(pat, replacement))
    case Not(f) => Not(f.withSubst(pat, replacement))
    case Fact(name, args) => Fact(name, args.map(_.withSubst(pat, replacement)))
    case RelApp(name, args) => RelApp(name, args.map(_.withSubst(pat, replacement)))

  def withSubsts(substs: Substs): Formula = substs.foldLeft(this) {
    case (f, (pat, replacement)) => f.withSubst(pat, replacement)
  }

  def solve(using facts: Set[Fact], relations: Map[String, Relation]): Option[PartialSubsts] = this match {
    case Eq(t1, t2) => unify(t1, t2, Map.empty).map(_.toList)
    case And(f1, f2) => for {
      substs1 <- f1.solve
      substs2 <- f2.solve
    } yield substs1 ++ substs2
    case Or(f1, f2) => Some(List(Or(f1, f2))) // would swapping these cause interleaving?
    case Not(f) => ???
    case Fact(name, args) =>
      val validSubsts = for {
        fact <- facts.filter(_.name == name).filter(_.args.length == args.length)
        fargs = fact.args
        term = conjunct(args.zip(fargs).map({ case (x, y) => Eq(x, y) })*)
      } yield term

      if (validSubsts.isEmpty) None
      else Some(List(disjunct(validSubsts.toSeq: _*)))
    case RelApp(name, args) =>
      val Relation(argNames, body) = relations(name)
      val vs = (body.freeVars -- argNames.toSet).toList
      val nvs = vs.map(FreshVar.freshVar)
      val vsubsts = vs.map(Term.Var(_)).zip(nvs.map(Term.Var(_))).toMap

      val nargs = argNames.map(FreshVar.freshVar)
      val argSubsts = argNames.map(Term.Var(_)).zip(nargs.map(Term.Var(_))).toMap

      val newBody = body.withSubsts(vsubsts).withSubsts(argSubsts)

      val cs = newBody.solve
      val as = nargs.zip(args).map({ case (name, arg) => (Term.Var(name), arg) })
      cs.map(as ++ _)
  }
}
