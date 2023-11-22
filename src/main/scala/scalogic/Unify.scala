package scalogic.unify

enum Term {
  case Const(i: Int)
  case Var(name: String)
  case Tuple(ts: List[Term])

  def withSubst(pat: Term, replacement: Term): Term = this match {
    case `pat` => replacement
    case Tuple(ts) => Tuple(ts.map(_.withSubst(pat, replacement)))
    case _ => this
  }

  def withSubsts(substs: Substs): Term = substs.foldLeft(this) {
    case (t, (pat, replacement)) => t.withSubst(pat, replacement)
  }

  def occursIn(t: Term): Boolean = t match {
    case _ if t == this => true
    case Tuple(ts) => ts.exists(occursIn)
    case _ => false
  }

  def freeVars: Set[String] = this match {
    case Const(i) => Set.empty
    case Var(name) => Set(name)
    case Tuple(ts) => ts.flatMap(_.freeVars).toSet
  }

  override def toString: String = this match {
    case Const(i) => i.toString
    case Var(name) => name
    case Tuple(ts) => s"(${ts.mkString(", ")})"
  }
}
import Term._

def unify(t1: Term, t2: Term): Option[Substs] = (t1, t2) match {
    case (t1: Const, t2: Const) if t1 == t2 => Some(Map.empty)
    case (t1: Const, t2: Const) if t1 == t2 => Some(Map.empty)
    case (t1: Const, t2: Const) => None
    case (t1: Var, t2: Var) if t1 == t2 => Some(Map.empty)
    case (t1: Var, t2) => Some(Map(t1 -> t2))
    case (t1, t2: Var) => Some(Map(t2 -> t1))
    // case _ if t1 != t2 && t1.occursIn(t2) || t2.occursIn(t1) => None // TODO
    case (Tuple(ts1), Tuple(ts2)) if ts1.length == ts2.length => {
      ts1.zip(ts2).foldLeft(Some(Map.empty): Option[Substs]) { case (substAcc, (l, r)) =>
        for {
          substs <- substAcc
          newSubsts <- unify(l.withSubsts(substs), r.withSubsts(substs))
        } yield {
          substs ++ newSubsts
        }
      }
    }
    case _ => None
}

type Substs = Map[Term, Term]

extension (s: Substs) {
  def fullEval(v: Term): Term = s.get(v) match {
    case None => v
    case Some(Tuple(ts)) => Tuple(ts.map(s.fullEval))
    case Some(t) => s.fullEval(t)
  }
}

case class Relation(argNames: List[String], body: Formula)

def conjunct(fs: Formula*): Formula = fs.reduceLeft(Formula.And(_, _))
def disjunct(fs: Formula*): Formula = fs.reduceLeft(Formula.Or(_, _))

enum Formula {
  case Eq(t1: Term, t2: Term)
  case And(f1: Formula, f2: Formula)
  case Or(f1: Formula, f2: Formula)
  case Not(f: Formula)
  case Fact(name: String, args: List[Term])
  case RelApp(name: String, args: List[Term])

  def freeVars: Set[String] = this match {
    case Eq(t1, t2) => t1.freeVars ++ t2.freeVars
    case And(f1, f2) => f1.freeVars ++ f2.freeVars
    case Or(f1, f2) => f1.freeVars ++ f2.freeVars
    case Not(f) => f.freeVars
    case Fact(name, args) => args.flatMap(_.freeVars).toSet
    case RelApp(name, args) => args.flatMap(_.freeVars).toSet
  }

  def withSubst(pat: Term, replacement: Term): Formula = this match {
    case Eq(t1, t2) => Eq(t1.withSubst(pat, replacement), t2.withSubst(pat, replacement))
    case And(f1, f2) => And(f1.withSubst(pat, replacement), f2.withSubst(pat, replacement))
    case Or(f1, f2) => Or(f1.withSubst(pat, replacement), f2.withSubst(pat, replacement))
    case Not(f) => Not(f.withSubst(pat, replacement))
    case Fact(name, args) => Fact(name, args.map(_.withSubst(pat, replacement)))
    case RelApp(name, args) => RelApp(name, args.map(_.withSubst(pat, replacement)))
  }

  def withSubsts(substs: Substs): Formula = substs.foldLeft(this) {
    case (f, (pat, replacement)) => f.withSubst(pat, replacement)
  }

  def solve(using facts: Set[Fact], relations: Map[String, Relation]): Option[Substs] = this match {
    case Eq(t1, t2) => unify(t1, t2)
    case And(f1, f2) => for {
      substs1 <- f1.solve
      substs2 <- f2.withSubsts(substs1).solve
    } yield substs1 ++ substs2
    case Or(f1, f2) => f1.solve.orElse(f2.solve)
    case Not(f) => if f.solve.isEmpty then Some(Map.empty) else None // doesn't work
    case Fact(name, args) => {
      val validSubsts = for {
        fact <- facts.filter(_.name == name).filter(_.args.length == args.length)
        fargs = fact.args
        term = conjunct(args.zip(fargs).map({ case (x, y) => Eq(x, y) })*)
        substs <- term.solve
      } yield substs
      validSubsts.headOption
    }
    case RelApp(name, args) => {
      val Relation(argNames, body) = relations(name)
      val fstAssigns: List[Formula] = argNames.zip(args).map({ 
        case (name: String, arg: Term) => Eq(Term.Var(name), arg)
      })
      val term = conjunct(fstAssigns :+ body: _*)

      val newRel = {
        val vs = body.freeVars.toList
        val nvs = vs.map(v => Term.Var(v + "'"))
        val substs = vs.map(Term.Var(_)).zip(nvs).toMap
        val nargs = argNames.map(_ + "'")
        val newBody = body.withSubsts(substs)
        Relation(nargs, newBody)
      }

      val cs = term.solve(using facts, relations + (name -> newRel))
      cs.map { cs =>
        // println(s"cs: $cs")
        val vs = args.map(_.freeVars).reduceLeft(_ ++ _).map(Term.Var(_))
        cs.filter({ case (k, v) => vs.contains(k) }).map({case (k, v) => (k, cs.fullEval(k)) })
      }
    }
  }

  override def toString: String = this match {
    case Formula.Eq(t1, t2) => s"$t1 == $t2"
    case Formula.And(f1, f2) => s"($f1 && $f2)"
    case Formula.Or(f1, f2) => s"($f1 || $f2)"
    case Formula.Not(f) => s"!($f)"
    case Formula.Fact(name, args) => s"$name(${args.mkString(", ")})"
    case Formula.RelApp(name, args) => s"$name(${args.mkString(", ")})"
  }
}
