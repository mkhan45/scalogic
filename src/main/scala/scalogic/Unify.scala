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
    case (Tuple(ts1), Tuple(ts2)) if ts1.length == ts2.length => {
      val substs = ts1.zip(ts2).map({ case (x, y) => unify(x, y) })
      if (substs.contains(None)) { 
        None 
      } else {
        val flattened = substs.flatten
        if flattened.isEmpty then Some(Map.empty)
        else Some(flattened.reduceLeft(_ ++ _))
      }
    }
    case _ => None
}

type Substs = Map[Term, Term]

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
        substs <- conjunct(args.zip(fargs).map({ case (x, y) => Eq(x, y) })*).solve
      } yield substs
      validSubsts.headOption
    }
    case RelApp(name, args) => {
      val Relation(argNames, body) = relations(name)
      // val newBody = conjunct((assigns :+ body): _*)
      // println(s"body: $body, newBody: $newBody")
      val substs = argNames.map(Term.Var(_)).zip(args).toMap[Term, Term]
      val newBody = body.withSubsts(substs)
      val newBindings = newBody.solve

      newBindings.map { bindings =>
        args.map(a => (a, a.withSubsts(bindings))).filter({ case (a, b) => a != b }).toMap
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
