package scalogic.unify

case class Relation(name: String, args: List[String], body: Formula) {
  def withSubst(v: String, `for`: Term): Relation = {
    println(s"Relation withSubst($v, ${`for`})($this)")
    this match {
      case Relation(name, args, body) => Relation(name, args, body.withSubst(v, `for`))
    }
  }
}

object Relation {
  def apply(name: String, args: List[String])(body: Relation => Formula): Relation = {
    Relation(name, args, body(Relation(s"_rec_$name", args, null)))
  }
}

case class Fact(name: String) {
  // def apply(args: Term*): Formula = Formula.RelApp(Relation(name, this.args, null), args.toList)
}

enum Term {
  case Var(name: String)
  case Const(v: Int)
  case FactApp(fact: Fact, args: List[Term])

  def withSubst(v: String, `for`: Term): Term = this match {
    case Var(name) if name == v => `for`
    case FactApp(fact, args) => FactApp(fact, args.map(_.withSubst(v, `for`)))
    case _ => this
  }
}

type Substs = Map[String, Term]
extension (substs: Substs) {
  def apply(v: String): Term = substs(v) match {
    case Term.Var(v2) => substs(v2)
    case t => t
  }
}

enum Formula {
  case True
  case False
  case Eq(t1: Term, t2: Term)
  case And(f1: Formula, f2: Formula)
  case Or(f1: Formula, f2: Formula)
  case Not(f: Formula)
  case RelApp(rel: Relation, args: List[Term])

  def withSubst(v: String, `for`: Term): Formula = {
    println(s"Formula withSubst($v, ${`for`})($this)")
    this match {
    case True => True
    case False => False
    case Eq(t1, t2) => Eq(t1.withSubst(v, `for`), t2.withSubst(v, `for`))
    case And(f1, f2) => And(f1.withSubst(v, `for`), f2.withSubst(v, `for`))
    case Or(f1, f2) => Or(f1.withSubst(v, `for`), f2.withSubst(v, `for`))
    case Not(f) => Not(f.withSubst(v, `for`))
    case RelApp(rel, args) => {
      RelApp(rel, args.map(_.withSubst(v, `for`)))
    }
    }
  }

  def withSubsts(substs: Substs): Formula = substs.foldLeft(this) { case (f, (v, t)) => f.withSubst(v, t) }

  def solve: Option[Substs] = {
    println(s"solve($this)")
    this match {
    case True => Some(Map.empty)
    case False => None
    case Eq(t1, t2) => unify(t1, t2)
    case And(f1, f2) => for {
      s1 <- f1.solve
      s2 <- f2.withSubsts(s1).solve
    } yield s1 ++ s2
    case Or(f1, f2) => f1.solve.orElse(f2.solve)
    case Not(f) => f.solve.map(_ => Map.empty)
    case RelApp(rel, args) => {
      print(s"Rel Subst: $rel")
      rel.body.withSubsts(rel.args.zip(args).toMap).solve
    }
    }
  }
}

def unify(t1: Term, t2: Term): Option[Substs] = {
  println(s"unify($t1, $t2)")
  (t1, t2) match {
  case (Term.Const(v1), Term.Const(v2)) if v1 == v2 => Some(Map.empty)
  case (Term.Var(v1), Term.Var(v2)) if v1 == v2 => Some(Map.empty)
  case (Term.Var(v1), t2) => Some(Map(v1 -> t2))
  case (t1, Term.Var(v2)) => Some(Map(v2 -> t1))
  case _ => None
  }
}

object Syntax {
  extension (f: Formula) {
    def &&(g: Formula): Formula = Formula.And(f, g)
    def ||(g: Formula): Formula = Formula.Or(f, g)
    def unary_! : Formula = Formula.Not(f)
  }

  extension (t: Term) {
    def ===(u: Term): Formula = Formula.Eq(t, u)
  }

  extension (rel: Relation) {
    def apply(args: Term*): Formula = Formula.RelApp(rel, args.toList)
  }
}
