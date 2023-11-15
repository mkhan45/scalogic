package scalogic.unifyunion

type Term = Var | Const | Relation
case class Var(name: String)
case class Const(v: Int)
case class Relation(name: String, args: List[String], body: Formula) {
  def withSubst(v: String, `for`: Term): Relation = Relation(name, args, body.withSubst(v, `for`))
}

extension (term: Term) {
  def withSubst(v: String, `for`: Term): Term = term match {
    case Var(name) if name == v => `for`
    case r@Relation(_, _, _) => r.withSubst(v, `for`)
    case _ => term
  }
}

type Substs = Map[String, Term]

enum Formula {
  case Eq(t1: Term, t2: Term)
  case And(f1: Formula, f2: Formula)
  case Or(f1: Formula, f2: Formula)
  case Not(f: Formula)
  case RelApp(rel: Relation, args: List[Term])

  def withSubst(v: String, `for`: Term): Formula = this match {
    case Eq(t1, t2) => Eq(t1.withSubst(v, `for`), t2.withSubst(v, `for`))
    case And(f1, f2) => And(f1.withSubst(v, `for`), f2.withSubst(v, `for`))
    case Or(f1, f2) => Or(f1.withSubst(v, `for`), f2.withSubst(v, `for`))
    case Not(f) => Not(f.withSubst(v, `for`))
    case RelApp(rel, args) => RelApp(rel.withSubst(v, `for`), args.map(_.withSubst(v, `for`)))
  }

  def withSubsts(substs: Substs): Formula = substs.foldLeft(this) { case (f, (v, t)) => f.withSubst(v, t) }

  def solve: Option[Substs] = this match {
    case Eq(t1, t2) => unify(t1, t2)
    case And(f1, f2) => for {
      s1 <- f1.solve
      s2 <- f2.withSubsts(s1).solve
    } yield s1 ++ s2
    case Or(f1, f2) => f1.solve.orElse(f2.solve)
    case Not(f) => f.solve.map(_ => Map.empty)
    case RelApp(rel, args) => rel.body.withSubsts(rel.args.zip(args).toMap).solve
  }
}

def unify(t1: Term, t2: Term): Option[Substs] = (t1, t2) match {
  case (Const(v1), Const(v2)) if v1 == v2 => Some(Map.empty)
  case (Var(v1), Var(v2)) if v1 == v2 => Some(Map.empty)
  case (Var(v1), t2) => Some(Map(v1 -> t2))
  case (t1, Var(v2)) => Some(Map(v2 -> t1))
  case _ => None
}
