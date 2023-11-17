package scalogic.unifysyntax

import scalogic.unify.*

extension (t: Term) {
  def ===(t2: Term): Formula.Eq = Formula.Eq(t, t2)
}

case class Res(substs: Option[Substs]) {
  override def toString: String = substs match {
    case Some(m) if m.isEmpty => "true"
    case None => "false"
    case Some(m) => m.toString
  }
}


extension (e: Formula) {
  def &&(e2: Formula): Formula.And = Formula.And(e, e2)
  def ||(e2: Formula): Formula.Or = Formula.Or(e, e2)
  def unary_! : Formula.Not = Formula.Not(e)

  def ?(using facts: Set[Formula.Fact], relations: Map[String, Relation]): Res = Res(e.solve)

  def toString: String = e match {
    case Formula.Eq(t1, t2) => s"$t1 == $t2"
    case Formula.And(f1, f2) => s"($f1 && $f2)"
    case Formula.Or(f1, f2) => s"($f1 || $f2)"
    case Formula.Not(f) => s"!($f)"
    case Formula.Fact(name, args) => s"$name(${args.mkString(", ")})"
    case Formula.RelApp(name, args) => s"$name(${args.mkString(", ")})"
  }
}

case class MkFact(name: String) {
  def apply(args: Term *): Formula.Fact = Formula.Fact(name, args.toList)
}

case class MkRelation(name: String) {
  def apply(args: Term *): Formula.RelApp = Formula.RelApp(name, args.toList)

  def define(argNames: String*)(body: Seq[Term] => Formula): Relation = {
    Relation(argNames.toList, body(argNames))
  }
}
