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
}

case class MkFact(name: String) {
  def apply(args: Term *): Formula.Fact = Formula.Fact(name, args.toList)
}

case class MkRelation(name: String) {
  def apply(args: Term *): Formula.RelApp = Formula.RelApp(name, args.toList)

  def define(argNames: String*)(body: Seq[Term] => Formula): Relation = {
    Relation(argNames.toList, body(argNames.map(Term.Var(_))))
  }
}

def Tuple(ts: Term*): Term.Tuple = Term.Tuple(ts.toList)

object Conversions {
  given Conversion[Int, Term] = Term.Const(_)
  given Conversion[String, Term] = Term.Var(_)
  given Conversion[List[Term], Term] = Term.Tuple(_)
  given[T](using conv: Conversion[T, Term]): Conversion[List[T], Term] = _.map(conv).toList
}
