package scalogic.unifysyntax

import scalogic.unify.*

extension (t: Term) {
  def ===(t2: Term): Formula.Eq = Formula.Eq(t, t2)
}

extension (e: Formula) {
  def &&(e2: Formula): Formula.And = Formula.And(e, e2)
  def ||(e2: Formula): Formula.Or = Formula.Or(e, e2)
  def unary_! : Formula.Not = Formula.Not(e)
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
