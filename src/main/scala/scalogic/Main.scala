package scalogic

import scala.scalajs.js
import scala.scalajs.js.annotation.*

import org.scalajs.dom

import scalogic.unify.*
import scalogic.unifysyntax.*
import scalogic.unifysyntax.Conversions.given

import Formula.Fact

def fib(n: Int): Int = n match {
  case 0 | 1 => n
  case _ => fib(n - 1) + fib(n - 2)
}

@main
def Main(): Unit = {
  val examples = dom.document.querySelector("#examples")

  val stuff = dom.document.createElement("pre")
  examples.appendChild(stuff)
  def run(fs: Formula*)(using facts: Set[Fact], relations: Map[String, Relation]): Unit = {
    stuff.textContent += "\n"
    for (fact <- facts) stuff.textContent += s"$fact\n"
    for ((name, relation) <- relations) 
      stuff.textContent += s"$name(${relation.argNames.mkString(", ")}) :- ${relation.body}\n"
    stuff.textContent += "\n"

    for (f <- fs) {
      stuff.textContent += s"$f: ${f.?}\n"
    }
  }

  {
    def edge = MkFact("edge")
    val facts = Set[Fact](
      edge(1, 2),
      edge(2, 3),
    )

    val connected = MkRelation("connected")

    val relations = Map[String, Relation](
      "connected" -> connected.define("x", "z") { case Seq(x, z) =>
        (x === z) || edge(x, z) || (edge(x, "y") && connected("y", z))
      },
    )

    given Set[Fact] = facts
    given Map[String, Relation] = relations

    run(
      edge(1, 2), edge(2, 3),
      edge(1, 3),
      connected(1, 3),
    )
  }

  {
    given Set[Fact] = Set[Fact]()

    val relations = Map[String, Relation]()
    given Map[String, Relation] = relations

    val a = Tuple("x", 1, "z")
    val b = Tuple("z", "y", "x")

    run(a === b)
  }

  {
    given Set[Fact] = Set[Fact]()

    def sameLength = MkRelation("sameLength")
    val relations = Map[String, Relation](
      "sameLength" -> sameLength.define("x", "y") { case Seq(x, y) =>
        ((x === Tuple()) && (y === Tuple())) || (
          ("x" === Tuple("xh", "xs")) && ("y" === Tuple("yh", "ys")) && sameLength("xs", "ys")
        )
      },
    )
    given Map[String, Relation] = relations

    println("===")
    val l1 = ConsList(1, 2, 3, 4)
    val l2 = ConsList(5, 4, 2, 1)
    val l3 = ConsList(1, 2, 3)
    run(
      Tuple("xh", "xs") === ConsList(1, 2, 3, 4),
      sameLength(ConsList(1, 2), ConsList(2, 3)),
      sameLength(ConsList(1, 2), ConsList(2)),
      sameLength(Tuple(1, Tuple(3, Tuple(5, Tuple()))), Tuple(1, "a")),
      sameLength(ConsList(1, 3, 5), "a")
    )
  }
}
