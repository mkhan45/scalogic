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
  val app = dom.document.querySelector("#app")

  val label = dom.document.createElement("label")
  label.textContent = "Input:"
  app.appendChild(label)

  val inp: dom.HTMLInputElement = dom.document.createElement("input").asInstanceOf[dom.HTMLInputElement]
  inp.setAttribute("type", "text")
  app.appendChild(inp)

  val out = dom.document.createElement("div")
  val outSpan = dom.document.createElement("span")
  out.textContent = "Output: "
  outSpan.textContent = "None"
  out.appendChild(outSpan)
  app.appendChild(out)

  val timer = dom.document.createElement("div")
  val timerSpan = dom.document.createElement("span")
  timer.textContent = "Time: "
  timerSpan.textContent = "None"
  timer.appendChild(timerSpan)
  app.appendChild(timer)

  val btn = dom.document.createElement("button")
  btn.textContent = "Calculate"
  btn.addEventListener("click", (_: dom.MouseEvent) => update())
  app.appendChild(btn)

  def update(): Unit = {
    val n = inp.value.toInt
    println(s"Calculating fib($n)")
    val t1 = js.Date.now()
    val res = fib(n)
    val t2 = js.Date.now()
    outSpan.textContent = res.toString
    timerSpan.textContent = (t2 - t1).toString + "ms"
  }

  val stuff = dom.document.createElement("pre")
  app.appendChild(stuff)

  def run(fs: Formula*)(using facts: Set[Fact], relations: Map[String, Relation]): Unit = {
    for (fact <- facts) stuff.textContent += s"$fact\n"
    for ((name, relation) <- relations) 
      stuff.textContent += s"$name(${relation.argNames.mkString(", ")}) :- ${relation.body}\n"
    stuff.textContent += "\n"

    for (f <- fs) stuff.textContent += s"$f: ${f.?}\n"
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

    val l1 = ConsList(1, 2, 3, 4)
    val l2 = ConsList(5, 4, 2, 1)
    val l3 = ConsList(1, 2, 3)
    run(
      sameLength(l1, l2),
      sameLength(l1, l3),
      sameLength(ConsList(1, 2, 3, 4), "x")
    )
  }
}
