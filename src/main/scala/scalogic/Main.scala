package scalogic

import scala.scalajs.js
import scala.scalajs.js.annotation.*

import org.scalajs.dom

// import scalogic.unify.{Term, Relation, Formula}
// import scalogic.unify.Syntax.*

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

  // {
  //   val x = Term.Var("x")
  //   val y = Term.Var("y")
  //   val f = (x === y) && (x === Term.Const(1))
  //   stuff.textContent += f.solve.toString + "\n"
  // }

  // {
  //   val x = Term.Var("a")
  //   val y = Term.Var("b")
  //   val eq1 = Relation("eq1", List("x"), x === Term.Const(1))
  //   val f = (x === y) && eq1(x)
  //   stuff.textContent += f.solve.toString + "\n"
  // }

  // {
  //   val x = Term.Var("x")
  //   val y = Term.Var("y")
  //   val z = Term.Var("z")
  //   // def connected: Relation = Relation("connected", List("x", "z"), (x === z) || (connected(x, y) && connected(y, z)))
  //   def connected: Relation = Relation("connected", List("x", "z")) { connected =>
  //     edge(x, z) || (edge(x, y) && connected(y, z))
  //   }
  //   def edge: Relation = Relation("edge", List("x", "y"), 
  //     (x === y) 
  //     || ((x === Term.Const(1)) && (y === Term.Const(2)))
  //     || ((x === Term.Const(2)) && (y === Term.Const(3)))
  //   )
  //   val f = connected(Term.Const(1), Term.Const(3))
  //   stuff.textContent += f.solve.toString + "\n"
  // }
  //

  {
    import scalogic.unifynew.*
    import Formula.*

    val facts = Set[Fact](
      Fact("edge", List(1, 2)),
      Fact("edge", List(2, 3)),
    )

    def edge = MkFact("edge")

    val connected = MkRelation("connected")

    val relations = Map[String, Relation](
      "connected" -> Relation("connected", List("x", "z"), Or(
        edge("x", "z"),
        And(edge("x", "y"), connected("y", "z"))
      ))
    )

    given Set[Fact] = facts
    given Map[String, Relation] = relations

    val f = connected(1, 3)
    stuff.textContent += s"$f: ${f.solve}\n"
  }
}
