package scalogic

import scala.scalajs.js
import scala.scalajs.js.annotation.*

import org.scalajs.dom

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

  {
    import scalogic.unify.*
    val x = Term.Var("x")
    val y = Term.Var("y")
    val f = Formula.And(Formula.Eq(x, y), Formula.Eq(x, Term.Const(1)))
    stuff.textContent += f.solve.toString + "\n"
  }

  {
    import scalogic.unify.*
    val x = Term.Var("a")
    val y = Term.Var("b")
    val eq1: Term.Relation = Term.Relation("eq1", List("x"), Formula.Eq(x, Term.Const(1)))
    val f = Formula.And(Formula.Eq(x, y), Formula.RelApp(eq1, List(y)))
    stuff.textContent += f.solve.toString + "\n"
  }

  {
    import scalogic.unifyunion.*
    val x = Var("c")
    val y = Var("d")
    val eq1 = Relation("eq1", List("x"), Formula.Eq(x, Const(1)))
    val f = Formula.And(Formula.Eq(x, y), Formula.RelApp(eq1, List(y)))
    stuff.textContent += f.solve.toString + "\n"
  }
}
