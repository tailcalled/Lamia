package lamia

import bfvm._
import Ast._

object LamiaCompiler {
  
  def compile(lamia: Ast): BFAst = {
    def registerWidth(ast: Ast): Int = ast match {
      case Program(body) => body.map(registerWidth _).foldLeft(0)(_ max _)
      case Alloc(_, body) => 1 + registerWidth(body)
      case Rep(_, body) => registerWidth(body)
      case _ => 0
    }
    val width = 1 + registerWidth(lamia)
    def move(i: Int) =
      if (i > 0) BFAst(Vector.fill(i)(BFInstr.Next))
      else BFAst(Vector.fill(-i)(BFInstr.Prev))
    def at(i: Int, body: BFInstr*) =
      move(i) ++ BFAst(body:_*) ++ move(-i)
    def addTo(d: Int) = BFInstr.Loop(BFAst(BFInstr.Pred) ++ at(d, BFInstr.Succ))
    def convert(part: Ast, env: Map[String, Int]): BFAst = part match {
      case Program(body) => body.map(convert(_, env)).foldLeft(BFAst())(_ ++ _)
      case Alloc(name, body) =>
        if (env.contains(name)) throw new Exception
        val i = env.size
        val env2 = env + (name -> i)
        at(i, BFInstr.Loop(BFAst(BFInstr.Pred))) ++ convert(body, env2)
      case Succ(name) =>
        at(env(name), BFInstr.Succ)
      case Pred(name) =>
        at(env(name), BFInstr.Pred)
      case Output(name) =>
        at(env(name), BFInstr.Outp)
      case Input(name) =>
        at(env(name), BFInstr.Inpt)
      case Next(name) =>
        val i = env(name)
        // name var = h, stack L = L, stack R = R, left vars = A, right vars = B, right head = r
        // -0i+w++
        // LAhB rR
        val moveAfter = (width - 1 until i by -1).map(p => at(p, addTo(1))).foldLeft(BFAst())(_ ++ _)
        // LAh BrR
        val moveInto = at(width + 1, addTo(i - width))
        // LAhrB R
        val moveOut = at(i, addTo(width + 1 - i))
        // LA rBhR
        val moveBefore = (i - 1 to 0 by -1).map(p => at(p, addTo(1))).foldLeft(BFAst())(_ ++ _)
        // L ArBhR
        val moveThrough = at(width + 1, addTo(-width - 1))
        // LhArB R
        moveAfter ++ moveInto ++ moveOut ++ moveBefore ++ moveThrough ++ BFAst(BFInstr.Next)
      case Prev(name) =>
        val i = env(name)
        // --0i+w+
        // LlAhB R
        val moveOut = at(i, addTo(width - i))
        // LlA BhR
        val moveIn = at(-1, addTo(i + 1))
        // L AlBhR
        val moveBack = (0 until width).map(p => at(p, addTo(-1))).foldLeft(BFAst())(_ ++ _)
        // LAlB hR
        moveOut ++ moveIn ++ moveBack ++ BFAst(BFInstr.Prev)
      case Rep(name, body) => at(env(name), BFInstr.Loop(at(-env(name), convert(body, env).body:_*)))
    }
    convert(lamia, Map("" -> 0))
  }
  
}


//       X
//  012ABCDEF 345
// >X
//  012ABC DEF345
//  012ABC3DEF 45
//  012AB 3DEFC45
//  012 AB3DEFC45
//        X
//  012CAB3DEF 45
// <X
//  012CAB DEF345
//  012 ABCDEF345
//  012ABCDEF 345
//  