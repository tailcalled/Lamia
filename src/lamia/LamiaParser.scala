package lamia

import java.io.Reader

object LamiaParser {
  
  def parseProgram(in: Input): Program = {
    var instrs = Vector[Ast]()
    while (in.ch != '\0') {
      instrs :+= parseInstr(in)
    }
    in.next()
    Program(instrs)
  }
  def parseBlock(in: Input): Program = {
    var instrs = Vector[Ast]()
    while (in.ch != ')') {
      instrs :+= parseInstr(in)
    }
    in.next()
    Program(instrs)
  }
  def parseInstr(in: Input): Ast = {
    if (in.ch == '(') parseBlock(in)
    else {
      val instr = in.ch
      in.next()
      var name = ""
      while (in.ch > 'a' && in.ch <  'z' || in.ch > 'A' && in.ch < 'Z' || in.ch == '_') {
        name += in.ch
        in.next()
      }
      instr match {
        case '+' => Ast.Succ(name)
        case '-' => Ast.Pred(name)
        case '>' => Ast.Next(name)
        case '<' => Ast.Prev(name)
        case '.' => Ast.Output(name)
        case ',' => Ast.Input(name)
        case '*' => Ast.Rep(name, parseInstr(in))
        case '?' => Ast.Alloc(name, parseInstr(in))
        case _ => throw new Exception
      }
    }
  }
  def parse(in: Reader): Ast = {
    parseProgram(new Input(in))
  }
  
}