package lamia

/**
 * 
 * Conceptually speaking, the Lamia memory model consists of two stacks,
 * which we will call L and R, along with a set of local variables.
 * 
 * Instructions:
 * 
 *  * ?XB   X is a new local variable (initially set to zero) in the code B
 *          X may not already be allocated.
 *  * >X    Push X onto L, pop R onto X.
 *  * <X    Push X onto R, pop L onto X
 *  * .X    Output X
 *  * ,X    Input a byte into X
 *  * *XB   While X is nonzero, run B
 *  * (B)   Group a set of operations as a single op
 *  * +X    Increment X
 *  * -X    Decrement X
 * 
 */

sealed trait Ast
case class Program(body: Vector[Ast]) extends Ast {
  
  override def toString = body.mkString("(", "", ")")
  
}
object Ast {
  type Variable = String
  case class Rep(condition: Variable, body: Ast) extends Ast {
    override def toString = s"*$condition$body"
  }
  case class Alloc(name: Variable, body: Ast) extends Ast {
    override def toString = s"?$name$body"
  }
  case class Succ(name: Variable) extends Ast {
    override def toString = s"+$name"
  }
  case class Pred(name: Variable) extends Ast {
    override def toString = s"-$name"
  }
  case class Output(name: Variable) extends Ast {
    override def toString = s".$name"
  }
  case class Input(name: Variable) extends Ast {
    override def toString = s",$name"
  }
  case class Next(leave: Variable) extends Ast {
    override def toString = s">$leave"
  }
  case class Prev(leave: Variable) extends Ast {
    override def toString = s"<$leave"
  }
}