package lamia

import java.io.StringReader
import bfvm.BFCompiler
import bfvm.BFVM

object MoveTest {
  
  val source = "?A?B?C?XYZ(+A+B+B+C+C+C--XYZ-XYZ>><<>A>B>C<A<B<C*XYZ-XYZ)"
  
  def main(args: Array[String]) = {
    val input = new StringReader(source)
    val lamia = LamiaParser.parse(input)
    println(lamia)
    val bf = LamiaCompiler.compile(lamia)
    println(bf)
    val asm = BFCompiler.compile(bf)
    println(asm)
    val vm = new BFVM(asm)
    while (vm.step()) {
      //println(vm.ram.take(10).mkString("[", ", ", "]"))
    }
  }
  
}