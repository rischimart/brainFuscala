/*-- syntax --
>   Increment data pointer so that it point to next location in memory.
< Decrement data pointer so that it point to previous locaion in memory.
+ Increment the byte pointed by data pointer by 1. If it is already at its maximum value, 255, then new value will be 0.
- Decrement the byte pointed by data pointer by 1. If it is at its minimum value, 0, then new value will be 255.
. Output the character represented by the byte at the data pointer.
, Read one byte and store it at the memory location pointed by data pointer.
[ If the byte pointed by data pointer is zero, then move instruction pointer to next matching ']', otherwise move instruction pointer to next command.
] If the byte pointed by data pointer is non-zero, then move instruction pointer to previous matching '[' command, otherwise to next command.

* */ 
package probablyfuckedup
import scala.util.parsing.combinator._
import scalaz._
import scala.Vector
import pure.IOUtil._
import pure.StateTransUtil._


sealed trait Command
case class IncDataPtr(step : Int) extends Command
case class DecDataPtr(step : Int) extends Command
case class IncByte(value : Int) extends Command
case class DecByte(value: Int) extends Command
case class OutputChar() extends Command
case class ReadInputByte() extends Command
case class Seq(head : Command, tail : Command) extends Command
case class Loop(body : Command) extends Command
case class Error(msg : String) extends Command
case class Null() extends Command

object BrainFParser extends RegexParsers {
  def parseIncDataPtr : Parser[Command] = """>+""".r ^^ { (s : String) => IncDataPtr(s.length()) }
  def parseDecDataPtr : Parser[Command] = """<+""".r ^^ { (s : String) => DecDataPtr(s.length()) }
  def parseIncByte : Parser[Command] = """(\+)+""".r ^^ {(s : String) => IncByte(s.length())}
  def parseDecByte : Parser[Command] = """(-)+""".r ^^ {(s : String) => DecByte(s.length())}
  def parseOutputChar : Parser[Command] = """\.""" .r ^^ {(s : String) => OutputChar()}
  def parseRead : Parser[Command] = """,""".r ^^ {(s : String) => ReadInputByte()}
 
  def parseLoop : Parser[Command] = "[" ~> parseCommands <~ "]" ^^ {Loop(_)}
    
  
  def parseCommand : Parser[Command] = parseIncDataPtr | parseDecDataPtr | parseIncByte | parseDecByte | parseOutputChar | parseRead | parseLoop
  def parseDelimiters : Parser[String] = """[^><\+-\.,\[\]]*""".r 
  def parseCommands : Parser[Command] = rep(parseDelimiters ~> parseCommand) <~ parseDelimiters ^^ {buildSeq(_)}
  
  def apply(input: String): Command = parseAll(parseCommands, input) match {
    case Success(result, _) => result
    case failure : NoSuccess => scala.sys.error(failure.msg)
  }
  
  def buildSeq(commands : List[Command]) : Command = {
    commands match {
      case Nil => Null()
      case x :: xs => Seq(x, buildSeq(xs))
    }
  }
}

class BrainFInterpreter(program : Command) {
  type Memory = Vector[Int]
  type DataPointer = Int
  type UserInput = Vector[Char]
  
  type Environment = (Memory, DataPointer, UserInput)
  type EvalResult = StateTrans[Environment, IO, Unit]
  
  def updateMemory(memory: Memory, dataPtr: DataPointer, delta: Int): Memory = {
    var newVal = memory(dataPtr) + delta
    if (newVal > 255) newVal = newVal % 255 
    if (newVal < 0) {
      while (newVal < 0) newVal += 256
    } 
    memory.updated(dataPtr, newVal)
  }
  
  def moveDataPointer(env: Environment, delta: Int): Environment = {
    val(mem, dp, inpt) = env
    val newDp = if (dp + delta < 0) 0 else dp + delta
    val len = mem.length
    val newMem = if (newDp < len) mem else mem.padTo(len * 2, 0)
    (newMem, newDp, inpt)
  }
  
  def printChar(c : Char) : IO[Unit] = Continue(() => Return(print(c)))
  
  def printStr(str : String) : IO[Unit] = Continue(() => Return(print(str)))
  
  def readUserInput(env: Environment): Environment = {
    val(mem, dp, inpt) = env
    if (inpt.isEmpty) env
    else {
      val newMem = mem.updated(dp, inpt.head.toInt)
      (newMem, dp, inpt.tail)
    }
  }
  
  def shouldBreak(env: Environment): Boolean = {
    val(mem, dp, _) = env
    mem(dp) == 0
  }
    
  def interp(command : Command) : EvalResult = {
    command match {
      case IncDataPtr(steps) =>
          get[Environment, IO] flatMap { env => 
            put[Environment, IO](moveDataPointer(env, steps))
          }
      case DecDataPtr(steps) => interp(IncDataPtr(-steps))
      case IncByte(delta) => 
          get[Environment, IO] flatMap { case (m, dp, us) => 
            val nm = updateMemory(m, dp, delta)
            put[Environment, IO](nm, dp, us)
          }  
      case DecByte(delta) => interp(IncByte(-delta))
      case OutputChar() => 
          get[Environment, IO] flatMap { case (m, dp, us) =>  
            val ch = m(dp).toChar
            lift[Environment, IO, Unit](printChar(ch))
          }
      case ReadInputByte() =>
          get[Environment, IO] flatMap { env =>  
            val newEnv = readUserInput(env)
            put[Environment, IO](newEnv)
          }
      case Seq(h, t) => interp(h) flatMap { _ => interp(t) }     
      case l @ Loop(body) => 
        get[Environment, IO] flatMap 
        { env =>if (shouldBreak(env)) StateTransMonad[Environment, IO](IOMonad)(()) 
                else interp(Seq(body, l)) }
      case Error(err) => lift[Environment, IO, Unit](printStr(err))
      case Null() => StateTransMonad[Environment, IO](IOMonad)(()) 
    }
  }
}

object Main {
  def main(args : Array[String]) : Unit = {
    val program = """
+++++ +++++             initialize counter (cell #0) to 10
[                       use loop to set the next four cells to 70/100/30/10
    > +++++ ++              add  7 to cell #1
    > +++++ +++++           add 10 to cell #2
    > +++                   add  3 to cell #3
    > +                     add  1 to cell #4
    <<<< -                  decrement counter (cell #0)
]
> ++ .                  print 'H'
> + .                   print 'e'
+++++ ++ .              print 'l'
.                       print 'l'
+++ .                   print 'o'
> ++ .                  print ' '
<< +++++ +++++ +++++ .  print 'W'
> .                     print 'o'
+++ .                   print 'r'
----- - .               print 'l'
----- --- .             print 'd'
> + .                   print '!'
"""
    val program2 = ">>,[>>,]<< [[-<+<]>[>[>>]<[.[-]<[[>>+<<-]<]>>]>]<<]"
    val parsedCmds = BrainFParser(program2)
    //println(parsedCmds)
    val interpreter = new BrainFInterpreter(parsedCmds)
    val initMem = Vector(0, 0, 0, 0)
    val userInput = "8479532167".toVector
    interpreter.interp(parsedCmds).runStateT((initMem, 0, userInput)).runIO()
  }
}