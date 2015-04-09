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

class Environment(userInput :List[Char]) {
  private var memory : Vector[Int] = Vector(0, 0, 0, 0, 0)
  private var dataPtr : Int = 0
  private var remainingInput : List[Char] = userInput
  
  private def expandMemory {
    memory = memory padTo(memory.length * 2, 0)
  }
  
  private def store(value : Int) = {
    memory = memory updated(dataPtr, value)
  }
  
  def updateMemory(delta :Int) {
    if (dataPtr >= memory.length) expandMemory
    var newVal = memory(dataPtr) + delta
    if (newVal > 255) newVal = newVal % 255 
    if (newVal < 0) {
      while (newVal < 0) newVal += 256
    } 

    memory = memory updated(dataPtr, newVal)
  }
  
  def moveDataPointer(delta : Int) {
    dataPtr += delta
    if (dataPtr < 0) dataPtr = 0
    else {
      if (dataPtr >= memory.length) expandMemory
    }
  }
  
  def readInput : Unit = {
    val nextChar = remainingInput.headOption
    nextChar match {
      case Some(c) => {
        remainingInput = remainingInput.tail
        store(c)
      }
      case _ => ()
    }
  }
  
  def printChar : Unit = {
    print(memory(dataPtr).toChar)
  }
  
  def shouldExit : Boolean = {
    memory(dataPtr) == 0
  }
}

/*
type Memory = List[Int]
type DataPointer = Int
type UserInput = [Char]
type Enviroment = (Memory, DataPointer, UserInput)
*/
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

class BrainFInterpreter(program : Command, environment : Environment) {
  var env = environment
  def interp(command : Command) : Unit = {
    command match {
      case IncDataPtr(steps) => env moveDataPointer(steps)
      case DecDataPtr(steps) => env moveDataPointer(-steps)
      case IncByte(delta) => env updateMemory(delta)
      case DecByte(delta) => env updateMemory(-delta)
      case OutputChar() => env printChar
      case ReadInputByte() => env readInput
      case Seq(h, t) => {
        interp(h)
        interp(t)
      }
      case l @ Loop(body) => {
        if (env shouldExit) ()
        else interp(Seq(body, l))
      }
      case Error(err) => println(err)
      case Null() => ()
    }
  }
  
  //def apply(input: Command): Unit = interp(input)
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
    val interpreter = new BrainFInterpreter(parsedCmds, new Environment("84732167".toList))
    interpreter.interp(parsedCmds)
  }
}