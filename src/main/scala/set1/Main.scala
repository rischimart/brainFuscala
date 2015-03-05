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



package set1
import scala.util.parsing.combinator._
import scalaz._

/*
data Cmd = IncDataPtr Int
| DecDataPtr Int
| IncByte Int
| DecByte Int
| OutputChar
| ReadInputByte
| Seq Cmd Cmd
| Loop Cmd
| Error String
deriving (Show) 
*/
class Environment(userInput :List[Char]) {
  private var memory : Vector[Int] = Vector(0, 0, 0, 0, 0)
  private var dataPtr : Int = 0
  private var remainingInput : List[Char] = userInput
  
  def updateMemory(index : Int, delta :Int) {
    
  }
  
  def moveDataPointer(delta : Int) {
    
  }
  
  def readInput : Char = {
    val nextChar = remainingInput.head
    remainingInput = remainingInput.tail
    nextChar
  }
  
  def printChar : Unit = {
    print(memory(dataPtr))
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
case class None() extends Command


object BrainFParser extends RegexParsers {
  //why "IncDataPtr(_.length())" does not compile?
  def parseIncDataPtr : Parser[Command] = """>+""".r ^^ { (s : String) => IncDataPtr(s.length()) }
  def parseDecDataPtr : Parser[Command] = """<+""".r ^^ { (s : String) => DecDataPtr(s.length()) }
  def parseIncByte : Parser[Command] = """(\+)+""".r ^^ {(s : String) => IncByte(s.length())}
  def parseDecByte : Parser[Command] = """(-)+""".r ^^ {(s : String) => DecByte(s.length())}
  def parseOutputChar : Parser[Command] = """\.""" .r ^^ {(s : String) => OutputChar()}
  def parseRead : Parser[Command] = """,""".r ^^ {(s : String) => ReadInputByte()}
 
  def parseLoop : Parser[Command] = "[" ~> parseCommands <~ "]" ^^ {Loop(_)}
    
  
  def parseCommand : Parser[Command] = parseIncDataPtr | parseDecDataPtr | parseIncByte | parseDecByte | parseOutputChar | parseRead | parseLoop
  def parseDelimiters : Parser[String] = """[^><\+-\.,\[\]]*""".r 
  def parseCommands : Parser[Command] = rep(parseDelimiters ~> parseCommand) ^^ {buildSeq(_)}
  
  def apply(input: String): Command = parseAll(parseCommands, input) match {
    case Success(result, _) => result
    case failure : NoSuccess => scala.sys.error(failure.msg)
  }
  
  //use foldRight?
  def buildSeq(commands : List[Command]) : Command = {
    commands match {
      case Nil => None()
      case x :: xs => Seq(x, buildSeq(xs))
    }
  }
}


class BrainFInterpreter(program : Command, environment : Environment) {
  var env = environment
  def interp(command : Command) : Unit = {
    command match {
      case IncDataPtr(steps) => {
        
      } 
      case DecDataPtr(steps) => {
        
      }
      
      case IncByte(value) => {
        
      }
      
      case DecByte(value) => {
        
      }
      
      case OutputChar() => {
        
      }
      
      case ReadInputByte() => {
        
      }
      
      case Seq(h, t) => {
        interp(h)
        interp(t)
      }
      
      case Loop(body) => {
        
      }
      
      case Error(err) => {
        
      }
      
      case None() => {
        ()
      }
    }
  }
}


object Main {
  def main(args : Array[String]) : Unit = {
    //>>,[>>,]<< [[-<+<]>[>[>>]<[.[-]<[[>>+<<-]<]>>]>]<<]
    println(BrainFParser(">>,[>>,]<< [[-<+<]>[>[>>]<[.[-]<[[>>+<<-]<]>>]>]<<]"))
  }
}