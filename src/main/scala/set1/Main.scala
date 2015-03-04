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

object Calculator extends RegexParsers {
def number: Parser[Double] = """\d+(\.\d*)?""".r ^^ { _.toDouble }
def factor: Parser[Double] = number | "(" ~> expr <~ ")"
def term  : Parser[Double] = factor ~ rep( "*" ~ factor | "/" ~ factor) ^^ {
  case number ~ list => (number /: list) {
    case (x, "*" ~ y) => x * y
    case (x, "/" ~ y) => x / y
  }
}
def expr  : Parser[Double] = term ~ rep("+" ~ log(term)("Plus term") | "-" ~ log(term)("Minus term")) ^^ {
  case number ~ list => list.foldLeft(number) { // same as before, using alternate name for /:
    case (x, "+" ~ y) => x + y
    case (x, "-" ~ y) => x - y
  }
}

def apply(input: String): Double = parseAll(expr, input) match {
  case Success(result, _) => result
  case failure : NoSuccess => scala.sys.error(failure.msg)
}

     class SimpleParser extends RegexParsers {

       def word: Parser[String]   = """[a-z]+""".r       ^^ { _.toString }

       def number: Parser[Int]    = """(0|[1-9]\d*)""".r ^^ { _.toInt }

       def freq: Parser[WordFreq] = word ~ number        ^^ { case wd ~ fr => WordFreq(wd,fr) }

     } 
}

object Main {
  def main(args : Array[String]) : Unit = {
    println(Calculator("1 + 3 * 9"))
  }
}