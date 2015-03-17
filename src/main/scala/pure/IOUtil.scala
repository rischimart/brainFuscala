package pure

import scalaz._, Scalaz._

object IOMonad {
  sealed trait IO[A] { self =>
    //val foo = unit[B]
    def unit[A](a : A) : IO[A] = Return(a)
    def flatMap[B](f : A => IO[B]) : IO[B] = Sequence(self, f)
    def map[B](f : A => B) : IO[B] = flatMap((unit[B] _) compose f)
    //def map[B](f : A => B) : IO[B] = flatMap(f andThen (unit _))
    def runIO() : A = {
      self match {
        case Return(c) => c
        case Continue(cont) => (cont()).runIO()
        //seq(seq(p, b), f)
        case Sequence(prev, f) => prev match {
          case Return(p) => f(p).runIO()
          case Continue(cont) => f(cont()).runIO()
          // pr >>= g >>= f  ===> pr >>= \a -> g a >>= f
          case Sequence(pr, g) =>  (pr >>= (a => g(a) >>= f)).runIO()
        }
      }
    }
  } 
  
  case class Return[A](a : A) extends IO[A] 
  case class Continue[A](cont : () => IO[A]) extends IO[A] 
  case class Sequence[A, B](prev : IO[A], binder : A => IO[B]) extends IO[B]
  
  
  implicit object IO extends Monad[IO] {
    def point[A](a : => A) : IO[A] = Return(a)
    def bind[A, B](prev : IO[A])(f : A => IO[B]) : IO[B] = prev flatMap f
    def apply[A](io : => A) : IO[A] = point(io)
  }
  

}
object main {
  import pure.IOMonad._
  def printChar(c : Char) : IO[Unit] = Continue(() => IO {print(c)})
  def wrap(c : Char) : IO[Char] = c.point[IO]
  def main(args : Array[String]) : Unit = {
    val u = printChar('c') >> printChar('u')
    //u.runIO()
  }
}