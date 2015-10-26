package pure

import scalaz._, Scalaz._

object IOUtil {
  sealed trait IO[A] { self =>
    def unit[A](a : A) : IO[A] = { Return(a) }
    def flatMap[B](f : A => IO[B]) : IO[B] = Sequence(self, f)
    def map[B](f : A => B) : IO[B] = flatMap((unit[B] _) compose f)
    @annotation.tailrec
    final def runIO() : A = {
      self match {
        case Return(c) => c
        case Continue(cont) => cont()
        case Sequence(prev, f) => prev match {
          case Return(p) => f(p).runIO()
          case Continue(cont) => f(cont()).runIO()
          // pr >>= g >>= f  ===> pr >>= \a -> g a >>= f
          case Sequence(pr, g) =>  (pr flatMap (a => g(a) flatMap f)).runIO()
        }
      }
    }
  } 
  
  case class Return[A](a : A) extends IO[A] 
  case class Continue[A](cont : () => A) extends IO[A] 
  case class Sequence[A, B](prev : IO[A], binder : A => IO[B]) extends IO[B]
  
  implicit object IOMonad extends Monad[IO] {
    def point[A](a : => A) : IO[A] = { Return(a) }
    def bind[A, B](prev : IO[A])(f : A => IO[B]) : IO[B] = prev flatMap f
    def apply[A](io : => A) : IO[A] = point(io)
  }
}


object main {
  import pure.IOUtil  ._
  def printChar(c : Char) : IO[Unit] = Continue(() => Return(print(c)))
  def wrap(c : Char) : IO[Char] = c.point[IO]
  def main(args : Array[String]) : Unit = {
    val u = printChar('c') >> printChar('u')
    u.runIO()
  }
}