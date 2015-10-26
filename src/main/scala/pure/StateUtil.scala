package pure
//import scalaz.State
import scalaz._, Scalaz._

object StateUtil {
  case class State[S, A](stateFn: S => (A, S)) {
    def unit[A](a : A) : State[S, A] = State(s => (a, s))
    def runState(s : S) : (A, S) = stateFn(s) 
    def flatMap[B](f : A => State[S, B]) : State[S, B] = {
      def sfn(s : S): (B, S) = {
        val (a, st) = runState(s)
        f(a).runState(st)
      }
      State(sfn)
    }
   
    def map[B](f : A => B) : State[S, B] = flatMap(f andThen (unit _))
  }
  
 
  implicit def StateMonad[S] = new Monad[({type λ[A] = State[S, A]})#λ] {
    def point[A](a : => A) : State[S, A] = State(s => (a, s))
    def bind[A, B](prev : State[S, A])(f : A => State[S, B]) : State[S, B] = prev flatMap f
    def apply[A](a : => A) : State[S, A] = point(a)
  }
  
  //def put[S](s : S) : State[S, Unit] = State(_ => ((), s))
  //def get[S]: State[S, S] = State(s => (s, s))
  
}
