package pure
//import scalaz.State
import scalaz._, Scalaz._
/**
 * @author rialmat
 */
object StateUtil {
  sealed trait State[S, A] { self =>
    def unit[A](a : A) : State[S, A] = StateM(s => (a, s))
    def runState(s : S) : (A, S) 
    def flatMap[B](f : A => State[S, B]) : State[S, B] = {
      val stateFun = (s : S) => {
        val (v, ss) = self.runState(s)
        f(v).runState(ss)
      }
      StateM(stateFun)
    }
    
    def map[B](f : A => B) : State[S, B] = flatMap(f andThen (unit _))
  }
  
  case class StateM[S, A](run : S => (A, S)) extends State[S, A] {
    def runState(s : S) : (A, S) = run(s)
  }
  
  implicit def StateMonad[S] = new Monad[({type λ[A] = State[S, A]})#λ] {
    def point[A](a : => A) : State[S, A] = StateM(s => (a, s))
    def bind[A, B](prev : State[S, A])(f : A => State[S, B]) : State[S, B] = prev flatMap f
    def apply[A](a : => A) : State[S, A] = point(a)
  }
  
  def put[S](s : S) : State[S, Unit] = StateM(_ => ((), s))
  
  def get[S]: State[S, S] = StateM(s => (s, s))
  //implicit def monadify[S, A](st: => State[S, A]) = new StateMonad[S]("state") 

}

object Main {
    import pure.StateUtil._
    
    /*
     *  stackyStack :: State Stack ()  
        stackyStack = do  
          stackNow <- get  
          if stackNow == [1,2,3]  
            then put [8,3,1]  
            else put [9,2,1]  
     */
    type Stack = List[Int]
    
    /*
    def stacky : State[Stack, Unit] = {
      val program = for {
        st <- get[Stack]
        _ <-
      }
      
    }*/

    def main(args : Array[String]) : Unit = {
      val program = get[Stack] >>= {st => if (st == List(1, 2, 3)) put(List(8, 3, 1))
                                        else put(List(9, 2, 1))}
      val (_, finalstate) = program.runState(List(1, 2, 4))
      print(finalstate)
    }
} 