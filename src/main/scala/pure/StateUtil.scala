package pure
//import scalaz.State
import scalaz._, Scalaz._

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

  
  implicit def StateTransMonad[S, M[_]](implicit m : Monad[M]) = new Monad[({type λ[A] = StateT[S, M, A]})#λ] {
    def point[A](a : => A) : StateT[S, M, A] = StateTrans(s => m.point(a, s))
    def bind[A, B](m : StateT[S, M, A])(f : A => StateT[S, M, B]) : StateT[S, M, B] = m flatMap f
  }
  
  sealed trait StateT[S, M[_], A] { self =>
    implicit def M: Monad[M]
    def runStateT(s : S) : M[(A, S)]
    def flatMap[B](f : A => StateT[S, M, B]) : StateT[S, M, B] = {
      val stateTFun : S => M[(B, S)] = s => {
          val mon = self.runStateT(s)
          mon.flatMap {
            case (a, ss) => f(a).runStateT(ss)
          }
      } 
      StateTrans(stateTFun)
    }    
  }

  def get[S, M[_]](implicit Mo : Monad[M]) : StateT[S, M, S] = {
      val stateTFun : S => M[(S, S)] = s => Mo.point((s, s))
      StateTrans(stateTFun)
  }
    
  def put[S, M[_]](s : S)(implicit Mo : Monad[M]) : StateT[S, M, Unit] = {
      val stateTFun : S => M[(Unit, S)] = _ => Mo.point(((), s))
      StateTrans(stateTFun)
  }
  
  def lift[S, M[_], A](m : M[A])(implicit Mo : Monad[M]) : StateT[S, M, A] = {
      val stateTFun : S => M[(A, S)] = s => {
        m.flatMap { a => Mo.point((a, s)) }
      }
      StateTrans(stateTFun)
  }
  
  case class StateTrans[S, M[_], A](run : S => M[(A, S)])(implicit M0 : Monad[M]) extends StateT[S, M, A] {
    
    def runStateT(s : S) : M[(A, S)] = run(s)
    implicit def M: Monad[M] = M0
  }
}

object Main {
    import pure.StateUtil._
    import pure.IOUtil._   
    type Stack = List[Int]
    def main(args : Array[String]) : Unit = {
      val program = get[Stack] >>= {st => if (st == List(1, 2, 3)) put(List(8, 3, 1))
                                        else put(List(9, 2, 1))}
      val program2 = for {
        st <- get[Stack]
        _ <- if (st == List(1, 2, 3)) put[Stack](List(8, 3, 1)) else put[Stack](List(9, 3, 1))
      } yield ()
      
      def printChar(c : Char) : IO[Unit] = Continue(() => IO {print(c)})

      val (_, finalstate) = program2.runState(List(1, 2, 4))
      print(finalstate)
      //put[Stack, IO](List(1, 2, 3))
      //val result = prog.runStateT(List())
      //val fst = result.flatMap {case (_, st) =>  st.point[IO]}
      //print(fst)
    }
} 