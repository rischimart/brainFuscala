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
  
  def put[S](s : S) : State[S, Unit] = State(_ => ((), s))
  def get[S]: State[S, S] = State(s => (s, s))

  
  
  case class StateTrans[S, M[_], A](run : S => M[(A, S)])(implicit m0 : Monad[M]) {
    def unit(a: A): StateTrans[S, M, A] = StateTrans(s => m0.point((a, s)))
    def flatMap[B](f: A => StateTrans[S, M, B]): StateTrans[S, M, B] = {
      def fn(s: S): M[(B, S)] = {
        val m = run(s)
        m0.bind(m) { case (a, st) => f(a).runStateT(st) }
      }
      StateTrans(fn)
    }
    def runStateT(s : S) : M[(A, S)] = run(s)
  }
  
  implicit def StateTransMonad[S, M[_]](implicit m : Monad[M]) = new Monad[({type λ[A] = StateTrans[S, M, A]})#λ] {
    def point[A](a : => A) : StateTrans[S, M, A] = StateTrans(s => m.point((a, s)))
    def bind[A, B](m : StateTrans[S, M, A])(f : A => StateTrans[S, M, B]) : StateTrans[S, M, B] = m flatMap f
  }
  

  def get[S, M[_]](implicit mo : Monad[M]) : StateTrans[S, M, S] = {
      val stateTFun : S => M[(S, S)] = s => mo.point((s, s))
      StateTrans(stateTFun)
  }
    
  def put[S, M[_]](s : S)(implicit mo : Monad[M]) : StateTrans[S, M, Unit] = {
      val stateTFun : S => M[(Unit, S)] = _ => mo.point(((), s))
      StateTrans(stateTFun)
  }
  
  def lift[S, M[_], A](m : M[A])(implicit mo : Monad[M]) : StateTrans[S, M, A] = {
      def stateTFun(s: S): M[(A, S)] = {
        mo.bind(m) { a => mo.point((a, s)) }
      }
      StateTrans(stateTFun)
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