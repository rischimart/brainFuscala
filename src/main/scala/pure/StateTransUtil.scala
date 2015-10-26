package pure
import scalaz._, Scalaz._

object StateTransUtil {
  case class StateTrans[S, M[_], A](run : S => M[(A, S)])(implicit m0 : Monad[M]) {
    def unit[A](a: A): StateTrans[S, M, A] = StateTrans(s => m0.point((a, s)))
    def flatMap[B](f: A => StateTrans[S, M, B]): StateTrans[S, M, B] = {
      StateTrans { s =>
        m0.bind(run(s)) { case (a, st) => f(a).runStateT(st) }
      }
    }
    def map[B](f : A => B) : StateTrans[S, M, B] = flatMap(f andThen (unit _))

    def runStateT(s : S) : M[(A, S)] = run(s)
  }
  
  implicit def StateTransMonad[S, M[_]](implicit m : Monad[M]) = new Monad[({type λ[A] = StateTrans[S, M, A]})#λ] {
    def point[A](a : => A) : StateTrans[S, M, A] = StateTrans(s => m.point((a, s)))
    def bind[A, B](m : StateTrans[S, M, A])(f : A => StateTrans[S, M, B]) : StateTrans[S, M, B] = m flatMap f
    def apply[A](a : => A) : StateTrans[S, M, A] = point(a)
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
