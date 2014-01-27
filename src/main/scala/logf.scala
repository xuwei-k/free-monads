package example 

import scalaz._
import Free._, Id.Id

// needs to be covarient because of scalaz.Free
sealed trait LogF[+A] 

object Logging {
  type Log[A] = FreeC[LogF, A]

  def freeC[T[_], A](ta: T[A]): FreeC[T, A] =
    Free.liftF[({type λ[α]=Coyoneda[T, α]})#λ, A](Coyoneda(ta))

  def interpret[M[_], N[_], A](f: N ~> M, c: FreeC[N, A])(implicit M: Monad[M]): M[A] =
    c.resume match {
      case \/-(a) => M.pure(a)
      case -\/(a) => M.bind(f(a.fi))(x => interpret(f, a.k(x)))
    }

  case class Debug[A](msg: String, o: A) extends LogF[A]
  case class Info[A](msg: String, o: A) extends LogF[A]
  case class Warn[A](msg: String, o: A) extends LogF[A]
  case class Error[A](msg: String, o: A) extends LogF[A]

  object log {
    def debug(msg: String): Log[Unit] = freeC(Debug(msg, ()))
    def info(msg: String): Log[Unit]  = freeC(Info(msg, ()))
    def warn(msg: String): Log[Unit]  = freeC(Warn(msg, ()))
    def error(msg: String): Log[Unit] = freeC(Error(msg, ()))
  }
}

object Println {
  import Logging._

  private def write(prefix: String, msg: String): Unit = 
    println(s"[$prefix] $msg")

  private def debug(msg: String): Unit = write("DEBUG", msg)
  private def info(msg: String): Unit  = write("INFO", msg)
  private def warn(msg: String): Unit  = write("WARN", msg)
  private def error(msg: String): Unit = write("ERROR", msg)

  private val exe: LogF ~> Id = new (LogF ~> Id) {
    def apply[B](l: LogF[B]): B = l match { 
      case Debug(msg,a) => { debug(msg); a } 
      case Info(msg,a) => { info(msg); a } 
      case Warn(msg,a) => { warn(msg); a } 
      case Error(msg,a) => { error(msg); a } 
    }
  }

  def apply[A](log: Log[A]): A = 
    interpret(exe, log)
}

/**
  * Interpreter for SLF4J
  */ 
object SLF4J {
  import Logging._
  import org.slf4j.{Logger,LoggerFactory}

  private val log = LoggerFactory.getLogger(SLF4J.getClass)

  private val exe: LogF ~> Id = new (LogF ~> Id) {
    def apply[B](l: LogF[B]): B = l match { 
      case Debug(msg,a) => { log.debug(msg); a } 
      case Info(msg,a) => { log.info(msg); a } 
      case Warn(msg,a) => { log.warn(msg); a } 
      case Error(msg,a) => { log.error(msg); a } 
    }
  }

  def apply[A](log: Log[A]): A = 
    interpret(exe, log)
}

object Main {
  import Logging.log

  val program: FreeC[LogF, Unit] =
    for {
      a <- log.info("fooo")
      b <- log.error("OH NOES")
    } yield b

  def main(args: Array[String]): Unit = {
    SLF4J(program)
  }
}
