

package no.k.m6809.junk

import scala.actors.Actor
import scala.actors.Actor._
import scala.actors.TIMEOUT
import java.lang.System._
import scala.math._

object Pingpong extends App {

	def threadName = "(" + Thread.currentThread().getName() + ")"
	def __LOGPT(s:String) = println(" <-- " + s + " " + threadName)
	

  	def align(v:Long, multiple:Int) = v / multiple * multiple
  	def now = currentTimeMillis   	

	val FREQ_HZ = 5
  	val SEC_MS = 1000
	val PERIOD_MS = SEC_MS / FREQ_HZ
	val T0 = align(now, SEC_MS)
	
	def lapsed = now - T0
	def timeToPulse = PERIOD_MS - lapsed % PERIOD_MS

    __LOGPT("A")

	val pulsee = actor {
	  __LOGPT("B")
	  loop {
	    __LOGPT("C")
	    react {
	      case _ => {
	    	__LOGPT("D")
		  	println(lapsed + " " + timeToPulse)
		  	__LOGPT("E")
	      }
	    }
	  }
	}
	
	
	__LOGPT("F")
  	val pulsar = actor {
	  __LOGPT("G")
	  loop {
	    __LOGPT("H")
		reactWithin (timeToPulse) {
		  case TIMEOUT => {
		    __LOGPT("I")
		    pulsee ! 0
		    __LOGPT("J")
		  }
		}
	  }
	}
	
  	__LOGPT("K")
  	(0 to 50).foreach { i =>
  	  __LOGPT("L")
  	  Thread.sleep(500);
  	}
  	__LOGPT("M")
	in.read()
	///////////////////////////////////////////////////
	
	
  
    println("Main thread: " + Thread.currentThread().getName())
	val pong = new Pong
	val ping = new Ping(100000, pong)
	ping.start
	pong.start

	
	case object Ping
	case object Pong
	case object Stop

	
	class Ping(count: Int, pong: Actor) extends Actor {
	  def act() {
		println("Ping.act thread: " + Thread.currentThread().getName() + " PING")
	    var pingsLeft = count - 1
	    
	    pong ! Ping
	    
	    loop {
		  react {
	        case Pong =>
	          if (pingsLeft % 1000 == 0) {
	        	  println("Ping: pong " + pingsLeft + " " + Thread.currentThread().getName() + " PING")	        	  
	          }
	          
	          if (pingsLeft > 0) {
	            pong ! Ping
	            pingsLeft -= 1
	          } else {
	            println("Ping: stop")
	            pong ! Stop
	            exit
	          }
	      }
	    }
	  }
	}
	
	
	class Pong extends Actor {
	  def act() {
		println("Pong.act thread: " + Thread.currentThread().getName() + " PONG")
	    var pongCount = 0
	    
	    loop {
	      react {
	        
	        case Ping =>
	          if (pongCount % 1000 == 0) println("Pong: ping " + pongCount + " " + Thread.currentThread().getName() + " PONG")
	          sender ! Pong
	          pongCount = pongCount + 1
	          
	        case Stop =>
	          println("Pong: stop")
	          exit
	      }
	    }
	  }
	}
	
	
}