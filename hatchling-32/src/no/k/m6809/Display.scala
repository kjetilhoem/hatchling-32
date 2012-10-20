package no.k.m6809

import scala.swing._
import scala.swing.Swing
import scala.swing.Dimension
import scala.swing.event._
import java.awt.{ Graphics2D, Font, Rectangle }
import java.awt.Color._
import java.io.File
import javax.imageio.ImageIO
import scala.actors._
import scala.actors.Actor._

class Display(smem:Array[Byte], startAddress:Int) extends MainFrame {
    
	val SCALE = 3
  
	val SCR_TXT_W = 32
	val SCR_TXT_H = 16
	val SCR_TXT_LEN = SCR_TXT_W * SCR_TXT_H
	
	val CHAR_W = 8
	val CHAR_H = 12
	
	val SCR_W = SCR_TXT_W * CHAR_W
	val SCR_H = SCR_TXT_H * CHAR_H
	
    title = "PROTODRAGON - Scala Motorola 6809E/Dragon32 casloader/emulator prototype"
    preferredSize = new Dimension(11 + SCR_W * SCALE, 29 + SCR_H * SCALE)
	

	val e = new Event() {}
	val p = new Publisher() {}
	
  	def align(v:Long, multiple:Int) = v / multiple * multiple
  	def now = System.currentTimeMillis   	
	val FREQ_HZ = 50
  	val SEC_MS = 1000
	val PERIOD_MS = SEC_MS / FREQ_HZ
	val T0 = align(now, SEC_MS)
	def lapsed = now - T0
	def periodLapsed = lapsed % PERIOD_MS
	def timeToPulse = PERIOD_MS - periodLapsed
	
	var tP:Long = 0
	
	actor {
  		loop {	    
  			reactWithin (timeToPulse) {
  				case TIMEOUT => {
  					tP = now
  					p.publish(e)
  				}
  			}
  		}
  	}
  	
  	val img = ImageIO.read(new File("/home/kjetil/dragon32/gfx/charset_1to1.png"))

  	actor {
  	  loop {
  	    reactWithin (10) {
  	      case TIMEOUT => {
  	        smem(System.nanoTime() % 512 toInt) = 0xFF toByte 
  	      }
  	    }
  	  }
  	}
	
    contents = new Panel {
      
	  listenTo(p)
	  
      reactions += {	    
        case update => {
          repaint
        }
      }
	  
      override def paintComponent(g: Graphics2D) {
        g.setColor(red)
        g.fill(new Rectangle(SCR_W * SCALE, SCR_H * SCALE))
        g.setColor(black)
        
        for (i <- 0 until SCR_TXT_LEN) {
	        val char = mapChar((smem(i + startAddress)) & 0xFF)
	        
	        val locX = i % SCR_TXT_W
	        val locY = i / SCR_TXT_W
	        
	        val chX = char % SCR_TXT_W
	        val chY = char / SCR_TXT_W
	        
	        val dstX1 = locX * CHAR_W * SCALE
	        val dstY1 = locY * CHAR_H * SCALE 
	        
	        val dstX2 = dstX1 + CHAR_W * SCALE
	        val dstY2 = dstY1 + CHAR_H * SCALE
	        
	        val srcX1 = chX * CHAR_W
	        val srcY1 = chY * CHAR_H
	        
	        val srcX2 = srcX1 + CHAR_W
	        val srcY2 = srcY1 + CHAR_H
	        
	        g.drawImage(img, 
	            dstX1, dstY1, dstX2, dstY2, 
	            srcX1, srcY1, srcX2, srcY2, 
	            null)
        }
        
        //println("PERIOD LAPSE=" + (now - tP))
      }
    }

	def mapChar(c:Int) = c
	
	
}