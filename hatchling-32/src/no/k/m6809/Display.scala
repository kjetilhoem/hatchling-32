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

class Display(smem:Array[Byte], startAddress:Int, basedir:String, mode:Array[Int]) extends MainFrame {
    
    val TEXT = 1
    val GFX = 2
    
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
  	
  	val file = new File(basedir + "/gfx/charset_1to1.png")
  	val img = ImageIO.read(file)
  	
  	val srcPmode3c1 = new File(basedir + "/gfx/pmode3c1_ntsc_artifact_v2.png")
  	val imgPmode3c1 = ImageIO.read(srcPmode3c1)
  	
  	val startAddressGfx = 0x1A00

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
        
        if (mode(0) == 0) {
            //printf("TEXTMODE @ %04X\n", mode(1))
	        for (i <- 0 until SCR_TXT_LEN) {
		        val char = mapChar((smem(i + mode(1))) & 0xFF)
		        
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
        } else if (mode(0) == 1) {
            //printf("GFXMODE @ %04X\n", mode(1))
            for (y <- 0 until 192) {
                for (x <- 0 until 32) {
                    val pattern = ((smem(x + y * 32 + mode(1))) & 0xFF)
                    
                    val dstX1 = x * 8 * SCALE
                    val dstY1 = y * SCALE
                    val dstX2 = dstX1 + 8 * SCALE
                    val dstY2 = dstY1 + 1 * SCALE
                    
                    val srcX1 = 0
                    val srcY1 = pattern
                    val srcX2 = srcX1 + 8
                    val srcY2 = srcY1 + 1
                    
                    g.drawImage(imgPmode3c1,
			            dstX1, dstY1, dstX2, dstY2, 
			            srcX1, srcY1, srcX2, srcY2, 
			            null)
                }
            }
        } else {
            println("UNKNOWN MODE")
            System.exit(0)
        }
      }
    }

	def mapChar(c:Int) = c
	
	
}