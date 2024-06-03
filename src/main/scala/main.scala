package engine

import base.*

import engine.render.RenderedImage

import java.awt.image.{BufferedImage, DataBufferInt}
import java.awt.{Dimension, Toolkit}
import javax.swing.{JFrame, JPanel, WindowConstants}
import render.*
import base.*

import engine.base.input.Query
import engine.base.light.{ParallelCamera, PerspectiveCamera}

import java.awt.event.{KeyAdapter, KeyEvent, KeyListener}
import scala.util.Random

@main
def main(): Unit = {
  val x = RawVector.xAxisUnit
  val y = RawVector.yAxisUnit
  val x2 = (x + y).normalizeLength()
  val z = (x2 ** y).normalizeLength()
  println(s"x + y: $x2")
  println(s"z    : $z")
  val z2 = z.rotateOnXAxis((math.Pi / 2))
  val z3 = z2.rotateOnXAxis(math.Pi * 3 / 2)
  println(s"z rotate on x: $z2")
  println(s"z rotate on x inverse: $z3")
}

@main
def main$0() : Unit = {
  val frame = JFrame()
  val panel = JPanel()
  frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  frame.setContentPane(panel)
  frame.setResizable(false)
  val platformDim = Toolkit.getDefaultToolkit.getScreenSize
  frame.setLocationRelativeTo(null)
  panel.setPreferredSize(platformDim)
  frame.pack()
  frame.setVisible(true)
  val relatedImage = BufferedImage(platformDim.width, platformDim.height, BufferedImage.TYPE_INT_ARGB)
  // val camera = ParallelCamera.fromForward0(RawVector.zeroPoint, RawVector.xAxisUnit, width = 800, height = 600).
  val camera = PerspectiveCamera.fromForward0(RawVector.zeroPoint, RawVector.xAxisUnit, width = 5, height = 4)
  var relatedPosition : RawVector = RawVector(300, 700, 700, 1)
  val query = Query()
  val keyEventClass = classOf[KeyEvent]
  val ars : Vector[Int] = keyEventClass.getFields.filter { f => f.getName.startsWith("VK") }.map( f => f.get(null).asInstanceOf[Int] ).toVector
  for (elem <- ars) {
    query.initRelatedInt(elem)
  }
  frame.addKeyListener(new KeyAdapter {
    override def keyPressed(e: KeyEvent): Unit = {
      super.keyPressed(e)
      val kd = query.map.get(e.getKeyCode)
      kd match {
        case Some(v) => v.set(true)
        case _ => {}
      }
    }
    override def keyReleased(e: KeyEvent): Unit = {
      super.keyReleased(e)
      val kd = query.map.get(e.getKeyCode)
      kd match {
        case Some(v) => v.set(false)
        case _ => {}
      }
    }
  })
  var time = System.currentTimeMillis()
  val random = Random()
  var timeCount : Double = 0
  var frameCount : Double = 0
  val startP = time
  
  var stopTick = 0
  var upCounter : Double = 0
  
  while (true) {
    
    // physics
    val randomVector = RawVector(
      0,
      random.nextGaussian(),
      random.nextGaussian(),
      0
    ).scalaExpand(6)
    // relatedPosition = relatedPosition + randomVector
    
    val m = query.map
    val w = m(KeyEvent.VK_W).get()
    val s = m(KeyEvent.VK_S).get()
    (w, s) match {
      case (true, false) => {
        if (upCounter - 0.01 > - Math.PI / 2) {
          upCounter -= 0.01
          camera.doRotate(fromEulerAngular(0, 0, -0.01))
        }
        if (stopTick > 0) {
          stopTick -= 1
        } else {
          stopTick = 18
          println(s"Look up")
        }
      }
      case (false, true) => {
        if (upCounter + 0.01 < Math.PI / 2) {
          upCounter += 0.01
          camera.doRotate(fromEulerAngular(0, 0, 0.01))
        }
        if (stopTick > 0) {
          stopTick -= 1
        } else {
          stopTick = 18
          println(s"Look down")
        }
      }
      case _ => {}
    }
    val a = m(KeyEvent.VK_A).get()
    val d = m(KeyEvent.VK_D).get()
    (a, d) match {
      case (true, false) => {
        camera.doRotate(fromEulerAngular(0, 0.01, 0))
        if (stopTick > 0) {
          stopTick -= 1
        } else {
          stopTick = 18
          println(s"Look left")
        }
      }
      case (false, true) => {
        camera.doRotate(fromEulerAngular(0, -0.01, 0))
        if (stopTick > 0) {
          stopTick -= 1
        } else {
          stopTick = 18
          println(s"Look right")
        }
      }
      case _ => {}
    }
    
    // camera
    camera.resetBackground(None)
    camera.pointColor = 0xfff0f000
    camera.rawDraw(
      ( relatedPosition + RawVector.zeroPoint.toVector ) ::
          ( relatedPosition + RawVector(0, -1, -1, 0).scalaExpand(300) ) ::
          ( relatedPosition + RawVector(0, 1, -1, 0).scalaExpand(200) ) :: Nil
    )
    
    // render
    camera.render.fillTo(platformDim.width, platformDim.height, relatedImage.getAlphaRaster.getDataBuffer.asInstanceOf[DataBufferInt].getData)
    frameCount += 1
    
    // render other
    val nowDuration = System.currentTimeMillis() - startP
    val fps0 = (frameCount / nowDuration * 1000).intValue
    relatedImage.getGraphics.drawString(s"fps: $fps0", 10, 10)
    
    // render final
    panel.getGraphics.drawImage(
      relatedImage,
      0,
      0,
      null
    )
    
    var nowT = System.currentTimeMillis()
    while (nowT < time + (1000 / 60)) {
      Thread.onSpinWait()
      nowT = System.currentTimeMillis()
    }
    time = nowT
  }
}

@main
def main$1() : Unit = {
  val fa = fromEulerAngular(1, Math.PI / 2, 1);
  val fa2 = fromEulerAngular(2, Math.PI / 2, 2);
  println(s"$fa")
  println(s"$fa2")
}

@main
def main$2() : Unit = {
  val cp = ParallelCamera.fromForward0(RawVector.zeroPoint, RawVector.zAxisUnit.negate(), width = 800, height = 600)
  println(s"$cp")
}