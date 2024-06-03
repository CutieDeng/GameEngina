package engine

import base.input.Query
import render.*
import render.rawui.AlphabetPrinter

import java.awt.Toolkit
import java.awt.event.{KeyAdapter, KeyEvent}
import java.awt.image.{BufferedImage, DataBufferInt}
import java.nio.file.{Files, Paths}
import javax.swing.{JFrame, JPanel, WindowConstants}
import scala.util.Random

@main
def pureTextRender() : Unit = {
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
    val renderImage = RenderedImage()
    val query = Query()
    val keyEventClass = classOf[KeyEvent]
    val ars : Vector[Int] = keyEventClass.getFields.filter { f => f.getName.startsWith("VK") }.map( f => f.get(null).asInstanceOf[Int] ).toVector
    val preLoads = (0 until 10).map(i => s"VK_$i").map(s => keyEventClass.getField(s)).map(s => s.get(null).asInstanceOf[Int]).toArray
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
    
    var contexter : Vector[Vector[Int]] = Vector.empty.appended(Vector.empty)
    val printer = AlphabetPrinter.loadFromLines(Files.readAllLines(Paths.get("res", "fontdata")).toArray(Array.ofDim[String](0)).iterator)
    
    val flushBuffer = Array.ofDim[Double](12)
    
    val xStart = 30
    val yStart = 40
    
    var lastEps = 0.05
    var position = (0, 0)
    
    while (true) {
        
        // input
        val h = handleInput(query, preLoads, flushBuffer, lastEps, 0.16, contexter, position)
        contexter = h(0)
        position = h(1)
        
        renderImage.fillArgb(0xff000000)
        
        for ((j, oneLineContexter) <- contexter.indices.zip(contexter)) {
            for ((i, c) <- oneLineContexter.indices.zip(oneLineContexter)) {
                rawui.printOnImageByString(
                    renderImage.screen,
                    renderImage.image.getWidth,
                    renderImage.image.getHeight,
                    printer,
                    c,
                    (xStart + i * 15),
                    (yStart + j * 29),
                    3.0,
                    3.0,
                    0xffffffff,
                    None,
                )
            }
        }
        
        // render on related Image
        renderImage.fillTo(platformDim.width, platformDim.height, relatedImage.getAlphaRaster.getDataBuffer.asInstanceOf[DataBufferInt].getData)
        
        // render
        frameCount += 1
        val nowDuration = System.currentTimeMillis() - startP
        val fps0 = (frameCount / nowDuration * 1000).intValue
        relatedImage.getGraphics.drawString(s"fps: $fps0", 20, 14)
        
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
        lastEps = (nowT - time) * 0.001
        time = nowT
    }
}

def handleInput(inputQuery: Query, vk_numbers: Array[Int], toFlush: Array[Double], eps: Double, threshold : Double, field: Vector[Vector[Int]], position: (Int, Int) ) : (Vector[Vector[Int]], (Int, Int)) = {
    assert ( vk_numbers.length == 10 )
    assert ( threshold > 0 )
    // value here
    var effect : List[Int] = Nil
    for ((i, v) <- vk_numbers.indices.zip(vk_numbers)) {
        val g = inputQuery.map(v).get()
        if (g) {
            if (toFlush(i) > 0) {
                toFlush(i) -= eps
            } else {
                // make it effective
                toFlush(i) = threshold
                effect = i :: effect
            }
        } else {
            toFlush(i) = 0
        }
    }
    var rst = field
    var pos = position
    for (e <- effect.reverse) {
        var f = rst(pos(0))
        f = f.patch(pos(1), Seq(e + '0'), 0)
        rst = rst.updated(pos(0), f)
        pos = (pos(0), pos(1) + 1)
    }
    // delete one
    if (inputQuery.map(KeyEvent.VK_BACK_SPACE).get()) {
        if (pos(1) > 0) {
            if (toFlush(10) > 0) {
                toFlush(10) -= eps
            } else {
                toFlush(10) = threshold
                var f = rst(pos(0))
                f = f.patch(pos(1) - 1, Seq(), 1)
                rst = rst.updated(pos(0), f)
                pos = (pos(0), pos(1) - 1)
            }
        } else {
            val curLine = pos(0)
            if (curLine < rst.length) {
                toFlush(10) = threshold
                rst = rst.patch(pos(0), Seq(), 1)
                if (pos(0) != 0) {
                    pos = (pos(0) - 1, rst(pos(0) - 1).length)
                }
            } else {
                toFlush(10) = 0
            }
        }
    } else {
        toFlush(10) = 0
    }
    if (inputQuery.map(KeyEvent.VK_ENTER).get()) {
        if (toFlush(11) > 0) {
            toFlush(11) -= eps
        } else {
            toFlush(11) = threshold
            rst = rst.patch(pos(0) + 1, Seq(Vector.empty[Int]), 0)
            pos = (pos(0) + 1, 0)
        }
    } else {
        toFlush(11) = 0
    }
    (rst, pos)
}
