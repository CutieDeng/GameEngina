package engine
package render

import render.RenderedImage.{HEIGHT, WIDTH}

import java.awt.image.{BufferedImage, DataBufferInt}

object RenderedImage {
    val CONST: Int = 2048
    val WIDTH: Int = CONST
    val HEIGHT: Int = CONST
}

// default width and height
case class RenderedImage() {
    val image : BufferedImage = BufferedImage(WIDTH, HEIGHT, BufferedImage.TYPE_INT_ARGB)
    val screen : Array[Int] = image.getAlphaRaster.getDataBuffer.asInstanceOf[DataBufferInt].getData
    def update(idx: (Int, Int), value: Int) : Unit = {
        idx match {
            case (i, j) => {
                if (i < 0 || i > HEIGHT) {
                    throw RuntimeException()
                }
                if (j < 0 || j > WIDTH) {
                    throw RuntimeException()
                }
                screen(i * WIDTH + j) = value
            }
        }
    }
}

extension (r: RenderedImage) {
    def fillArgb(argb: Int) : Unit = {
        for (i <- r.screen.indices) {
            r.screen(i) = argb
        }
    }
    def fillTo(width: Int, height: Int, data: Array[Int]) : Unit = {
        val wRate = WIDTH / width.floatValue
        val hRate = HEIGHT / height.floatValue
        for (i <- 0 until height) {
            for (j <- 0 until width) {
                val idx = i * width + j
                val w = (j * wRate).intValue
                val h = (i * hRate).intValue
                val idx2 = h * WIDTH + w
                data(idx) = r.screen(idx2)
            }
        }
    }
}
