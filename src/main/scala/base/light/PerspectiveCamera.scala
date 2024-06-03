package engine
package base.light

import base.*
import render.*

case class PerspectiveCamera(render: RenderedImage, var point: RawVector, var ignoreDistance: Double, var edge0: RawVector, var edge1: RawVector,
                             var defaultColor: Int) {
    if (!(point.isPoint && edge0.isVector && edge1.isVector)) {
        throw RuntimeException("Invalid camera parameters. ")
    }
    var normalVector: RawVector = null
    flushNormalVector()
    
    var pointColor: Int = defaultColor
    
    var tick : Int = 60
    
    def flushNormalVector(): Unit = {
        normalVector = (edge1 ** edge0).normalizeLength()
    }
    
    def rawDraw(points: List[RawVector]): Unit = points match {
        case a :: b :: c :: d => {
            val pts = a :: b :: c :: Nil
            val pts2 = pts.map { f => {
                if (f.isVector) {
                    throw RuntimeException("Use vector in draw point method. ")
                }
                (f - point).toVector
            }
            }
            val e0Len2 = edge0.lengthSquare()
            val e1Len2 = edge1.lengthSquare()
            val len3 = pts2 exists { f => {
                (f * normalVector).sum() < ignoreDistance
            }}
            if (!len3) {
                val pts3 = pts2 map { f =>
                    val f2 = f * edge0
                    val f3 = f2.sum()
                    val f4 = f3 / e0Len2
                    val g2 = f * edge1
                    val g3 = g2.sum()
                    val g4 = g3 / e1Len2
                    
                    val rst0 = RawVector(f4, g4, 0, 0)
                    
                    val k2 = (f * normalVector).sum()
                    // val k3 = normalVector.scalaExpand(k2)
                    
                    rst0.scalaExpand(1 / k2)
                } map {
                    case RawVector(x, y, z, a) => RawVector(x + 0.5, - y + 0.5, z, a)
                }
                rawDrawRelativeUnsafe(pts3)
            } else {
                // handle here )
                if (tick > 0) {
                    tick -= 1
                } else {
                    tick = 60
                    println(s"[Warning] invalid render for $pts")
                    println(s"[normal Vector]: $normalVector")
                }
            }
            rawDraw(d)
        }
        case Nil => {}
    }
    
    private def rawDrawRelativeUnsafe(points: List[RawVector]): Unit = points match {
        case a :: b :: c :: Nil => {
            if (a.x > b.x) {
                return rawDrawRelativeUnsafe(b :: a :: c :: Nil)
            }
            if (a.x > c.x) {
                return rawDrawRelativeUnsafe(c :: b :: a :: Nil)
            }
            val kb = (b.y - a.y) / (b.x - a.x)
            val kc = (c.y - a.y) / (c.x - a.x)
            if (kb < kc) {
                return rawDrawRelativeUnsafe(a :: c :: b :: Nil)
            }
            if (b.x <= c.x) {
                if (a.x >= 1) return
                if (c.x < 0) return
                if (a.y < 0 && b.y < 0 && c.y < 0) return
                if (a.y >= 1 && c.y >= 1) return
                val xLeftStart: Int = if (a.x < 0) 0 else (a.x * RenderedImage.WIDTH).intValue()
                val xMid: Int = if (b.x <= 1) (b.x * RenderedImage.WIDTH).intValue() else RenderedImage.WIDTH
                for (x <- xLeftStart until xMid) {
                    val xPercent = x.doubleValue / RenderedImage.WIDTH
                    val yHigh = ((kb * (xPercent - a.x) + a.y) * RenderedImage.HEIGHT).intValue
                    val yBelow = ((kc * (xPercent - a.x) + a.y) * RenderedImage.HEIGHT).intValue
                    for (y <- yBelow until yHigh) {
                        try {
                            render((y, x)) = pointColor
                        } catch {
                            case _: RuntimeException => {}
                        }
                    }
                }
                if (b.x >= 1) return
                val xRightEnd: Int = if (c.x > 1) RenderedImage.WIDTH else (c.x * RenderedImage.WIDTH).intValue
                val kbc = (c.y - b.y) / (c.x - b.x)
                for (x <- xMid until xRightEnd) {
                    val xPercent = x.doubleValue / RenderedImage.WIDTH
                    val yHigh = ((kbc * (xPercent - b.x) + b.y) * RenderedImage.HEIGHT).intValue
                    val yBelow = ((kc * (xPercent - a.x) + a.y) * RenderedImage.HEIGHT).intValue
                    for (y <- yBelow until yHigh) {
                        try {
                            render((y, x)) = pointColor
                        } catch {
                            case _: RuntimeException => {}
                        }
                    }
                }
            } else {
                if (a.x >= 1) return
                if (b.x < 0) return
                if (a.y < 0 && b.y < 0) return
                if (a.y >= 1 && c.y >= 1 && b.y >= 1) return
                val xLeftStart: Int = if (a.x < 0) 0 else (a.x * RenderedImage.WIDTH).intValue()
                val xMid: Int = if (c.x <= 1) (c.x * RenderedImage.WIDTH).intValue() else RenderedImage.WIDTH
                for (x <- xLeftStart until xMid) {
                    val xPercent = x.doubleValue / RenderedImage.WIDTH
                    val yHigh = ((kb * (xPercent - a.x) + a.y) * RenderedImage.HEIGHT).intValue
                    val yBelow = ((kc * (xPercent - a.x) + a.y) * RenderedImage.HEIGHT).intValue
                    for (y <- yBelow until yHigh) {
                        try {
                            render((y, x)) = pointColor
                        } catch {
                            case _: RuntimeException => {}
                        }
                    }
                }
                if (c.x >= 1) return
                val xRightEnd: Int = if (b.x > 1) RenderedImage.WIDTH else (b.x * RenderedImage.WIDTH).intValue
                val kbc = (c.y - b.y) / (c.x - b.x)
                for (x <- xMid until xRightEnd) {
                    val xPercent = x.doubleValue / RenderedImage.WIDTH
                    val yBelow = ((kbc * (xPercent - b.x) + b.y) * RenderedImage.HEIGHT).intValue
                    val yHigh = ((kb * (xPercent - a.x) + a.y) * RenderedImage.HEIGHT).intValue
                    for (y <- yBelow until yHigh) {
                        try {
                            render((y, x)) = pointColor
                        } catch {
                            case _: RuntimeException => {}
                        }
                    }
                }
            }
        }
    }
    
    def doRotate(rotator: RawMatrix): Unit = {
        edge0 = multiplyWithRawVector(rotator, edge0)
        edge1 = multiplyWithRawVector(rotator, edge1)
        flushNormalVector()
    }
    
    def resetBackground(argb: Option[Int]): Unit = {
        render.fillArgb(argb.getOrElse(defaultColor))
    }
    
}

object PerspectiveCamera {
    /// When runtimeException, use z axis as the forward direction here.
    def fromForward0(p: RawVector, f: RawVector, defaultColor: Int = 0xff000000, width: Double, height: Double): PerspectiveCamera = {
        if (!f.isVector) {
            throw RuntimeException("Invalid forward target is not Vector. ")
        }
        val up = RawVector.yAxisUnit
        val cross = (f ** up).normalizeLength()
        val up0 = (cross ** f).normalizeLength()
        PerspectiveCamera(
            RenderedImage(),
            p,
            0,
            cross.scalaExpand(width),
            up0.scalaExpand(height),
            defaultColor
        )
    }
}