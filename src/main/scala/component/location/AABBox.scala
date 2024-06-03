package engine
package component.location

import base.RawVector
import base.*

case class AABBox(minR: RawVector, maxR: RawVector) {

}

object AABBox {
    def apply(minR: RawVector, maxR: RawVector): AABBox = {
        assert( minR.isPoint )
        assert( maxR.isPoint )
        assert( minR.x <= maxR.x )
        assert( minR.y <= maxR.y )
        assert( minR.z <= maxR.z )
        new AABBox(minR, maxR)
    }
}

extension (point: RawVector) {
    def isInAABBox(aabBox: AABBox): Boolean *: Boolean *: Boolean *: EmptyTuple = {
        val a = point.x >= aabBox.minR.x && point.x <= aabBox.maxR.x
        val b = point.y >= aabBox.minR.y && point.y <= aabBox.maxR.y
        val c = point.z >= aabBox.minR.z && point.z <= aabBox.maxR.z
        (a, b, c)
    }
}

extension (aabBox: AABBox) {
    def intersects(aabBox2: AABBox) : Boolean = {
        val l = aabBox.minR.isInAABBox(aabBox2)
        val l2 = aabBox2.minR.isInAABBox(aabBox)
        !l.toList.zip(l2.toList).map((a, b) => a || b).exists(!_)
    }
}

@main
def inter() : Unit = {
    val a = AABBox(RawVector.zeroPoint, RawVector.fromLocation(5, 1, 1))
    val b = AABBox(RawVector.zeroPoint, RawVector.fromLocation(5, 1, 1))
    val c = AABBox(RawVector.fromLocation(-1, -1, 0), RawVector.fromLocation(-0.5, 0, 0))
    println(s"$a & $b ? ${a.intersects(b)}")
    val cb = c.intersects(b)
    println(s"$cb")
}