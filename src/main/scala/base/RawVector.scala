package engine
package base

import scala.annotation.targetName

case class RawVector(x: Double, y: Double, z: Double, a: Double)

object RawVector {
    def apply(x: Double, y: Double, z: Double, a: Double) : RawVector = {
        if (x.isNaN || y.isNaN || z.isNaN || a.isNaN) {
            throw RuntimeException("NaN on RawVector")
        }
        new RawVector(x, y, z, a)
    }
    val zeroPoint: RawVector = RawVector(0, 0, 0, 1)
    val xAxisUnit: RawVector = RawVector(1, 0, 0, 0)
    val yAxisUnit: RawVector = RawVector(0, 1, 0, 0)
    val zAxisUnit: RawVector = RawVector(0, 0, 1, 0)
    def fromLocation(x: Double, y: Double, z: Double) : RawVector = {
        RawVector(x, y, z, 1)
    }
    def fromVector(x: Double, y: Double, z: Double) : RawVector = {
        RawVector(x, y, z, 0) 
    }
}

extension (rv: RawVector) {
    def normalize() : RawVector = {
        if (rv.a == 0 || rv.a == 1) {
            return rv
        }
        RawVector(rv.x / rv.a, rv.y / rv.a, rv.z / rv.a, 1.0)
    }
    @targetName("plus")
    def +(other: RawVector) : RawVector = {
        RawVector(rv.x + other.x, rv.y + other.y, rv.z + other.z, rv.a + other.a).normalize()
    }
    def negate(): RawVector = {
        RawVector(-rv.x, -rv.y, -rv.z, rv.a)
    }
    @targetName("minus")
    def -(other: RawVector) : RawVector = {
        rv + (other.negate())
    }
    def isPoint: Boolean = rv.a != 0
    def isVector: Boolean = !isPoint
    def sum(): Double = {
        val n = normalize()
        n.x + n.y + n.z
    }
    @targetName("multiply")
    def *(other: RawVector) : RawVector = RawVector(rv.x * other.x, rv.y * other.y, rv.z * other.z, rv.a * other.a)
    def scalaExpand(f: Double) : RawVector = RawVector(rv.x * f, rv.y * f, rv.z * f, rv.a)
    def lengthSquare(): Double = {
        if (!isVector) {
            throw RuntimeException("length method only support the vector type. ")
        }
        val n = normalize()
        n.x * n.x + n.y * n.y + n.z * n.z
    }
    def length(): Double = {
        val ls = lengthSquare()
        math.sqrt(ls)
    }
    def toVector: RawVector = {
        if (isVector) {
            rv
        } else {
            RawVector(rv.x, rv.y, rv.z, 0)
        }
    }
    def toPoint: RawVector = {
        if (isPoint) {
            rv
        } else {
            RawVector.zeroPoint + rv
        }
    }
    @targetName("cross")
    def **(other: RawVector) : RawVector = {
        if (!(isVector && other.isVector)) {
            throw RuntimeException("Cross Method, only support the vector type. ")
        }
        return RawVector(
            rv.y * other.z - rv.z * other.y,
            rv.z * other.x - rv.x * other.z,
            rv.x * other.y - rv.y * other.x,
            0,
        )
    }
    def normalizeLength() : RawVector = {
        val n = normalize()
        val l = n.length() 
        RawVector(n.x / l, n.y / l, n.z / l, n.a)
    }
    def rotateOnXAxis(a: Double) : RawVector = {
        val c = math.cos(a)
        val s = math.sin(a) 
        RawVector(rv.x, c * rv.y + s * rv.z, c * rv.z - s * rv.y, rv.a)
    }
    def rotateOnYAxis(a: Double) : RawVector = {
        val c = math.cos(a)
        val s = math.sin(a)
        RawVector(c * rv.x + s * rv.z, rv.y, c * rv.z - s * rv.x, rv.a)
    }
    def rotateOnZAxis(a: Double) : RawVector = {
        val c = math.cos(a)
        val s = math.sin(a)
        RawVector(c * rv.x - s * rv.y, c * rv.y + s * rv.x, rv.z, rv.a)
    }
    def distance2(other: RawVector) : Double = {
        if ((isVector ^ other.isVector)) {
            throw RuntimeException("Unmatched distance method on operands. ")
        }
        val d = rv - other
        val d2 = d.toVector
        d2.lengthSquare()
    }
    def distance(other: RawVector) : Double = math.sqrt(distance2(other))
}
