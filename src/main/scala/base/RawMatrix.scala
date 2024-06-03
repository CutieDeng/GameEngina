package engine
package base

case class RawMatrix(private val array: Array[Double] = Array.ofDim(16)) {
    def get(i: Int, j: Int) : Double = {
        if (i < 0 || i > 4) {
           throw RuntimeException("Invalid idx")
        }
        if (j < 0 || j > 4) {
            throw RuntimeException("Invalid idx")
        }
        array(i * 4 + j)
    }
    def apply: (Int, Int) => Double = get
    
    override def toString: String = array.mkString("RawMatrix(", ", ", ")")
}

object RawMatrix {
    def zero() : RawMatrix = {
        RawMatrix()
    }
    def fromSetter(s: (Int, Int) => Double) : RawMatrix = {
        val r = RawMatrix()
        for (i <- 0 until 4) {
            for (j <- 0 until 4) {
                r.array(i * 4 + j) = s(i, j)
            }
        }
        r
    }
    def identity3D() : RawMatrix = {
        fromSetter((i, j) => if (i == j && i != 3) 1 else 0)
    }
}

def matrixMultiply(l: RawMatrix, r: RawMatrix): RawMatrix = {
    RawMatrix.fromSetter((i, j) => {
        var c : Double = 0
        for (k <- 0 until 4) {
            c += l.apply(i, k) * r.apply(k, j)
        }
        c
    })
}

def fromXRotate(a: Double) : RawMatrix = {
    val cos = Math.cos(a)
    val sin = Math.sin(a)
    RawMatrix.fromSetter({
        case (0, 0) => 1;
        case (1, 1) => cos;
        case (1, 2) => sin;
        case (2, 1) => -sin;
        case (2, 2) => cos;
        case _ => 0;
    })
}

def fromYRotate(a: Double) : RawMatrix = {
    val cos = Math.cos(a)
    val sin = Math.sin(a)
    RawMatrix.fromSetter({
        case (1, 1) => 1;
        case (0, 0) => cos;
        case (0, 2) => sin;
        case (2, 0) => -sin;
        case (2, 2) => cos;
        case _ => 0;
    })
}

def fromZRotate(a: Double) : RawMatrix = {
    val cos = Math.cos(a)
    val sin = Math.sin(a)
    RawMatrix.fromSetter({
        case (2, 2) => 1;
        case (0, 0) => cos;
        case (0, 1) => sin;
        case (1, 0) => -sin;
        case (1, 1) => cos;
        case _ => 0;
    })
}

def rawVectorGet(r: RawVector, i: Int) : Double = {
    i match {
        case 0 => r.x;
        case 1 => r.y;
        case 2 => r.z;
        case 3 => r.a;
    }
}

def rawVectorFromArray(ar: Array[Double]) : RawVector = {
    if (ar.length != 4) {
        throw RuntimeException("Invalid array length " + ar.length)
    }
    RawVector(
        ar(0),
        ar(1),
        ar(2),
        ar(3)
    )
}

def multiplyWithRawVector(rm: RawMatrix, rv: RawVector): RawVector = {
    val rst = Array.ofDim[Double](4)
    for (i <- 0 until 4) {
        for (j <- 0 until 4) {
            rst(i) += rm.apply(i, j) * rawVectorGet(rv, j)
        }
    }
    rawVectorFromArray(rst)
}

def rotateRawVector = multiplyWithRawVector

def fromEulerAngular(xAxisA: Double, yAxisA: Double, zAxisA: Double) : RawMatrix = {
    matrixMultiply(matrixMultiply(fromZRotate(zAxisA), fromYRotate(yAxisA)), fromXRotate(xAxisA))
}