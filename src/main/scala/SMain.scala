package engine

import jdk.jfr.MemoryAddress

import java.lang.foreign.Arena.ofConfined
import java.lang.foreign.{Arena, FunctionDescriptor, Linker, MemoryLayout, MemorySegment, ValueLayout}
import scala.util.Using

@main
def sMain() : Unit = {
    // val linker = Linker.nativeLinker()
    val linker = Linker.nativeLinker()
    val lookup = linker.defaultLookup()
    val radixSortSeg = lookup.find("radixsort").get()
    println(s"$radixSortSeg")
    val fde = FunctionDescriptor.ofVoid(ValueLayout.ADDRESS, ValueLayout.JAVA_INT, ValueLayout.ADDRESS, ValueLayout.JAVA_CHAR)
    val handle = linker.downcallHandle(radixSortSeg, fde)
    println(s"$handle")
    val javaStrings = Array("mouse", "cat", "dog", "car")
    Using(ofConfined()) { offHeap =>
        val pointers = offHeap.allocateArray(ValueLayout.ADDRESS, javaStrings.length)
        for ((elem, idx) <- javaStrings.iterator.zipWithIndex) {
            val cStr = offHeap.allocateUtf8String(elem)
            println(s"Assign [$idx] = $elem; $cStr")
            pointers.setAtIndex(ValueLayout.ADDRESS, idx, cStr)
        }
        // handle.invoke(pointers, javaStrings.length, MemorySegment.NULL, 0.toChar)
        for (i <- javaStrings.indices) {
            val cStrI = pointers.getAtIndex(ValueLayout.ADDRESS, i)
            println(s"current [$i] = $cStrI")
            javaStrings(i) = cStrI.getUtf8String(0)
        }
        println(s"${javaStrings.mkString("Array(", ", ", ")")}")
    }.recover( { err =>
        println(s":: $err")
        println(s"${err.getStackTrace.mkString(":: \n ", "\n ", "")}")
    })
}

val linker = Linker.nativeLinker()
val lookup = linker.defaultLookup()

@main
def sMain2() : Unit = {
    val strlenHandle = linker.downcallHandle(lookup.find("fork").get(), FunctionDescriptor.of(ValueLayout.JAVA_INT))
    strlenHandle.invoke()
    println(s"Hello World")
    Using(Arena.ofConfined()) { heap =>
    
    }.recover({ err =>
        println(s"$err")
    })
}


