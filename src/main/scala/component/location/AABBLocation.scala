package engine
package component.location

import base.*

given DEFAULT_LOCAL_WORLD: LocalWorld = LocalWorld()

case class AABBLocation(aabBox: AABBox) (using val localWorld: LocalWorld)

@main
def check(): Unit = {
    println("Start check")
    val a = AABBLocation(AABBox(RawVector.zeroPoint, RawVector.zeroPoint + RawVector.xAxisUnit))
    val b = AABBLocation(AABBox(RawVector.zeroPoint, RawVector.zeroPoint + RawVector.xAxisUnit))
    if (a.localWorld eq b.localWorld) {
        println(s"same world")
    } else {
        println(s"diff world")
    }
    println(s"test 1 end")
    val c = AABBLocation(AABBox(RawVector.zeroPoint, RawVector.zeroPoint + RawVector.xAxisUnit))(using LocalWorld())
    if (b.localWorld eq c.localWorld) {
        println(s"same world")
    } else {
        println(s"diff world")
        if (b.localWorld == c.localWorld) {
            println(s"equals pass")
        } else {
            println(s"equals also fails")
        }
    }
    println(s"test 2 end")
}