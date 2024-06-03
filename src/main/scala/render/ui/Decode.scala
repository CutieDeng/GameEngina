package engine
package render.ui

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.{OpenOption, Paths, StandardOpenOption}

type StructureForMap = List[Int]

val offsetSubTable = 4 :: 2 :: 2 :: 2 :: 2 :: Nil
val offsetSubTableExplain = "scala type" :: "numTables" :: "searchRange" :: "entrySelector" :: "rangeShift" :: Nil

val baseTable = 4 :: 4 :: 4 :: 4 :: Nil
val baseTableExplain = "tag" :: "checkSum" :: "offset" :: "length" :: Nil

def decodeUnsafeAsMap(map: StructureForMap, buffer: ByteBuffer) : List[Long] = {
    var mapIt = map
    buffer.mark()
    var rstList = List[Long]()
    while (mapIt.nonEmpty) {
        mapIt match {
            case m :: apIt => {
                val outV = m match {
                    case 1 => {
                        val f = buffer.get()
                        if (f < 0) f.toLong + 0x100L else f.toLong
                    }
                    case 2 => {
                        val f = buffer.getShort()
                        if (f < 0) f.toLong + 0x10000L else f.toLong
                    }
                    case 4 => {
                        val f = buffer.getInt()
                        if (f < 0) f.toLong + 0x100000000L else f.toLong
                    }
                    case 8 => {
                        buffer.getLong()
                    }
                }
                rstList = outV :: rstList
                mapIt = apIt
            }
        }
    }
    return rstList.reverse
}

def decodeChannel(channel: FileChannel) : Unit = {
    val len = channel.size()
    if (len > Int.MaxValue) {
        throw RuntimeException(s"$len too large for mapping. ")
    }
    val mapper = channel.map(FileChannel.MapMode.READ_ONLY, 0, len)
    val map0 = decodeUnsafeAsMap(offsetSubTable, mapper)
    val dbg0 = map0 zip offsetSubTableExplain map ((v, s) => s"$s: $v")
    println(dbg0.reduce((a, b) => s"$a\n$b"))
    var map1 : List[String] = Nil
    val map0v = map0.toVector
    var map1raw : List[List[Long]] = Nil
    for (_ <- 0L until map0(1)) {
        val smap1 = decodeUnsafeAsMap(baseTable, mapper)
        val smap2 = smap1.zip(baseTableExplain).zipWithIndex.map({
            case ((l, s), 0) => String.format("%s: %c%c%c%c", s,
                ((l >> 24) % 256).toChar, ((l >> 16) % 256).toChar,
                ((l >> 8) % 256).toChar, ((l >> 0) % 256).toChar,
                )
            case ((l, s), i) => s"$s: $l"
        }).reduce((a, b) => s"$a; $b")
        map1 = smap2 :: map1
        map1raw = smap1 :: map1raw
    }
    for ((m, i) <- map1.reverse.zipWithIndex) {
        println(s"$i| $m")
    }
    // val cmapFilter = map1raw.map({ case })
    val cmapIdx = map1raw.zip(map1).filter((_, r) => r.contains("tag: cmap")).map((l, r) => l).map(v => (v(2), v(3)))
    cmapIdx match {
        case (cmapIdx, cmapLen) :: Nil
            if cmapIdx.isValidInt && cmapLen.isValidInt
            => {
            val subMapper = mapper.slice(cmapIdx.intValue(), cmapLen.intValue())
            val cmapIndex = 2 :: 2 :: Nil
            val cmapThis = decodeUnsafeAsMap(cmapIndex, subMapper)
            val cmapIndexExplain = "version" :: "numberSubtables" :: Nil
            println (cmapThis.zip(cmapIndexExplain).map((t, e) => s"$e: $t").reduce((a, b) => s"$a\n$b"))
            val subTableNum = cmapThis(1)
            val cmapSubTable = 2 :: 2 :: 4 :: Nil
            val cmapSubTableExplain = "platformID" :: "platformSpecificID" :: "offset" :: Nil
            for (_ <- 0L until subTableNum) {
                val cmapSubTableIt = decodeUnsafeAsMap(cmapSubTable, subMapper)
                println (cmapSubTableIt.zip(cmapSubTableExplain).map((i, e) => s"$e: $i").reduce((a, b) => s"$a\n$b"))
                val off = cmapSubTableIt(2)
                
            }
        }
    }
}

@main
def decode() : Unit = {
    println(s"Decode method start. ")
    val filePath = Paths.get( "/Library/Fonts/Arial Unicode.ttf" )
    val fileChannel = FileChannel.open(filePath, StandardOpenOption.READ)
    decodeChannel(fileChannel)
}

