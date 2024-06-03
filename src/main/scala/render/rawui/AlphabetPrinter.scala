package engine
package render.rawui

import java.nio.file.{Files, Paths}

case class AlphabetPrinter(width: Int, height: Int, contents: Array[Boolean]) {
    assert( contents.length == width * height * (26 * 2 + 10))
    val elementSize: Int = width * height
    def getCodeRange(codePoint: Int) : (Int, Int) = {
        val idx =
        if (codePoint >= '0' && codePoint <= '9') {
            codePoint - '0'
        } else if (codePoint >= 'a' && codePoint <= 'z') {
            codePoint - 'a' + 10
        } else if (codePoint >= 'A' && codePoint <= 'Z') {
            codePoint - 'A' + 36
        } else {
            throw RuntimeException()
        }
        (idx * elementSize, (idx + 1) * elementSize)
    }
}

object AlphabetPrinter {
    def loadFromLines(linesIt: Iterator[String]) : AlphabetPrinter = {
        val firstLine = linesIt.next()
        val secondLine = linesIt.next()
        val width = firstLine.toInt
        val height = secondLine.toInt
        val rawContent = Array.ofDim[Boolean](width * height * (26 * 2 + 10))
        assert( linesIt.next().isEmpty )
        var groupers : List[List[String]] = Nil
        var currentGroup : List[String] = Nil
        for (line <- linesIt) {
            if (line.isEmpty) {
                groupers = currentGroup.reverse :: groupers
                currentGroup = Nil
            } else {
                currentGroup = line :: currentGroup
            }
        }
        val contentSplit = groupers.reverse
        val as = (contentSplit.length == 26 * 2 + 10)
        if (!as) {
            println(s"split length: ${contentSplit.length}")
        }
        assert(as)
        for ((i0, content) <- contentSplit.indices.zip(contentSplit)) {
            assert (content.length == height)
            for ((v, c) <- content.indices.zip(content)) {
                val cp = c.codePoints().toArray
                assert (cp.length == width, s"cp.length expected $width, but ${cp.length}, on idx $i0, height: $v")
                for (c <- cp.indices) {
                    val idx = i0 * width * height + v * width + c
                    cp(c) match {
                        case ' ' => {
                            rawContent(idx) = false
                        }
                        case 'O' => {
                            rawContent(idx) = true
                        }
                    }
                }
            }
        }
        AlphabetPrinter(width, height, rawContent)
    }
}

def printOnImageByString(image: Array[Int], width: Int, height: Int,
                  printer: AlphabetPrinter, codePoint: Int, xStart: Double, yStart: Double, xScale: Double, yScale: Double,
                  color: Int, backColor: Option[Int]) : Unit = {
    
    val widthF : Double = width
    val heightF : Double = height
    
    val xEnd : Int = ((printer.width * xScale) + xStart).intValue() min width
    val yEnd : Int = ((printer.height * yScale) + yStart).intValue() min height
    
    for (y <- yStart.intValue until yEnd;
        x <- xStart.intValue until xEnd) {
        // println(s"DEBUG: $x; $y")
        val oriValue = image(x + y * width)
        // related value
        val xRelative = ((x - xStart) / xScale).intValue
        val yRelative = ((y - yStart) / yScale).intValue
        val (start, end) = printer.getCodeRange(codePoint)
        val idx = start + xRelative + yRelative * printer.width
        val isPixel = printer.contents(idx)
        if (isPixel) {
            image(x + y * width) = color
        } else {
            backColor match {
                case Some(color) => image(x + y * width) = color
                case _ => {}
            }
        }
    }
    return
}


@main
def base0() : Unit = {
    println(s"hey base. ")
    val ff = Paths.get("res", "fontdata")
    val lds : Array[String] = Files.readAllLines(ff).toArray(Array.ofDim[String](0))
    val a = AlphabetPrinter.loadFromLines(lds.iterator)
    println(s"$a")
}
