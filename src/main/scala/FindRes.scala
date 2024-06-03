package engine

import java.nio.channels.{Channel, Channels}

@main
def find() : Unit = {
    val cl = getClass()
    val r = cl.getResource("/Hey.text")
    val rr = cl.getResourceAsStream("/Hey.text")
    val s = r.openStream()
    val c = Channels.newChannel(s)
    c.close()
}


