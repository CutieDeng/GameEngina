package engine
package base.input

import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.immutable.HashMap

case class Query(var map: scala.collection.immutable.HashMap[Int, AtomicBoolean] = HashMap()) {
    def initRelatedInt(code: Int): Unit = {
        map = map.updated(code, AtomicBoolean(false))
    }
}


