package engine
package orderconstraint

import scala.collection.mutable

case class OrderMap(path: Vector[OrderTag])(using var ctx: OrderContext | Null) {
    def isValid: Boolean = ctx != null
}

//noinspection RedundantBlock
object OrderMap {
    def apply()(using ctx: OrderContext) : OrderMap = {
        val inDegree = mutable.HashMap[OrderTag, Integer]()
        val outCache = mutable.HashMap[OrderTag, List[OrderTag]]()
        for (o <- ctx.ctx) {
            inDegree.update(o, 0)
        }
        for (o <- ctx.ctx) {
            outCache.update(o, Nil)
        }
        for ((p, n) <- ctx.orders) {
            val v = inDegree(p) + 1
            inDegree.update(n, v)
        }
        for ((p, n) <- ctx.orders) {
            var pp = outCache(p)
            pp = n :: pp
            outCache.update(p, pp)
        }
        val builder = mutable.PriorityQueue.newBuilder[(Int, OrderTag)](Ordering.by((_: (Int, OrderTag))._1).reverse)
        for ((o, v) <- inDegree) {
           builder.addOne((v, o))
        }
        val normalHeap = builder.result()
        val numberOrders = mutable.PriorityQueue.newBuilder[(Int, Predefined)](Ordering.by((_: (Int, Predefined))._1).reverse)
        for ((v, o) <- ctx.priCtxCache) {
            numberOrders.addOne((v, o))
        }
        val numberHeap = numberOrders.result()
        var normalHeapAlreadyOut = Set[OrderTag]()
        var zeroBuffer = Set[Predefined]()
        var rst: List[OrderTag] = Nil
        var flag: Boolean = true
        while (flag) {
            // check the normal heap
            normalHeap.headOption match {
                case Some(value) => {
                    if (value(0) == 0) {
                        normalHeap.dequeue()
                        // check it output or not before, if yes, ignore
                        val n = value._2
                        if (normalHeapAlreadyOut.contains(n)) {
                            // ignore directly
                        } else {
                            // check it as a correct with the number limitation
                            normalHeapAlreadyOut += n
                            n match {
                                case p: Predefined => {
                                    //
                                    zeroBuffer += p
                                }
                                case _ => {
                                    // normal situation, just pop it as the result here ~
                                    rst = n :: rst
                                    val outs = outCache(n)
                                    for (o <- outs) {
                                        val inD = inDegree(o) - 1
                                        inDegree(o) = inD
                                        normalHeap.addOne((inD, o))
                                    }
                                }
                            }
                        }
                    } else {
                        // normal heap is needless.
                        flag = false
                    }
                }
                case None => {
                    flag = false
                }
            }
            numberHeap.headOption match {
                case Some((h, p)) => {
                    if (zeroBuffer.contains(p)) {
                        numberHeap.dequeue()
                        flag = true
                        rst = p :: rst
                        val outs = outCache(p)
                        for (o <- outs) {
                            val inD = inDegree(o) - 1
                            inDegree(o) = inD
                            normalHeap.addOne((inD, o))
                        }
                    }
                }
                case _ => {}
            }
        }
        if (rst.length != ctx.ctx.size) {
            // something still in the map can't be judged...
            // println(s"$rst; ${ctx.ctx}")
            throw OrderTagOrdersException(Nil)
        }
        val path = rst.reverse.toVector
        val ans = OrderMap(path)
        ctx.orderMapBinding match {
            case orderMap: OrderMap => {
                orderMap.ctx = null
            }
            case null => {}
        }
        ctx.orderMapBinding = ans
        ans
    }
}

@main
def test1() : Unit = {
    val ctx = OrderContext()
    given OrderContext = ctx
    println("test")
    val one = Predefined(1)
    val four = Predefined(4)
    val three = Named("3")
    one before four
    val judge = OrderMap()
    println(judge.path)
    println("---")
    three before one
    val judge2 = OrderMap()
    println(judge2.path)
    println(judge.isValid)
    println(judge2.isValid)
}