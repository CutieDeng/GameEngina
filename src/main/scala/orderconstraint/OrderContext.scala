package engine
package orderconstraint

case class OrderContext (var ctx: Set[OrderTag], var namedCtxCache: Map[String, Named], var priCtxCache: Map[Int, Predefined], var orders: Set[(OrderTag, OrderTag)],
                        var orderMapBinding: OrderMap | Null
                        ) { }

object OrderContext {
    def apply() : OrderContext = {
        new OrderContext(Set.empty, Map.empty, Map.empty, Set.empty, null)
    }
}

class OrderException extends Exception

case class OrderTagOrdersException(relatedTags: List[OrderTag]) extends OrderException {

}

case class OrderContextMismatchException(ctx: List[OrderContext]) extends OrderException { }
