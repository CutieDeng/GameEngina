package engine
package orderconstraint

sealed trait OrderTag (using val ctx: OrderContext) {
    private def check(o: OrderTag) : Unit = {
        if (this eq o) {
            throw OrderTagOrdersException(this :: Nil)
        }
        if (!(ctx eq o.ctx)) {
            throw OrderContextMismatchException(ctx :: o.ctx :: Nil)
        }
    }
    private def asBefore(other: OrderTag) : Unit = {
        this check other
        ctx.orders += (this, other)
    }
    def before(other: OrderTag) : OrderTag = {
        this.asBefore(other)
        other
    }
    private def asAfter(other: OrderTag) : Unit = {
        this check other
        ctx.orders += (other, this)
    }
    def after(other: OrderTag) : OrderTag = {
        this.asAfter(other)
        other
    }
}

class Unnamed(using ctx: OrderContext) extends OrderTag { }

object Unnamed {
    def apply()(using ctx: OrderContext): Unnamed = {
        val u = new Unnamed()
        ctx.ctx += u
        u
    }
}

case class Named(name: String)(using ctx: OrderContext) extends OrderTag { }

object Named {
    def apply(name: String)(using ctx: OrderContext) : Named = {
        ctx.namedCtxCache.get(name) match {
            case Some(value) => value
            case None => {
                val rst = new Named(name)
                ctx.namedCtxCache += (name, rst)
                ctx.ctx += rst
                rst
            }
        }
    }
}

case class Predefined(priority: Int)(using ctx: OrderContext) extends OrderTag { }

object Predefined {
    def apply(priority: Int)(using ctx: OrderContext) : Predefined = {
        ctx.priCtxCache.get(priority) match {
            case Some(value) => value;
            case None => {
                var rst = new Predefined(priority)
                ctx.priCtxCache += (priority, rst)
                ctx.ctx += rst
                rst
            }
        }
    }
}
