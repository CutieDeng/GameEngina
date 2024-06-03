package engine
package component

class Entity (val name: String) (using world: World) {
    var components: Set[Component] = Set.empty 
    // var typedComponents: Map[Class[?], Component] 
}

object Entity { 
    def apply() : Entity = {
        // handle it 
        
        ??? 
    }
}
