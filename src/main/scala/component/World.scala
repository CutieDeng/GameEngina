package engine
package component

import scala.collection.immutable.HashSet

// quote a lot of interesting thing here 
class World extends BaseConfig {
    var entities: Set[Entity] = Set() 
    // var components: Set[Component] = Set.empty
}

trait BaseConfig {
    
}
