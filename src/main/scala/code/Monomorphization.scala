package code

import core.Bindings.*
import core.Context.*

object Monomorphization {

  /** Entry point: replace generic function invocations with monomorphic
    * specialized functions.
    *
    * Delegates to MonoDriver.monomorphize which implements a worklist-based
    * three-phase architecture (discover, specialize, rewrite).
    */
  def replace(binds: List[Bind]): List[Bind] =
    MonoDriver.monomorphize(binds).runEmptyA.value
}
