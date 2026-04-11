package code

import core.Bindings.*
import core.Types.*
import core.Instantiations.Instantiation

object MonoTypes {

  /** A work item representing one specialization to perform.
    *
    * MonoItem: each item says "specialize function X with types Y".
    */
  case class MonoItem(
      sourceName: String,
      concreteTys: List[Type],
      cls: List[TypeClass] = List(),
      specializedName: String,
      origin: MonoOrigin = MonoOrigin.TopLevel,
      isTraitDefault: Boolean = false,
      originalInst: Instantiation
  )

  /** Tracks where a MonoItem was discovered. */
  enum MonoOrigin {
    case TopLevel
    case Transitive(parent: String)
    case DeferredDataConstr
  }

  /** Extract the Instantiation list from a list of MonoItems (for interfacing
    * with bind-level code that uses Instantiation).
    */
  def toInstantiations(items: List[MonoItem]): List[Instantiation] =
    items.map(_.originalInst)
}
