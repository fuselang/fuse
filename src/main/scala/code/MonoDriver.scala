package code

import cats.data.State
import cats.implicits.*
import core.Bindings.*
import core.Context.*
import core.Instantiations
import core.Instantiations.Instantiation
import core.Instantiations.Resolution
import core.Shifting.*
import core.Terms.*
import core.Types.*
import code.GrinUtils.toContextState
import code.MonoTypes.*
import code.MonoSpecialize.*
import code.MonoRewrite.*

object MonoDriver {

  /** Main entry point: monomorphize all binds using a worklist-based approach.
    *
    * Three-phase architecture:
    *   1. Discover MonoItems from concrete binds
    *   2. Specialize each item (producing new binds + transitive items)
    *   3. Rewrite references to use specialized versions
    *
    * The worklist loop continues until no new items are discovered.
    */
  def monomorphize(binds: List[Bind]): ContextState[List[Bind]] =
    for {
      initialItems <- MonoDiscovery.discoverItems(binds, binds.length)
      savedGenerics = MonoDiscovery.collectGenericBinds(binds)
      allBindNames = binds.map(_.i).toSet ++ savedGenerics.keySet
      result <- processWorklist(
        initialItems,
        Set.empty,
        binds,
        savedGenerics,
        allBindNames
      )
    } yield result

  /** Process the worklist iteratively until no new items remain. */
  def processWorklist(
      worklist: List[MonoItem],
      processed: Set[String],
      binds: List[Bind],
      savedGenericBinds: Map[String, Bind],
      allBindNames: Set[String]
  ): ContextState[List[Bind]] = {
    val pending =
      worklist.filterNot(item => processed.contains(item.specializedName))
    pending match {
      case Nil   => binds.pure[ContextState]
      case items =>
        val resolvableInsts = Instantiations.distinct(
          MonoTypes.toInstantiations(items)
        )
        val newSavedGenericBinds =
          MonoDiscovery.collectGenericBinds(binds, savedGenericBinds)
        val newAllBindNames =
          binds.map(_.i).toSet ++ newSavedGenericBinds.keySet
        for {
          (specializedBinds, shiftedGenericBinds) <- toSpecializedBinds(
            binds,
            resolvableInsts,
            newAllBindNames
          )
          bindsWithDataConstrSpecs <- createMissingDataConstrSpecs(
            specializedBinds,
            newSavedGenericBinds
          )
          // `createMissingDataConstrSpecs` inserts new data-constructor specs
          // into the bind list and shifts every bind after the insertion via
          // `shiftBindAfterInsert`. The `shiftedGenericBinds` map produced by
          // `toSpecializedBinds` predates these insertions — its entries are
          // stale copies whose stored TypeVar `n` stamps lag the post-insert
          // ctx length by the number of data-constructor specs inserted before
          // the entry's source position. Refresh the map by re-locating each
          // generic in the NEW bind list, so deferred specs in
          // `createMissingGenericSpecs` are built from a generic whose body has
          // ALL pre-insert and data-constructor-insert shifts applied. Without
          // this, `typeShiftOnContextDiff` at GRIN time over-compensates by the
          // missing data-constructor delta, drifting e.g. `List` references in
          // a Nil-armed bind's transitive spec to `read`.
          refreshedShiftedGenericBinds = shiftedGenericBinds.map {
            case (name, staleBind) =>
              name -> bindsWithDataConstrSpecs
                .find(_.i == name)
                .getOrElse(staleBind)
          }
          bindsWithDeferredSpecs <- createMissingGenericSpecs(
            bindsWithDataConstrSpecs,
            newSavedGenericBinds,
            refreshedShiftedGenericBinds
          )
          modifiedBinds <- bindsWithDeferredSpecs.traverse(
            replaceInstantiations(_, bindsWithDeferredSpecs)
          )
          newItems <- MonoDiscovery.discoverItems(
            modifiedBinds,
            modifiedBinds.length
          )
          newProcessed = processed ++ items.map(_.specializedName)
          result <- processWorklist(
            newItems,
            newProcessed,
            modifiedBinds,
            newSavedGenericBinds,
            newAllBindNames
          )
        } yield result
    }
  }
}
