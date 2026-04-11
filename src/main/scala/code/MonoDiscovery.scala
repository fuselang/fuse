package code

import cats.data.State
import cats.implicits.*
import core.Bindings.*
import core.Context.*
import core.Desugar
import core.Instantiations
import core.Instantiations.{BindTypeSeparatorChar, Instantiation, Resolution}
import core.Desugar.SelfTypeName
import core.Terms.*
import code.GrinUtils.toContextState
import code.MonoTypes.*

object MonoDiscovery {

  /** Discover all MonoItems from concrete (non-generic) binds.
    *
    * This is Phase 1 of the worklist architecture.
    */
  def discoverItems(
      binds: List[Bind],
      numBinds: Int
  ): ContextState[List[MonoItem]] = {
    val concreteBindInsts = binds.collect {
      case b if !b.b.isInstanceOf[TermAbbBind]            => b.insts
      case b @ Bind(_, TermAbbBind(_: TermTAbs, _), _, _) => Nil
      case b                                              => b.insts
    }.flatten
    val allInsts = Instantiations.distinct(concreteBindInsts)
    val insts = allInsts.filter(i =>
      i.r match {
        case Resolution.Closure | Resolution.DeferredDataConstr => false
        case _                                                  => true
      }
    )
    insts match {
      case Nil => List.empty[MonoItem].pure[ContextState]
      case _   =>
        for {
          resolvableInsts <- insts.filterA(
            _.tys.forallM(MonoSpecialize.isTypeFullyResolved(_, numBinds))
          )
          items <- resolvableInsts.traverse(instToMonoItem(_))
        } yield items.flatten
    }
  }

  /** Convert an Instantiation to a MonoItem by computing the specialized bind
    * name.
    */
  def instToMonoItem(
      inst: Instantiation
  ): ContextState[Option[MonoItem]] =
    for {
      name <- toContextState(inst.bindName())
    } yield Some(
      MonoItem(
        sourceName = inst.i,
        concreteTys = inst.tys,
        cls = inst.cls,
        specializedName = name,
        origin = MonoOrigin.TopLevel,
        isTraitDefault = inst.i.contains(SelfTypeName),
        originalInst = inst
      )
    )

  /** Collect generic binds (TermTAbs) that may be needed for later
    * specialization. These are saved before specialization replaces them.
    */
  def collectGenericBinds(
      binds: List[Bind],
      existing: Map[String, Bind] = Map.empty
  ): Map[String, Bind] =
    existing ++ binds.collect {
      case b @ Bind(name, TermAbbBind(_: TermTAbs, _), _, _)
          if name
            .stripPrefix(Desugar.MethodNamePrefix)
            .count(_ == BindTypeSeparatorChar) <= 1 ||
            Desugar.TypeInstanceMethodPattern.matches(name) =>
        name -> b
    }.toMap
}
