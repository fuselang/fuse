package core

import core.Bindings.*
import core.Types.*
import core.Terms.*
import parser.Info.*
import core.Instantiations.Instantiation

object Shifting {

  type ShiftVarFunc[T] = (Info, Int, Int, Int) => T
  type TypeVarFunc = (Int, Type) => Type

  case class Shift(d: Int, c: Int)

  def incrShifts(shifts: List[Shift]): List[Shift] =
    shifts.map(s => Shift(s.d, s.c + 1))

  def bindShift(d: Int, b: Bind, c: Int = 0): Bind =
    Bind(
      b.i,
      bindingShift(d, b.b, c),
      b.insts.map(i =>
        Instantiation(
          i.i,
          termShiftAbove(d, c, i.term),
          i.tys.map(typeShiftAbove(d, c, _)),
          i.cls,
          i.r
        )
      )
    )

  def bindingShift(d: Int, b: Binding, c: Int = 0): Binding = b match {
    case NameBind                 => NameBind
    case TypeEMarkBind            => TypeEMarkBind
    case TypeEFreeBind            => TypeEFreeBind
    case TempVarBind              => TempVarBind
    case TypeESolutionBind(ty, _) => TypeESolutionBind(typeShiftAbove(d, c, ty))
    case t @ TypeVarBind(_, _)    => t
    case VarBind(ty)              => VarBind(typeShiftAbove(d, c, ty))
    case TypeAbbBind(ty, k)       => TypeAbbBind(typeShiftAbove(d, c, ty), k)
    case c: TypeClassBind         => c
    case TypeClassInstanceBind(cls, ty, m) =>
      TypeClassInstanceBind(cls, typeShiftAbove(d, c, ty), m)
    case TermAbbBind(term, Some(ty)) =>
      TermAbbBind(termShiftAbove(d, c, term), Some(typeShiftAbove(d, c, ty)))
    case t @ TermAbbBind(_, None) => t
  }

  /** Substitutes `TypeVar` instances having `c` index with `tyS` type in
    * provided `t` Term .
    *
    * It also shifts `TermVar` instances by -1.
    */
  def termSubstituteType(
      tyS: Type,
      c: Integer,
      t: Term
  ): Term =
    termMap(
      (info, c, k, n) =>
        if (k >= c) TermVar(info, k - 1, n - 1) else TermVar(info, k, n - 1),
      (c, tyT) => typeSubstitute(tyS, c, tyT),
      c,
      t
    )

  /** Substitutes `tC: TermVar` instance with `tS: TermVar` in provided `tT`
    * term.
    */
  def termVarSubstitute(s: Int, d: Option[Int], tC: TermVar, tT: Term): Term =
    termMap(
      (info, j, x, n) =>
        if (tC.info == info && tC.i1 == x)
          TermVar(info, s + d.map(n - _).getOrElse(j - 1), n)
        else TermVar(info, x, n),
      (_, ty) => ty,
      0,
      tT
    )

  /** Shifts all `TermVar` & `TypeVar` instances found on specified term `t` by
    * `d` value.
    */
  def termShift(d: Int, t: Term): Term =
    termShiftAbove(d, 0, t)

  /** Shifts all `TermVar` & `TypeVar` instances found on specified term `t` by
    * `d` value.
    */
  def termShiftAbove(d: Int, c: Int, t: Term): Term =
    termMap(
      (info, c, k, n) =>
        if (k >= c) TermVar(info, k + d, n + d) else TermVar(info, k, n + d),
      (c, ty) => typeShiftAbove(d, c, ty),
      c,
      t
    )

  /** Applies `onVar` function on all `TermVar` instances & `onType` function on
    * all `Type` instances in provided term `t`.
    *
    * The `c` parameter is passed down to the `onVar` and `onType` functions as
    * first integer parameter. On _mapping_ over abstracted terms (`TermAbs`,
    * `TermClosure`) the `c` is incremented by one, as context length is
    * increased on these type instances.
    */
  def termMap(
      onVar: ShiftVarFunc[Term],
      onType: TypeVarFunc,
      c: Int,
      t: Term
  ): Term = {
    def iter(c: Int, term: Term): Term = term match {
      case TermVar(info, x, n) => onVar(info, c, x, n)
      case TermAbs(info, i, ty, e, r) =>
        TermAbs(
          info,
          i,
          onType(c, ty),
          iter(c + 1, e),
          r.map(onType(c + 1, _))
        )
      case TermTAbs(info, i, cls, e) =>
        TermTAbs(
          info,
          i,
          cls,
          iter(c + 1, e)
        )
      case TermClosure(info, i, ty, e) =>
        TermClosure(
          info,
          i,
          ty.map(onType(c, _)),
          iter(c + 1, e)
        )
      case TermApp(info, t1, t2) => TermApp(info, iter(c, t1), iter(c, t2))
      case TermFix(info, t)      => TermFix(info, iter(c, t))
      case TermMatch(info, t, cases) =>
        TermMatch(info, t, cases.map((p, e) => (p, iter(c, e))))
      case TermLet(info, i, t1, t2) =>
        TermLet(info, i, iter(c, t1), iter(c + 1, t2))
      case TermProj(info, t, i)       => TermProj(info, iter(c, t), i)
      case TermMethodProj(info, t, i) => TermMethodProj(info, iter(c, t), i)
      case TermAssocProj(info, ty, i) =>
        TermAssocProj(info, onType(c, ty), i)
      case TermRecord(info, v) =>
        TermRecord(info, v.map((i, t) => (i, iter(c, t))))
      case TermTag(info, i, t, ty) =>
        TermTag(info, i, iter(c, t), onType(c, ty))
      case TermAscribe(info, t, ty) =>
        TermAscribe(info, iter(c, t), onType(c, ty))
      case TermTApp(info, t, ty) => TermTApp(info, iter(c, t), ty)
      case v                     => v
    }
    iter(c, t)
  }

  /** Substitutes `TypeVar` instances having zero-index with `tyS` type in
    * provided `tyT` type.
    *
    * It also shifts all `TypeVar` by -1 to ensure indexes are being maintained
    * once the substition happens.
    */
  def typeSubstituteTop(tyS: Type, tyT: Type): Type =
    typeShift(-1, typeSubstitute(typeShift(1, tyS), 0, tyT))

  /** Substitutes `TypeVar` instances having `c` index with `tyS` type in
    * provided `tyT` type .
    */
  def typeSubstitute(tyS: Type, c: Int, tyT: Type): Type =
    typeMap(
      (info, j, x, n) => if (x == j) typeShift(j, tyS) else TypeVar(info, x, n),
      c,
      tyT
    )

  /** Shifts all `TypeVar` instances found on specified type `ty` by `d` value.
    */
  def typeShift(d: Int, ty: Type): Type =
    typeShiftAbove(d, 0, ty)

  /** Shifts `TypeVar` instances found on specified type `ty` by `d` value, only
    * when the index of a `TypeVar` is greater than the `c` value.
    */
  def typeShiftAbove(d: Int, c: Int, ty: Type): Type =
    typeMap(
      (info, c, k, n) =>
        if (k >= c) TypeVar(info, k + d, n + d) else TypeVar(info, k, n + d),
      c,
      ty
    )

  /** Applies `onVar` function on all `TypeVar` instances in provided type `t`.
    *
    * The `c` parameter is passed down to the `onVar` function as first integer
    * parameter. On _mapping_ over abstracted types (`TypeAbs`, `TypeAll`,
    * `TypeRec`) the `c` is incremented by one, as context length is increased
    * on these type instances.
    */
  def typeMap(onVar: ShiftVarFunc[Type], c: Int, t: Type): Type = {
    def iter(c: Int, tyT: Type): Type = tyT match {
      case TypeVar(info, x, n) => onVar(info, c, x, n)
      case TypeEVar(_, _, _)   => tyT
      case TypeAny(_)          => tyT
      case TypeId(_, _)        => tyT
      case TypeClass(_, _)     => tyT
      case TypeRecord(info, fieldTypes) =>
        TypeRecord(
          info,
          fieldTypes.map { case (l, tyTi) => (l, iter(c, tyTi)) }
        )
      case TypeVariant(info, fieldTypes) =>
        TypeVariant(
          info,
          fieldTypes.map { case (l, tyTi) => (l, iter(c, tyTi)) }
        )
      case TypeArrow(info, tyT1, tyT2) =>
        TypeArrow(info, iter(c, tyT1), iter(c, tyT2))
      case TypeRec(info, x, k, tyTi) => TypeRec(info, x, k, iter(c + 1, tyTi))
      case TypeAll(info, x, k, cls, tyTi) =>
        TypeAll(info, x, k, cls, iter(c + 1, tyTi))
      case TypeAbs(info, x, tyTi) => TypeAbs(info, x, iter(c + 1, tyTi))
      case TypeApp(info, tyT1, tyT2) =>
        TypeApp(info, iter(c, tyT1), iter(c, tyT2))
      case TypeString(info) => TypeString(info)
      case TypeUnit(info)   => TypeUnit(info)
      case TypeInt(info)    => TypeInt(info)
      case TypeFloat(info)  => TypeFloat(info)
      case TypeBool(info)   => TypeBool(info)
    }
    iter(c, t)
  }
}
