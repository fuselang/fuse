package core

import parser.Info.*
import core.Instantiations.Instantiation

object Types {
  sealed trait Kind
  case object KindStar extends Kind
  case class KindArrow(k1: Kind, k2: Kind) extends Kind

  sealed abstract trait Type {
    def isMono: Boolean = true
    def isPrimitive: Boolean = false

    /** Returns whether `eV` is in the free variables of this type. */
    def containsEVar(eV: TypeEVar): Boolean
  }

  case class TypeVar(info: Info, index: Integer, length: Integer) extends Type {
    def containsEVar(eV: TypeEVar): Boolean = false
    override def isPrimitive: Boolean = true
  }
  case class TypeEVar(info: Info, name: String, cls: List[TypeClass] = List())
      extends Type {
    def containsEVar(eV: TypeEVar): Boolean = this.name == eV.name
  }
  case class TypeClass(info: Info, name: String) extends Type {
    def containsEVar(eV: TypeEVar): Boolean = false
  }
  case class TypeId(info: Info, i: String) extends Type {
    def containsEVar(eV: TypeEVar): Boolean = false
  }
  case class TypeArrow(info: Info, t1: Type, t2: Type) extends Type {
    override def isMono: Boolean = t1.isMono && t2.isMono
    override def isPrimitive: Boolean = t1.isPrimitive && t2.isPrimitive
    def containsEVar(eV: TypeEVar): Boolean =
      t1.containsEVar(eV) || t2.containsEVar(eV)
  }
  case class TypeUnit(info: Info) extends Type {
    override def isPrimitive: Boolean = true
    def containsEVar(eV: TypeEVar): Boolean = false
  }
  case class TypeRecord(info: Info, f: List[(String, Type)]) extends Type {
    def containsEVar(eV: TypeEVar): Boolean =
      f.unzip._2.exists(_.containsEVar(eV))
    override def isPrimitive: Boolean = f.unzip._2.forall(_.isPrimitive)
  }
  case class TypeVariant(info: Info, v: List[(String, Type)]) extends Type {
    def containsEVar(eV: TypeEVar): Boolean =
      v.unzip._2.exists(_.containsEVar(eV))
    override def isPrimitive: Boolean = v.unzip._2.forall(_.isPrimitive)
  }
  case class TypeRec(info: Info, v: String, k: Kind, t: Type) extends Type {
    override def isPrimitive: Boolean = t.isPrimitive
    def containsEVar(eV: TypeEVar): Boolean = t.containsEVar(eV)
  }
  case class TypeAll(
      info: Info,
      i: String,
      k: Kind,
      cls: List[TypeClass] = List(),
      t: Type
  ) extends Type {
    override def isMono: Boolean = false
    def containsEVar(eV: TypeEVar): Boolean = t.containsEVar(eV)
  }
  case class TypeAbs(info: Info, i: String, t: Type) extends Type {
    override def isMono: Boolean = false
    def containsEVar(eV: TypeEVar): Boolean = t.containsEVar(eV)
  }
  case class TypeApp(info: Info, t1: Type, t2: Type) extends Type {
    def containsEVar(eV: TypeEVar): Boolean =
      t1.containsEVar(eV) || t2.containsEVar(eV)
  }
  case class TypeBool(info: Info) extends Type {
    override def isPrimitive: Boolean = true
    def containsEVar(eV: TypeEVar): Boolean = false
  }
  case class TypeString(info: Info) extends Type {
    override def isPrimitive: Boolean = true
    def containsEVar(eV: TypeEVar): Boolean = false
  }
  case class TypeFloat(info: Info) extends Type {
    override def isPrimitive: Boolean = true
    def containsEVar(eV: TypeEVar): Boolean = false
  }
  case class TypeInt(info: Info) extends Type {
    override def isPrimitive: Boolean = true
    def containsEVar(eV: TypeEVar): Boolean = false
  }
  case class TypeAny(info: Info) extends Type {
    def containsEVar(eV: TypeEVar): Boolean = false
  }

  implicit val showTypeInfo: ShowInfo[Type] = ShowInfo.info(_ match {
    case TypeVar(info, _, _)       => info
    case TypeClass(info, _)        => info
    case TypeEVar(info, _, _)      => info
    case TypeId(info, _)           => info
    case TypeArrow(info, _, _)     => info
    case TypeUnit(info)            => info
    case TypeRecord(info, _)       => info
    case TypeVariant(info, _)      => info
    case TypeRec(info, _, _, _)    => info
    case TypeAll(info, _, _, _, _) => info
    case TypeAbs(info, _, _)       => info
    case TypeApp(info, _, _)       => info
    case TypeBool(info)            => info
    case TypeString(info)          => info
    case TypeFloat(info)           => info
    case TypeInt(info)             => info
    case TypeAny(info)             => info
  })
}

object Terms {
  import Types.*

  sealed trait Term

  // Abstraction
  case class TermFix(info: Info, t: Term) extends Term
  case class TermAbs(
      info: Info,
      i: String,
      t: Type,
      e: Term,
      r: Option[Type] = None
  ) extends Term
  case class TermClosure(info: Info, i: String, t: Option[Type], e: Term)
      extends Term
  // Expressions
  case class TermVar(info: Info, i1: Integer, i2: Integer) extends Term
  case class TermApp(info: Info, f: Term, v: Term) extends Term
  case class TermMatch(info: Info, t: Term, c: List[(Pattern, Term)])
      extends Term
  case class TermLet(info: Info, i: String, t1: Term, t2: Term) extends Term
  case class TermProj(info: Info, t: Term, i: String) extends Term
  case class TermMethodProj(info: Info, t: Term, i: String) extends Term
  case class TermAssocProj(info: Info, t: Type, i: String) extends Term
  // ADT
  case class TermRecord(info: Info, v: List[(String, Term)]) extends Term
  case class TermTag(info: Info, i: String, t: Term, typ: Type) extends Term
  case class TermAscribe(info: Info, t: Term, typ: Type) extends Term
  case class TermFold(info: Info, t: Type) extends Term
  // Higher Kind
  case class TermTAbs(info: Info, i: String, cls: List[TypeClass], t: Term)
      extends Term
  case class TermTApp(info: Info, t: Term, typ: Type) extends Term
  // Built-in functions with pre-defined type
  case class TermBuiltin(typ: Type) extends Term
  // Type classes
  case class TermClassMethod(info: Info, ty: Type, cls: TypeClass) extends Term

  sealed trait Pattern

  // Literals + Patterns
  case class TermFloat(info: Info, f: Float) extends Term with Pattern
  case class TermInt(info: Info, i: Int) extends Term with Pattern
  case class TermString(info: Info, s: String) extends Term with Pattern
  case class TermTrue(info: Info) extends Term with Pattern
  case class TermFalse(info: Info) extends Term with Pattern
  case class TermUnit(info: Info) extends Term with Pattern

  case class PatternNode(info: Info, tag: String, vars: List[String] = List())
      extends Pattern
  case class PatternDefault(info: Info) extends Pattern

  implicit val showTermInfo: ShowInfo[Term] = ShowInfo.info(_ match {
    case TermFloat(info, _)          => info
    case TermInt(info, _)            => info
    case TermString(info, _)         => info
    case TermTrue(info)              => info
    case TermFalse(info)             => info
    case TermUnit(info)              => info
    case TermBuiltin(_)              => UnknownInfo
    case TermFold(info, _)           => info
    case TermTApp(info, _, _)        => info
    case TermTAbs(info, _, _, _)     => info
    case TermAscribe(info, _, _)     => info
    case TermTag(info, _, _, _)      => info
    case TermRecord(info, _)         => info
    case TermMethodProj(info, _, _)  => info
    case TermAssocProj(info, _, _)   => info
    case TermFix(info, _)            => info
    case TermAbs(info, _, _, _, _)   => info
    case TermClosure(info, _, _, _)  => info
    case TermVar(info, _, _)         => info
    case TermApp(info, _, _)         => info
    case TermMatch(info, _, _)       => info
    case TermLet(info, _, _, _)      => info
    case TermProj(info, _, _)        => info
    case TermClassMethod(info, _, _) => info
  })

  implicit val showPatternInfo: ShowInfo[Pattern] = ShowInfo.info(_ match {
    case TermFloat(info, _)      => info
    case TermInt(info, _)        => info
    case TermString(info, _)     => info
    case TermTrue(info)          => info
    case TermFalse(info)         => info
    case TermUnit(info)          => info
    case PatternNode(info, _, _) => info
    case PatternDefault(info)    => info
  })
}

object Bindings {
  import Types.*
  import Terms.*

  sealed trait Binding

  case object NameBind extends Binding
  case class TypeVarBind(k: Kind, cls: List[TypeClass] = List()) extends Binding
  case class TypeAbbBind(t: Type, k: Option[Kind] = None) extends Binding
  case class TermAbbBind(t: Term, ty: Option[Type] = None) extends Binding
  case class TypeClassBind(k: Kind) extends Binding
  case class TypeClassInstanceBind(
      cls: String,
      ty: Type,
      methods: List[String]
  ) extends Binding

  // contexts (Γ,∆,Θ): · | Γ,α | Γ,x:A | Γ,â | Γ,â = τ | Γ,▶â
  case class VarBind(t: Type) extends Binding
  sealed trait Mark extends Binding
  case class TypeESolutionBind(t: Type, cls: List[TypeClass] = List())
      extends Mark
  case object TypeEFreeBind extends Mark
  case object TypeEMarkBind extends Mark

  // Used by code generation phase (grin).
  case object TempVarBind extends Binding

// Global bindings.
  case class Bind(i: String, b: Binding, insts: List[Instantiation] = List())
}
