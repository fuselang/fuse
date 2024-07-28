package core

import core.Bindings.*
import core.Terms.*
import core.Types.*
import parser.Info.*
import core.Context.emptyContext
import cats.instances.boolean

object BuiltIn {
  def buildFunc(args: List[Type], r: Type): Type =
    (args :+ r).reduceRight(TypeArrow(UnknownInfo, _, _))

  def buildBind(name: String, binding: Binding): List[Bind] =
    List(Bind(name, binding))

  def buildClassMethodBind(name: String, ty: Type, cls: TypeClass): List[Bind] =
    buildBind(name, TermAbbBind(TermClassMethod(i, ty, cls)))

  def buildClassBind(name: String): List[Bind] =
    buildBind(name, TypeClassBind(KindStar))

  def buildClassInstanceBind(
      typeClass: String,
      typeName: String,
      ty: Type,
      method: String,
      body: Term
  ): List[Bind] =
    val typeClassInstance = buildBind(
      Desugar.toTypeInstanceBindID(typeName, typeClass),
      TypeClassInstanceBind(typeClass, ty, List(method))
    )
    val methodBind = buildBind(
      Desugar.toTypeInstanceMethodID(method, typeName, typeClass),
      TermAbbBind(body)
    )
    typeClassInstance ++ methodBind

  val i = UnknownInfo

  case class Op(
      cls: String,
      operator: String,
      tys: List[Type] = List(TypeInt(i), TypeFloat(i), TypeString(i)),
      compareOp: Boolean = false
  )

  def buildOperator(op: Op): List[Bind] = List(
    buildClassBind(op.cls),
    buildClassMethodBind(
      op.operator,
      TypeAll(
        i,
        "T",
        KindStar,
        List(TypeClass(i, op.cls)),
        buildFunc(
          List(TypeVar(i, 0, 1), TypeVar(i, 0, 1)),
          if (op.compareOp) TypeBool(i) else TypeVar(i, 0, 1)
        )
      ),
      TypeClass(i, op.cls)
    ),
    op.tys
      .map(ty =>
        buildClassInstanceBind(
          op.cls,
          Representation.typeToString(ty).value.runA(emptyContext).value.merge,
          ty,
          op.operator,
          TermBuiltin(buildFunc(List(ty, ty), ty))
        )
      )
      .flatten
  ).flatten

  val AddOp = Op("Add", "+")
  val SubOp = Op("Sub", "-")
  val MulOp = Op("Mul", "*", tys = List(TypeInt(i), TypeFloat(i)))
  val DivOp = Op("Div", "/", tys = List(TypeInt(i), TypeFloat(i)))
  val ModOp = Op("Mod", "%", tys = List(TypeInt(i), TypeFloat(i)))
  val EqOp = Op("Eq", "==", compareOp = true)
  val NotEqOp = Op("NotEq", "!=", compareOp = true)
  val LessThanOp = Op("LessThan", "<", compareOp = true)
  val LessThanEqOp = Op("LessThanEq", "<=", compareOp = true)
  val GreaterThanOp = Op("GreaterThan", ">", compareOp = true)
  val GreaterThanEqOp = Op("GreaterThanEq", ">=", compareOp = true)
  val AndOp = Op("And", "&&", tys = List(TypeBool(i)))
  val OrOp = Op("Or", "||", tys = List(TypeBool(i)))

  val Ops = List(
    AddOp,
    SubOp,
    MulOp,
    DivOp,
    ModOp,
    EqOp,
    NotEqOp,
    LessThanOp,
    LessThanEqOp,
    GreaterThanOp,
    GreaterThanEqOp,
    AndOp,
    OrOp
  )

  val Binds: List[Bind] = List(
    Ops.map(buildOperator(_)).flatten,
    buildBind(
      "print",
      TermAbbBind(TermBuiltin(buildFunc(List(TypeString(i)), TypeUnit(i))))
    ),
    buildBind(
      "int_to_str",
      TermAbbBind(TermBuiltin(buildFunc(List(TypeInt(i)), TypeString(i))))
    )
  ).flatten
}
