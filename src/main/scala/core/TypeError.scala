package core

import cats.data.EitherT
import cats.data.State
import cats.implicits.*
import core.Context.*
import core.Terms.*
import core.Types.*
import fuse.Utils.consoleError
import parser.Info.*

sealed trait TypeError

case class AscribeWrongTypeError(
    info: Info,
    termType: Type,
    ascribedType: Type,
    code: String = "E0001"
) extends TypeError

case class VariableNotFunctionTypeError(
    info: Info,
    termType: Type,
    code: String = "E0002"
) extends TypeError

case class MismatchFunctionTypeError(
    info: Info,
    termType: Type,
    expectedType: Type,
    code: String = "E0003"
) extends TypeError

case class TypeArgumentsNotAllowedTypeError(
    info: Info,
    termType: Type,
    code: String = "E0004"
) extends TypeError

case class InvalidTypeArgumentTypeError(
    info: Info,
    ty: Type,
    code: String = "E0005"
) extends TypeError

case class NoFieldsOnTypeError(
    info: Info,
    termType: Type,
    code: String = "E0006"
) extends TypeError

case class FieldNotFoundTypeError(
    info: Info,
    ty: Type,
    field: String,
    code: String = "E0007"
) extends TypeError

case class NoMethodsOnTypeError(
    info: Info,
    termType: Type,
    code: String = "E0008"
) extends TypeError

case class MethodNotFoundTypeError(
    info: Info,
    termType: Type,
    method: String,
    code: String = "E0009"
) extends TypeError

case class NotFoundTypeError(
    info: Info,
    variable: String = "",
    code: String = "E0010"
) extends TypeError

case class WrongReturnTypeError(
    info: Info,
    termType: Type,
    expectedType: Type,
    code: String = "E0011"
) extends TypeError

case class MatchTypeMismatchPatternTypeError(
    info: Info,
    matchExprType: Type,
    patternType: Type,
    code: String = "E0012"
) extends TypeError

case class MatchCasesTypeError(
    info: Info,
    caseExprType: Type,
    expectedType: Type,
    code: String = "E0013"
) extends TypeError

case class MatchExprNotDataTypeError(
    info: Info,
    exprType: Type,
    pattern: String,
    code: String = "E0014"
) extends TypeError

case class MatchVariantPatternMismatchTypeError(
    info: Info,
    variantType: Type,
    pattern: String,
    code: String = "E0015"
) extends TypeError

case class MatchRecordPatternMismatchTypeError(
    info: Info,
    variantType: Type,
    pattern: String,
    code: String = "E0016"
) extends TypeError

case class MatchPatternWrongVarsTypeError(
    info: Info,
    exprType: Type,
    expectedVariables: Int,
    patternVariables: Int,
    code: String = "E0017"
) extends TypeError

case class InvalidFunctionTypeError(
    info: Info,
    ty: Type,
    code: String = "E0018"
) extends TypeError

case class InvalidFoldForRecursiveTypeError(
    info: Info,
    ty: Type,
    code: String = "E0019"
) extends TypeError

case class TagNotVariantTypeError(
    info: Info,
    ty: Type,
    code: String = "E0020"
) extends TypeError

case class TagVariantFieldNotFoundTypeError(
    info: Info,
    variantType: Type,
    field: String,
    code: String = "E0021"
) extends TypeError

case class TagFieldMismatchTypeError(
    info: Info,
    termType: Type,
    expectedType: Type,
    code: String = "E0022"
) extends TypeError

case class NoTypeArgumentsTypeError(
    info: Info,
    ty: Type,
    code: String = "E0023"
) extends TypeError

case class KindParameterMismatchTypeError(
    info: Info,
    ty: Type,
    typeKind: Kind,
    expectedKind: Kind,
    code: String = "E0024"
) extends TypeError

case class NoKindForTypeError(
    info: Info,
    typeIdx: Int,
    code: String = "E0025"
) extends TypeError

case class BindingNotFoundTypeError(
    info: Info,
    code: String = "E0026"
) extends TypeError

case class NoTypeForVariableTypeError(
    info: Info,
    varIdx: Int,
    code: String = "E0027"
) extends TypeError

case class WrongBindingForVariableTypeError(
    info: Info,
    varIdx: Int,
    code: String = "E0028"
) extends TypeError

case class MainFunctionNotFoundTypeError(code: String = "E0029")
    extends TypeError

case class MultipleSolutionsFoundTypeError(
    info: Info,
    typeIdx: Int,
    code: String = "E0030"
) extends TypeError

case class UnboundExistentialVariableTypeError(
    info: Info,
    eA: TypeEVar,
    code: String = "E0031"
) extends TypeError

case class FailedToInstantiateTypeError(
    info: Info,
    fromType: Type,
    toType: Type,
    code: String = "E0032"
) extends TypeError

case class ExistentialVariableNotFoundTypeError(
    eA: TypeEVar,
    code: String = "E0033"
) extends TypeError

case class VariableRequiresTypeAnnotationError(
    eA: TypeEVar,
    code: String = "E0043"
) extends TypeError

case class InvalidSubtypeTypeError(
    info: Info,
    termType: Type,
    expectedType: Type,
    code: String = "E0034"
) extends TypeError

case class MissingTypeAnnotation(
    info: Info,
    ty: Type,
    code: String = "E0035"
) extends TypeError

case class MatchNodePatternNotFound(
    info: Info,
    nodePatter: PatternNode,
    code: String = "E0036"
) extends TypeError

case class TypeClassInstanceNotFound(
    info: Info,
    cls: List[TypeClass],
    typeClassInstances: List[Type],
    ty: Type,
    code: String = "E0037"
) extends TypeError

case class TypeClassNotFound(
    info: Info,
    typeClass: String,
    code: String = "E0038"
) extends TypeError

case class MultipleTypeClassMethodsFound(
    info: Info,
    typeClass: List[TypeClass],
    method: String,
    code: String = "E0039"
) extends TypeError

case class InvalidTypeInstanceMethod(
    info: Info,
    methodType: Type,
    expectedMethodType: Type,
    method: String,
    typeName: String,
    className: String,
    code: String = "E0040"
) extends TypeError

case class MissingTypeInstanceMethods(
    info: Info,
    typeName: String,
    className: String,
    methods: List[String],
    code: String = "E0041"
) extends TypeError

case class KindTypeClassMismatchTypeError(
    info: Info,
    typeClass: List[TypeClass],
    code: String = "E0042"
) extends TypeError

object TypeError {
  def format[T](error: TypeError): StateEither[T] = {
    val errorMessage = error match {
      case AscribeWrongTypeError(info, termType, ascribedType, code) =>
        for {
          termTypeName <- Representation.typeToString(termType)
          ascribedTypeName <- Representation.typeToString(ascribedType)
        } yield consoleError(
          s"ascribed term is `$termTypeName`, expected `$ascribedTypeName`",
          info,
          Some(code)
        )
      case VariableNotFunctionTypeError(info, termType, code) =>
        Representation
          .typeToString(termType, true)
          .map(name =>
            consoleError(
              s"expected function, found `$name`",
              info,
              Some(code)
            )
          )
      case MismatchFunctionTypeError(info, termType, expectedType, code) =>
        for {
          termTypeName <- Representation.typeToString(termType)
          expectedTypeName <- Representation.typeToString(expectedType)
        } yield consoleError(
          s"expected `$expectedTypeName`, found `$termTypeName`",
          info,
          Some(code)
        )
      case TypeArgumentsNotAllowedTypeError(info, termType, code) =>
        Representation
          .typeToString(termType)
          .map(name =>
            consoleError(
              s"type arguments are not allowed for type `$name`",
              info,
              Some(code)
            )
          )
      case InvalidTypeArgumentTypeError(info, ty, code) =>
        for {
          typeName <- Representation.typeToString(ty)
        } yield consoleError(
          s"missing generics for type `$typeName`, expected type arguments",
          info,
          Some(code)
        )
      case NoFieldsOnTypeError(info, termType, code) =>
        Representation
          .typeToString(termType)
          .map(name =>
            consoleError(
              s"`$name` isn't a record type and therefore doesn't have fields",
              info,
              Some(code)
            )
          )
      case FieldNotFoundTypeError(info, termType, field, code) =>
        Representation
          .typeToString(termType)
          .map(name =>
            consoleError(
              s"no field `$field` on `$name` record",
              info,
              Some(code)
            )
          )
      case NoMethodsOnTypeError(info, termType, code) =>
        Representation
          .typeToString(termType)
          .map(name =>
            consoleError(
              s"`$name` isn't a data type and therefore can't have methods",
              info,
              Some(code)
            )
          )
      case MethodNotFoundTypeError(info, termType, method, code) =>
        Representation
          .typeToString(termType)
          .map(name =>
            consoleError(
              s"`$method` method not found in `$name` type",
              info,
              Some(code)
            )
          )
      case MultipleTypeClassMethodsFound(info, cls, method, code) =>
        cls
          .traverse(Representation.typeToString(_))
          .map(_.mkString(", "))
          .map(boundTypeClasses =>
            consoleError(
              s"multiple `$method` method implementations found for `{$boundTypeClasses}` bounds",
              info,
              Some(code)
            )
          )
      case NotFoundTypeError(info, varName, code) =>
        consoleError(
          s"type not found $varName",
          info,
          Some(code)
        ).pure[StateEither]
      case WrongReturnTypeError(info, termType, expectedType, code) =>
        for {
          expectedTypeName <- Representation.typeToString(expectedType)
          termTypeName <- Representation.typeToString(termType, true)
        } yield consoleError(
          s"expected `$expectedTypeName` return type, found `$termTypeName` in function expression",
          info,
          Some(code)
        )
      case MatchTypeMismatchPatternTypeError(
            info,
            matchType,
            patternType,
            code
          ) =>
        for {
          matchTypeName <- Representation.typeToString(matchType)
          patternTypeName <- Representation.typeToString(patternType)
        } yield consoleError(
          s"expected `$matchTypeName` match expression type, found `$patternTypeName`",
          info,
          Some(code)
        )
      case MatchNodePatternNotFound(info, pattern, code) =>
        consoleError(
          s"`${pattern.tag}` type not found",
          info,
          Some(code)
        ).pure[StateEither]
      case MatchCasesTypeError(
            info,
            caseExprType,
            expectedType,
            code
          ) =>
        for {
          caseExprTypeName <- Representation.typeToString(caseExprType)
          expectedTypeName <- Representation.typeToString(expectedType)
        } yield consoleError(
          s"expected `$expectedTypeName` case expression type, found `$caseExprTypeName`",
          info,
          Some(code)
        )
      case MatchExprNotDataTypeError(
            info,
            matchExprType,
            pattern,
            code
          ) =>
        Representation
          .typeToString(matchExprType)
          .map(name =>
            consoleError(
              s"`$name` isn't a data type and therefore `$pattern` pattern can't be used",
              info,
              Some(code)
            )
          )
      case MatchVariantPatternMismatchTypeError(
            info,
            matchExprType,
            pattern,
            code
          ) =>
        Representation
          .typeToString(matchExprType)
          .map(name =>
            consoleError(
              s"variant `$name` type doesn't have `$pattern` option",
              info,
              Some(code)
            )
          )
      case MatchRecordPatternMismatchTypeError(
            info,
            matchExprType,
            pattern,
            code
          ) =>
        Representation
          .typeToString(matchExprType)
          .map(name =>
            consoleError(
              s"record `$name` type doesn't have `$pattern` field",
              info,
              Some(code)
            )
          )
      case MatchPatternWrongVarsTypeError(
            info,
            matchExprType,
            expectedVariables,
            patternVariables,
            code
          ) =>
        Representation
          .typeToString(matchExprType)
          .map(name =>
            consoleError(
              s"wrong number of variables for `$name`, expected $expectedVariables, given $patternVariables",
              info,
              Some(code)
            )
          )
      case InvalidFunctionTypeError(
            info,
            ty,
            code
          ) =>
        Representation
          .typeToString(ty)
          .map(name =>
            consoleError(
              s"expected a function type, found `$name`",
              info,
              Some(code)
            )
          )
      case InvalidFoldForRecursiveTypeError(
            info,
            ty,
            code
          ) =>
        Representation
          .typeToString(ty)
          .map(name =>
            consoleError(
              s"expected a recursive type, found `$name`",
              info,
              Some(code)
            )
          )
      case NoTypeArgumentsTypeError(info, ty, code) =>
        Representation
          .typeToString(ty)
          .map(name =>
            consoleError(
              s"type arguments are not allowed for `$name` type",
              info,
              Some(code)
            )
          )
      case KindParameterMismatchTypeError(
            info,
            ty,
            typeKind,
            expectedKind,
            code
          ) =>
        val typeKindRepr = Representation.kindToString(typeKind)
        val expectedKindRepr = Representation.kindToString(expectedKind)
        Representation
          .typeToString(ty)
          .map(name =>
            consoleError(
              s"expected `$expectedKindRepr` kind, given `$typeKindRepr` with `$name` type",
              info,
              Some(code)
            )
          )
      case TagNotVariantTypeError(info, ty, code) =>
        Representation
          .typeToString(ty)
          .map(name =>
            consoleError(
              s"expected variant type, given `$name`",
              info,
              Some(code)
            )
          )
      case TagVariantFieldNotFoundTypeError(info, ty, field, code) =>
        Representation
          .typeToString(ty)
          .map(name =>
            consoleError(
              s"variant `$name` type doesn't have `$field` field",
              info,
              Some(code)
            )
          )
      case TagFieldMismatchTypeError(info, termType, expectedType, code) =>
        for {
          termTypeName <- Representation.typeToString(termType)
          expectedTypeName <- Representation.typeToString(expectedType)
        } yield consoleError(
          s"expected `$expectedTypeName` variant type, found `$termTypeName`",
          info,
          Some(code)
        )
      case BindingNotFoundTypeError(info, code) =>
        consoleError("symbol not found", info, Some(code)).pure[StateEither]
      case NoKindForTypeError(info, _, code) =>
        consoleError("no kind recorded for type", info, Some(code))
          .pure[StateEither]
      case NoTypeForVariableTypeError(info, idx, code) =>
        EitherT.liftF(
          State.inspect((ctx: Context) =>
            consoleError(
              s"no type recorded for variable `${indexToName(ctx, idx)}`",
              info,
              Some(code)
            )
          )
        )
      case WrongBindingForVariableTypeError(info, idx, code) =>
        EitherT.liftF(
          State.inspect((ctx: Context) =>
            consoleError(
              s"wrong kind of variable for `${indexToName(ctx, idx)}`",
              info,
              Some(code)
            )
          )
        )
      case MainFunctionNotFoundTypeError(code) =>
        consoleError("`main` function not defined", UnknownInfo, Some(code))
          .pure[StateEither]
      case MultipleSolutionsFoundTypeError(info, idx, code) =>
        EitherT.liftF(
          State.inspect((ctx: Context) =>
            consoleError(
              s"multiple solutions found for `${indexToName(ctx, idx)}`",
              info,
              Some(code)
            )
          )
        )
      case MissingTypeAnnotation(info, termType, code) =>
        Representation
          .typeToString(termType)
          .map(name =>
            consoleError(
              s"type annotation is missing for type `$name`",
              info,
              Some(code)
            )
          )
      case FailedToInstantiateTypeError(info, fromType, toType, code) =>
        for {
          fromTypeName <- Representation.typeToString(fromType)
          toTypeName <- Representation.typeToString(toType)
        } yield consoleError(
          s"can't determine the type of variable, type annotation is required",
          info,
          Some(code)
        )
      case UnboundExistentialVariableTypeError(info, eA, code) =>
        consoleError(
          s"`${eA.name}` existential variable not bound",
          eA.info,
          Some(code)
        )
          .pure[StateEither]
      case ExistentialVariableNotFoundTypeError(eA, code) =>
        consoleError(
          s"`${eA.name}` existential variable not found",
          eA.info,
          Some(code)
        )
          .pure[StateEither]
      case VariableRequiresTypeAnnotationError(eA, code) =>
        consoleError(
          s"explicit type annotation required for variable",
          eA.info,
          Some(code)
        )
          .pure[StateEither]
      case InvalidSubtypeTypeError(info, termType, expectedType, code) =>
        for {
          termTypeName <- Representation.typeToString(termType)
          expectedTypeName <- Representation.typeToString(expectedType)
        } yield consoleError(
          s"expected type of `$expectedTypeName`, found `$termTypeName`",
          info,
          Some(code)
        )
      case TypeClassInstanceNotFound(
            info,
            cls,
            typeClassInstances,
            termType,
            code
          ) =>
        for {
          termTypeName <- Representation.typeToString(termType)
          expectedTypeInstances <- typeClassInstances.traverse(
            Representation.typeToString(_)
          )
          requiredTypeClasses <- cls.traverse(Representation.typeToString(_))
          expectedString = expectedTypeInstances match {
            case Nil =>
              s"expected type that implements `{${requiredTypeClasses.mkString(", ")}}` traits"
            case _ =>
              s"expected one of types `{${expectedTypeInstances.mkString(", ")}}`"
          }
        } yield consoleError(
          s"${expectedString}, found `$termTypeName` type",
          info,
          Some(code)
        )
      case TypeClassNotFound(info, name, code) =>
        consoleError(
          s"`${name}` type class not found",
          info,
          Some(code)
        )
          .pure[StateEither]
      case InvalidTypeInstanceMethod(
            info,
            methodType,
            expectedType,
            method,
            typeName,
            className,
            code
          ) =>
        for {
          termTypeName <- Representation.typeToString(
            methodType,
            buildContext = true
          )
          expectedTypeName <- Representation.typeToString(
            expectedType,
            buildContext = true
          )
        } yield consoleError(
          s"expected `$expectedTypeName`, found `$termTypeName` for `$method`",
          info,
          Some(code)
        )
      case MissingTypeInstanceMethods(
            info,
            typeName,
            className,
            methods,
            code
          ) =>
        consoleError(
          s"`{${methods.mkString(", ")}}` methods not implemented for `$typeName` type",
          info,
          Some(code)
        )
          .pure[StateEither]
      case KindTypeClassMismatchTypeError(info, cls, code) =>
        consoleError(
          s"`{${cls.map(_.name).mkString(", ")}}` type classes have different kinds",
          info,
          Some(code)
        ).pure[StateEither]
    }
    errorMessage.flatMap(e => EitherT.leftT[ContextState, T](e))
  }
}
