package core

import cats.data.EitherT
import cats.data.State
import cats.implicits.*
import core.Context.*
import fuse.Utils.consoleError
import parser.Info.*

sealed trait DesugarError

case class TypeNotSupportedDesugarError(info: Info) extends DesugarError
case class TypeBoundNotSupportedDesugarError(info: Info) extends DesugarError
case class ExpressionNotSupportedDesugarError(info: Info) extends DesugarError
case class TypeAnnotationRequiredDesugarError(info: Info) extends DesugarError
case class DeclarationNotSupportedDesugarError(info: Info) extends DesugarError
case class VariableNotFoundDesugarError(info: Info, variable: String)
    extends DesugarError
case class TypeVariableNotFoundDesugarError(info: Info, variable: String)
    extends DesugarError
case class FunctionOperatorNotFoundDesugarError(info: Info, func: String)
    extends DesugarError
case class CaseNotSupportedDesugarError(info: Info) extends DesugarError
case class NestedPatternNotSupportedDesugarError(info: Info)
    extends DesugarError
case class DoRequiresYieldExprDesugarError(info: Info) extends DesugarError
case class DoExpectsAssignmentDesugarError(info: Info) extends DesugarError
case class MainIONestedDesugarError(info: Info) extends DesugarError

object DesugarError {
  def format[T](error: DesugarError): StateEither[T] =
    EitherT.leftT(error match {
      case TypeNotSupportedDesugarError(info) =>
        consoleError("type not supported", info)
      case TypeBoundNotSupportedDesugarError(info) =>
        consoleError("type bound not supported, use trait name", info)
      case ExpressionNotSupportedDesugarError(info) =>
        consoleError("expression not supported", info)
      case TypeAnnotationRequiredDesugarError(info) =>
        consoleError("type annotation required", info)
      case DeclarationNotSupportedDesugarError(info) =>
        consoleError("declaration not supported", info)
      case VariableNotFoundDesugarError(info, variable) =>
        consoleError(s"cannot find symbol `$variable` in this scope", info)
      case TypeVariableNotFoundDesugarError(info, variable) =>
        consoleError(
          s"cannot find type variable `$variable` in this scope",
          info
        )
      case FunctionOperatorNotFoundDesugarError(info, func) =>
        consoleError(s"function operator `$func` not found", info)
      case CaseNotSupportedDesugarError(info) =>
        consoleError("case not supported", info)
      case NestedPatternNotSupportedDesugarError(info) =>
        consoleError("nested pattern not supported", info)
      case DoRequiresYieldExprDesugarError(info) =>
        consoleError("yield expression not found", info)
      case DoExpectsAssignmentDesugarError(info) =>
        consoleError("assignment expression expected", info)
      case MainIONestedDesugarError(info) =>
        consoleError(
          "`main` cannot return a nested `IO` type; unwrap to a single `IO[T]`",
          info
        )
    })
}
