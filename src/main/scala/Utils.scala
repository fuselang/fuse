package fuse

import parser.Info.*
import cats.implicits.*
import scala.quoted.*
import core.Terms.*
import core.Desugar

object Utils {
  def consoleError(message: String, info: Info, code: Option[String] = None) = {
    val errorRepr = code match {
      case Some(code) => s"error[$code]"
      case None       => "error"
    }
    val errorBold = fansi.Bold.On(fansi.Color.LightRed(errorRepr))
    val messageBold = fansi.Bold.On(s": $message")
    s"$errorBold$messageBold${info.show}"
  }

  def union[T](s1: List[T], s2: List[T]): List[T] =
    (s1.toSet | s2.toSet).toList

  def difference[T](s1: List[T], s2: List[T]): List[T] =
    (s1.toSet.diff(s2.toSet)).toList

  def debug(l: String, v: Any): Unit = println(s">>> ${l}\n${v.toString}")
}

object SpecializedMethodUtils {
  /** Check if a method name is specialized (starts with the method prefix) */
  def isSpecializedMethod(methodName: String): Boolean =
    methodName.startsWith(Desugar.MethodNamePrefix)

  /** Extract the base method name from a specialized method name */
  def extractBaseMethodName(specializedName: String): String = {
    if (isSpecializedMethod(specializedName)) {
      // Remove the prefix and extract the method name before the first '#'
      specializedName.stripPrefix(Desugar.MethodNamePrefix).takeWhile(_ != '#')
    } else {
      specializedName
    }
  }

  /** Check if a term contains any specialized method projections */
  def containsSpecializedMethod(term: Term): Boolean = term match {
    case TermMethodProj(_, _, method) => isSpecializedMethod(method)
    case TermApp(_, t1, t2) =>
      containsSpecializedMethod(t1) || containsSpecializedMethod(t2)
    case TermLet(_, _, t1, t2) =>
      containsSpecializedMethod(t1) || containsSpecializedMethod(t2)
    case TermAbs(_, _, _, expr, _) => containsSpecializedMethod(expr)
    case TermTag(_, _, term, _) => containsSpecializedMethod(term)
    case TermProj(_, term, _) => containsSpecializedMethod(term)
    case TermRecord(_, fields) =>
      fields.map(_._2).exists(containsSpecializedMethod)
    case TermMatch(_, term, clauses) =>
      containsSpecializedMethod(term) || clauses.exists { case (_, t) => containsSpecializedMethod(t) }
    case _ => false
  }
}
