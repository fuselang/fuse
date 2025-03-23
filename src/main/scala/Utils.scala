package fuse

import parser.Info.*
import cats.implicits.*
import scala.quoted.*

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

  def debug(l: String, v: Any): Unit = println(s"${l}: ${v.toString}")
}
