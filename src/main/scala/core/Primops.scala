package core

import core.Bindings.*
import core.Terms.*
import core.Types.*
import parser.Info.UnknownInfo

/** Registry of Fuse-visible primitive ops — the primops a user can call from
  * `.fuse` source via a free name. Each spec ties together the Fuse-level
  * identifier (how it appears in user code), the GRIN runtime symbol (how it is
  * lowered for code generation), and the parameter/return types (used by
  * `BuiltIn.Binds` to type-check calls).
  *
  * Adding a new user-visible primop means:
  *   1. Add a `PrimopSpec` here.
  *   2. Add a matching FFI declaration line to `GrinPrelude.scala` and a
  *      matching C symbol declaration to `grin/prim_ops.{c,h}` (these two live
  *      outside this registry because they preserve exact whitespace/alignment
  *      for bit-identical GRIN output).
  */
object Primops {
  val i = UnknownInfo

  case class PrimopSpec(
      fuseName: String,
      grinName: String,
      paramTypes: List[Type],
      returnType: Type
  )

  val specs: List[PrimopSpec] = List(
    PrimopSpec(
      fuseName = "_print",
      grinName = "_prim_string_print",
      paramTypes = List(TypeString(i)),
      returnType = TypeUnit(i)
    ),
    PrimopSpec(
      fuseName = "_file_read",
      grinName = "_prim_file_read",
      paramTypes = List(TypeString(i)),
      returnType = TypeString(i)
    ),
    PrimopSpec(
      fuseName = "_file_write",
      grinName = "_prim_file_write",
      paramTypes = List(TypeString(i), TypeString(i)),
      returnType = TypeUnit(i)
    ),
    PrimopSpec(
      fuseName = "int_to_str",
      grinName = "_prim_int_str",
      paramTypes = List(TypeInt(i)),
      returnType = TypeString(i)
    )
  )

  val fuseToGrinName: Map[String, String] =
    specs.map(s => s.fuseName -> s.grinName).toMap
}
