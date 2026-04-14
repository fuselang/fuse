package code

object GrinPrelude {

  val ffiEffectful: List[(String, String)] = List(
    ("_prim_int_print", "_prim_int_print     :: T_Int64 -> T_Int64"),
    ("_prim_usleep", "_prim_usleep        :: T_Int64 -> T_Int64"),
    ("_prim_string_print", "_prim_string_print  :: T_String -> T_Int64"),
    ("_prim_read_string", "_prim_read_string   :: T_String"),
    ("_prim_error", "_prim_error         :: T_String -> T_Int64"),
    ("_prim_ffi_file_eof", "_prim_ffi_file_eof  :: T_Int64 -> T_Int64"),
    ("_prim_file_read", "_prim_file_read     :: T_String -> T_String"),
    (
      "_prim_file_write",
      "_prim_file_write    :: T_String -> T_String -> T_Int64"
    )
  )

  val ffiPure: List[(String, String)] = List(
    (
      "_prim_string_concat",
      "_prim_string_concat  :: T_String -> T_String -> T_String"
    ),
    ("_prim_string_reverse", "_prim_string_reverse :: T_String -> T_String"),
    (
      "_prim_string_lt",
      "_prim_string_lt      :: T_String -> T_String -> T_Bool"
    ),
    (
      "_prim_string_eq",
      "_prim_string_eq      :: T_String -> T_String -> T_Bool"
    ),
    ("_prim_string_head", "_prim_string_head    :: T_String -> T_Int64"),
    ("_prim_string_tail", "_prim_string_tail    :: T_String -> T_String"),
    (
      "_prim_string_cons",
      "_prim_string_cons    :: T_Int64  -> T_String -> T_String"
    ),
    ("_prim_string_len", "_prim_string_len     :: T_String -> T_Int64"),
    (
      "_prim_string_ne",
      "_prim_string_ne      :: T_String -> T_String -> T_Bool"
    ),
    ("_prim_int_str", "_prim_int_str        :: T_Int64 -> T_String"),
    ("_prim_str_int", "_prim_str_int        :: T_String -> T_Int64"),
    ("_prim_int_float", "_prim_int_float      :: T_Int64 -> T_Float"),
    ("_prim_float_string", "_prim_float_string   :: T_Float -> T_String"),
    ("_prim_char_int", "_prim_char_int       :: T_Char  -> T_Int64")
  )

  val primopPure: List[(String, String)] = List(
    ("_prim_int_shr", "_prim_int_shr   :: T_Int64 -> T_Int64"),
    ("_prim_int_add", "_prim_int_add   :: T_Int64 -> T_Int64 -> T_Int64"),
    ("_prim_int_sub", "_prim_int_sub   :: T_Int64 -> T_Int64 -> T_Int64"),
    ("_prim_int_mul", "_prim_int_mul   :: T_Int64 -> T_Int64 -> T_Int64"),
    ("_prim_int_div", "_prim_int_div   :: T_Int64 -> T_Int64 -> T_Int64"),
    ("_prim_int_ashr", "_prim_int_ashr  :: T_Int64 -> T_Int64 -> T_Int64"),
    ("_prim_int_eq", "_prim_int_eq    :: T_Int64 -> T_Int64 -> T_Bool"),
    ("_prim_int_ne", "_prim_int_ne    :: T_Int64 -> T_Int64 -> T_Bool"),
    ("_prim_int_gt", "_prim_int_gt    :: T_Int64 -> T_Int64 -> T_Bool"),
    ("_prim_int_ge", "_prim_int_ge    :: T_Int64 -> T_Int64 -> T_Bool"),
    ("_prim_int_lt", "_prim_int_lt    :: T_Int64 -> T_Int64 -> T_Bool"),
    ("_prim_int_le", "_prim_int_le    :: T_Int64 -> T_Int64 -> T_Bool"),
    ("_prim_int_mod", "_prim_int_mod   :: T_Int64 -> T_Int64 -> T_Int64"),
    ("_prim_float_add", "_prim_float_add :: T_Float -> T_Float -> T_Float"),
    ("_prim_float_sub", "_prim_float_sub :: T_Float -> T_Float -> T_Float"),
    ("_prim_float_mul", "_prim_float_mul :: T_Float -> T_Float -> T_Float"),
    ("_prim_float_div", "_prim_float_div :: T_Float -> T_Float -> T_Float"),
    ("_prim_float_eq", "_prim_float_eq  :: T_Float -> T_Float -> T_Bool"),
    ("_prim_float_ne", "_prim_float_ne  :: T_Float -> T_Float -> T_Bool"),
    ("_prim_float_gt", "_prim_float_gt  :: T_Float -> T_Float -> T_Bool"),
    ("_prim_float_ge", "_prim_float_ge  :: T_Float -> T_Float -> T_Bool"),
    ("_prim_float_lt", "_prim_float_lt  :: T_Float -> T_Float -> T_Bool"),
    ("_prim_float_le", "_prim_float_le  :: T_Float -> T_Float -> T_Bool"),
    ("_prim_float_mod", "_prim_float_mod :: T_Float -> T_Float -> T_Float"),
    ("_prim_bool_eq", "_prim_bool_eq   :: T_Bool -> T_Bool -> T_Bool"),
    ("_prim_bool_ne", "_prim_bool_ne   :: T_Bool -> T_Bool -> T_Bool"),
    ("_prim_bool_and", "_prim_bool_and  :: T_Bool -> T_Bool -> T_Bool"),
    ("_prim_bool_or", "_prim_bool_or   :: T_Bool -> T_Bool -> T_Bool")
  )

  val missingFFI: List[(String, String)] = List(
    ("_prim_int_mod", "_prim_int_mod :: T_Int64 -> T_Int64 -> T_Int64"),
    ("_prim_float_mod", "_prim_float_mod :: T_Float -> T_Float -> T_Float"),
    ("_prim_bool_and", "_prim_bool_and :: T_Bool -> T_Bool -> T_Bool"),
    ("_prim_bool_or", "_prim_bool_or :: T_Bool -> T_Bool -> T_Bool"),
    ("_prim_string_ne", "_prim_string_ne :: T_String -> T_String -> T_Bool")
  )
}
