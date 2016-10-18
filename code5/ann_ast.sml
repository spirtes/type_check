(*  COMP 321 Homework 5:  Type-checking for a fragment of C.
*   
*   Torie Davids
*   Lex Spirtes
this is a change i swear

*)

structure AnnAst =
struct

  (*  You will need a type for the source language types.  Do not
  *   use the TNone constructor.
  *)
  datatype typ = Tbool | Tint | Tdouble | Tstring | Tvoid

  (*  The annotated type of expressions.  I have provided a couple of 
  *   clauses for you, but you need to supply the rest.
  *)
  datatype exp = EInt of int (*  Doesn't need a type parameter, because
                                 an EInt can only be of type int. *)
               | EDouble of real
               | EString of string
               | ETrue of bool
               | EFalse of bool
               | EAdd of (exp*exp)*typ 
                             (*  Needs a type parameter, because a +
                                 expression could have more than one type. *)
               | ESub of (exp*exp)*typ
               | EMul of (exp*exp)*typ
               | EDiv of (exp*exp)*typ
               | EMod of (exp*exp)*int
               | ELshift of (exp*exp)*int | ERshift of (exp*exp)*int
               | EEq of (exp*exp)*bool | ENeq of (exp*exp)*bool
               | ELt of (exp*exp)*bool | EGt of (exp*exp)*bool
               | ELe of (exp*exp)*bool | EGe of (exp*exp)*bool
               | EConj of (exp*exp)*bool | EDisj of (exp*exp)*bool

  (*  The type of programs.  Replace the PNone constructor with your
  *   definition.
  *)
  datatype program = PNone

  (*  typToString t = a string representation of t.
  *)
  fun typToString (t : typ) : string =
    case t of
         Tbool => "TBool"
       | Tint => "Tint"
       | Tdouble => "Tdouble"
       | Tvoid => "Tvoid"
       | Tstring => "Tstring"

  (*  You must supply a function to convert (annotated) expressions to
  *   strings for the driver program.
  *)
  fun expToString (e : exp) : string =
      let
        fun binToStr(con : string, e : exp, e' : exp, t: typ) : string =
          String.concat [
            con, "(", expToString e, ", ", expToString e', ", ", typToString t, ")"
          ]
      in
        case e of 
        EString(s) =>
          "EString(" ^ s ^ ")"
        | EInt(n) =>
          "EInt(" ^ (Int.toString n) ^ ")"
        | EDouble(n) =>
          "EDouble(" ^ (Real.toString n) ^ ")"
        | EBool(b) =>
          "EBool(" ^ (Bool.toString b) ^ ")"
        | EAdd(e0, e1,t) => binToStr("EAdd", e0, e1, t)
        | ESub(e0, e1,t) => binToStr("ESub", e0, e1, t)
        | EMul(e0, e1,t) => binToStr("EMul", e0, e1, t)
        | EDiv(e0, e1,t) => binToStr("EDiv", e0, e1, t)
        | EMod(e0, e1, t) => binToStr("EMod", e0, e1, t)
        | ELshift(e0, e1, t) => binToStr("ELshift", e0, e1, t)
        | ERshift(e0, e1, t) => binToStr("ERshift", e0, e1, t)
        | EEq(e0, e1, t) => binToStr("Eeq", e0, e1, t)
        | ENeq(e0, e1, t) => binToStr("ENeq", e0, e1, t)
        | ELt(e0, e1, t) => binToStr("ELt", e0, e1, t)
        | EGt(e0, e1, t) => binToStr("EGt", e0, e1, t)
        | ELe(e0, e1, t) => binToStr("ELe", e0, e1, t)
        | EGe(e0, e1, t) => binToStr("EGe", e0, e1, t)
        | EConj(e0, e1, t) => binToStr("EConj", e0, e1, t)
        | EDisj(e0, e1, t) => binToStr("EDisj", e0, e1, t)
      end

  (*  You must spply a function to convert (annotated) programs to
  *   strings for the driver program.
  *)
  fun programToString(p : program) : string =
    ""

end
