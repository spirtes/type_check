(*  COMP 321 Homework 5:  Type-checking for a fragment of C.
*   
*   N. Danner.
*)

structure AnnAst =
struct

  (*  You will need a type for the source language types.  Do not
  *   use the TNone constructor.
  *)
  datatype typ = TNone

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
               | EMod of (exp*exp)*typ

  (*  The type of programs.  Replace the PNone constructor with your
  *   definition.
  *)
  datatype program = PNone

  (*  You must supply a function to convert (annotated) expressions to
  *   strings for the driver program.
  *)
  fun expToString (e : exp) : string =
      case e of 
      EString(s) =>
        "EString(" ^ s ^ ")"
      | EInt(n) =>
        "EInt(" ^ (Int.toString n) ^ ")"
      | EDouble(n) =>
        "EDouble(" ^ (Real.toString n) ^ ")"
      | EBool(b) =>
        "EBool(" ^ (Bool.toString b) ^ ")"
      | EAdd(e0, e1,t) =>
        "EAdd(" ^ (expToString e0) ^ "," ^ (expToString e1) ^ "," (expToString t) ^ ")"
      | ESub(e0, e1,t) =>
        "ESub(" ^ (expToString e0) ^ "," ^ (expToString e1) ^ "," (expToString t) ^ ")"
      | EMul(e0, e1,t) =>
        "ETimes(" ^ (expToString e0) ^ "," ^ (expToString e1) ^ "," (expToString t) ^ ")"
      | EDiv(e0, e1,t) =>
        "EDiv(" ^ (expToString e0) ^ "," ^ (expToString e1) ^ "," (expToString t) ^ ")"


  (*  You must spply a function to convert (annotated) programs to
  *   strings for the driver program.
  *)
  fun programToString(p : program) : string =
    ""

end
