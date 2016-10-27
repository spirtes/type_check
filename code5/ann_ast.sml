(*  COMP 321 Homework 5:  Type-checking for a fragment of C.
*   
*   Torie Davids
*   Lex Spirtes
*
*
*)

structure AnnAst =
struct

  (*  You will need a type for the source language types.  Do not
  *   use the TNone constructor.
  *)
  datatype typ = Tbool | Tint | 
                 Tdouble | Tstring |
                 Tvoid | Tfunc

  type id = string 
  type paramdecl = typ*id

  (*  The annotated type of expressions.  I have provided a couple of 
  *   clauses for you, but you need to supply the rest.
  *)
  datatype exp = EInt of int (*  Doesn't need a type parameter, because
                                 an EInt can only be of type int. *)
               | EDouble of real
               | EString of string
               | ETrue of bool
               | EFalse of bool
               | EId of id*typ
               | ECall of (id*typ)*(exp list)
               | EPostIncr of id*typ | EPostDecr of id*typ
               | EPreIncr of id*typ | EPreDecr of id*typ
               | ENot of exp*typ
               | EAdd of (exp*exp)*typ 
               | ESub of (exp*exp)*typ
               | EMul of (exp*exp)*typ
               | EDiv of (exp*exp)*typ
               | EMod of (exp*exp)*typ
               | ELShift of (exp*exp)*typ | ERShift of (exp*exp)*typ
               | EEq of (exp*exp)*typ | ENeq of (exp*exp)*typ
               | ELt of (exp*exp)*typ | EGt of (exp*exp)*typ
               | ELe of (exp*exp)*typ | EGe of (exp*exp)*typ
               | EConj of (exp*exp)*typ | EDisj of (exp*exp)*typ
               | EAsst of id*exp*typ
               | ECond of (exp*exp*exp)*typ

  (*  The type of programs.  Replace the PNone constructor with your
  *   definition.
  *)
  datatype stm = SExp of exp
                | SDecl of typ*(id list) (*follows a different form than asst, this ok?*)
                | SInit of typ*(id*exp list)
                | SRet of exp (*worth making exp*typ or not since exps have types?*)
                | SWhile of exp*stm
                | SFor of (id*exp*typ)*exp*exp*stm (*order of the first trouple? is an assignment*)
                | SIf of exp*stm 
                | SIfElse of exp*stm*stm
                | SBlock of stm list

  datatype def = DFun of id*typ*(paramdecl list)*(stm list)
                | FProt of id*typ*(id*typ list)

  
  datatype program = PDef of def list

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
        | ETrue(b) =>
          "ETrue(" ^ (Bool.toString b) ^ ")"
        | EFalse(b) =>
          "EFalse(" ^ (Bool.toString b) ^ ")"
        | EAdd((e0, e1), t) => binToStr("EAdd", e0, e1, t)
        | ESub((e0, e1), t) => binToStr("ESub", e0, e1, t)
        | EMul((e0, e1), t) => binToStr("EMul", e0, e1, t)
        | EDiv((e0, e1), t) => binToStr("EDiv", e0, e1, t)
        | EMod((e0, e1), t) => binToStr("EMod", e0, e1, t)
        | ELShift((e0, e1), t) => binToStr("ELshift", e0, e1, t)
        | ERShift((e0, e1), t) => binToStr("ERshift", e0, e1, t)
        | EEq((e0, e1), t) => binToStr("Eeq", e0, e1, t)
        | ENeq((e0, e1), t) => binToStr("ENeq", e0, e1, t)
        | ELt((e0, e1), t) => binToStr("ELt", e0, e1, t)
        | EGt((e0, e1), t) => binToStr("EGt", e0, e1, t)
        | ELe((e0, e1), t) => binToStr("ELe", e0, e1, t)
        | EGe((e0, e1), t) => binToStr("EGe", e0, e1, t)
        | EConj((e0, e1), t) => binToStr("EConj", e0, e1, t)
        | EDisj((e0, e1), t) => binToStr("EDisj", e0, e1, t)
        | ECond((e0, e1, e2), t) => "ECond(" ^ 
                                    expToString e0 ^ ", " ^ 
                                    expToString e1 ^ ", " ^
                                    expToString e2 ^ ", " ^ 
                                    typToString t ^ ")"
      end

  (*  You must spply a function to convert (annotated) programs to
  *   strings for the driver program.
  *)

  (*given an exp list l, returns it as a string*)
  fun expLToString (l: exp list) : string =
    case l of
      [] => ""
      | x::xs => expToString(x) ^ expLToString(xs) 

  (*given a statement list, returns them as strings*)
  (*fun stmLToString (l: stm list) : string =
    case l of 
      [] => ""
      | x::xs => stmToString(x) ^ "," ^ stmLToString(xs)*)

  (*fun stmToString (s: stm) : string = 
    case s of
      SExp(e) => "SExp(" ^ expToString(e) ^ ")"
      | SRet(r) => "SRet(" ^ expToString(r) ^ ")"
      | SDecl(t, l) => 
          "SDecl(" ^ typToString(t) ^ "," ^
          ListFormat.listToString String.toString ids ")"*)
      (*| SInit()*)
      (*| SWhile(e, s) => 
          "SWhile(" ^ expToString(e) ^ "," ^ stmToString(s) ^ ")"
      | SFor((i, e, t), e1, e2, s) => 
          "SFor(" ^ expToString(e1) ^ "," ^ 
          expToString(e2) ^ "," ^ stmToString(s) ")"
      | SIf(e,s) => "SIf(" ^ expToString(e) ^ "," ^ stmToString(s) ^ ")"
      | SIfElse(e, s1, s2) => "SIfElse(" ^ expToString(e) ^ "," 
       ^ stmToString(s1) ^ "," stmToString(s2) ^ ")"
      | SBlock(ss) => "SBlock(" ^ stmLToString(ss) ^ ")"*)

  fun programToString(p : program) : string =
    ""

end
