(*  COMP 321 Homework 5:  CPP typing engine.
*   
*   N. Danner
*)

structure Typing =
struct

(*Environment structure of type SplayMapFn*)
  structure Environment = SplayMapFn(struct
                                    type ord_key = string
                                    val compare = String.compare
                                    end)


(*exceptions*)
  exception TypeError
  exception UndeclaredError of Ast.id
  exception MultiplyDeclaredError of Ast.id
  exception ReturnTypeError

  (* intOrDouble(e1,e2) = t where t is the AnnAst.typ associated
   *  with both e1 and e2. t can only be int or double and only
   *  if both e1 and e2 share that type 
   *)
  fun intOrDouble (env: ORD_MAP, e1: Ast.exp, e2: Ast.exp, opr: string) : AnnAst.exp =
    case e2 of (* starting from e2 b/c right associative *)
      Ast.EInt(n) => (case e1 of 
                          Ast.EInt(n') => (case opr of
                                            "plus" => AnnAst.EPlus(e1,e2,AnnAst.Tint)
                                            | "minus" => AnnAst.ESub(e1,e2,AnnAst.Tint)
                                            | "times" => AnnAst.EMul(e1,e2,AnnAst.Tint)
                                            | "div" => AnnAst.EDiv(e1,e2,AnnAst.Tint)
                                            | _ => raise TypeError
                                            )
                          | _ => raise TypeError)
      | Ast.EDouble(n) => (case e1 of
                                Ast.EDouble(n') => (case opr of
                                                      "plus" => AnnAst.EPlus(e1,e2,AnnAst.Tdouble)
                                                    | "minus" => AnnAst.ESub(e1,e2,AnnAst.Tdouble)
                                                    | "times" => AnnAst.EMul(e1,e2,AnnAst.Tdouble)
                                                    | "div" => AnnAst.EDiv(e1,e2,AnnAst.Tdouble)
                                                    | _ => raise TypeError
                                                    )
                                | _ => raise TypeError)
      | Ast.EAdd(n1,n2) | Ast.ESub(n1, n2) | Ast.EMul(n1, n2) 
      | Ast.EDiv(n1, n2) | Ast.EMod(n1, n2) | Ast.ELshift(n1, n2) 
      | Ast.ERShift(n1, n2) => intOrDouble(env, inferExp env n1, inferExp env n2, opr)
      | _ => raise TypeError

  fun mustInt (env: ORD_MAP, e1: Ast.exp, e2: Ast.exp, opr: string) : AnnAst.exp =
    case e2 of
      AnnAst.EInt(n) => (case e1 of
                          AnnAst.EInt(n') => (case opr of
                                                "mod" => AnnAst.EMod(e1,e2,AnnAst.Tint)
                                              | "lshift" => AnnAst.ELshift(e1,e2,AnnAst.Tint)
                                              | "rshift" => AnnAst.ERShift(e1,e2,AnnAst.Tint)
                                              | _ => raise TypeError
                                              )
                          | _ => raise TypeError
                        )
      | Ast.EAdd(n1,n2) | Ast.ESub(n1, n2) | Ast.EMul(n1, n2) 
      | Ast.EDiv(n1, n2) | Ast.EMod(n1, n2) | Ast.ELshift(n1, n2) 
      | Ast.ERShift(n1, n2) => mustInt(env, inferExp env n1, inferExp env n2, opr)
      | _ => raise TypeError

    fun boolChecker (env: ORD_MAP, e1: Ast.exp, e2: Ast.exp, opr: string) : AnnAst.exp =
      case e2 of
        | Ast.ETrue | Ast.EFalse => (case e1 of
                                      Ast.ETrue | Ast.EFalse =>
                                          (case opr of
                                            "eeq" => AnnAst.EEq(e1, e2, AnnAst.Tbool)
                                            | "neq" => AnnAst.Neq(e1, e2, AnnAst.Tbool)
                                            | "lt" => AnnAst.Lt(e1, e2, AnnAst.Tbool)
                                            | "gt" => AnnAst.Gt(e1, e2, AnnAst.Tbool)
                                            | "le" => AnnAst.Le(e1, e2, AssAst.Tbool)
                                            | "ge" => AnnAst.Ge(e1, e2, AssAst.Tbool)
                                            | "and" => AnnAst.Conj(e1, e2, AnnAst.Tbool)
                                            | "or" => AnnAst.Disj(e1, e2, AnnAst.Tbool)
                                            | _ => raise TypeError)
                                      | _ => raise TypeError
                                    )
        | Ast.EDouble | Ast.EInt | Ast.EString => raise TypeError
        | _ => boolChecker(env, inferExp env n1, inferExp env n2, opr)

  (* inferExp env e = e', where e' is the annotated expression
   * corresponding to e given an environment env. 
   *)
  fun inferExp (env: ORD_MAP) (e: Ast.exp) : AnnAst.exp =
    case e of
      Ast.EInt(n) => AnnAst.EInt(n)
      | Ast.EDouble(n) => AnnAst.EDouble(n)
      | Ast.EString(s) => AnnAst.EString(s)
      | Ast.ETrue => AnnAst.ETrue(true)
      | Ast.EFalse => AnnAst.EFalse(false)
      (*| Ast.EId(id) => (* not implemented in AnnAst yet *)
      (*| Ast.ECall(id, l) => (* not implemented in AnnAst yet *)
      (*| Ast.EPostIncr(id) => (* not implemented in AnnAst yet *)
      (*| Ast.EPostDecr(id) => (* not implemented in AnnAst yet *)
      (*| Ast.ENot(n) => (* not implemented in AnnAst yet *)
      (*| Ast.EPreIncr(id) => (* not implemented in AnnAst yet *)
      (*| Ast.EPreDecr(id) => (* not implemented in AnnAst yet *)
      | Ast.EAdd(e, e') => intOrDouble(env, e, e', "plus")
      | Ast.ESub(e, e') => intOrDouble(env, e, e', "minus")
      | Ast.EMul(e, e') => intOrDouble(env, e, e', "times")
      | Ast.EDiv(e, e') => intOrDouble(env, e, e', "divide")
      | Ast.EMod(e, e') => mustInt(env, e, e', "mod")
      | Ast.ELshift(e, e') => mustInt(env, e, e', "lshift")
      | Ast.ERShift(e, e') => mustInt(env, e, e', "rshift")
      | Ast.ELt(e, e') => boolChecker(env, e, e', "lt")
      | Ast.EGt(e, e') => boolChecker(env, e, e', "gt")
      | Ast.ELe(e, e') => boolChecker(env, e, e', "le")
      | Ast.EGe(e, e') => boolChecker(env, e, e', "ge")
      | Ast.EEq(e, e') => boolChecker(env, e, e', "eeq")
      | Ast.ENeq(e, e') => boolChecker(env, e, e', "neq")
      | Ast.EAnd(e, e') => boolChecker(env, e, e', "and")
      | Ast.EOr( e, e') => boolChecker(env, e, e', "or")
      (*| Ast.EAsst(id, e) => (* not implemented in AnnAst yet *)
      (*| Ast.ECond(e, e', e'') => (* not implemented in AnnAst yet *)
                                                                          )

  (*  inferExpNoEnv e = e', where e' is the annotated expression
  *   corresponding to e.  e must be typeable from the empty environment.
  *
  *   Really you need to define a function inferExp that takes an 
  *   environment and an expression, and just call that function with
  *   an empty environment.
  *)
  fun inferExpNoEnv (e : Ast.exp) : AnnAst.exp =
      inferExp(Environment.empty(), e)
     (* case e of
        EInt e => EInt e
        | EAdd(e1,e2) => EAdd((inferExpNoEnv e1), (inferExpNoEnv e2), 
                        change((inferExpNoEnv e1), (inferExpNoEnv e2))) *)



  (*  checkPgm p = p', where p' is the annotated program corresponding to p'.
  *)
  fun checkPgm (p : Ast.program) : AnnAst.program =
    raise TypeError


end


