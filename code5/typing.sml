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
  fun intOrDouble (env (*type??*), e1: Ast.exp, e2: Ast.exp, opr: string) : AnnAst.exp =
    case e2 of (* starting from e2 b/c right associative *)
      Ast.EInt(n) => (case e1 of 
                              Ast.EInt(n') => (case opr of
                                            "plus" => AnnAst.EAdd((AnnAst.EInt(n),
                                                                  AnnAst.EInt(n')),
                                                                  AnnAst.Tint)
                                            | "minus" => AnnAst.ESub((AnnAst.EInt(n),
                                                                      AnnAst.EInt(n')),
                                                                    AnnAst.Tint)
                                            | "times" => AnnAst.EMul((AnnAst.EInt(n),
                                                                      AnnAst.EInt(n')),
                                                                      AnnAst.Tint)
                                            | "div" => AnnAst.EDiv((AnnAst.EInt(n),
                                                                    AnnAst.EInt(n')),
                                                                    AnnAst.Tint)
                                            | _ => raise TypeError
                                            )
                              | (Ast.EAdd(n1,n2) | Ast.ESub(n1, n2) 
                                  | Ast.EMul(n1, n2) | Ast.EDiv(n1, n2) 
                                  | Ast.EMod(n1, n2) | Ast.ELShift(n1, n2) 
                                  | Ast.ERShift(n1, n2)) => intOrDouble(env, n1, n2, opr)
                              | _ => raise TypeError)
      | Ast.EDouble(n) => (case e1 of
                              Ast.EDouble(n') => (case opr of
                                                      "plus" => AnnAst.EAdd((AnnAst.EDouble(n),
                                                                             AnnAst.EDouble(n')),
                                                                             AnnAst.Tdouble)
                                                    | "minus" => AnnAst.ESub((AnnAst.EDouble(n),
                                                                             AnnAst.EDouble(n')),
                                                                             AnnAst.Tdouble)
                                                    | "times" => AnnAst.EMul((AnnAst.EDouble(n),
                                                                             AnnAst.EDouble(n')),
                                                                             AnnAst.Tdouble)
                                                    | "div" => AnnAst.EDiv((AnnAst.EDouble(n),
                                                                             AnnAst.EDouble(n')),
                                                                             AnnAst.Tdouble)
                                                    | _ => raise TypeError
                                                    )
                              | (Ast.EAdd(n1,n2) | Ast.ESub(n1, n2) 
                                  | Ast.EMul(n1, n2) | Ast.EDiv(n1, n2) 
                                  | Ast.EMod(n1, n2) | Ast.ELShift(n1, n2) 
                                  | Ast.ERShift(n1, n2)) => intOrDouble(env, n1, n2, opr)
                              | _ => raise TypeError)
      | (Ast.EAdd(n1,n2) | Ast.ESub(n1, n2) | Ast.EMul(n1, n2) 
      | Ast.EDiv(n1, n2) | Ast.EMod(n1, n2) | Ast.ELShift(n1, n2) 
      | Ast.ERShift(n1, n2)) => intOrDouble(env, n1, n2, opr)
      | _ => raise TypeError

  fun mustInt (env (*type??*), e1: Ast.exp, e2: Ast.exp, opr: string) : AnnAst.exp =
    case e2 of
      Ast.EInt(n) => (case e1 of
                          Ast.EInt(n') => (case opr of
                                                "mod" => AnnAst.EMod((AnnAst.EInt(n),
                                                                    AnnAst.EInt(n')),
                                                                    AnnAst.Tint)
                                              | "lshift" => AnnAst.ELShift((AnnAst.EInt(n),
                                                                    AnnAst.EInt(n')),
                                                                    AnnAst.Tint)
                                              | "rshift" => AnnAst.ERShift((AnnAst.EInt(n),
                                                                    AnnAst.EInt(n')),
                                                                    AnnAst.Tint)
                                              | _ => raise TypeError
                                              )
                          | (Ast.EAdd(n1,n2) | Ast.ESub(n1, n2) 
                            | Ast.EMul(n1, n2) | Ast.EDiv(n1, n2) 
                            | Ast.EMod(n1, n2) | Ast.ELShift(n1, n2) 
                            | Ast.ERShift(n1, n2)) => mustInt(env, n1, n2, opr)
                          | _ => raise TypeError
                        )
      | (Ast.EAdd(n1,n2) | Ast.ESub(n1, n2) | Ast.EMul(n1, n2) 
      | Ast.EDiv(n1, n2) | Ast.EMod(n1, n2) | Ast.ELShift(n1, n2) 
      | Ast.ERShift(n1, n2)) => mustInt(env, n1, n2, opr)
      | _ => raise TypeError

    fun boolChecker (env (* type?? *), e1: Ast.exp, e2: Ast.exp, opr: string) : AnnAst.exp =
      case e2 of
        Ast.ETrue => (case e1 of
                          Ast.ETrue =>
                            (case opr of
                                  "eeq" => AnnAst.EEq((AnnAst.ETrue(true),
                                                       AnnAst.ETrue(true)), 
                                                      AnnAst.Tbool)
                                  | "neq" => AnnAst.ENeq((AnnAst.ETrue(true),
                                                       AnnAst.ETrue(true)), 
                                                      AnnAst.Tbool)
                                  | "lt" => AnnAst.ELt((AnnAst.ETrue(true),
                                                       AnnAst.ETrue(true)), 
                                                      AnnAst.Tbool)
                                  | "gt" => AnnAst.EGt((AnnAst.ETrue(true),
                                                       AnnAst.ETrue(true)), 
                                                      AnnAst.Tbool)
                                  | "le" => AnnAst.ELe((AnnAst.ETrue(true),
                                                       AnnAst.ETrue(true)), 
                                                      AnnAst.Tbool)
                                  | "ge" => AnnAst.EGe((AnnAst.ETrue(true),
                                                       AnnAst.ETrue(true)), 
                                                      AnnAst.Tbool)
                                  | "and" => AnnAst.EConj((AnnAst.ETrue(true),
                                                       AnnAst.ETrue(true)), 
                                                      AnnAst.Tbool)
                                  | "or" => AnnAst.EDisj((AnnAst.ETrue(true),
                                                       AnnAst.ETrue(true)), 
                                                      AnnAst.Tbool)
                                  | _ => raise TypeError)
                          | Ast.EFalse =>
                            (case opr of
                                "eeq" => AnnAst.EEq((AnnAst.ETrue(true),
                                                       AnnAst.EFalse(false)), 
                                                      AnnAst.Tbool)
                                | "neq" => AnnAst.ENeq((AnnAst.ETrue(true),
                                                       AnnAst.EFalse(false)), 
                                                      AnnAst.Tbool)
                                | "lt" => AnnAst.ELt((AnnAst.ETrue(true),
                                                       AnnAst.EFalse(false)), 
                                                      AnnAst.Tbool)
                                | "gt" => AnnAst.EGt((AnnAst.ETrue(true),
                                                       AnnAst.EFalse(false)), 
                                                      AnnAst.Tbool)
                                | "le" => AnnAst.ELe((AnnAst.ETrue(true),
                                                       AnnAst.EFalse(false)), 
                                                      AnnAst.Tbool)
                                | "ge" => AnnAst.EGe((AnnAst.ETrue(true),
                                                       AnnAst.EFalse(false)), 
                                                      AnnAst.Tbool)
                                | "and" => AnnAst.EConj((AnnAst.ETrue(true),
                                                       AnnAst.EFalse(false)), 
                                                      AnnAst.Tbool)
                                | "or" => AnnAst.EDisj((AnnAst.ETrue(true),
                                                       AnnAst.EFalse(false)), 
                                                      AnnAst.Tbool)
                                | _ => raise TypeError)
                          | (Ast.EAdd(n1,n2) | Ast.ESub(n1,n2)
                              | Ast.EMul(n1,n2) | Ast.EDiv(n1,n2)
                              | Ast.EMod(n1,n2) | Ast.ELShift(n1,n2) 
                              | Ast.ERShift(n1,n2) | Ast.EEq(n1,n2) 
                              | Ast.ENeq(n1,n2)| Ast.ELt(n1,n2) 
                              | Ast.EGt(n1,n2) | Ast.ELe(n1,n2)
                              | Ast.EGe(n1,n2)| Ast.EAnd(n1,n2) 
                              | Ast.EOr(n1,n2)) => 
                                boolChecker(env, n1, n2, opr)
                          | _ => raise TypeError
                                    )
        | Ast.EFalse => (case e1 of
                              Ast.ETrue =>
                                (case opr of
                                  "eeq" => AnnAst.EEq((AnnAst.EFalse(false),
                                                       AnnAst.ETrue(true)), 
                                                      AnnAst.Tbool)
                                | "neq" => AnnAst.ENeq((AnnAst.EFalse(false),
                                                       AnnAst.ETrue(true)), 
                                                      AnnAst.Tbool)
                                | "lt" => AnnAst.ELt((AnnAst.EFalse(false),
                                                       AnnAst.ETrue(true)), 
                                                      AnnAst.Tbool)
                                | "gt" => AnnAst.EGt((AnnAst.EFalse(false),
                                                       AnnAst.ETrue(true)), 
                                                      AnnAst.Tbool)
                                | "le" => AnnAst.ELe((AnnAst.EFalse(false),
                                                       AnnAst.ETrue(true)), 
                                                      AnnAst.Tbool)
                                | "ge" => AnnAst.EGe((AnnAst.EFalse(false),
                                                       AnnAst.ETrue(true)), 
                                                      AnnAst.Tbool)
                                | "and" => AnnAst.EConj((AnnAst.EFalse(false),
                                                       AnnAst.ETrue(true)), 
                                                      AnnAst.Tbool)
                                | "or" => AnnAst.EDisj((AnnAst.EFalse(false),
                                                       AnnAst.ETrue(true)), 
                                                      AnnAst.Tbool)
                                | _ => raise TypeError)
                            | Ast.EFalse =>
                              (case opr of
                                "eeq" => AnnAst.EEq((AnnAst.EFalse(false),
                                                       AnnAst.EFalse(false)), 
                                                      AnnAst.Tbool)
                                | "neq" => AnnAst.ENeq((AnnAst.EFalse(false),
                                                       AnnAst.EFalse(false)), 
                                                      AnnAst.Tbool)
                                | "lt" => AnnAst.ELt((AnnAst.EFalse(false),
                                                       AnnAst.EFalse(false)), 
                                                      AnnAst.Tbool)
                                | "gt" => AnnAst.EGt((AnnAst.EFalse(false),
                                                       AnnAst.EFalse(false)), 
                                                      AnnAst.Tbool)
                                | "le" => AnnAst.ELe((AnnAst.EFalse(false),
                                                       AnnAst.EFalse(false)), 
                                                      AnnAst.Tbool)
                                | "ge" => AnnAst.EGe((AnnAst.EFalse(false),
                                                       AnnAst.EFalse(false)), 
                                                      AnnAst.Tbool)
                                | "and" => AnnAst.EConj((AnnAst.EFalse(false),
                                                       AnnAst.EFalse(false)), 
                                                      AnnAst.Tbool)
                                | "or" => AnnAst.EDisj((AnnAst.EFalse(false),
                                                       AnnAst.EFalse(false)), 
                                                      AnnAst.Tbool)
                                | _ => raise TypeError) 
                            | (Ast.EAdd(n1,n2) | Ast.ESub(n1,n2)
                              | Ast.EMul(n1,n2) | Ast.EDiv(n1,n2)
                              | Ast.EMod(n1,n2) | Ast.ELShift(n1,n2) 
                              | Ast.ERShift(n1,n2) | Ast.EEq(n1,n2) 
                              | Ast.ENeq(n1,n2)| Ast.ELt(n1,n2) 
                              | Ast.EGt(n1,n2) | Ast.ELe(n1,n2)
                              | Ast.EGe(n1,n2)| Ast.EAnd(n1,n2) 
                              | Ast.EOr(n1,n2)) => 
                                boolChecker(env, n1, n2, opr)
                            |_ => raise TypeError
                                    )
        | (Ast.EAdd(n1,n2) | Ast.ESub(n1,n2)| Ast.EMul(n1,n2) | Ast.EDiv(n1,n2)
            | Ast.EMod(n1,n2) | Ast.ELShift(n1,n2) | Ast.ERShift(n1,n2)
            | Ast.EEq(n1,n2) | Ast.ENeq(n1,n2)| Ast.ELt(n1,n2) | Ast.EGt(n1,n2)
            | Ast.ELe(n1,n2)| Ast.EGe(n1,n2)| Ast.EAnd(n1,n2) | Ast.EOr(n1,n2)) => 
                        boolChecker(env, n1, n2, opr)
        |_ => raise TypeError

  (* inferExp env e = e', where e' is the annotated expression
   * corresponding to e given an environment env. 
   *)
  fun inferExp (env, (*type??*)e: Ast.exp) : AnnAst.exp =
    case e of
      Ast.EInt(n) => AnnAst.EInt(n)
      | Ast.EDouble(n) => AnnAst.EDouble(n)
      | Ast.EString(s) => AnnAst.EString(s)
      | Ast.ETrue => AnnAst.ETrue(true)
      | Ast.EFalse => AnnAst.EFalse(false)
      (*| Ast.EId(id) =>  not implemented in AnnAst yet *) 
      (*| Ast.ECall(id, l) => not implemented in AnnAst yet *)
      (*| Ast.EPostIncr(id) => not implemented in AnnAst yet *)
      (*| Ast.EPostDecr(id) => not implemented in AnnAst yet *)
      (*| Ast.ENot(n) => not implemented in AnnAst yet *)
      (*| Ast.EPreIncr(id) => not implemented in AnnAst yet *)
      (*| Ast.EPreDecr(id) => not implemented in AnnAst yet *)
      | Ast.EAdd(e, e') => intOrDouble(env, e, e', "plus")
      | Ast.ESub(e, e') => intOrDouble(env, e, e', "minus")
      | Ast.EMul(e, e') => intOrDouble(env, e, e', "times")
      | Ast.EDiv(e, e') => intOrDouble(env, e, e', "divide")
      | Ast.EMod(e, e') => mustInt(env, e, e', "mod")
      | Ast.ELShift(e, e') => mustInt(env, e, e', "lshift")
      | Ast.ERShift(e, e') => mustInt(env, e, e', "rshift")
      | Ast.ELt(e, e') => boolChecker(env, e, e', "lt")
      | Ast.EGt(e, e') => boolChecker(env, e, e', "gt")
      | Ast.ELe(e, e') => boolChecker(env, e, e', "le")
      | Ast.EGe(e, e') => boolChecker(env, e, e', "ge")
      | Ast.EEq(e, e') => boolChecker(env, e, e', "eeq")
      | Ast.ENeq(e, e') => boolChecker(env, e, e', "neq")
      | Ast.EAnd(e, e') => boolChecker(env, e, e', "and")
      | Ast.EOr(e, e') => (boolChecker(env, e, e', "or")
      (*| Ast.EAsst(id, e) => not implemented in AnnAst yet *)
      (*| Ast.ECond(e, e', e'') => not implemented in AnnAst yet *)
                                                                          )

  (*  inferExpNoEnv e = e', where e' is the annotated expression
  *   corresponding to e.  e must be typeable from the empty environment.
  *
  *   Really you need to define a function inferExp that takes an 
  *   environment and an expression, and just call that function with
  *   an empty environment.
  *)
  fun inferExpNoEnv (e : Ast.exp) : AnnAst.exp =
    inferExp(Environment.empty, e)
     (* case e of
        EInt e => EInt e
        | EAdd(e1,e2) => EAdd((inferExpNoEnv e1), (inferExpNoEnv e2), 
                        change((inferExpNoEnv e1), (inferExpNoEnv e2))) *)



  (*  checkPgm p = p', where p' is the annotated program corresponding to p'.
  *)
  fun checkPgm (p : Ast.program) : AnnAst.program =
    raise TypeError


end


