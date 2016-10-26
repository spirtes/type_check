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

  (* QUESTION: so right now 4 < 5 / 6 < 7 will 
   * type check because everything is an int
   * and those two operators take ints
   * however how do we tell them that if it is
   * (4 < 5) / (6 < 7) AKA true/true should NOT
   * type check??? 
   * as opposed to 4 < (5/6) < 7*)

  (* determines if the expression is 
   * or is comprised of type int exps *)
  fun typeInt (e0 : Ast.exp) : bool =
    case e0 of
      Ast.EInt(n) => true
      | (Ast.EAdd(n1,n2) | Ast.ESub(n1,n2)
        | Ast.EMul(n1,n2) | Ast.EDiv(n1,n2)
        | Ast.EMod(n1,n2) | Ast.ELShift(n1,n2)
        | Ast.ERShift(n1,n2) | Ast.EEq(n1,n2)
        | Ast.ENeq(n1,n2) | Ast.EGt(n1,n2)
        | Ast.EGe(n1,n2) | Ast.ELt(n1,n2)
        | Ast.ELe(n1,n2)) => typeInt(n1) andalso typeInt(n2)
      | _ => false

  fun typeDouble (e0 : Ast.exp) : bool =
    case e0 of
      Ast.EDouble(n) => true
      | (Ast.EAdd(n1,n2) | Ast.ESub(n1,n2)
        | Ast.EMul(n1,n2) | Ast.EDiv(n1,n2)
        | Ast.EEq(n1,n2) | Ast.ENeq(n1,n2)
        | Ast.EGt(n1,n2) | Ast.EGe(n1,n2)
        | Ast.ELt(n1,n2) | Ast.ELe(n1,n2)) =>
          typeDouble(n1) andalso typeDouble(n2)
      | _ => false

  fun typeString (e0 : Ast.exp) : bool =
    case e0 of
      Ast.EString(s) => true
      | (Ast.EEq(n1,n2) | Ast.ENeq(n1,n2)) =>
        typeString(n1) andalso typeString(n2)
      | _ => false

  fun typeBool (e0 : Ast.exp) : bool =
    case e0 of
      Ast.ETrue => true
      | Ast.EFalse => true
      | (Ast.ELt(n1,n2) | Ast.ELe(n1,n2) 
        | Ast.EGt(n1,n2) | Ast.EGe(n1,n2)) =>
          (typeInt(n1) andalso typeInt(n2)) orelse
           (typeDouble(n1) andalso typeDouble(n2))
      | (Ast.EEq(n1,n2) | Ast.ENeq(n1,n2)) =>
          (typeInt(n1) andalso typeInt(n2) orelse
            (typeDouble(n1) andalso typeDouble(n2)) orelse
            (typeString(n1) andalso typeString(n2)) orelse
            (typeBool(n1) andalso typeBool(n2))
            )
      | (Ast.EAnd(n1,n2) | Ast.EOr(n1,n2)) =>
          typeBool(n1) andalso typeBool(n2)
      | Ast.ENot(n) => typeBool(n)
      | Ast.ECond(n1,n2,n3) => (typeBool(n1)
          andalso ((typeInt(n2) andalso typeInt(n3)) orelse
                    (typeDouble(n2) andalso typeDouble(n3)) orelse
                    (typeString(n2) andalso typeString(n3)) orelse
                    (typeBool(n2) andalso typeBool(n3))) )
      |_ => false

 (*  fun typeIsolator(e : Ast.exp) : AnnAst.typ =
      case e of
        Ast.EInt _ => AnnAst.Tint
        | Ast.EDouble _ => AnnAst.Tdouble
        | Ast.EString _ => AnnAst.Tstring
        | Ast.ETrue => AnnAst.Tbool
        | Ast.EFalse => AnnAst.Tbool
        | Ast.EId(id) => case Environment.find(env, id) of
                          NONE => raise UndeclaredError(id)
                          | SOME t => t
        | Ast.ECall *)

  (* inferExp env e = e', where e' is the annotated expression
   * corresponding to e given an environment env. 
   *)
  and inferExp (env,(*type??*) e: Ast.exp) : AnnAst.exp =
    case e of
      Ast.EInt(n) => AnnAst.EInt(n)
      | Ast.EDouble(n) => AnnAst.EDouble(n)
      | Ast.EString(s) => AnnAst.EString(s)
      | Ast.ETrue => AnnAst.ETrue(true)
      | Ast.EFalse => AnnAst.EFalse(false)
      | Ast.EId(id) => (case Environment.find(env, id) of
                          NONE => raise UndeclaredError(id)
                          | SOME t => AnnAst.EId(id, t))
      | Ast.ECall(id, (l)) => (case Environment.find(env, id) of
                              NONE => raise UndeclaredError(id)
                              | SOME t => (*AnnAst.ECall(inferExp(id), 
                                          (map (fn x => inferExp(x)) (l) ))
                                          need to do some lookups or something??*)
                                          raise TypeError
                              )
      | Ast.EPostIncr(id) => (case Environment.find(env, id) of 
                              NONE => raise UndeclaredError(id)
                              | SOME t => if t = AnnAst.Tint 
                                          then AnnAst.EPostIncr(id, t)
                                          else raise TypeError
                             ) 
      | Ast.EPostDecr(id) => (case Environment.find(env, id) of
                              NONE => raise UndeclaredError(id)
                              | SOME t => if t = AnnAst.Tint
                                          then AnnAst.EPostDecr(id, t)
                                          else raise TypeError
                             )
      | Ast.ENot(n) => if typeBool(e)
                        then AnnAst.ENot(inferExp(env, n), AnnAst.Tbool)
                       else raise TypeError
      | Ast.EPreIncr(id) => (case Environment.find(env, id) of
                              NONE => raise UndeclaredError(id)
                              | SOME t => if t = AnnAst.Tint
                                          then AnnAst.EPreIncr(id, t)
                                          else raise TypeError
                             )
      | Ast.EPreDecr(id) => (case Environment.find(env, id) of
                              NONE => raise UndeclaredError(id)
                              | SOME t => if t = AnnAst.Tint
                                          then AnnAst.EPreDecr(id, t)
                                          else raise TypeError
                             )
      | Ast.EAdd(e0, e1) => if typeInt(e) 
                              then AnnAst.EAdd((inferExp(env, e0), 
                                inferExp(env, e1)), AnnAst.Tint)
                           else if typeDouble(e)
                              then AnnAst.EAdd((inferExp(env, e0),
                                inferExp(env, e1)), AnnAst.Tdouble)
                           else raise TypeError
      | Ast.ESub(e0, e1) => if typeInt(e) 
                              then AnnAst.ESub((inferExp(env, e0), 
                                inferExp(env, e1)), AnnAst.Tint)
                           else if typeDouble(e)
                              then AnnAst.ESub((inferExp(env, e0),
                                inferExp(env, e1)), AnnAst.Tdouble)
                           else raise TypeError
      | Ast.EMul(e0, e1) => if typeInt(e) 
                              then AnnAst.EMul((inferExp(env, e0), 
                                inferExp(env, e1)), AnnAst.Tint)
                           else if typeDouble(e)
                              then AnnAst.EMul((inferExp(env, e0),
                                inferExp(env, e1)), AnnAst.Tdouble)
                           else raise TypeError
      | Ast.EDiv(e0, e1) => if typeInt(e) 
                              then AnnAst.EDiv((inferExp(env, e0), 
                                inferExp(env, e1)), AnnAst.Tint)
                           else if typeDouble(e)
                              then AnnAst.EDiv((inferExp(env, e0),
                                inferExp(env, e1)), AnnAst.Tdouble)
                           else raise TypeError
      | Ast.EMod(e0, e1) => if typeInt(e) 
                              then AnnAst.EMod((inferExp(env, e0),
                                inferExp(env, e1)), AnnAst.Tint)
                            else raise TypeError
      | Ast.ELShift(e0, e1) => if typeInt(e) 
                              then AnnAst.ELShift((inferExp(env, e0),
                                inferExp(env, e1)), AnnAst.Tint)
                              else raise TypeError
      | Ast.ERShift(e0, e1) => if typeInt(e) 
                              then AnnAst.ERShift((inferExp(env, e0),
                                inferExp(env, e1)), AnnAst.Tint)
                              else raise TypeError
      | Ast.ELt(e0, e1) => if typeBool(e)
                            then AnnAst.ELt((inferExp(env, e0),
                                inferExp(env, e1)), AnnAst.Tbool)
                            else raise TypeError
      | Ast.EGt(e0, e1) => if typeBool(e)
                            then AnnAst.EGt((inferExp(env, e0),
                                inferExp(env, e1)), AnnAst.Tbool)
                            else raise TypeError
      | Ast.ELe(e0, e1) => if typeBool(e)
                            then AnnAst.ELe((inferExp(env, e0),
                                inferExp(env, e1)), AnnAst.Tbool)
                            else raise TypeError
      | Ast.EGe(e0, e1) => if typeBool(e)
                            then AnnAst.EGe((inferExp(env, e0),
                                inferExp(env, e1)), AnnAst.Tbool)
                            else raise TypeError
      | Ast.EEq(e0, e1) => if typeBool(e)
                            then AnnAst.EEq((inferExp(env, e0),
                                inferExp(env, e1)), AnnAst.Tbool)
                            else raise TypeError
      | Ast.ENeq(e0, e1) => if typeBool(e)
                            then AnnAst.ENeq((inferExp(env, e0),
                                inferExp(env, e1)), AnnAst.Tbool)
                            else raise TypeError
      | Ast.EAnd(e0, e1) => if typeBool(e)
                            then AnnAst.EConj((inferExp(env, e0),
                                inferExp(env, e1)), AnnAst.Tbool)
                            else raise TypeError
      | Ast.EOr(e0, e1) => if typeBool(e)
                            then AnnAst.EDisj((inferExp(env, e0),
                                inferExp(env, e1)), AnnAst.Tbool)
                            else raise TypeError
      | Ast.EAsst(id, e) => case Environment.find(id, env) of
                              NONE => raise UndeclaredError(id)
                              | SOME t => let 
                                            val exptype(n) = inferExp(e)
                                          in
                                            if exptype = AnnAst.EInt 
                                          end
      | Ast.ECond(e0, e1, e2) => if typeBool(e)
                                  then AnnAst.ECond((inferExp(env, e0),
                                    inferExp(env, e1), inferExp(env, e2)),
                                    AnnAst.Tbool)
                                else raise TypeError
                                                                          

  (*  inferExpNoEnv e = e', where e' is the annotated expression
  *   corresponding to e.  e must be typeable from the empty environment.
  *
  *   Really you need to define a function inferExp that takes an 
  *   environment and an expression, and just call that function with
  *   an empty environment.
  *)
  fun inferExpNoEnv (e : Ast.exp) : AnnAst.exp =
    inferExp(Environment.empty, e)


  (*  checkPgm p = p', where p' is the annotated program corresponding to p'.
  *)
  fun checkPgm (p : Ast.program) : AnnAst.program =
    raise TypeError


end


