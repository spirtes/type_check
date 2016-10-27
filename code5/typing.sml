(*  COMP 321 Homework 5:  CPP typing engine.
*   
*   N. Danner
*)

structure Typing =
struct

(*Environment structure of type SplayMapFn*)
  structure Environ = SplayMapFn(struct
                                    type ord_key = string
                                    val compare = String.compare
                                    end)

  type env = AnnAst.typ Environ.map * (AnnAst.typ Environ.map list)
  type context = AnnAst.typ Environ.map list

  val emptyEnv : env = (Environ.empty, [Environ.empty])

(*exceptions*)
  exception TypeError
  exception UndeclaredError of Ast.id
  exception MultiplyDeclaredError of Ast.id
  exception ReturnTypeError

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

  fun incrHelper (id: Ast.id, ex: AnnAst.id*AnnAst.typ -> AnnAst.exp, cont: context) : AnnAst.exp =
    case cont of
      [] => raise UndeclaredError(id)
      | x :: xs => (case Environ.find(x, id) of
                      NONE => incrHelper(id, ex, xs)
                      | SOME t => ex(id, t))

  fun idHelper (id: Ast.id, cont: context) : AnnAst.exp =
    case cont of
      [] => raise UndeclaredError(id)
      | x :: xs => (case Environ.find(x, id) of
                      NONE => idHelper(id, xs)
                      | SOME t => AnnAst.EId(id, t))

  fun asstHelper (id: Ast.id, envi: env, e: Ast.exp) : AnnAst.exp =
    let val (_,cont) = envi
        fun asstHelperHelper (id: Ast.id, cont: context) : AnnAst.typ =
              case cont of
                [] => raise UndeclaredError(id)
                | x :: xs => (case Environ.find(x, id) of
                    NONE => asstHelperHelper(id, xs)
                    | SOME t => t)
        val ty = asstHelperHelper(id, cont)
        in
          if typeInt(e) 
            then (if ty = AnnAst.Tint 
                    then AnnAst.EAsst(id,inferExp(envi,e),ty)
                    else raise TypeError)
          else if typeString(e) 
                  then (if ty = AnnAst.Tstring
                        then AnnAst.EAsst(id,inferExp(envi,e),ty)
                        else raise TypeError)
          else if typeDouble(e) 
                then (if ty = AnnAst.Tdouble
                      then AnnAst.EAsst(id,inferExp(envi,e),ty)
                      else raise TypeError)
          else if typeBool(e) 
                then (if ty = AnnAst.Tbool
                      then AnnAst.EAsst(id,inferExp(envi,e),ty)
                      else raise TypeError)
          else raise TypeError
        end

  (* inferExp env e = e', where e' is the annotated expression
   * corresponding to e given an environment env. 
   *)
  and inferExp (envi: env, e: Ast.exp) : AnnAst.exp =
    let
      val (funcs, context) = envi
    in
      case e of
        Ast.EInt(n) => AnnAst.EInt(n)
        | Ast.EDouble(n) => AnnAst.EDouble(n)
        | Ast.EString(s) => AnnAst.EString(s)
        | Ast.ETrue => AnnAst.ETrue(true)
        | Ast.EFalse => AnnAst.EFalse(false)
        | Ast.EId(id) => idHelper(id, context)
        | Ast.ECall(id, (l)) => (case Environ.find(funcs, id) of
                                NONE => raise UndeclaredError(id)
                                | SOME t => (*AnnAst.ECall(inferExp(id), 
                                            (map (fn x => inferExp(x)) (l) ))
                                            need to do some lookups or something??*)
                                            raise TypeError
                                )
        | Ast.EPostIncr(id) => incrHelper(id, AnnAst.EPostIncr, context)
        | Ast.EPostDecr(id) => incrHelper(id, AnnAst.EPostDecr, context)
        | Ast.ENot(n) => if typeBool(e)
                          then AnnAst.ENot(inferExp(envi, n), AnnAst.Tbool)
                         else raise TypeError
        | Ast.EPreIncr(id) => incrHelper(id, AnnAst.EPreIncr, context)
        | Ast.EPreDecr(id) => incrHelper(id, AnnAst.EPreDecr, context)
        | Ast.EAdd(e0, e1) => if typeInt(e) 
                                then AnnAst.EAdd((inferExp(envi, e0), 
                                  inferExp(envi, e1)), AnnAst.Tint)
                             else if typeDouble(e)
                                then AnnAst.EAdd((inferExp(envi, e0),
                                  inferExp(envi, e1)), AnnAst.Tdouble)
                             else raise TypeError
        | Ast.ESub(e0, e1) => if typeInt(e) 
                                then AnnAst.ESub((inferExp(envi, e0), 
                                  inferExp(envi, e1)), AnnAst.Tint)
                             else if typeDouble(e)
                                then AnnAst.ESub((inferExp(envi, e0),
                                  inferExp(envi, e1)), AnnAst.Tdouble)
                             else raise TypeError
        | Ast.EMul(e0, e1) => if typeInt(e) 
                                then AnnAst.EMul((inferExp(envi, e0), 
                                  inferExp(envi, e1)), AnnAst.Tint)
                             else if typeDouble(e)
                                then AnnAst.EMul((inferExp(envi, e0),
                                  inferExp(envi, e1)), AnnAst.Tdouble)
                             else raise TypeError
        | Ast.EDiv(e0, e1) => if typeInt(e) 
                                then AnnAst.EDiv((inferExp(envi, e0), 
                                  inferExp(envi, e1)), AnnAst.Tint)
                             else if typeDouble(e)
                                then AnnAst.EDiv((inferExp(envi, e0),
                                  inferExp(envi, e1)), AnnAst.Tdouble)
                             else raise TypeError
        | Ast.EMod(e0, e1) => if typeInt(e) 
                                then AnnAst.EMod((inferExp(envi, e0),
                                  inferExp(envi, e1)), AnnAst.Tint)
                              else raise TypeError
        | Ast.ELShift(e0, e1) => if typeInt(e) 
                                then AnnAst.ELShift((inferExp(envi, e0),
                                  inferExp(envi, e1)), AnnAst.Tint)
                                else raise TypeError
        | Ast.ERShift(e0, e1) => if typeInt(e) 
                                then AnnAst.ERShift((inferExp(envi, e0),
                                  inferExp(envi, e1)), AnnAst.Tint)
                                else raise TypeError
        | Ast.ELt(e0, e1) => if typeBool(e)
                              then AnnAst.ELt((inferExp(envi, e0),
                                  inferExp(envi, e1)), AnnAst.Tbool)
                              else raise TypeError
        | Ast.EGt(e0, e1) => if typeBool(e)
                              then AnnAst.EGt((inferExp(envi, e0),
                                  inferExp(envi, e1)), AnnAst.Tbool)
                              else raise TypeError
        | Ast.ELe(e0, e1) => if typeBool(e)
                              then AnnAst.ELe((inferExp(envi, e0),
                                  inferExp(envi, e1)), AnnAst.Tbool)
                              else raise TypeError
        | Ast.EGe(e0, e1) => if typeBool(e)
                              then AnnAst.EGe((inferExp(envi, e0),
                                  inferExp(envi, e1)), AnnAst.Tbool)
                              else raise TypeError
        | Ast.EEq(e0, e1) => if typeBool(e)
                              then AnnAst.EEq((inferExp(envi, e0),
                                  inferExp(envi, e1)), AnnAst.Tbool)
                              else raise TypeError
        | Ast.ENeq(e0, e1) => if typeBool(e)
                              then AnnAst.ENeq((inferExp(envi, e0),
                                  inferExp(envi, e1)), AnnAst.Tbool)
                              else raise TypeError
        | Ast.EAnd(e0, e1) => if typeBool(e)
                              then AnnAst.EConj((inferExp(envi, e0),
                                  inferExp(envi, e1)), AnnAst.Tbool)
                              else raise TypeError
        | Ast.EOr(e0, e1) => if typeBool(e)
                              then AnnAst.EDisj((inferExp(envi, e0),
                                  inferExp(envi, e1)), AnnAst.Tbool)
                              else raise TypeError
        | Ast.EAsst(id, e) => asstHelper(id, envi, e)
        | Ast.ECond(e0, e1, e2) => if typeBool(e)
                                    then AnnAst.ECond((inferExp(envi, e0),
                                      inferExp(envi, e1), inferExp(envi, e2)),
                                      AnnAst.Tbool)
                                   else raise TypeError
    end                                                              

  (*  inferExpNoEnv e = e', where e' is the annotated expression
  *   corresponding to e.  e must be typeable from the empty environment.
  *
  *   Really you need to define a function inferExp that takes an 
  *   environment and an expression, and just call that function with
  *   an empty environment.
  *)
  fun inferExpNoEnv (e : Ast.exp) : AnnAst.exp =
    inferExp(emptyEnv, e)


  (*  checkPgm p = p', where p' is the annotated program corresponding to p'.
  *)
  fun checkPgm (p : Ast.program) : AnnAst.program =
    raise TypeError


end


