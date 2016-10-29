(*  COMP 321 Homework 5:  CPP typing engine.
*   
*   Torie Davids
*   Lex Spirtes
*)

structure Typing =
struct

 (*Environment structure of type SplayMapFn*)
  structure Environ = SplayMapFn(struct
                                    type ord_key = string
                                    val compare = String.compare
                                    end)

  (* sign : keys are function ids, values are type, param list (type, id) tuples *)
  (* context: list of (type, id) maps that display local environments and variables in scope *)
  (* the env is a tuple of signatures and contexts *)
  type env = (AnnAst.typ * AnnAst.typ list) Environ.map * (AnnAst.typ Environ.map list)
  type sign = (AnnAst.typ * AnnAst.typ list) Environ.map

  type context = AnnAst.typ Environ.map list

  val emptyEnv : env = (Environ.empty, [Environ.empty])
  val emptyMap : (AnnAst.typ * AnnAst.typ list) Environ.map = Environ.empty
  val emptyContext : AnnAst.typ Environ.map = Environ.empty

(*exceptions*)
  exception TypeError
  exception UndeclaredError of Ast.id
  exception MultiplyDeclaredError of Ast.id
  exception ReturnTypeError

  (*change an Ast.id to an Annast.id*)
  fun idToId(i : Ast.id) : AnnAst.id =
    i

  (* checks to see if a stored ID is an int *)  
  fun idHelperBoolI (id: Ast.id, cont: context) : bool =
  case cont of
    [] => raise UndeclaredError(id)
    | x :: xs => (case Environ.find(x, id) of
                    NONE => idHelperBoolI(id, xs)
                    | SOME t => if t = AnnAst.Tint 
                                then true
                                else false)

  (* checks to see if a stored ID is a double *)
  fun idHelperBoolD (id: Ast.id, cont: context) : bool =
  case cont of
    [] => raise UndeclaredError(id)
    | x :: xs => (case Environ.find(x, id) of
                    NONE => idHelperBoolD(id, xs)
                    | SOME t => if t = AnnAst.Tdouble 
                                then true
                                else false)

    (* checks to see if a stored ID is a string *)
    fun idHelperBoolS (id: Ast.id, cont: context) : bool =
    case cont of
    [] => raise UndeclaredError(id)
    | x :: xs => (case Environ.find(x, id) of
                    NONE => idHelperBoolS(id, xs)
                    | SOME t => if t = AnnAst.Tstring
                                then true
                                else false)

    (* checks to see if a stored ID is a bool *)
    fun idHelperBoolB (id: Ast.id, cont: context) : bool =
    case cont of
    [] => raise UndeclaredError(id)
    | x :: xs => (case Environ.find(x, id) of
                    NONE => idHelperBoolB(id, xs)
                    | SOME t => if t = AnnAst.Tbool
                                then true
                                else false)

  (* determines if the expression is 
   * or is comprised of type int exps *)
  fun typeInt (e0 : Ast.exp, envi: env) : bool =
    let val (funcs, cont) = envi
    in
      case e0 of
        Ast.EInt(n) => true
        | (Ast.EAdd(n1,n2) | Ast.ESub(n1,n2)
          | Ast.EMul(n1,n2) | Ast.EDiv(n1,n2)
          | Ast.EMod(n1,n2) | Ast.ELShift(n1,n2)
          | Ast.ERShift(n1,n2) | Ast.EEq(n1,n2)
          | Ast.ENeq(n1,n2) | Ast.EGt(n1,n2)
          | Ast.EGe(n1,n2) | Ast.ELt(n1,n2)
          | Ast.ELe(n1,n2)) => typeInt(n1, envi) andalso typeInt(n2, envi)
        | Ast.EId(id) => idHelperBoolI(id, cont)
        | _ => false
    end

  (* determines if the expression is or
   * is comprised of type double exps *)
  fun typeDouble (e0 : Ast.exp, envi: env) : bool =
    let val (funcs, cont) = envi
    in
      case e0 of
        Ast.EDouble(n) => true
       | (Ast.EAdd(n1,n2) | Ast.ESub(n1,n2)
          | Ast.EMul(n1,n2) | Ast.EDiv(n1,n2)
          | Ast.EEq(n1,n2) | Ast.ENeq(n1,n2)
          | Ast.EGt(n1,n2) | Ast.EGe(n1,n2)
          | Ast.ELt(n1,n2) | Ast.ELe(n1,n2)) =>
            typeDouble(n1, envi) andalso typeDouble(n2, envi)
       | Ast.EId(id) => idHelperBoolD(id, cont)
       | _ => false
    end

    (* determines if the expression is or
   * is comprised of type string exps *)
    fun typeString (e0 : Ast.exp, envi: env) : bool =
      let val (funcs, cont) = envi
      in
      case e0 of
        Ast.EString(s) => true
        | (Ast.EEq(n1,n2) | Ast.ENeq(n1,n2)) =>
          typeString(n1, envi) andalso typeString(n2, envi)
        | Ast.EId(id) => idHelperBoolS(id, cont)
        | _ => false
      end

    (* determines if the expression is or
   * is comprised of type bool exps *)
    fun typeBool (e0 : Ast.exp, envi: env) : bool =
      let val (func, cont) = envi
      in
      case e0 of
        Ast.ETrue => true
        | Ast.EFalse => true
        | (Ast.ELt(n1,n2) | Ast.ELe(n1,n2) 
          | Ast.EGt(n1,n2) | Ast.EGe(n1,n2)) =>
            (typeInt(n1, envi) andalso typeInt(n2, envi)) orelse
             (typeDouble(n1, envi) andalso typeDouble(n2, envi))
        | (Ast.EEq(n1,n2) | Ast.ENeq(n1,n2)) =>
            (typeInt(n1, envi) andalso typeInt(n2, envi) orelse
              (typeDouble(n1, envi) andalso typeDouble(n2, envi)) orelse
              (typeString(n1, envi) andalso typeString(n2, envi)) orelse
              (typeBool(n1, envi) andalso typeBool(n2, envi))
              )
        | (Ast.EAnd(n1,n2) | Ast.EOr(n1,n2)) =>
            typeBool(n1, envi) andalso typeBool(n2, envi)
        | Ast.ENot(n) => typeBool(n, envi)
        | Ast.ECond(n1,n2,n3) => (typeBool(n1, envi)
            andalso ((typeInt(n2, envi) andalso typeInt(n3, envi)) orelse
                      (typeDouble(n2, envi) andalso typeDouble(n3, envi)) orelse
                      (typeString(n2, envi) andalso typeString(n3, envi)) orelse
                      (typeBool(n2, envi) andalso typeBool(n3, envi))) )
        | Ast.EId(id) => idHelperBoolB(id, cont)
        |_ => false
      end

  (* checks for id presence in the increment operators *)
  fun incrHelper (id: Ast.id, ex: AnnAst.id*AnnAst.typ -> AnnAst.exp, cont: context) : AnnAst.exp =
    case cont of
      [] => raise UndeclaredError(id)
      | x :: xs => (case Environ.find(x, id) of
                      NONE => incrHelper(id, ex, xs)
                      | SOME t => ex(id, t))

   (* returns true if the expression type checks with the given type *)
  fun typeMatch (ty : AnnAst.typ, e: Ast.exp, envi: env) : bool =
    if ((typeInt(e, envi) andalso ty = AnnAst.Tint) orelse
        (typeDouble(e, envi) andalso ty = AnnAst.Tdouble) orelse
        (typeBool(e, envi) andalso ty = AnnAst.Tbool) orelse
        (typeString(e, envi) andalso ty = AnnAst.Tstring))
    then true
    else false

    (* checks for presence of an id in the environment *)
   fun idHelper (id: Ast.id, cont: context) : AnnAst.exp =
  case cont of
    [] => raise UndeclaredError(id)
    | x :: xs => (case Environ.find(x, id) of
                    NONE => idHelper(id, xs)
                    | SOME t => AnnAst.EId(id, t))

    (* checks if a function is in the environment and if the params are valid*)
    fun funcLookup (id: Ast.id,envi: env,param: Ast.exp list,dex: int): AnnAst.id * AnnAst.typ =
      let val (funcs, cont) = envi
          val newL = List.length(param)
          fun setCurrFun(id: Ast.id) : AnnAst.typ * AnnAst.typ list =
            case Environ.find(funcs, id) of
              NONE => raise UndeclaredError(id)
              | SOME (currFret, fParam) => (currFret, fParam)
          val (currFRet, fParams) = setCurrFun(id)
          val storedL = List.length(fParams)
      in
        (* NOTE: we have been trying to check param lengths against stored param lengths
          * but they are different lengths depending on where they are in the function,
          * and not their content, so we cannot figure out how to fix this issue.
          *)
       (* case newL = storedL of
          false => raise TypeError
          | true => *)case param of
                    [] => (idToId(id), currFRet) 
                    | x :: xs => let
                      val (ty) = List.nth(fParams, dex)
                      in
                        case typeMatch(ty, x, envi) of
                          true => funcLookup(id, envi, xs, dex+1)
                          | false => raise TypeError
                      end
      end

      (* checks if an id in an assignment operation is declared and 
        * creates the AnnAst.exp if it is
        *)
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
           if typeMatch(ty, e, envi) then AnnAst.EAsst(id, inferExp(envi,e),ty)
          else raise TypeError
        end

      (* changes an Ast.exp list to an AnnAst.exp list using inferExp*)
      and expToExp(envi: env, e : Ast.exp list) : AnnAst.exp list =
        case e of
          [] => []
          | x :: xs => inferExp(envi, x)::expToExp(envi, xs)

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
        | Ast.ECall(id, (l)) => AnnAst.ECall(funcLookup(id, envi, l, 0),
                                             expToExp(envi, l))

        | Ast.EPostIncr(id) => incrHelper(id, AnnAst.EPostIncr, context)
        | Ast.EPostDecr(id) => incrHelper(id, AnnAst.EPostDecr, context)
        | Ast.ENot(n) => if typeBool(e, envi)
                          then AnnAst.ENot(inferExp(envi, n), AnnAst.Tbool)
                         else raise TypeError
        | Ast.EPreIncr(id) => incrHelper(id, AnnAst.EPreIncr, context)
        | Ast.EPreDecr(id) => incrHelper(id, AnnAst.EPreDecr, context)
        | Ast.EAdd(e0, e1) => if typeInt(e, envi) 
                                then AnnAst.EAdd((inferExp(envi, e0), 
                                  inferExp(envi, e1)), AnnAst.Tint)
                             else if typeDouble(e, envi)
                                then AnnAst.EAdd((inferExp(envi, e0),
                                  inferExp(envi, e1)), AnnAst.Tdouble)
                             else raise TypeError
        | Ast.ESub(e0, e1) => if typeInt(e, envi) 
                                then AnnAst.ESub((inferExp(envi, e0), 
                                  inferExp(envi, e1)), AnnAst.Tint)
                             else if typeDouble(e, envi)
                                then AnnAst.ESub((inferExp(envi, e0),
                                  inferExp(envi, e1)), AnnAst.Tdouble)
                             else raise TypeError
        | Ast.EMul(e0, e1) => if typeInt(e, envi) 
                                then AnnAst.EMul((inferExp(envi, e0), 
                                  inferExp(envi, e1)), AnnAst.Tint)
                             else if typeDouble(e, envi)
                                then AnnAst.EMul((inferExp(envi, e0),
                                  inferExp(envi, e1)), AnnAst.Tdouble)
                             else raise TypeError
        | Ast.EDiv(e0, e1) => if typeInt(e, envi) 
                                then AnnAst.EDiv((inferExp(envi, e0), 
                                  inferExp(envi, e1)), AnnAst.Tint)
                             else if typeDouble(e, envi)
                                then AnnAst.EDiv((inferExp(envi, e0),
                                  inferExp(envi, e1)), AnnAst.Tdouble)
                             else raise TypeError
        | Ast.EMod(e0, e1) => if typeInt(e, envi) 
                                then AnnAst.EMod((inferExp(envi, e0),
                                  inferExp(envi, e1)), AnnAst.Tint)
                              else raise TypeError
        | Ast.ELShift(e0, e1) => if typeInt(e, envi) 
                                then AnnAst.ELShift((inferExp(envi, e0),
                                  inferExp(envi, e1)), AnnAst.Tint)
                                else raise TypeError
        | Ast.ERShift(e0, e1) => if typeInt(e, envi) 
                                then AnnAst.ERShift((inferExp(envi, e0),
                                  inferExp(envi, e1)), AnnAst.Tint)
                                else raise TypeError
        | Ast.ELt(e0, e1) => if typeBool(e, envi)
                              then AnnAst.ELt((inferExp(envi, e0),
                                  inferExp(envi, e1)), AnnAst.Tbool)
                              else raise TypeError
        | Ast.EGt(e0, e1) => if typeBool(e, envi)
                              then AnnAst.EGt((inferExp(envi, e0),
                                  inferExp(envi, e1)), AnnAst.Tbool)
                              else raise TypeError
        | Ast.ELe(e0, e1) => if typeBool(e, envi)
                              then AnnAst.ELe((inferExp(envi, e0),
                                  inferExp(envi, e1)), AnnAst.Tbool)
                              else raise TypeError
        | Ast.EGe(e0, e1) => if typeBool(e, envi)
                              then AnnAst.EGe((inferExp(envi, e0),
                                  inferExp(envi, e1)), AnnAst.Tbool)
                              else raise TypeError
        | Ast.EEq(e0, e1) => if typeBool(e, envi)
                              then AnnAst.EEq((inferExp(envi, e0),
                                  inferExp(envi, e1)), AnnAst.Tbool)
                              else raise TypeError
        | Ast.ENeq(e0, e1) => if typeBool(e, envi)
                              then AnnAst.ENeq((inferExp(envi, e0),
                                  inferExp(envi, e1)), AnnAst.Tbool)
                              else raise TypeError
        | Ast.EAnd(e0, e1) => if typeBool(e, envi)
                              then AnnAst.EConj((inferExp(envi, e0),
                                  inferExp(envi, e1)), AnnAst.Tbool)
                              else raise TypeError
        | Ast.EOr(e0, e1) => if typeBool(e, envi)
                              then AnnAst.EDisj((inferExp(envi, e0),
                                  inferExp(envi, e1)), AnnAst.Tbool)
                              else raise TypeError
        | Ast.EAsst(id, e) => asstHelper(id, envi, e)
        | Ast.ECond(e0, e1, e2) => if typeBool(e, envi)
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

  fun tToT(t : Ast.typ) : AnnAst.typ =
    case t of
      Ast.Tbool => AnnAst.Tbool
      | Ast.Tint => AnnAst.Tint
      | Ast.Tdouble => AnnAst.Tdouble
      | Ast.Tstring => AnnAst.Tstring
      | Ast.Tvoid => AnnAst.Tvoid


   fun declCheck(t: Ast.typ, idl: Ast.id list, envi: env) : env = 
        (* only checking if this is in the context at top of stack. should we check all? *)
        (let val (func, conte) = envi
             val x = List.hd(conte)
             val tail = List.tl(conte)
        in
          case idl of
            [] => envi
            | id :: ids => (case Environ.find(x, id) of
                              NONE => let 
                                      val newEnv = ((func),Environ.insert(x, 
                                                            idToId(id), 
                                                            tToT(t))::tail)
                                    in
                                        declCheck(t, ids, newEnv)
                                    end
                              | SOME t => raise MultiplyDeclaredError(id)
                              )
        end)

    fun tupToSing(l : (Ast.id*Ast.exp) list) : Ast.id list = 
          case l of
            [] => []
            | (id,e) :: xs => id :: tupToSing(xs)

  fun stmToStm (sl : Ast.stm list, envi: env, ret: Ast.typ) : AnnAst.stm list =
          case sl of
            [] => [] 
            |x::xs => (case x of 
                          Ast.SDecl(t, idl) => checkStmt(envi,x,ret)::
                          stmToStm(xs, declCheck(t, idl, envi), ret)
                          | Ast.SInit(t, l) => checkStmt(envi,x,ret)::
                          stmToStm(xs, declCheck(t, tupToSing(l), envi), ret)
                          | _ => (checkStmt(envi, x, ret))::stmToStm(xs, envi, ret))
              

    


  (* checkStmt s = s', where s' is the annotated statement datatype
   * corresponding to s'
   *)
  and checkStmt (envi : env, s : Ast.stm, ret : Ast.typ) : AnnAst.stm = 
    let
      val (funcs, cont) = envi

      fun declCheck(t: Ast.typ, idl: Ast.id list, envi: env) : env = 
        (* only checking if this is in the context at top of stack. should we check all? *)
        (let val (func, conte) = envi
             val x = List.hd(conte)
             val tail = List.tl(conte)
        in
          case idl of
            [] => envi
            | id :: ids => (case Environ.find(x, id) of
                              NONE => let 
                                      val newEnv = ((func),Environ.insert(x, 
                                                            idToId(id), 
                                                            tToT(t))::tail)
                                    in
                                        declCheck(t, ids, newEnv)
                                    end
                              | SOME t => raise MultiplyDeclaredError(id)
                              )
        end)

        fun tupToSing(l : (Ast.id*Ast.exp) list) : Ast.id list = 
          case l of
            [] => []
            | (id,e) :: xs => id :: tupToSing(xs)

        fun initCheck(idel: (Ast.id*Ast.exp) list, t: AnnAst.typ, envi: env) : bool =
          case idel of
            [] => true
            | (id,ex) :: xs => case typeMatch(t, ex, envi) of 
                                true => initCheck(xs, t, envi)
                                | false => raise TypeError


        fun isIn (id : Ast.id) : bool = 
          (* this could be funky? should this be checking all layers of context? *)
          case Environ.find(List.hd(cont), id) of
            NONE => true
            | SOME t => raise MultiplyDeclaredError(id)

        fun idLToIdl(idl : Ast.id list) : AnnAst.id list =
          case idl of
            [] => []
            | x :: xs => idToId(x)::idLToIdl(xs)

        val finalEnv = envi  
    in
      case s of
        Ast.SExp(e) => AnnAst.SExp(inferExp(envi, e))
        | Ast.SDecl(t, idl) => let val newEnv = declCheck(t, idl, envi) 
                                in
                                    AnnAst.SDecl(tToT(t), idLToIdl(idl))
                               end
        | Ast.SInit(t, (l)) => let val newEnv = declCheck(t, tupToSing(l), envi) 
                                in
                                if initCheck((l), tToT(t), newEnv) 
                                  then
                                   AnnAst.SInit(tToT(t), 
                                            map (fn(x,y) => (idToId(x), 
                                                     inferExp(newEnv,y))) 
                                                      (l))
                                  else raise TypeError
                                end
        | Ast.SReturn(e) => if typeMatch(tToT(ret), e, envi) 
                              then AnnAst.SRet(inferExp(envi,e))
                              else raise ReturnTypeError
        | Ast.SDoWhile(s0, e) => if typeBool(e, envi) 
                                  then AnnAst.SDoWhile(checkStmt(envi,s0,ret),
                                                        inferExp(envi, e)               
                                                        )
                                  else raise TypeError
        | Ast.SWhile(e, s0) => if typeBool(e, envi) 
                                  then AnnAst.SWhile(inferExp(envi, e),
                                                      checkStmt(envi,s0,ret)
                                                      )
                                  else raise TypeError
        | Ast.SFor((t,id,e0),e1,e2,s0) => if isIn(id) 
                                          then let 
                                            val newEnv = (*make it type env*)
                                              (funcs, Environ.insert(List.hd(cont), 
                                                          idToId(id), 
                                                          tToT(t))::cont)
                                            in
                                              AnnAst.SFor((idToId(id),
                                                    inferExp(newEnv,e0),
                                                    tToT(t)), 
                                                    inferExp(newEnv,e1),
                                                    inferExp(newEnv,e2),
                                                    checkStmt(newEnv,s0,ret))
                                            end
                                          else raise TypeError
        | Ast.SBlock(sl) => AnnAst.SBlock(stmToStm(sl, finalEnv, ret))
        | Ast.SIf(e, s0) => if typeBool(e, envi) 
                              then AnnAst.SIf(inferExp(envi,e),
                                              checkStmt(envi,s0,ret))
                            else raise TypeError
       | Ast.SIfElse(e, s0, s1) => if typeBool(e, envi)
                                      then AnnAst.SIfElse(inferExp(envi,e),
                                                          checkStmt(envi,s0,ret),
                                                          checkStmt(envi,s1,ret))
                                    else raise TypeError
    end
    
  fun checkParam (p: Ast.paramdecl list, f: Ast.id, envi: env) : env  =
            let
            val (funcs,cont) = envi

            fun newCont (p: Ast.paramdecl list, m: AnnAst.typ Environ.map) : env = 
              case p of 
                [] => (funcs, m::cont)
                |(t, id)::xs => newCont(xs, Environ.insert(m, idToId(id),tToT(t)))

            fun compareParams (prot: AnnAst.typ list, newb: Ast.paramdecl list) : bool =
              case newb of 
                [] => (case prot of 
                        [] => true
                        | _ => false)
                |(t, id)::xs => (case prot of
                                [] => false
                                |p::ps => if tToT(t) = p then
                                           compareParams(ps, xs)
                                           else raise TypeError )

                in
                  case Environ.find(funcs, idToId(f)) of 
                    NONE => newCont(p, Environ.empty)
                    | SOME (t, tl) => if compareParams(tl, p) 
                                        then newCont(p, Environ.empty)
                                      else raise MultiplyDeclaredError(f)
                end 
    
    (*changes an Ast.paramdecl to AnnAst.paramdecl*)
   fun pToP (t: Ast.typ, i: Ast.id) : AnnAst.paramdecl =
    (tToT(t), idToId(i)) 

    (*given an Ast.paramdecl list will change to AnnAst.paramdecl list*)
    fun paramToParam (p: Ast.paramdecl list) : AnnAst.paramdecl list = 
      case p of 
        [] => []
        |(t,i)::xs => pToP(t,i)::paramToParam(xs)

    (*changes Ast.prototype to Annast.typ*)
    fun protoToTyp (p: Ast.prototype) : AnnAst.typ = 
      tToT(p)
    
    (*given an Ast.paramdecl list gives back a typ list*)
    fun protoToParamType (p: Ast.prototype list) : AnnAst.typ list =
      case p of
        [] => []
        |(t)::xs => protoToTyp(t)::protoToParamType(xs)

    (*given a Ast.paramdecl list creates it as an AnnAst.typ list*)
    fun paramToParamType (p: Ast.paramdecl list) : AnnAst.typ list =
      case p of
        [] => []
        |(t,i)::xs => tToT(t)::paramToParamType(xs) 

  fun addFToEnv(id: Ast.id, t: Ast.typ, p: Ast.paramdecl list, envi: env) : env =
    let
       val (funcs, cont) = envi
     in
       case Environ.find(funcs, id) of
        NONE => 
          (Environ.insert(funcs, idToId(id), (tToT(t), 
          paramToParamType(p))), cont)
        | SOME t => raise MultiplyDeclaredError(id)

     end 

  fun addFProtToEnv(id: Ast.id, t: Ast.typ, p: Ast.prototype list, envi: env) : env =
    let
       val (funcs, cont) = envi
     in
       case Environ.find(funcs, id) of
        NONE => 
          (Environ.insert(funcs, idToId(id), (tToT(t), 
          protoToParamType(p))), cont)
        | SOME t => raise MultiplyDeclaredError(id)

     end 


  fun checkDef (d : Ast.def, envi: env) : AnnAst.def = 
    case d of
      Ast.DFun(t,id, (p),(s)) => let
                                  val newEnv = 
                                  addFToEnv(id, t, p, checkParam(p, id, envi))
                                    (*ugly *)
                                  in     
                                    AnnAst.DFun(tToT(t),id,
                                    paramToParam(p), stmToStm(s, newEnv, t))
                                  end
              
      | Ast.DFunProt(t, id, (p)) => let (* need to use newEnv somehow *)
                                      val newEnv = addFProtToEnv(id,t, p, envi)
                                        in
                                      AnnAst.DProt(tToT(t), id, protoToParamType(p))
                                   end 
                                    
      | _ => raise TypeError


    fun addToList (l: AnnAst.typ list, t: AnnAst.typ) : AnnAst.typ list =
      t::l

    (*functions to assist in creating base environment*)
    val envRI = Environ.insert(emptyMap, "readInt", (AnnAst.Tint, []))
    val envRB = Environ.insert(envRI, "readBool", (AnnAst.Tint, []))
    val envRD = Environ.insert(envRB, "readDouble", (AnnAst.Tint, []))
    val envPI = Environ.insert(envRD, "printInt", (AnnAst.Tvoid, addToList([], AnnAst.Tint)))
    val envPB = Environ.insert(envPI, "printBool", (AnnAst.Tvoid, addToList([], AnnAst.Tint)))
    val envPD = Environ.insert(envPB, "printDouble", (AnnAst.Tvoid, addToList([], AnnAst.Tint)))
    val baseEnv : env = (envPD, [Environ.empty])

  
  (*  checkPgm p = p', where p' is the annotated program corresponding to p'.
  *)
  fun checkPgm (p : Ast.program) : AnnAst.program =
    let
       fun listToList(e : env, d : Ast.def list) : AnnAst.def list =
          case d of
          [] => []
          | x :: xs => 
              (case x of
                Ast.DFun(t,id,(p), (s)) => checkDef(x,e)
                ::listToList(addFToEnv(id, t, p, checkParam(p, id, e)), xs)
                | Ast.DFunProt(t,id, (p)) => checkDef(x,e)::
                listToList(addFProtToEnv(id,t,p,e), xs)
                |_ => raise TypeError)
    in
    case p of
      Ast.PDefs(dl) => AnnAst.PDef(listToList(baseEnv, dl))
    end 

end


