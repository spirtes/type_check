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

(*exceptions*)
  exception TypeError
  exception UndeclaredError of Ast.id
  exception MultiplyDeclaredError of Ast.id
  exception ReturnTypeError

  (*change an Ast.id to an Annast.id*)
  fun idToId(i : Ast.id) : AnnAst.id =
    i

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

   (* returns true if the expression type checks with the given type *)
  fun typeMatch (ty : AnnAst.typ, e: Ast.exp) : bool =
    if ((typeInt(e) andalso ty = AnnAst.Tint) orelse
        (typeDouble(e) andalso ty = AnnAst.Tdouble) orelse
        (typeBool(e) andalso ty = AnnAst.Tbool) orelse
        (typeString(e) andalso ty = AnnAst.Tstring))
    then true
    else false


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
        case newL = storedL of
          false => raise TypeError
          | true => case param of
                    [] => (idToId(id), currFRet) 
                    | x :: xs => let
                      val (ty) = List.nth(fParams, dex)
                      in
                        case typeMatch(ty, x) of
                          true => funcLookup(id, envi, xs, dex+1)
                          | false => raise TypeError
                      end
      end

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
           if typeMatch(ty, e) then AnnAst.EAsst(id, inferExp(envi,e),ty)
          else raise TypeError
        end

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

  fun tToT(t : Ast.typ) : AnnAst.typ =
    case t of
      Ast.Tbool => AnnAst.Tbool
      | Ast.Tint => AnnAst.Tint
      | Ast.Tdouble => AnnAst.Tdouble
      | Ast.Tstring => AnnAst.Tstring
      | Ast.Tvoid => AnnAst.Tvoid

  fun stmToStm (sl : Ast.stm list, envi: env, ret: Ast.typ) : AnnAst.stm list =
          case sl of
            [] => [] 
            |x :: xs => (checkStmt(envi, x, ret))::stmToStm(xs, envi, ret)
  (* checkStmt s = s', where s' is the annotated statement datatype
   * corresponding to s'
   *)
  and checkStmt (envi : env, s : Ast.stm, ret : Ast.typ) : AnnAst.stm = 
    let
      val (funcs, cont) = envi

      fun declCheck(t: Ast.typ, idl: Ast.id list, envi: env) : bool = 
        (* only checking if this is in the context at top of stack. should we check all? *)
        (let val (func, conte) = envi
             val x = List.hd(conte)
        in
          case idl of
            [] => true
            | id :: ids => (case Environ.find(x, id) of
                              NONE => let 
                                      val newEnv = ((func),Environ.insert(x, 
                                                            idToId(id), 
                                                            tToT(t))::conte)
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

        fun initCheck(idel: (Ast.id*Ast.exp) list, t: AnnAst.typ) : bool =
          case idel of
            [] => true
            | (id,ex) :: xs => case typeMatch(t, ex) of 
                                true => initCheck(xs, t)
                                | false => raise TypeError


        fun isIn (id : Ast.id) : bool = 
          (* this could be funky? should this be checking all layers of context? *)
          case Environ.find(List.hd(cont), id) of
            NONE => true
            | SOME t => raise MultiplyDeclaredError(id)

        
    in
      case s of
        Ast.SExp(e) => AnnAst.SExp(inferExp(envi, e))
        (* \/ valid if none of the variables have been previously assigned a type 
                in the current block or function*)
        (* process: pull the current environment off the stack, see if any ids are
          there if not you're good if they are you're bad *)
        | Ast.SDecl(t, idl) => if declCheck(t, idl, envi) then AnnAst.SDecl(tToT(t), idl)
                            else raise TypeError
        (* \/ valid if none of the variables have been previously assigned a type
        in the current block or function and the type of the expression assigned to the
        variable is the initialization type *)
        (* process: pull current environment, see if any ids are there, if so raise error
          otherwise keep going and make sure all e's have same type as t *)
        | Ast.SInit(t, (l)) => if declCheck(t, tupToSing(l), envi) 
                                then if initCheck((l), tToT(t)) 
                                  then AnnAst.SInit(tToT(t), 
                                            map (fn(x,y) => (idToId(x), 
                                                     inferExp(envi,y))) 
                                                      (l))
                                  else raise TypeError
                              else raise TypeError
        (* Valid if the type of e is the return type of the current function *)
        (* process: pull current environment, make sure type e is same as function type*)
        | Ast.SReturn(e) => if typeMatch(tToT(ret), e) 
                              then AnnAst.SRet(inferExp(envi,e))
                              else raise TypeError
        (* Valid is e is of type bool, and s is valid *)
        (* process: check e is bool, call checkStm s0 *)
        | Ast.SDoWhile(s0, e) => if typeBool(e) 
                                  then AnnAst.SDoWhile(checkStmt(envi,s0,ret),
                                                        inferExp(envi, e)               
                                                        )
                                  else raise TypeError
        | Ast.SWhile(e, s0) => if typeBool(e) 
                                  then AnnAst.SWhile(inferExp(envi, e),
                                                      checkStmt(envi,s0,ret)
                                                      )
                                  else raise TypeError
        (* Valid if x has not been declared by an initialization or decl in the current block or func, 
        e0 has type tau, e1, e2 can be any type, s is valid *)
        (* process: look up id make sure it isn't in environment if it is multiplydclared error, if not
         * add it to the environment, then check that e1 and e2 are valid and s0 is valid
            *)
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
        (*is valid if ss is valid, variable decs in ss are local to block*)

        | Ast.SBlock(sl) => AnnAst.SBlock(stmToStm(sl, envi, ret))
        (*valid if e has type bool and s is valid*)
        | Ast.SIf(e, s0) => if typeBool(e) 
                              then AnnAst.SIf(inferExp(envi,e),
                                              checkStmt(envi,s0,ret))
                            else raise TypeError
      (*valid if e has type bool and s0 and s1 valid*)
       | Ast.SIfElse(e, s0, s1) => if typeBool(e)
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

  (*makes params map compatible*)
  (*fun insertParams(p: Ast.paramdecl list, envi: env) : AnnAst.typ Environ.map =
    case p of
      [] => envi
      | x::xs => let
        val (t, id) = x
        val ((funcs, params), cont) = envi
      in
        case Environ.find(params, id) of
          NONE => insertParams(p, ((funcs, 
            Environ.insert(List.hd(params), idToId(id), tToT(t))),
            cont))
          | SOME t => raise MultiplyDeclaredError(id)
      end*)


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
              
      | Ast.DFunProt(t, id, (p)) => let
                                      val newEnv = addFProtToEnv(id,t, p, envi)
                                        in
                                      AnnAst.DProt(tToT(t), id, protoToParamType(p))
                                   end 
                                    
      | _ => raise TypeError

    (*functions to assist in creating base environment*)
    val envRI = Environ.insert(emptyMap, "readInt", (AnnAst.Tint, []))
    val envRB = Environ.insert(envRI, "readBool", (AnnAst.Tint, []))
    val envRD = Environ.insert(envRB, "readDouble", (AnnAst.Tint, []))
    val envPI = Environ.insert(envRD, "printInt", (AnnAst.Tvoid, [AnnAst.Tint]))
    val envPB = Environ.insert(envPI, "printBool", (AnnAst.Tvoid, [AnnAst.Tint]))
    val envPD = Environ.insert(envPB, "printDouble", (AnnAst.Tvoid, [AnnAst.Tint]))
    val baseEnv : env = (envPD, [Environ.empty])

  (*  checkPgm p = p', where p' is the annotated program corresponding to p'.
  *)
  fun checkPgm (p : Ast.program) : AnnAst.program =
    let
       fun listToList(e : env, d : Ast.def list) : AnnAst.def list =
          case d of
          [] => []
          | x :: xs => 
            let val d2 = checkDef(x, e)
            in
              d2 :: listToList(e, xs)
            end
    in
    case p of
      Ast.PDefs(dl) => AnnAst.PDef(listToList(baseEnv, dl))
    end 

end


