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

  fun change (e1: AnnAst.exp, e2: AnnAst.exp) : AnnAst.typ =
    case e1 of 
      EInt => (case e2 of 
                EInt => Tint
                | _ => raise TypeError)
      |EDouble => (case e2 of
                    EDouble => Tdouble
                    | _ => raise TypeError)
      |EAdd(n1,n2) => change(n1, n2) 

      | _ => raise TypeError



  (*  inferExpNoEnv e = e', where e' is the annotated expression
  *   corresponding to e.  e must be typeable from the empty environment.
  *
  *   Really you need to define a function inferExp that takes an 
  *   environment and an expression, and just call that function with
  *   an empty environment.
  *)
  fun inferExpNoEnv (e : Ast.exp) : AnnAst.exp =
      case e of
        EInt e => EInt e
        | EAdd(e1,e2) => EAdd((inferExpNoEnv e1), (inferExpNoEnv e2), 
                        change((inferExpNoEnv e1), (inferExpNoEnv e2))) 



  (*  checkPgm p = p', where p' is the annotated program corresponding to p'.
  *)
  fun checkPgm (p : Ast.program) : AnnAst.program =
    raise TypeError


end


