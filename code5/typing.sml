(*  COMP 321 Homework 5:  CPP typing engine.
*   
*   N. Danner
*)

structure Typing =
struct

  exception TypeError
  exception UndeclaredError of Ast.id
  exception MultiplyDeclaredError of Ast.id
  exception ReturnTypeError

  (*  inferExpNoEnv e = e', where e' is the annotated expression
  *   corresponding to e.  e must be typeable from the empty environment.
  *
  *   Really you need to define a function inferExp that takes an 
  *   environment and an expression, and just call that function with
  *   an empty environment.
  *)
  fun inferExpNoEnv (e : Ast.exp) : AnnAst.exp =
    raise TypeError


  (*  checkPgm p = p', where p' is the annotated program corresponding to p'.
  *)
  fun checkPgm (p : Ast.program) : AnnAst.program =
    raise TypeError


end
