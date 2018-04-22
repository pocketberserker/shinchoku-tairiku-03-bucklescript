open Type
open Printf

exception Translate_error of string
let error message = raise (Translate_error message)

let literal value raw : Estree.t =
  Obj.magic [%bs.obj {
    _type = "Literal";
    value;
    raw
  }]

let number_ast value =
  literal (Obj.magic value) (sprintf "%f" value)

let bool_ast value =
  literal (Js.Boolean.to_js_boolean value) (sprintf "%b" value)

let identifier name : Estree.t =
  Obj.magic [%bs.obj {
    _type = "Identifier";
    name
  }]

let env: (Env.t list) ref = ref []

let rec translate = function
| Number v -> number_ast v
| Bool v -> bool_ast v
| Cons(Symbol s, args) as exprs -> begin
  match Env.lookup !env s with
  | Some f -> f args
  | None -> begin
    match args with
    | Cons(_, _) -> call (identifier s) args
    | Nil -> identifier s
    | _ -> error ("illegal call: " ^ (to_str exprs))
  end
end
| expr -> error ("not implemented: " ^ (to_str expr))

and call callee exprs : Estree.t =
  let rec inner acc = function
  | Cons(x, xs) -> inner (translate x :: acc) xs
  | Nil -> List.rev acc
  | expr -> translate expr :: acc |> List.rev
  in Obj.magic [%bs.obj {
    _type = "CallExpression";
    callee;
    arguments = inner [] exprs |> Array.of_list
  }]

let unary operator arg : Estree.t =
  Obj.magic [%bs.obj {
    _type = "UnaryExpression";
    operator = operator;
    argument = arg
  }]

let binary operator left right : Estree.t =
  Obj.magic [%bs.obj {
    _type = "BinaryExpression";
    operator = operator;
    left = left;
    right = right
  }]

let arithmetic operator unary identity = function
| Cons(e, Nil) -> unary operator (translate e)
| Cons(left, xs) ->
  fold_left (fun acc x -> binary operator acc (translate x)) (translate left) xs
| Nil -> begin
  match identity with
  | Some e -> e
  | None -> error (sprintf "invalid arithmetic operator [%s]: %s" operator (to_str Nil))
end
| e -> error (sprintf "invalid arithmetic operator [%s]: %s" operator (to_str e));;

let arithmetic_unary operator identity expr =
  arithmetic operator (fun op expr -> unary op expr) identity expr

let arithmetic_binary operator identity expr =
  arithmetic operator (fun op expr -> binary op (number_ast 1.) expr) identity expr

let true_ast = bool_ast true

let reduce f xs =
  if Js.Array.length xs = 0 then raise (Invalid_argument "array is empty")
  else Js.Array.reduce f (xs.(0)) (Js.Array.sliceFrom 1 xs)

let comparison operator = function
| Nil
| Cons(_, Nil) -> true_ast
| Cons(_, _) as exprs ->
  exprs
  |> pairwise
  |> Js.Array.map (fun (left, right) -> binary operator (translate left) (translate right))
  |> reduce (binary "&&")
| e -> error (sprintf "invalid comparison operator [%s]: %s" operator (to_str e))

let and_or operator seed = function
| Cons(expr, Nil) -> translate expr
| Cons(left, xs) ->
  fold_left (fun acc x -> binary operator acc (translate x)) (translate left) xs
| Nil -> seed
| e -> error (sprintf "invalid logical operator [%s]: %s" operator (to_str e))

let bang = function
| Cons(e, Nil) -> unary "!" (translate e)
| e -> error ("invalid logical operator [!]: " ^ (to_str e))

let member receiver property : Estree.t =
  Obj.magic [%bs.obj {
    _type = "MemberExpression";
    _object = receiver;
    property;
    computed = Js.false_
  }]

let display = function
| Cons(_, Nil) as exprs ->
  call (member (identifier "console") (identifier "log")) exprs
| e -> error ("invalid call [display]: " ^ (to_str e))

let init () =

  env := [];

  Env.set env "+" (arithmetic_unary "+" (Some (number_ast 0.)));
  Env.set env "-" (arithmetic_unary "-" None);
  Env.set env "*" (arithmetic_binary "*" (Some (number_ast 1.)));
  Env.set env "/" (arithmetic_binary "/" None);
  Env.set env "and" (and_or "&&" true_ast);
  Env.set env "or" (and_or "||" (bool_ast false));
  Env.set env "not" bang;
  [|
    ("=", "===");
    ("<", "<");
    (">", ">");
    ("<=", "<=");
    (">=", ">=")
  |]
  |> Array.iter (fun (x, y) -> Env.set env x (comparison y));
  Env.set env "display" display

let run expr =
  init ();
  translate expr
