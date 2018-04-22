open Printf

type t =
  | Nil
  | Number of float
  | Symbol of string
  | Bool of bool
  | Cons of t * t

let rec to_str x =
  match x with
    Nil -> "nil"
  | Number v -> sprintf "number %f" v
  | Symbol s -> sprintf "symbol %s" s
  | Bool v -> sprintf "bool %s" (if v then "#t" else "#f")
  | Cons(car, cdr) -> sprintf "(%s . %s)" (to_str car) (to_str cdr)

let fold_left f seed expr =
  let rec inner acc = function
  | Cons(x, xs) -> inner (f acc x) xs
  | Nil -> acc
  | e -> failwith ("constructor is not cons or nil: " ^ (to_str e))
  in inner seed expr

let pairwise expr =
  let rec inner acc v = function
  | Cons(x, xs) -> begin
    acc |> Js.Array.push (v, x) |> ignore;
    inner acc x xs
  end
  | Nil -> acc
  | e -> failwith ("constructor is not cons or nil: " ^ (to_str e)) in
  match expr with
  | Cons(x, (Cons(_, _) as xs)) -> inner [||] x xs
  | _ -> [||]
