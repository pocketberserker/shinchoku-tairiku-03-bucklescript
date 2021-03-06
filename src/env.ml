type t = {
  name: string;
  mutable value: Type.t -> Estree.t
}

exception Env_error of string
let error message = raise (Env_error message)

let lookup env name =
  try
    let r = env |> List.find (fun x -> x.name = name)
    in Some r.value
  with Not_found -> None

let set env name value =
  try
    match List.find (fun x -> x.name = name) !env with
      x -> x.value <- value
  with Not_found ->
    let symbol = { name ; value } in
    env := symbol :: !env
