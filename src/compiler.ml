external mkdirSync : string -> unit = "" [@@bs.module "fs"]

let compile output target =

  let outDir = Node.Path.dirname output in
  try mkdirSync outDir
  with _ -> Js.log("output directory already exists: " ^ outDir);

  target
  |> Node.Fs.readFileAsUtf8Sync
  |> Lexing.from_string
  |> Parser.expr Lexer.token
  |> Translater.run
  |> Escodegen.generate
  |> Node.Fs.writeFileAsUtf8Sync output
