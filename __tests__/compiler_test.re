open Jest;
open Expect;

describe("Compiler", () => {

  describe({j|S式をJavaScript ASTに変換できる|j}, () => {

    let path = "__tests__/fixture/valid/";

    Glob.sync({j|$(path)**/*.scm|j})
    |> Js.Array.forEach ((filePath) => {
      let baseName =
        filePath
        |> Js.String.substrAtMost(~from = 0, ~length = String.length(filePath) - String.length("/content.scm"))
        |> Js.String.substr(~from = String.length(path));
      test(filePath, () => {
        let actual =
          filePath
          |> Node.Fs.readFileAsUtf8Sync
          |> Lexing.from_string
          |> Parser.expr (Lexer.token)
          |> Translater.run
          |> Obj.magic
          |> Js.Json.stringify
          |> Js.Json.parseExn;
        let expected =
          {j|$(path)$(baseName)/content.ast|j}
          |> Node.Fs.readFileAsUtf8Sync
          |> Js.Json.parseExn;
        expect(actual) |> toEqual(expected)
      })
    })
  });
});

