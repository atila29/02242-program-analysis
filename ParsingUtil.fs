module ParsingUtil

open System.IO
open FSharp.Text.Lexing
open Parser
open Lexer


let parseString (text:string) =
  let lexbuf = LexBuffer<_>.FromString text
  try
       Main token lexbuf
  with e ->
       let pos = lexbuf.EndPos
       printfn "Error near line %d, character %d\n" pos.Line pos.Column
       failwith "parser termination"


// Parse a file. (A statement is parsed) 
let parseFromFile filename =
  // Fix annoying VS pathing
  let abspath = Path.Combine(__SOURCE_DIRECTORY__, filename)

  if File.Exists(abspath)
  then parseString(File.ReadAllText(abspath))
  else invalidArg "ParserUtil" "File not found"