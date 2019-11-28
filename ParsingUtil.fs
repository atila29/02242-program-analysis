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
  if File.Exists(filename)    
  then parseString(File.ReadAllText(filename))
  else invalidArg "ParserUtil" "File not found"


                        

