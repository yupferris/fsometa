#r "../packages/FParsec.1.0.1/lib/net40-client/FParsecCS.dll"
#r "../packages/FParsec.1.0.1/lib/net40-client/FParsec.dll"

open FParsec

type AstGrammar =
    {
        name : string
    }

let whitespace = spaces
let ometa = pstring "ometa"
let identifier' = identifier (new IdentifierOptions())
let openBrace = pstring "{"
let closeBrace = pstring "}"

let grammar =
    ometa .>> whitespace
    >>. identifier' .>> whitespace
    .>> openBrace .>> whitespace
    .>> closeBrace
    |>> fun name -> { name = name }

let compilationUnit = whitespace >>. grammar .>> whitespace

let parse code =
    match run compilationUnit code with
    | Success (result, _, _) -> result
    | Failure (message, _, _) -> failwith message

let test = @"

ometa ExpRecognizer
{
}

"

parse test
