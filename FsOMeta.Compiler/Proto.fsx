#r "../packages/FParsec.1.0.1/lib/net40-client/FParsecCS.dll"
#r "../packages/FParsec.1.0.1/lib/net40-client/FParsec.dll"

open FParsec

type AstExpression =
    | CharAstExpression of string * char

type AstGrammar =
    {
        name : string
        expressions : AstExpression list
    }

let whitespace = spaces
let equals = pstring "="
let quote = pstring "'"
let comma = pstring ","
let openBrace = pstring "{"
let closeBrace = pstring "}"
let ometa = pstring "ometa"
let identifier' = identifier (new IdentifierOptions())

let char' = quote >>. anyChar .>> quote

let charExpression =
    identifier' .>> whitespace
    .>> equals .>> whitespace
    .>>. char'
    |>> CharAstExpression
    
let expression = charExpression

let grammar =
    ometa .>> whitespace
    >>. identifier' .>> whitespace
    .>> openBrace .>> whitespace
    .>>. (sepBy expression comma) .>> whitespace
    .>> closeBrace
    |>> fun (name, expressions) ->
        {
            AstGrammar.name = name
            expressions = expressions
        }

let compilationUnit = whitespace >>. grammar .>> whitespace

let parse code =
    match run compilationUnit code with
    | Success (result, _, _) -> result
    | Failure (message, _, _) -> failwith message

let test = @"

ometa ExpRecognizer
{
    dig = '0'
}

"

parse test
