module ParserUtils

open FParsec
open ParserConfigs

let achoice (ps:Parser<'a, 'u> seq) = choice [for x in ps -> attempt x]