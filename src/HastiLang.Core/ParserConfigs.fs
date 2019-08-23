module ParserConfigs
open FParsec

let tabStopDistance = 8 // must be a power of 2


type IndentationState =
    {Indentation: int}
    with
       static member Create() = {Indentation = -1}

type CharStream = CharStream<IndentationState>
type Parser<'t> = Parser<'t, IndentationState>
