module IndentationParserWithBacktracking

open FParsec
open ParserConfigs

let skipIndentation (stream: CharStream) =
    let mutable indentation = stream.SkipNewlineThenWhitespace(tabStopDistance, false)
    while stream.Peek() = '#' do
        stream.SkipRestOfLine(false) // skip comment
        indentation <- stream.SkipNewlineThenWhitespace(tabStopDistance, false)
    indentation

let indented p : Parser<_> =
    fun stream ->
        let state = stream.State
        let indentation = skipIndentation stream
        let expectedIndentation = stream.UserState.Indentation
        if indentation < expectedIndentation || stream.IsEndOfStream then
            stream.BacktrackTo(state)
            Reply(Error, NoErrorMessages)
        elif indentation = expectedIndentation then
            p stream
        else // indentation > expectedIndentation
            Reply(Error, messageError "فاصله گذاری نامناسب")

let indentedBlock p =
    Inline.Many(stateFromFirstElement = (fun x -> [x]),
                foldState = (fun xs x -> x::xs),
                resultFromState = List.rev,
                firstElementParser = p,
                elementParser = indented p)

let indentedMany1 (p: Parser<'t>) label : Parser<'t list> =
    let indentedBlock = indentedBlock p
    fun stream ->
        let oldIndentation = stream.UserState.Indentation
        let indentation = skipIndentation stream
        if indentation <= oldIndentation then
            Reply(Error, expected (if indentation < 0 then "خط جدید" else "فاصله " + label))
        else
            stream.UserState <- {stream.UserState with Indentation = indentation}
            let reply = indentedBlock stream
            if reply.Status = Ok then
                stream.UserState <- {stream.UserState with Indentation = oldIndentation}
            reply