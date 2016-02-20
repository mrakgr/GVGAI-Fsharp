#if INTERACTIVE
#r "../packages/FParsec.1.0.2/lib/net40-client/FParsecCS.dll"
#r "../packages/FParsec.1.0.2/lib/net40-client/FParsec.dll"
#endif

open FParsec

let resultChoose predicate msg (p: Parser<_,_>) : Parser<_,_> =
    let error = messageError msg
    fun stream ->
            let state = stream.State
            let reply = p stream
            if reply.Status = Ok then 
                match predicate reply.Result with
                | Some x -> Reply(x)
                | None -> 
                    stream.BacktrackTo(state) // backtrack to beginning
                    Reply(Error, error)
            else
                stream.BacktrackTo(state) // backtrack to beginning
                Reply(Error, error)

let nl = (newline |>> ignore) <|> eof
let c = anyChar
let p = 
    many1Till anyChar nl
    |> resultChoose (fun x -> 
        let t = Set(x)
        if t.Count = x.Length then Some t else None) "duplicates detected"
    |> fun x -> manyTill (x .>> spaces) nl

let t = """abcde
qwert
asd"""

run p t

