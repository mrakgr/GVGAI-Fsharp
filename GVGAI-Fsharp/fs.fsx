module fs

let stopWatch = 
  let sw = new System.Diagnostics.Stopwatch()
  sw.Start ()
  sw

let total = 300000000
let outer = 10000
let inner = total / outer

let timeIt (name : string) (a : unit -> 'T) : unit =
  let t = stopWatch.ElapsedMilliseconds
  let v = a ()
  for i = 2 to outer do
    a () |> ignore
  let d = stopWatch.ElapsedMilliseconds - t
  printfn "%s, result %A, time %d ms" name v d

let sumTest(args) = 
  let numsList = [1..inner]
  let numsArray = [|1..inner|]

  printfn "Total iterations: %d, Outer: %d, Inner: %d" total outer inner

  let sumsSeqReduce () = Seq.reduce (+) numsList
  timeIt "reduce sequence of list" sumsSeqReduce

  let sumsArray () = Array.reduce (+) numsArray
  timeIt "reduce array" sumsArray

  let sumsLoop () =
    let mutable total = 0
    for i in 0 .. inner - 1 do
      total <- total + numsArray.[i]
    total

  timeIt "for loop array" sumsLoop

  let sumsListReduce () = List.reduce (+) numsList

  timeIt "reduce list" sumsListReduce

  0
