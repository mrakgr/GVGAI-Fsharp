#r "../packages/FParsec.1.0.2/lib/net40-client/FParsecCS.dll"
#r "../packages/FParsec.1.0.2/lib/net40-client/FParsec.dll"

let countValues list value =
    let rec checkList list acc =
       match list with
       | head :: tail when head = value -> checkList tail (acc + 1)
       | head :: tail -> checkList tail acc
       | [] -> acc
    checkList list 0
    
let result = countValues [ for x in -10..10 -> x*x - 4 ] 0
printfn "%d" result