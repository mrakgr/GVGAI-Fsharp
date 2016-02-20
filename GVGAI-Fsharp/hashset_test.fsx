open System.Collections.Generic

type TType =
| A of int
| B of int
| C of int

let t = [|A 1;A 2; A 3; B 55; B 3|]

let distinction_func =
    function
    | A _ -> 0
    | B _ -> 1
    | C _ -> 2

let h = HashSet(t,HashIdentity.FromFunctions distinction_func (fun x y -> true))

printfn "%A" h