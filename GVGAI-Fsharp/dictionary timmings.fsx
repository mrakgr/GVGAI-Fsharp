// A benchmark of how long it take to iterate over a dictionary vs an array.

open System
open System.Collections.Generic

let rng = Random()

let ar = [|for i=0 to 10000000 do yield i|]
let hashset = HashSet(ar, HashIdentity.Structural)
let set = Set(ar)

#time
let test_hashset() =
    for i=1 to 10 do
        let mutable t = 0
        for x in hashset do t <-x

let test_set() =
    for i=1 to 10 do
        let mutable t = 0
        for x in set do t <-x

test_hashset()
test_set()