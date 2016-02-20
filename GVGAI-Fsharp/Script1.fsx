// Some benchmarking of records and structs.
// Conclusion - plain arrays and structs really mix well. Structs are really super optimized with plain arrays.
// Currently, structs do not mix well with ResizeArray as they can't be mutated, also, ResizeArray is much slower 
// than a plain array by around 2.5x. Probably due to index checking. With these microbenchmarks the small differences get amplified.
// The biggest performance killer by far is allocating the record reference types. That absolutely kills performance.

// I am guessing the excellent performance of the record types is because the GC orders them in managed memory so they end up contiguous.

#r "../packages/FSharpx.Collections.1.14.0/lib/net40/FSharpx.Collections.dll"
open Microsoft.FSharp.Collections

type Rec =
    {
    mutable pos_x : int
    mutable pos_y : int
    mutable v_x : int
    mutable v_y : int
    }

type Str =
    struct
    val mutable pos_x : int
    val mutable pos_y : int
    val mutable v_x : int
    val mutable v_y : int
    new (a,b,c,d) = {pos_x=a;pos_y=b;v_x=c;v_y=d}
    end

    member inline t.change(x) = t.pos_x <- x
    member inline t.apply(f: Str byref -> unit) = f (&t)

open System

let rng = Random()

let reco = 
    ResizeArray.init 10000000
        (fun _ ->
            {
            pos_x = rng.Next(0,10)
            pos_y = rng.Next(0,10)
            v_x = rng.Next(0,10)
            v_y = rng.Next(0,10)
            })

let test_reco() =
    for i=0 to reco.Count-1 do
        reco.[i].pos_x <- 5
        reco.[i].pos_y <- 5
        reco.[i].v_x <- 5
        reco.[i].v_y <- 5

for i=0 to reco.Count-1 do
    if i % 3 = 0 then reco.[i] <- {
            pos_x = rng.Next(0,10)
            pos_y = rng.Next(0,10)
            v_x = rng.Next(0,10)
            v_y = rng.Next(0,10)
            }
#time
test_reco()
#time

let mutable str = Array.init 10000000 (fun _ -> Str())

let inline test_str () =
    let inline change (s: Str byref) =
        s.pos_x <- 10
    for i=0 to str.Length-1 do
//        str.[i] <- change str.[i]
        str.[i].apply change
//        str.[i].pos_x <- 5
//        str.[i].pos_y <- 5
//        str.[i].v_x <- 5
//        str.[i].v_y <- 5

#time
test_str()
#time

let product1 zs = 
    let mutable re = 1.0 
    let mutable im = 0.0 
    for i = 0 to Array.length zs - 1 do 
        let zr, zi = zs.[i]
        let r = re * zr - im * zi 
        let i = re * zi + im * zr 
        re <- r 
        im <- i 
    re, im



type Complex = 
    struct 
    val re : float 
    val im : float 
    new(x, y) = { re = x; im = y }
    end

let product2 (zs : Complex array) = 
    let mutable re = 1.0 
    let mutable im = 0.0 
    for i = 0 to Array.length zs - 1 do 
        let r = re * zs. [i] .re - im * zs. [i] .im
        let i = re * zs. [i] .im + im * zs. [i] .re
        re <- r 
        im <- i 
    re, im

let zs  = Array.init 10000000 (fun _ -> rng.NextDouble(),rng.NextDouble())
let zs' = Array.init 10000000 (fun _ -> Complex(rng.NextDouble(),rng.NextDouble()))

#time
product1 zs
product2 zs'

#time
let h = System.Collections.Generic.HashSet([|1;2;3;4|],HashIdentity.Structural)
let mutable a = Array.create 1 0
let a' = [|9;1;1;3;5|]

/// The distinct function for integers. 2-3x faster than using Array.distinct.
/// This is assuming the types are annotated (ar': int[]) (ar: int[]) instead of (ar': _[]) (ar: _[])
let inline distinct ar' ar =
    let rec loop (ar': int[]) (ar: int[]) j i =
        if i < ar.Length then
            if ar.[i-1] = ar.[i] 
            then loop ar' ar j (i+1)
            else
                ar'.[j] <- ar.[i-1]
                loop ar' ar (j+1) (i+1)
        else ar'.[j] <- ar.[i-1]
    ar |> Array.sortInPlace
    loop ar' ar 0 1

for i=0 to 10000000 do
    try // While exceptions might be slow, I realized that there is no downside to try statements.
        distinct a a'
    with
        e -> 
            a <- Array.create (a.Length+1) 0
            a.CopyTo(a,0)
            
    