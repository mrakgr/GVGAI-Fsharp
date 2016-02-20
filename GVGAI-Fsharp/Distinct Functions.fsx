//#r "../packages/FSharpx.Collections.1.13.4/lib/net40/FSharpx.Collections.dll"
#r @"C:\Users\Marko\Documents\Visual Studio 2015\Projects\FSharpx.Collections\tests\FSharpx.Collections.Tests\bin\Debug\FSharpx.Collections.dll"
open Microsoft.FSharp.Collections
open System
open System.Collections.Generic
let rng = Random()
let ar = ResizeArray([|for i=1 to 0 do yield rng.Next(0,10)|])

let in_range x min max =
    x >= 0 && x < max

ar |> ResizeArray.distinct |> ResizeArray.toArray

let distinctBy keyf (array:ResizeArray<_>) =
    let temp = ResizeArray()
    let hashSet = HashSet(HashIdentity.Structural)
    for v in array do
        if hashSet.Add(keyf v) then
            temp.Add(v)
    temp

let distinct (array:ResizeArray<_>) =
    let temp = ResizeArray()
    let hashSet = HashSet(HashIdentity.Structural)
    for v in array do
        if hashSet.Add(v) then
            temp.Add(v)
    temp

let ar' = Array.zeroCreate<int*int> 5 |> ResizeArray

distinctBy (fun (x,_) -> x) ar'

