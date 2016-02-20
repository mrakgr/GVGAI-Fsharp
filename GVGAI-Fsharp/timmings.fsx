open System
open System.Diagnostics;

let rng = Random()

let numsArray = [|for i in 0..100000000 -> rng.Next(0,2) |]

for i=1 to 3 do
    let stopWatch = Stopwatch()
    stopWatch.Start()

    let sumsArray =        
        Array.reduce (+) numsArray
       
 
    Console.WriteLine("reduce array = {0} - Time = {1}", sumsArray, stopWatch.ElapsedMilliseconds);
    stopWatch.Restart()

    let sumsLoop =
        let mutable total = 0;
        for i=0 to numsArray.Length-1 do
            total <- total + numsArray.[i]
        total

    Console.WriteLine("for loop array = {0} - Time = {1}", sumsLoop, stopWatch.ElapsedMilliseconds); 
    stopWatch.Restart()

    let arraySum = 
        Array.sum numsArray
        

    Console.WriteLine("Array.sum = {0} - Time = {1}", arraySum, stopWatch.ElapsedMilliseconds); 
    stopWatch.Restart()

    printfn "-----"

