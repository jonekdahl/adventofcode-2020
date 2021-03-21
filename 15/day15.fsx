//let input = [| 0; 3; 6 |]
let input = [| 0; 5; 4; 1; 10; 14; 7 |]

type Memory = Map<int, List<int>>

let remember (acc: Memory) (round: int, spoken: int) =
    let values =
        acc
        |> Map.tryFind spoken
        |> Option.defaultValue []
        |> fun xs -> round :: xs
    acc
    |> Map.add spoken values

let initialMemory =
    input
    |> Array.mapi (fun idx spoken -> (idx + 1, spoken))
    |> Array.fold remember Map.empty

let doRound (lastSpoken: int, mem: Memory) round =
    let spoken = 
        mem
        |> Map.tryFind lastSpoken
        |> Option.bind (fun rounds ->
                            match rounds with
                            | r1 :: r2 :: xs -> r1 - r2 |> Some
                            | _ -> None)
        |> Option.defaultValue 0
    let newMem = remember mem (round, spoken)
    (spoken, newMem)

let runGame rounds =
    seq { input.Length + 1 .. rounds }
    |> Seq.fold doRound (Array.last input, initialMemory)
    |> fst

runGame 2020
|> printfn "Part 1: %A"

// Takes about a minute to run, a mutable solution would be faster
runGame 30_000_000 }
|> printfn "Part 2: %A"
