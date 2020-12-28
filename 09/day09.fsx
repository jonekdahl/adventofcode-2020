
//let preambleLength, fileName = 5, "example.txt"
let preambleLength, fileName = 25, "input.txt"

let input =
    System.IO.File.ReadAllLines(fileName)
    |> Array.map int64

let isValid (preambleLength: int) (pos: int) (xs: int64[]) =
    seq { for i in pos - preambleLength .. pos - 2 do
            for j in i + 1 .. pos - 1 do
              yield i, j }
    |> Seq.exists (fun (a, b) -> xs.[a] + xs.[b] = xs.[pos])

let part1 =
    seq { preambleLength .. input.Length - 1 }
    |> Seq.find (fun idx -> isValid preambleLength idx input |> not)
    |> (fun idx -> input.[idx])

part1
|> printf "Part 1: %d\n"

let sumTo (target: int64) (xs: int64[]) (idx: int) =
    let mutable acc = 0L
    let mutable curr = idx
    while acc < target do
        acc <- acc + xs.[curr]
        curr <- curr + 1

    if acc = target then
        Some xs.[idx..curr-1]
    else
        None

seq { 0 .. input.Length }
|> Seq.choose (sumTo part1 input)
|> Seq.head
|> fun xs -> Array.min xs + Array.max xs
|> printf "Part 2: %d\n"
