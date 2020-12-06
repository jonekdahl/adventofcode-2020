
let expenses =
    System.IO.File.ReadAllLines("input.txt")
    |> List.ofSeq
    |> List.map int

// Part 1
List.allPairs expenses expenses
|> List.find (fun (e1, e2) -> e1 + e2 = 2020)
|> fun (e1, e2) -> printf "Part 1: %d * %d = %d\n" e1 e2 (e1 * e2)

// Part 2
List.allPairs expenses expenses
|> fun expensePairs ->
    List.allPairs expensePairs expenses
    |> List.map (fun ((e1, e2), e3) -> (e1, e2, e3))
|> List.find (fun (e1, e2, e3) -> e1 + e2 + e3 = 2020)
|> fun (e1, e2, e3) -> printf "Part 2: %d * %d * %d = %d\n" e1 e2 e3 (e1 * e2 * e3)
