
let expenses =
    System.IO.File.ReadAllLines("example1.txt")
    |> Array.map int
    |> Array.indexed

// Part 1
seq {
    for (i1, e1) in expenses do
        for (i2, e2) in expenses do
            if i1 <> i2 && e1 + e2 = 2020 then (e1, e2)
}
|> Seq.head
|> fun (e1, e2) -> printf "Part 1: %d * %d = %d\n" e1 e2 (e1 * e2)

// Part 2
seq {
    for (i1, e1) in expenses do
        for (i2, e2) in expenses do
            for (i3, e3) in expenses do
                if i1 <> i2 && i2 <> i3 && e1 + e2 + e3 = 2020 then (e1, e2, e3)
}
|> Seq.head
|> fun (e1, e2, e3) -> printf "Part 1: %d * %d * %d = %d\n" e1 e2 e3 (e1 * e2 * e3)
