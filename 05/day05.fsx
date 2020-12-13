let input =
    System.IO.File.ReadLines("input.txt")
    
let toBinary (s: string) =
    s.Replace("F", "0").Replace("B", "1").Replace("L", "0").Replace("R", "1")
    
let parseToInt (s: string) =
    System.Convert.ToInt32(s, 2)
    
let seats =
    input
    |> Seq.map toBinary
    |> Seq.map parseToInt

seats
|> Seq.max
|> printf "Part 1: %A\n"

seats
|> Seq.sort
|> Seq.pairwise
|> Seq.find (fun (s1, s2) -> s2 - s1 > 1)
|> fun (s1, _) -> s1 + 1
|> printf "Part 2: %A\n"
