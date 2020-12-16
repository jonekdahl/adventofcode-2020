
let parseGroup (s: string) =
    s.Split("\n")
    |> Array.map (fun s -> s.ToCharArray())

let groups =
    System.IO.File.ReadAllText("input.txt").Split("\n\n")
    |> Array.map parseGroup

let sumOfCount extractAnswers groups =
    groups
    |> Array.map extractAnswers
    |> Array.map Seq.length 
    |> Array.sum

let someAnsweredYes (group: char[][]) =
    group
    |> Array.concat
    |> Set.ofArray

sumOfCount someAnsweredYes groups
|> printf "Part 1: %A\n"

let allAnsweredYes (group: char[][]) =
    let contains ch = Array.forall (Array.contains ch) group
    seq { for ch in 'a' .. 'z' do if contains ch then ch }

sumOfCount allAnsweredYes groups
|> printf "Part 1: %A\n"
