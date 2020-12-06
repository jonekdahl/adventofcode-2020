open System.Text.RegularExpressions

type Entry = {
    min: int
    max: int
    letter: char
    password: string
}

let input =
    System.IO.File.ReadAllLines("input.txt")
    |> Array.map (fun l ->
        let matched = Regex.Match(l, @"(?<min>\d+)-(?<max>\d+) (?<letter>[a-z]): (?<password>[a-z]+)")
        {
          min = int matched.Groups.["min"].Value
          max = int matched.Groups.["max"].Value
          letter = char matched.Groups.["letter"].Value
          password = matched.Groups.["password"].Value
        })

let count x = Seq.filter ((=) x) >> Seq.length // Hat tip to https://stackoverflow.com/a/40039688/1990961
let policy1 (p: Entry) =
    count p.letter p.password
    |> fun c -> c >= p.min && c <= p.max

input
|> Seq.filter policy1
|> Seq.length
|> printf "Part 1: %d\n"

let policy2 (p: Entry) =
    (p.password.[p.min - 1] = p.letter) <> (p.password.[p.max - 1] = p.letter)
    
input
|> Seq.filter policy2
|> Seq.length
|> printf "Part 2: %d\n"