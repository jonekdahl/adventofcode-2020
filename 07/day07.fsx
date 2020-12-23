

open System
open System.Text.RegularExpressions
type Content = {
    color: string
    count: int
}

type Rule = {
    color: string
    contents: Content list
}

let rules =
    System.IO.File.ReadLines("input.txt")
    |> List.ofSeq
    |> List.map (fun line ->
        let matched = Regex.Match(line, @"^(?<color>.+) bags contain (?<contents>.+).$")
        let contents = match matched.Groups.["contents"].Value with
                       | "no other bags" -> []
                       | contents ->
                           contents.Split(", ")
                           |> List.ofArray
                           |> List.map (fun content ->
                               let matched = Regex.Match(content, @"(?<count>\d+) (?<color>.+) bags?")
                               {
                                   color = matched.Groups.["color"].Value
                                   count = int matched.Groups.["count"].Value
                               })
        {
            color = matched.Groups.["color"].Value
            contents = contents
        })

let canCarry wantedColor (rules: Map<string, Rule>) =
    let rec transitiveColors color: string list =
        let contentColors =
            rules.[color].contents
            |> List.map (fun c -> c.color)
        let trColors =
            rules.[color].contents
            |> List.map (fun c -> transitiveColors c.color)
            |> List.concat
        List.append contentColors trColors
    
    let allColors =
        rules
        |> Map.toList
        |> List.map fst
        
    allColors
    |> List.map (fun color -> color, transitiveColors color)
    |> List.filter (fun (_, colors) -> List.contains wantedColor colors)
  
rules
|> List.map (fun r -> r.color, r)
|> Map.ofList
|> canCarry "shiny gold"
|> List.length
|> printf "Part 1: %A\n"


let rec countBags color (rules: Map<string, Rule>) =
    let contents =
        rules.[color].contents
        |> List.map (fun c -> c.count * (countBags c.color rules))
        |> List.sum
    1 + contents

rules
|> List.map (fun r -> r.color, r)
|> Map.ofList
|> countBags "shiny gold"
|> fun bags -> bags - 1
|> printf "Part 2: %A\n"
