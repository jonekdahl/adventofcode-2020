
let input =
    System.IO.File.ReadAllLines("input.txt")

let maxY = input.Length - 1
let repeatX = input.[0].Length

let at (x, y) = input.[y].[x % repeatX]

let trees (stepX, stepY) =
    let steps = [ 
        for y in stepY .. stepY .. maxY do
            let x = stepX * y / stepY 
            (x, y)
    ]
    steps
    |> List.map at
    |> List.filter ((=) '#')
    |> List.length
    
    
trees (3, 1)
|> printf "Part 1: %d\n"

let slopes = [
    1, 1
    3, 1
    5, 1
    7, 1
    1, 2
]

slopes
|> List.map trees
|> List.map int64 // int32 multiplication overflows
|> List.reduce (*)
|> printf "Part 2: %d\n"