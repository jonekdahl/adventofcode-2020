type Instruction = 
    | Move of char * int
    | Turn of char * int
    | Forward of int

let input =
    System.IO.File.ReadAllLines("input.txt")
    |> Array.map (fun l -> match l.[0] with
                           | 'N' | 'S' | 'E' | 'W' -> Move (l.[0], int (l.Substring(1)))
                           | 'L' | 'R' -> Turn (l.[0], int (l.Substring(1)))
                           | 'F' -> Forward (int (l.Substring(1)))
                           | other -> failwithf "Illegal prefix: %c" other)

type Position = {
    x: int
    y: int
    d: int
}

let degreesToDirection =
    Map.empty
        .Add(0, 'E')
        .Add(90, 'N')
        .Add(180, 'W')
        .Add(270, 'S')       

let move (cur: Position) (dir: char, steps: int) =
    match dir with
    | 'E' -> {x = cur.x + steps; y = cur.y; d = cur.d}
    | 'W' -> {x = cur.x - steps; y = cur.y; d = cur.d}
    | 'N' -> {x = cur.x; y = cur.y + steps; d = cur.d}
    | 'S' -> {x = cur.x; y = cur.y - steps; d = cur.d}

let turn (cur: Position) (dir: char, degrees: int) =
    let deltaDegs = if dir = 'L' then degrees else -degrees
    {x = cur.x; y = cur.y; d = ((cur.d + deltaDegs + 360) % 360)}

let next (cur: Position) =
    function
    | Move (d, s) -> move cur (d, s)
    | Turn (d, s) -> turn cur (d, s)
    | Forward steps ->
        let direction = degreesToDirection.[cur.d]
        move cur (direction, steps)

let manhattan (cur: Position) = abs cur.x + abs cur.y
                
input
|> Array.fold next {x = 0; y = 0; d = 0}
|> manhattan
|> printfn "Part 1: %A"

type Waypoint = {
    x: int
    y: int
}

let moveWaypoint (wp: Waypoint) (dir: char, steps: int) =
    match dir with
    | 'E' -> {x = wp.x + steps; y = wp.y}
    | 'W' -> {x = wp.x - steps; y = wp.y}
    | 'N' -> {x = wp.x; y = wp.y + steps}
    | 'S' -> {x = wp.x; y = wp.y - steps}

let rec turnWaypoint (wp: Waypoint) (dir: char, degrees: int) =
    if degrees = 0 then
        wp
    elif degrees < 0 then
        if dir = 'L' then
            turnWaypoint wp ('R', -degrees)
        else
            turnWaypoint wp ('L', -degrees)
    else
        match dir with
        | 'L' -> turnWaypoint {x = -wp.y; y = wp.x} ('L', degrees - 90)
        | 'R' -> turnWaypoint {x = wp.y; y = -wp.x} ('R', degrees - 90)

let nextWaypoint (cur: Position, wp: Waypoint) =
    function
    | Move (d, s) -> (cur, moveWaypoint wp (d, s))
    | Turn (d, s) -> (cur, turnWaypoint wp (d, s))
    | Forward steps -> ({x = cur.x + wp.x * steps; y = cur.y + wp.y * steps; d = cur.d}, wp)

input
|> Array.fold nextWaypoint ({x = 0; y = 0; d = 0}, {x = 10; y = 1})
|> fst
|> manhattan
|> printfn "Part 2: %A"