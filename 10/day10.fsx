let input =
    System.IO.File.ReadAllLines("input.txt")
    |> Array.map int
    |> fun s -> Array.append [| 0; (Array.max s + 3) |] s
    |> Array.sort

let diffs (adapters: int[]) =
    adapters
    |> Array.pairwise
    |> Array.map (fun (j1, j2) -> j2 - j1)
    |> Array.countBy id
    |> Map.ofArray

input
|> diffs
|> fun d -> d.[1] * d.[3]
|> printf "Part 1: %A\n"

let counts (adapters: int[]) =
    let w0 idx (map: Map<int,int64>) = map.TryFind idx |> Option.defaultValue 0L
    let ways =
        adapters
        |> Seq.fold (fun state jolts ->
            let count = if jolts = 0 then 1L else w0 (jolts-3) state + w0 (jolts-2) state + w0 (jolts-1) state
            Map.add jolts count state) Map.empty
    ways.[adapters.[adapters.Length-1]]
    
input
|> counts
|> printf "Part 2: %d\n"
