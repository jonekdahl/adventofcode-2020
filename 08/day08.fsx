open System
open System.Text.RegularExpressions

type Instruction = Tuple<string,int>
type Program = Instruction[]

let parseInstruction line =
    let matched = Regex.Match(line, @"^(?<operation>[a-z]+) (?<argument>[+-]\d+)$")
    (matched.Groups.["operation"].Value, int matched.Groups.["argument"].Value)
let input =
    System.IO.File.ReadLines("input.txt")
    |> Seq.map parseInstruction
    |> Array.ofSeq
    
type ExecutionResult =
    | InfiniteLoop of int
    | Terminated of int

let execute (program: Program) =
    let mutable pc = 0
    let mutable acc = 0
    let profile = Array.zeroCreate program.Length

    while pc < program.Length && profile.[pc] = 0 do
        //printf "pc: %d, acc: %d, prof: %d\n" pc acc profile.[pc]
        profile.[pc] <- profile.[pc] + 1
        let op, arg = program.[pc]
        match op with
        | "nop" ->
            pc <- pc + 1
        | "jmp" ->
            pc <- pc + arg
        | "acc" ->
            acc <- acc + arg
            pc <- pc + 1
        | other -> failwith other
        
    if pc = program.Length then Terminated acc else InfiniteLoop acc

input
|> execute
|> printf "Part 1: %A\n"

let modifiedPrograms (program: Instruction[]) =
    let swapInstruction (idx: int) =
        program
        |> Array.mapi (fun i instr ->
                            let op, arg = instr
                            if i = idx then
                                let newOp = if op = "jmp" then "nop" else "jmp"
                                newOp, arg
                            else
                                op, arg)

    seq { for i in 0 .. program.Length - 1 do 
            if (fst program.[i]) = "nop" || (fst program.[i]) = "jmp" then swapInstruction i }

input
|> modifiedPrograms
|> Seq.map execute
|> Seq.find (fun res -> match res with | Terminated _ -> true; | _ -> false)
|> printf "Part 2: %A\n"
