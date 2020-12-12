open System.Text.RegularExpressions

let lines =
    System.IO.File.ReadLines("input.txt")

let passports = seq {
    let mutable passport = Map.empty
    for line in lines do
        match line with
        | "" ->
            yield passport
            passport <- Map.empty
        | _ ->
            line.Split " "
            |> Seq.iter (fun field ->
                let pair = field.Split(":")
                passport <- passport.Add(pair.[0], pair.[1]))
    yield passport
}

let requiredFieldsPresent passport =
    ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"]
    |> List.fold (fun hasFieldNames fieldName -> hasFieldNames && Map.containsKey fieldName passport) true

passports
|> Seq.filter requiredFieldsPresent
|> Seq.length
|> printf "Part 1: %A\n"

let inline (>=<) a (min, max) = a >= min && a<= max

let fieldValid (fieldName, fieldValue) =
    match fieldName with
    | "byr" -> // four digits; at least 1920 and at most 2002
        let matched = Regex.Match(fieldValue, @"^\d{4}$")
        matched.Success && (int matched.Groups.[0].Value) >=< (1920, 2002)
    | "iyr" -> // four digits; at least 2010 and at most 2020
        let matched = Regex.Match(fieldValue, @"^\d{4}$")
        matched.Success && (int matched.Groups.[0].Value) >=< (2010, 2020)
    | "eyr" -> // four digits; at least 2020 and at most 2030
        let matched = Regex.Match(fieldValue, @"^\d{4}$")
        matched.Success && (int matched.Groups.[0].Value) >=< (2020, 2030)
    | "hgt" -> // a number followed by either cm or in
        let matched = Regex.Match(fieldValue, @"^(?<number>\d+)(?<unit>(cm|in))$")
        matched.Success &&                
            (
              // If cm, the number must be at least 150 and at most 193
              (matched.Groups.["unit"].Value = "cm" && (int matched.Groups.["number"].Value) >=< (150, 193)) ||
            
              // If in, the number must be at least 59 and at most 76            
              (matched.Groups.["unit"].Value = "in" && (int matched.Groups.["number"].Value) >=< (59, 76))
            )
    | "hcl" -> // a # followed by exactly six characters 0-9 or a-f
        let matched = Regex.Match(fieldValue, @"^#[0-9a-f]{6}$")
        matched.Success
    | "ecl" -> // exactly one of: amb blu brn gry grn hzl oth
        let matched = Regex.Match(fieldValue, @"^(amb|blu|brn|gry|grn|hzl|oth)$")
        matched.Success
    | "pid" -> // a nine-digit number, including leading zeroes
        let matched = Regex.Match(fieldValue, @"^\d{9}$")
        matched.Success
    | _ ->
        true


let fieldsValid passport =
    passport
    |> Map.toList
    |> List.map fieldValid
    |> List.reduce (&&)
    
passports
|> Seq.filter requiredFieldsPresent
|> Seq.filter fieldsValid
|> Seq.length
|> printf "Part 2: %A\n"
