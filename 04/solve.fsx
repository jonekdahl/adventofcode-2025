open System.IO

let parse (data: string array) = data
//|> Array.map (fun bank -> bank |> Seq.map string<char> |> Seq.map int<string> |> List.ofSeq)

let grid = "input.txt" |> File.ReadAllLines |> parse
let rows = grid.Length
let cols = grid[0].Length

let hasRoll removed (col, row) : bool =
    if col < 0 || col >= cols || row < 0 || row >= rows then
        false
    else if Set.contains (col, row) removed then
        false
    else
        grid[row][col] = '@'

let around = [| -1, -1; -1, 0; -1, 1; 0, -1; 0, 1; 1, -1; 1, 0; 1, 1 |]

module Seq =
    let countIf pred s =
        s |> Seq.fold (fun count item -> if pred item then count + 1 else count) 0

let hasAccessibleRoll removed (col, row) =
    if not <| hasRoll removed (col, row) then
        false
    else
        let count =
            around |> Seq.countIf (fun (dc, dr) -> hasRoll removed (col + dc, row + dr))

        count < 4

let gridCoords =
    seq {
        for r in 0 .. (rows - 1) do
            for c in 0 .. (cols - 1) -> (c, r)
    }

let accessibleRolls removed =
    gridCoords |> Seq.filter (hasAccessibleRoll removed)

accessibleRolls Set.empty |> Seq.length |> printfn "Part 1: %A"

let remove =
    let rec loop removed =
        let newRemoved = accessibleRolls removed |> Set.ofSeq

        if newRemoved.IsEmpty then
            removed
        else
            loop (Set.union removed newRemoved)

    loop Set.empty

remove |> Seq.length |> printfn "Part 2: %A"
