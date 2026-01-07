open System
open System.Collections.Generic
open System.IO

let parse (data: string array) = data

let fileName = "input.txt"
let diagram = fileName |> File.ReadAllLines |> parse
let rows = diagram.Length
let cols = diagram[0].Length

let isInside (col: int) = col >= 0 && col < cols

let isBeam (line: string) (col: int) =
    isInside col && (line[col] = 'S' || line[col] = '|')

let isSplitter (line: string) (col: int) = isInside col && line[col] = '^'

let updateRow (row: string) (rowAbove: string) =
    row
    |> Seq.mapi (fun idx ch ->
        match ch with
        | '^' -> '^'
        | '.' ->
            let isBeamSplitLeft = isSplitter row (idx - 1) && isBeam rowAbove (idx - 1)
            let isBeamAbove = isBeam rowAbove idx
            let isBeamSplitRight = isSplitter row (idx + 1) && isBeam rowAbove (idx + 1)

            if isBeamAbove || isBeamSplitLeft || isBeamSplitRight then
                '|'
            else
                '.'
        | _ -> failwith $"{ch}")
    |> String.Concat

let diagramWithBeams =
    let topRow = diagram[0]

    let updatedRows =
        (1, topRow)
        |> Array.unfold (fun state ->
            let rowIdx, rowAbove = state

            if rowIdx < rows then
                let updatedRow = updateRow diagram[rowIdx] rowAbove
                Some(updatedRow, (rowIdx + 1, updatedRow))
            else
                None)

    Array.concat (
        seq {
            [| topRow |]
            updatedRows
        }
    )


let splits (diagram: string array) =
    seq {
        for r in 1 .. rows - 2 do
            for c in 0 .. cols - 1 -> (r, c)
    }
    |> Seq.filter (fun (r, c) -> isSplitter diagram[r] c && isBeam diagram[r - 1] c)


diagramWithBeams |> splits |> Seq.length |> printfn "Part 1: %A"

let timelineCounts =
    let counts = Array.init rows (fun _ -> Array.create cols 0L)

    // start with bottom row ...
    for col in 0 .. cols - 1 do
        counts[rows - 1][col] <- if isBeam diagramWithBeams[rows - 1] col then 1L else 0L

    // ... and work upwards
    for row in rows - 2 .. -1 .. 0 do
        for col in 0 .. cols - 1 do
            counts[row][col] <-
                let splitterBelow = isSplitter diagram[row + 1] col
                let beamBelow = isBeam diagramWithBeams[row + 1] col
                let beam = isBeam diagramWithBeams[row] col

                match beam, beamBelow, splitterBelow with
                | true, false, true -> counts[row + 1][col - 1] + counts[row + 1][col + 1]
                | true, true, false -> counts[row + 1][col]
                | false, _, _ -> 0L
                | other -> failwith $"Other: %A{other}"

    counts


let startCol = diagram[0].IndexOf 'S'
timelineCounts[0][startCol] |> printfn "Part 2: %d"
