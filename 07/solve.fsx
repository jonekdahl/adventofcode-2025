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
            let isBeamAbove = isBeam rowAbove (idx)
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


let timelines =
    fun countTimelines (row, col) -> // Trick: Pass the (memoized) function as a parameter
        if row >= rows - 1 then
            1L
        else
            let splitterBelow = isSplitter diagram[row + 1] col

            if splitterBelow then
                countTimelines (row + 1, col - 1) + countTimelines (row + 1, col + 1)
            else
                countTimelines (row + 1, col)

let memoizeRecursive f =
    let cache = Dictionary<_, _>()

    let rec memoized param =
        match cache.TryGetValue param with
        | true, cachedValue -> cachedValue
        | false, _ ->
            let result = f memoized param // Pass the memoized function as a parameter to allow "recursive" calls to be memoized
            cache.Add(param, result)
            result

    memoized


let memoizedTimelines = memoizeRecursive timelines

let startPos = 0, diagram[0].IndexOf 'S'
startPos |> memoizedTimelines |> printfn "Part 2: %d"
