open System.Collections.Generic
open System.IO

let parse (data: string array) =
    data |> Array.map (fun l -> l.Split "," |> Array.map int64)

//let fileName, numClosest = "example.txt", 10
let fileName, numClosest = "input.txt", 1000

let boxes = fileName |> File.ReadAllLines |> parse

let distance (b1: int64 array) (b2: int64 array) =
    // skip the square root
    pown (b1[0] - b2[0]) 2 + pown (b1[1] - b2[1]) 2 + pown (b1[2] - b2[2]) 2

let pairsOrderedByClosest =
    seq {
        for i in 0 .. boxes.Length - 1 do
            for j in i + 1 .. boxes.Length - 1 ->
                let b1 = boxes[i]
                let b2 = boxes[j]
                (b1, b2), distance b1 b2
    }
    |> Seq.sortBy snd
    |> Seq.map fst

let connect (circuits: Set<int64 array> array) (b1, b2) =
    let matching, nonMatching =
        circuits |> Array.partition (fun s -> s.Contains b1 || s.Contains b2)

    if matching.Length = 1 then
        circuits
    elif matching.Length = 2 then
        seq {
            [| Set.union matching[0] matching[1] |]
            nonMatching
        }
        |> Array.concat
    else
        failwith (matching.Length.ToString())

let initialCircuits =
    pairsOrderedByClosest
    |> Seq.map (fun (b1, b2) ->
        seq {
            b1
            b2
        })
    |> Seq.concat
    |> Seq.distinct
    |> Seq.map Set.singleton
    |> Array.ofSeq

let circuitsAfterConnectingPairs =
    pairsOrderedByClosest
    |> Seq.take numClosest
    |> Seq.fold (fun circuits pair -> connect circuits pair) initialCircuits

circuitsAfterConnectingPairs
|> Seq.map Seq.length
|> Seq.sortDescending
|> Seq.take 3
|> Seq.fold (*) 1
|> printfn "Part 1: %A"

let connectUntilOneCircuit =
    let rec loop circuits (pairs: IEnumerator<int64 array * int64 array>) =
        pairs.MoveNext() |> ignore
        let currentPair: int64 array * int64 array = pairs.Current
        let circuits' = connect circuits currentPair

        match circuits'.Length with
        | 1 -> currentPair // return last connected pair
        | _ -> loop circuits' pairs

    loop initialCircuits (pairsOrderedByClosest.GetEnumerator())

connectUntilOneCircuit
|> (fun (b1, b2) -> b1[0] * b2[0])
|> printfn "Part 2: %d"
