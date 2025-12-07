open System.IO

let parse (data: string) =
    data
    |> _.Split(",")
    |> Seq.map (fun range ->
        match range.Split "-" |> List.ofSeq with
        | [ first; last ] -> (first |> int64, last |> int64)
        | _ -> failwith range)

let ranges = "input.txt" |> File.ReadAllLines |> Array.head |> parse |> List.ofSeq

let ids =
    ranges
    |> Seq.map (fun (first: int64, last: int64) -> seq { first..last })
    |> Seq.concat
    |> Seq.map string<int64>

let isInvalid (id: string) (numSegments: int) =
    if id.Length % numSegments <> 0 then
        false
    else
        let segLength = id.Length / numSegments
        let firstSeg = id[.. segLength - 1]

        let segments =
            seq {
                for s in 1 .. numSegments - 1 do
                    let segStart = s * segLength
                    id[segStart .. segStart + segLength - 1]
            }

        segments |> Seq.forall (fun seg -> seg = firstSeg)

let invalidIds1 (id: string) = isInvalid id 2

let invalidIds2 (id: string) =
    seq { 2 .. id.Length } |> Seq.exists (isInvalid id)

ids
|> Seq.filter invalidIds1
|> Seq.map int64<string>
|> Seq.sum
|> printfn "Part 1: %d"

ids
|> Seq.filter invalidIds2
|> Seq.map int64<string>
|> Seq.sum
|> printfn "Part 2: %d"
