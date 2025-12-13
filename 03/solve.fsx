open System.IO

let parse (data: string array) =
    data
    |> Array.map (fun bank -> bank |> Seq.map string<char> |> Seq.map int<string> |> List.ofSeq)

let banks = "input.txt" |> File.ReadAllLines |> parse

let leftmostMaxIndex (bank: int list) =
    let max = bank |> List.max
    bank |> List.findIndex ((=) max)

let joltage (bank: int list) : int64 =
    let rec loop (sum: int64) bank =
        match bank with
        | [] -> sum
        | x :: xs -> loop (sum + int64 x * pown 10L xs.Length) xs

    loop 0 bank

let largestJoltage (size: int) (bank: int list) =

    let rec largest (soFar: int list) (rest: int list) : int list =
        let remaining = size - soFar.Length

        if remaining = 0 then
            soFar
        else
            let maxIndex = leftmostMaxIndex rest[.. rest.Length - remaining] // leave at least remaining - 1
            largest (soFar @ [ rest[maxIndex] ]) (rest[maxIndex + 1 ..])

    largest [] bank |> joltage

banks |> Seq.map (largestJoltage 2) |> Seq.sum |> printfn "Part 1: %d"
banks |> Seq.map (largestJoltage 12) |> Seq.sum |> printfn "Part 2: %d"
