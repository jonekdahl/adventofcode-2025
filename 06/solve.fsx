open System
open System.IO
open System.Text.RegularExpressions

type Operator =
    | Plus
    | Times

let parse1 (data: string array) =
    let numbers =
        data[.. data.Length - 2]
        |> Array.map (fun s -> s.Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Array.map int64<string>)

    let operators =
        data[data.Length - 1].Split(" ", StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun s ->
            match s with
            | "*" -> Times
            | "+" -> Plus
            | _ -> failwith $"Other: '%s{s}'")

    operators
    |> Array.mapi (fun idx operator ->
        let nums = numbers |> Array.map (fun x -> x[idx])
        nums, operator)

let fileName = "input.txt"
let problems1 = fileName |> File.ReadAllLines |> parse1

let calculate (numbers, operator) =
    match operator with
    | Times -> Seq.fold (*) 1L numbers
    | Plus -> Seq.fold (+) 0L numbers

problems1 |> Seq.map calculate |> Seq.sum |> printfn "Part 1: %d"

let parse2 (data: string array) =

    let numberLines = data[.. data.Length - 2]
    let operatorLine = data[data.Length - 1]

    // Parse the operator line first, keep track of positions in string
    let operators =
        let regex = Regex @"(\*|\+)( +)(\s|$)" // the last operator doesn't have a space after it
        let matches = regex.Matches operatorLine

        seq {
            for m in matches ->
                let opGroup = m.Groups[1]

                let op =
                    match opGroup.Value with
                    | "*" -> Times
                    | "+" -> Plus
                    | s -> failwith $"Other: '%s{s}'"

                let numWidth = 1 + m.Groups[2].Length // the operator plus a number of spaces
                op, opGroup.Index, numWidth

        }
        |> Array.ofSeq


    operators
    |> Array.map (fun (operator, opIdx, width) ->
        let numbers =
            seq { opIdx + width - 1 .. -1 .. opIdx }
            |> Seq.map (fun idx ->
                numberLines
                |> Array.map (fun l -> l[idx..idx])
                |> String.concat ""
                |> int64<string>)
            |> Array.ofSeq

        numbers, operator)

let problems2 = fileName |> File.ReadAllLines |> parse2
problems2 |> Seq.map calculate |> Seq.sum |> printfn "Part 2: %d"
