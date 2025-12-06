open System.IO

type Rotation =
    | Left of int
    | Right of int

let parse (data: string array) =
    data
    |> Seq.map (fun line ->
        let num = line[1..] |> int<string>

        match line[0] with
        | 'L' -> Left num
        | 'R' -> Right num
        | _ -> failwith line)

let rotations = "input.txt" |> File.ReadAllLines |> parse |> List.ofSeq

let positionsStopped start rotations =
    let mutable pos = start

    seq {
        yield start

        for direction in rotations do
            pos <-
                pos
                + (match direction with
                   | Left num -> -num
                   | Right num -> num)

            yield pos
    }

let positions start rotations =
    let mutable pos = start

    seq {
        for rotation in rotations do
            let newPos =
                pos
                + match rotation with
                  | Right num -> num
                  | Left num -> -num

            let intermediates =
                if rotation.IsRight then
                    seq { pos + 1 .. newPos }
                else
                    seq { pos - 1 .. -1 .. newPos }

            pos <- newPos

            yield! intermediates
    }


let countZeroes max positions =
    positions |> Seq.filter (fun p -> p % max = 0) |> Seq.length

rotations |> positionsStopped 50 |> countZeroes 100 |> printfn "Part 1: %A"
rotations |> positions 50 |> countZeroes 100 |> printfn "Part 2: %A"
