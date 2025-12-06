open System.IO

type Rotation =
    | Left of int
    | Right of int

    member this.Rotate(position: int) =
        match this with
        | Left num -> position - num
        | Right num -> position + num

let parse (data: string array) =
    data
    |> Seq.map (fun line ->
        let num = line[1..] |> int<string>

        match line[0] with
        | 'L' -> Left num
        | 'R' -> Right num
        | _ -> failwith line)

let rotations = "example.txt" |> File.ReadAllLines |> parse |> List.ofSeq

let positionsStopped startPosition (rotations: Rotation seq) =
    let mutable pos = startPosition

    seq {
        yield startPosition

        for rotation in rotations do
            pos <- rotation.Rotate pos
            yield pos
    }

let positionsPassed startPosition (rotations: Rotation seq) =
    let mutable pos = startPosition

    seq {
        yield startPosition

        for rotation in rotations do
            let newPos = rotation.Rotate pos

            let intermediates =
                if rotation.IsRight then
                    seq { pos + 1 .. newPos }
                else
                    seq { pos - 1 .. -1 .. newPos }

            pos <- newPos

            yield! intermediates
    }


let countZeroes numPositions positions =
    positions |> Seq.filter (fun pos -> pos % numPositions = 0) |> Seq.length

rotations |> positionsStopped 50 |> countZeroes 100 |> printfn "Part 1: %A"
rotations |> positionsPassed 50 |> countZeroes 100 |> printfn "Part 2: %A"
