open System
open System.IO

type Shape = { occupied: int }

type Region =
    { area: int * int; presents: int array }

let parse (data: string array) =
    let shapes =
        seq { 0..5 }
        |> Seq.map (fun shIdx ->
            let shape = shIdx * 5

            data[shape + 1] + data[shape + 2] + data[shape + 3]
            |> Seq.filter (fun ch -> ch = '#')
            |> Seq.length)
        |> Seq.map (fun i -> { occupied = i })
        |> Seq.toArray

    let regions =
        data[30..]
        |> Seq.map (fun l ->
            let l = l.Split ": "
            let area, presents = l[0], l[1]
            let a = area.Split "x"
            let a1, a2 = int a[0], int a[1]

            let p = presents.Split " " |> Seq.map int |> Seq.toArray

            { area = a1, a2; presents = p })
        |> Seq.toArray

    shapes, regions

let fileName = "input.txt"

let shapes, regions = fileName |> File.ReadAllLines |> parse



//
let areaWithOptimalPacking (r: Region) =
    r.presents
    |> Seq.mapi (fun pIdx presentCount -> shapes[pIdx].occupied * presentCount)
    |> Seq.sum

let totalNumberOfPresents (r: Region) = r.presents |> Seq.sum

let threeByThree (r: Region) = fst r.area / 3 * snd r.area / 3

let canFit (r: Region) =
    let regionArea = fst r.area * snd r.area

    if regionArea < areaWithOptimalPacking r then false
    elif threeByThree r >= totalNumberOfPresents r then true
    else failwith "Non-trivial problem"

regions |> Seq.filter canFit |> Seq.length |> printfn "Part 1: %A"
