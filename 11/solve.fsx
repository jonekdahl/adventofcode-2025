open System.IO
open System.Collections.Generic

let parse (data: string array) =
    data |> Array.map (fun l -> l.Split ": " |> fun a -> a[0], a[1].Split " ")

//let fileName1, fileName2 = "example.txt", "example2.txt"
let fileName1, fileName2 = "input.txt", "input.txt"

let devices1 = fileName1 |> File.ReadAllLines |> parse
let devices2 = fileName2 |> File.ReadAllLines |> parse

type Paths =
    { neither: int64
      dac: int64
      fft: int64
      both: int64 }

module Paths =
    let plus (p1: Paths) (p2: Paths) =
        { neither = p1.neither + p2.neither
          dac = p1.dac + p2.dac
          fft = p1.fft + p2.fft
          both = p1.both + p2.both }

    let zero =
        { neither = 0
          dac = 0
          fft = 0
          both = 0 }

    let all p = p.neither + p.both + p.dac + p.fft

let addDeviceToPath (device: string) (p: Paths) =
    match device with
    | "dac" ->
        { p with
            neither = 0
            dac = p.neither + p.dac
            fft = 0
            both = p.both + p.fft }
    | "fft" ->
        { p with
            neither = 0
            fft = p.neither + p.fft
            dac = 0
            both = p.both + p.dac }
    | _ -> p

let paths devices memoizedPaths (fromDevice: string, toDevice: string) : Paths =

    if fromDevice = toDevice then
        { neither = 1
          dac = 0
          fft = 0
          both = 0 }
    else
        let _, nextDevices = devices |> Seq.find (fun d -> fst d = fromDevice)

        nextDevices
        |> Seq.map (fun nextDevice -> memoizedPaths (nextDevice, toDevice))
        |> Seq.fold Paths.plus Paths.zero
        |> addDeviceToPath fromDevice


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

let memoizedPaths devices = memoizeRecursive (paths devices)

("you", "out") |> memoizedPaths devices1 |> Paths.all |> printfn "Part 1: %d"
("svr", "out") |> memoizedPaths devices2 |> _.both |> printfn "Part 2: %d"
