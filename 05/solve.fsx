open System.IO

let parse (data: string array) =
    let emptyLineIdx = data |> Array.findIndex ((=) "")

    let ranges =
        data[.. emptyLineIdx - 1]
        |> Array.map (fun s ->
            let parts = s.Split("-")
            int64<string> parts[0], int64<string> parts[1])

    let ingredients = data[emptyLineIdx + 1 ..] |> Array.map int64<string>
    ranges, ingredients

let ranges, ingredients = "input.txt" |> File.ReadAllLines |> parse

let isFresh (i: int64) =
    ranges |> Array.exists (fun (start, stop) -> i >= start && i <= stop)

ingredients |> Seq.filter isFresh |> Seq.length |> printfn "Part 1: %A"

let sumIngredientIds =
    ranges
    |> Array.sortBy fst
    |> Array.fold
        (fun (count, lastIngredientId) (start, stop) ->
            if start > lastIngredientId then
                // next segment starts later with no overlap, count it entirely
                let newCount = count + stop - start + 1L
                newCount, stop
            else if stop <= lastIngredientId then
                // next segment ends before previous, skip it entirely
                count, lastIngredientId
            else
                // next segment overlaps previous, count part of it
                let newCount = count + (stop - lastIngredientId)
                newCount, stop)
        (0L, 0L)
    |> fst

sumIngredientIds |> printfn "Part 2: %d"
