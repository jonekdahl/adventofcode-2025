open System.IO

let parse (data: string array) =
    data |> Array.map (fun l -> l.Split "," |> fun a -> int a[0], int a[1])

let fileName = "input.txt"

let redTiles = fileName |> File.ReadAllLines |> parse

let pairs =
    redTiles
    |> Seq.allPairs redTiles
    |> Seq.filter (fun ((p1x, p1y), (p2x, p2y)) -> p1x <> p2x && p1y <> p2y)

let area ((p1x: int, p1y: int), (p2x: int, p2y: int)) =
    int64 (abs (p1x - p2x) + 1) * int64 (abs (p1y - p2y) + 1)

pairs |> Seq.map area |> Seq.max |> printfn "Part 1: %d"

type Direction =
    | North
    | South
    | East
    | West

type Turn =
    | Left
    | Right

type Point = int * int

type Edge =
    { fromPoint: Point
      toPoint: Point
      direction: Direction }

type Corner =
    { position: Point
      incomingEdge: Edge
      outgoingEdge: Edge
      turn: Turn }

let between min max a = a > min && a < max
let minmax a b = if a < b then a, b else b, a

let direction ((fromX, fromY), (toX, toY)) =
    match toX - fromX, toY - fromY with
    | 0, dy when dy < 0 -> North
    | 0, dy when dy > 0 -> South
    | dx, 0 when dx < 0 -> West
    | dx, 0 when dx > 0 -> East
    | other -> failwithf "direction: %A" other

let edges =
    seq {
        yield! redTiles |> Seq.pairwise
        yield redTiles[redTiles.Length - 1], redTiles[0] // final edge goes from last to first tile
    }
    |> Seq.map (fun (fromPoint, toPoint) ->
        { fromPoint = fromPoint
          toPoint = toPoint
          direction = direction (fromPoint, toPoint) })
    |> Array.ofSeq

let turn (incomingDirection, outgoingDirection) =
    match incomingDirection, outgoingDirection with
    | North, West
    | West, South
    | South, East
    | East, North -> Left
    | North, East
    | East, South
    | South, West
    | West, North -> Right
    | other -> failwithf "turn: %A" other

let corners =
    seq {
        yield! edges |> Seq.pairwise
        yield edges[edges.Length - 1], edges[0] // final corner formed by last and first edge
    }
    |> Seq.map (fun (incomingEdge, outgoingEdge) ->
        { position = outgoingEdge.fromPoint
          incomingEdge = incomingEdge
          outgoingEdge = outgoingEdge
          turn = turn (incomingEdge.direction, outgoingEdge.direction) })
    |> Array.ofSeq

let cornersMap = corners |> Seq.map (fun c -> c.position, c) |> Map.ofSeq

// If we turn right four times more than we turn left, the shape has a clockwise rotation.
let turns = corners |> Seq.countBy _.turn |> Map.ofSeq
assert (turns[Right] = turns[Left] + 4)

type InsideDirection =
    | NorthWest
    | NorthEast
    | SouthWest
    | SouthEast

// Inside calculation assumes that tiles were given in a clockwise rotation
// meaning that the inside of the shape is to the right of the edges
let isInside (d: InsideDirection) (corner: Corner) =
    match corner.incomingEdge.direction, d, corner.turn with
    | North, SouthWest, _ -> false
    | North, SouthEast, _ -> true
    | East, NorthWest, _ -> false
    | East, SouthWest, _ -> true
    | South, NorthEast, _ -> false
    | South, NorthWest, _ -> true
    | West, SouthEast, _ -> false
    | West, NorthEast, _ -> true
    | _, _, Left -> true
    | _, _, Right -> false

type Rectangle =
    { topLeft: Point
      topRight: Point
      bottomLeft: Point
      bottomRight: Point }

let rectangles =
    pairs
    |> Seq.map (fun ((p1x, p1y), (p2x, p2y)) ->
        let minX, maxX = minmax p1x p2x
        let minY, maxY = minmax p1y p2y

        { topLeft = minX, minY
          topRight = maxX, minY
          bottomLeft = minX, maxY
          bottomRight = maxX, maxY })
    |> Seq.distinct // (p1, p2) forms the same rect as (p2, p1)
    |> Array.ofSeq

let isRectangleCornersInside (r: Rectangle) =
    let insideOrUnknown d p =
        match cornersMap.TryGetValue p with
        | true, corner -> isInside d corner
        | _ -> true

    insideOrUnknown SouthEast r.topLeft
    && insideOrUnknown SouthWest r.topRight
    && insideOrUnknown NorthWest r.bottomRight
    && insideOrUnknown NorthEast r.bottomLeft

let horizontalEdges, verticalEdges =
    edges
    |> Array.partition (fun edge -> edge.direction = East || edge.direction = West)

let isOverlapping (i1min, i1max) (i2min, i2max) = not (i1min >= i2max || i2min >= i1max)

let withoutEdgesInside (r: Rectangle) =

    let noHorizontalEdgesIntersecting () =
        let candidateEdges =
            horizontalEdges
            |> Seq.filter (fun e ->
                let y = snd e.fromPoint
                y |> between (snd r.topLeft) (snd r.bottomLeft))

        let intersectingEdges =
            candidateEdges
            |> Seq.filter (fun e ->
                let edgeMinX, edgeMaxX = minmax (fst e.fromPoint) (fst e.toPoint)
                let rectMinX, rectMaxX = minmax (fst r.topLeft) (fst r.topRight)
                isOverlapping (edgeMinX, edgeMaxX) (rectMinX, rectMaxX))
            |> Array.ofSeq

        intersectingEdges |> Seq.isEmpty

    let noVerticalEdgesIntersecting () =
        let candidateEdges =
            verticalEdges
            |> Seq.filter (fun e ->
                let x = fst e.fromPoint
                x |> between (fst r.topLeft) (fst r.topRight))

        let intersectingEdges =
            candidateEdges
            |> Seq.filter (fun e ->
                let edgeMinY, edgeMaxY = minmax (snd e.fromPoint) (snd e.toPoint)
                let rectMinY, rectMaxY = snd r.topLeft, snd r.bottomLeft
                isOverlapping (edgeMinY, edgeMaxY) (rectMinY, rectMaxY))
            |> Seq.toArray

        intersectingEdges |> Seq.isEmpty

    noHorizontalEdgesIntersecting () && noVerticalEdgesIntersecting ()

// rectangles.Length |> printfn "    - rectangle count: %A"

let fullyPaintedRectangles =
    rectangles
    |> Seq.filter isRectangleCornersInside
    |> Seq.filter withoutEdgesInside
    |> Array.ofSeq

// fullyPaintedRectangles.Length
// |> printfn "    - fully painted rectangle count: %A"

fullyPaintedRectangles
|> Seq.map (fun r -> area (r.topLeft, r.bottomRight))
|> Seq.max
|> printfn "Part 2: %d"
