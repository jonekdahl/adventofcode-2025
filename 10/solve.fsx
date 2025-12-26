#r "nuget: FParsec, 1.1.1"
#r "nuget: Flips, 2.4.10"

open System.IO

type LightState =
    | On
    | Off

type MachineDescription = (LightState array * int32 array array * int32 array)

module Parser =
    open FParsec

    let lightState = stringReturn "#" On <|> stringReturn "." Off
    let statesBetweenBrackets = pstring "[" >>. many1 lightState .>> pstring "]"

    let intsBetweenBraces = pstring "(" >>. sepBy1 pint32 (pstring ",") .>> pstring ")"
    let intsBetweenCurlies = pstring "{" >>. sepBy1 pint32 (pstring ",") .>> pstring "}"

    let machineParser =
        pipe3
            (statesBetweenBrackets .>> spaces)
            (many1 (intsBetweenBraces .>> spaces))
            intsBetweenCurlies
            (fun lightStates buttons joltages ->
                lightStates |> Array.ofSeq, buttons |> Seq.map Array.ofSeq |> Array.ofSeq, joltages |> Array.ofSeq)

    let run parser input : MachineDescription =
        match run parser input with
        | Success(result, _, _) -> result
        | Failure(errorMsg, _, _) -> failwithf "%A" errorMsg

    let parseMachine = run machineParser

let parse (data: string array) = data |> Array.map Parser.parseMachine //|> Array.map parseMachine

let fileName = "input.txt"

let machines = fileName |> File.ReadAllLines |> parse

module FlipsSolver =
    open Flips
    open Flips.Types

    module Tuple =
        let create a b = a, b

    let private createButtonVariables (maxPresses: int) buttonSpecs =
        buttonSpecs
        |> Array.mapi (fun i _ -> Decision.createInteger $"b%d{i}presses" 0 maxPresses)

    let private createIndicatorConstraints
        (buttonVariables: Decision array)
        (lightStates: LightState array)
        (buttonSpecs: int32 array array)
        =
        lightStates
        |> Array.mapi (fun lightIdx lightState ->
            let buttonsSubset =
                Array.zip buttonVariables buttonSpecs
                |> Array.choose (fun (buttonVariable, buttonSpec) ->
                    if buttonSpec |> Array.contains lightIdx then
                        Some buttonVariable
                    else
                        None)

            let sumOfButtonPresses = buttonsSubset |> Array.fold (+) LinearExpression.Zero

            // In order to check if x is an even number we introduce another variable
            // of the form x = 2 * y, where y is also an integer-constrained variable.

            let integer = Decision.createInteger $"int%d{lightIdx}" 0 infinity

            let expression =
                match lightState with
                | LightState.Off -> sumOfButtonPresses == 2.0 * integer // even number of presses
                | LightState.On -> sumOfButtonPresses == 2.0 * integer + 1.0 // odd number of presses

            Constraint.create $"light%d{lightIdx}" expression)

    let createIndicatorModel (desc: MachineDescription) =
        let lightStates, buttonSpecs, _joltages = desc
        let maxPresses = 10

        let buttonVariables = buttonSpecs |> createButtonVariables maxPresses

        let totalPressesExpression = buttonVariables |> Seq.fold (+) LinearExpression.Zero

        let objective = Objective.create "MinimizePresses" Minimize totalPressesExpression
        let constraints = createIndicatorConstraints buttonVariables lightStates buttonSpecs
        let model = Model.create objective |> Model.addConstraints constraints

        model, objective

    let createJoltageConstraints
        (buttonVariables: Decision array)
        (joltageTargets: int array)
        (buttonSpecs: int32 array array)
        =
        joltageTargets
        |> Array.mapi (fun joltageIdx joltageTarget ->

            let buttonsSubset =
                Array.zip buttonVariables buttonSpecs
                |> Array.choose (fun (buttonVariable, buttonSpec) ->
                    if buttonSpec |> Array.contains joltageIdx then
                        Some buttonVariable
                    else
                        None)

            let sumOfButtonPresses = buttonsSubset |> Array.fold (+) LinearExpression.Zero
            Constraint.create $"joltage%d{joltageIdx}" (sumOfButtonPresses == float<int> joltageTarget))

    let createJoltageModel (desc: MachineDescription) =
        let _, buttons, joltages = desc
        let maxPresses = joltages |> Seq.max

        let buttonVariables = buttons |> createButtonVariables maxPresses
        let totalButtonPresses = buttonVariables |> Seq.fold (+) LinearExpression.Zero

        let objective = Objective.create "MinimizePresses" Minimize totalButtonPresses
        let constraints = createJoltageConstraints buttonVariables joltages buttons
        let model = Model.create objective |> Model.addConstraints constraints

        model, objective

    let solve (model, objective) =
        let settings =
            { SolverType = SolverType.CBC
              MaxDuration = 10_000L
              WriteLPFile = None
              WriteMPSFile = None }

        let result = Solver.solve settings model

        match result with
        | Optimal solution -> Objective.evaluate solution objective |> int<float>
        | Infeasible(_) -> failwith "Infeasible"
        | Unbounded(_) -> failwith "Unbounded"
        | Unknown(_) -> failwith "Unknown"

let optimalLightConfiguration (machine: MachineDescription) =
    machine |> FlipsSolver.createIndicatorModel |> FlipsSolver.solve

let optimalJoltageConfiguration (machine: MachineDescription) =
    machine |> FlipsSolver.createJoltageModel |> FlipsSolver.solve

machines |> Seq.map optimalLightConfiguration |> Seq.sum |> printfn "Part 1: %A"

machines
|> Seq.map optimalJoltageConfiguration
|> Seq.sum
|> printfn "Part 2: %A"
