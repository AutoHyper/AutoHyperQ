(*    
    Copyright (C) 2022-2023 Raven Beutner

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
*)

module AutoHyperQ.Program

open System
open System.IO

open FsOmegaLib.SAT
open FsOmegaLib.GNBA
open FsOmegaLib.Operations

open AutoHyperQCore.HyperQPTL
open AutoHyperQCore.ModelChecking

open Util
open CommandLineParser

let raiseExceptions = false

let private writeFormulaAndSystemString (systemOutputPaths: list<String>) formulaOutputPath (tsStringList : list<String>) (formulaString : String) =
    (systemOutputPaths, tsStringList)
    ||> List.zip
    |> List.iter (fun (file, tsString) -> 
        try 
            File.WriteAllText(file, tsString)
        with 
        | _ -> raise <| AutoHyperQException $"Could not write to file %s{file}"
    )

    try 
        File.WriteAllText(formulaOutputPath, formulaString)
    with 
    | _ -> raise <| AutoHyperQException $"Could not write to file %s{formulaOutputPath}"


[<EntryPoint>]
let main args =
    try 
        
        let swTotal = System.Diagnostics.Stopwatch()
        let sw = System.Diagnostics.Stopwatch()
        swTotal.Start()

        sw.Restart()
        let cmdArgs =
            match CommandLineParser.parseCommandLineArguments (Array.toList args) with
            | Result.Ok x -> x
            | Result.Error e ->
                raise <| AutoHyperQException $"%s{e}"

        let solverConfig = AutoHyperQCore.SolverConfiguration.getConfig()

        let autoHyperQOptions: AutoHyperQOptions = 
            {
                AutoHyperQOptions.Logger = fun s -> 
                    if cmdArgs.LogPrintouts then 
                        printf $"%s{s}"

                RaiseExceptions = raiseExceptions
            }
            
        autoHyperQOptions.LoggerN $"========================= Initialization ========================="
        autoHyperQOptions.LoggerN $"Read command line args and solver config. Time: %i{sw.ElapsedMilliseconds}ms (%.4f{double(sw.ElapsedMilliseconds) / 1000.0}s) "

        let systemInputPaths, formulaInputPath = 
            match cmdArgs.InputFiles with 
            | Some x -> x 
            | None -> 
                raise <| AutoHyperQException "Must specify input files"


        let tsList, formula = 
            match cmdArgs.InputType with 
            | HanoiSystem -> 
                autoHyperQOptions.Logger $"Start parsing (HANOI)..."
                sw.Restart()
                let tsStringList, formula = InstanceParsing.readAndParseHanoiInstance systemInputPaths formulaInputPath 
                autoHyperQOptions.LoggerN $"done. Time: %i{sw.ElapsedMilliseconds}ms (%.4f{double(sw.ElapsedMilliseconds) / 1000.0}s) "

                autoHyperQOptions.Logger $"Start translation..."
                sw.Restart()
                let tsList, formula = Translation.convertHanoiSystemInstanceToGNBA solverConfig autoHyperQOptions tsStringList formula
                autoHyperQOptions.LoggerN $"done. Time: %i{sw.ElapsedMilliseconds}ms (%.4f{double(sw.ElapsedMilliseconds) / 1000.0}s) "

                tsList, formula

            | SymbolicSystem -> 
                autoHyperQOptions.Logger $"Start parsing (symbolic)..."
                sw.Restart()
                let programList, formula = InstanceParsing.readAndParseSymbolicInstance systemInputPaths formulaInputPath 
                autoHyperQOptions.LoggerN $"done. Time: %i{sw.ElapsedMilliseconds}ms (%.4f{double(sw.ElapsedMilliseconds) / 1000.0}s) "

                autoHyperQOptions.Logger $"Start translation..."
                sw.Restart()
                let tsList, formula = Translation.convertSymbolicSystemInstanceToGNBA programList formula
                autoHyperQOptions.LoggerN $"done. Time: %i{sw.ElapsedMilliseconds}ms (%.4f{double(sw.ElapsedMilliseconds) / 1000.0}s) "

                tsList, formula

            | BooleanProgramSystem -> 
                autoHyperQOptions.Logger $"Start parsing (bp)..."
                sw.Restart()
                let booleanProgramList, formula = InstanceParsing.readAndParseBooleanProgramInstance systemInputPaths formulaInputPath 
                autoHyperQOptions.LoggerN $"done. Time: %i{sw.ElapsedMilliseconds}ms (%.4f{double(sw.ElapsedMilliseconds) / 1000.0}s) "

                autoHyperQOptions.Logger $"Start translation..."
                sw.Restart()
                let tsList, formula = Translation.convertBooleanProgramInstanceToGNBA booleanProgramList formula
                autoHyperQOptions.LoggerN $"done. Time: %i{sw.ElapsedMilliseconds}ms (%.4f{double(sw.ElapsedMilliseconds) / 1000.0}s) "

                tsList, formula

            | ExplictSystem -> 
                autoHyperQOptions.Logger $"Start parsing (explicit)..."
                sw.Restart()
                let explicitSystemList, formula = InstanceParsing.readAndParseExplicitInstance systemInputPaths formulaInputPath 
                autoHyperQOptions.LoggerN $"done. Time: %i{sw.ElapsedMilliseconds}ms (%.4f{double(sw.ElapsedMilliseconds) / 1000.0}s) "

                autoHyperQOptions.Logger $"Start translation..."
                sw.Restart()
                let tsList, formula = Translation.convertExplictSystemInstanceToGNBA explicitSystemList formula
                autoHyperQOptions.LoggerN $"done. Time: %i{sw.ElapsedMilliseconds}ms (%.4f{double(sw.ElapsedMilliseconds) / 1000.0}s) "

                tsList, formula

        autoHyperQOptions.LoggerN $"=================================================="
        autoHyperQOptions.LoggerN ""

        match cmdArgs.WriteExplicitInstance with 
        | None -> ()
        | Some (systemOutputPaths, formulaOutputPath) -> 
            autoHyperQOptions.LoggerN $"========================= Writing to Dics ========================="
            if systemOutputPaths.Length <> systemInputPaths.Length then 
                raise <| AutoHyperQException "The number of output files must match the input"


            autoHyperQOptions.Logger "Start writing GNBA system to disc..."

            let tsStringList = 
                tsList 
                |> List.map (GNBA.toHoaString string id)

            let formulaString = HyperQPTL.print id formula

            writeFormulaAndSystemString systemOutputPaths formulaOutputPath tsStringList formulaString

            autoHyperQOptions.LoggerN $"done. Time: %i{sw.ElapsedMilliseconds}ms (%.4f{double(sw.ElapsedMilliseconds) / 1000.0}s)"
            autoHyperQOptions.LoggerN $"=================================================="
            autoHyperQOptions.LoggerN ""


        if cmdArgs.Verify then 
            match AutoHyperQCore.ModelCheckingUtil.findErrorOnMcInstance tsList formula with 
            | None -> () 
            | Some msg -> 
                raise <| AutoHyperQException $"Error in model and/or formula: %s{msg}"

            let traceVarList =
                HyperQPTL.quantifiedTraceVariables formula

            let tsMap =
                if tsList.Length > 1 then 
                    (traceVarList, tsList)
                    ||> List.zip
                    |> Map.ofList
                else
                    traceVarList
                    |> List.map (fun x -> x, tsList.[0])
                    |> Map.ofList

            let mcOptions: ModelCheckingOptions = 
                {
                    ModelCheckingOptions.ComputeWitnesses = cmdArgs.ComputeWitnesses
                    InitialSystemSimplification = cmdArgs.InitialSystemSimplification
                    IntermediateSimplification = cmdArgs.IntermediateSimplification

                    Logger = autoHyperQOptions.Logger
                    RaiseExceptions = autoHyperQOptions.RaiseExceptions
                }
            
            let res, lasso = AutoHyperQCore.ModelChecking.modelCheck solverConfig mcOptions tsMap formula

            if res then 
                printfn "SAT"
            else
                printfn "UNSAT"


            if cmdArgs.ComputeWitnesses then 
                match lasso with 
                | None -> 
                    //printfn "Could not compute a Lasso"
                    ()
                | Some lassoMap -> 
                    // We can assume that each DNF in this lasso is SAT

                    let printList (l : list<list<Literal<String>>>) = 
                        l 
                        |> List.map (fun a -> 
                            a 
                            |> List.map (function 
                                | PL a -> "\"" + a + "\""
                                | NL a -> "! \"" + a + "\""
                                )
                            |> Util.combineStringsWithSeperator " & "
                            |> fun s -> "(" + s + ")"
                            )
                        |> Util.combineStringsWithSeperator " "
                        |> fun x -> "[" + x + "]"

                    lassoMap.Keys
                    |> Seq.iter (fun pi -> 
                        let lasso = lassoMap.[pi]
                        printfn $""
                        printfn $"%s{pi}"
                        printfn $"Prefix: %s{printList lasso.Prefix}"
                        printfn $"Loop: %s{printList lasso.Loop}"
                        )
            

        autoHyperQOptions.LoggerN $"Total Time: %i{swTotal.ElapsedMilliseconds} ms (%.4f{double(swTotal.ElapsedMilliseconds) / 1000.0} s)"
     
        0
    with 
    | AutoHyperQCore.Util.AutoHyperQCoreException err
    | AutoHyperQException err when raiseExceptions -> 
        printfn $"Error: %s{err}"
        reraise()
    | _  when raiseExceptions -> reraise()
    | AutoHyperQCore.Util.AutoHyperQCoreException err | AutoHyperQException err -> 
        printfn $"Error: %s{err}"
        exit -1
    | e -> 
        printfn $"General Error: %s{e.Message}"
        exit -1
