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

module AutoHyperQ.InstanceParsing 

open System.IO

open Util

let readAndParseHanoiInstance systemInputPaths formulaInputPath  = 
    let propcontent =   
        try 
            File.ReadAllText formulaInputPath
        with 
        | _ -> raise <| AutoHyperQException $"Could not open/read file %s{formulaInputPath}"
                
    let tsStringList = 
        systemInputPaths
        |> List.map (fun x -> 
            try 
                File.ReadAllText  x
            with 
            | _ -> raise <| AutoHyperQException $"Could not open/read file %s{x}"
        )

    let formula =
        match AutoHyperQCore.HyperQPTL.Parser.parseHyperQPTL Util.ParserUtil.escapedStringParser propcontent with 
            | Result.Ok x -> x
            | Result.Error err -> 
                raise <| AutoHyperQException $"The HyperQPTL formula could not be parsed: %s{err}"
        
    tsStringList, formula

let readAndParseSymbolicInstance systemInputPaths formulaInputPath =
    let systemList = 
        systemInputPaths 
        |> List.map (fun x -> 
                try 
                    File.ReadAllText  x
                with 
                | _ -> raise <| AutoHyperQException $"Could not open/read file %s{x}"
            )
        |> List.mapi (fun i s -> 
            match TransitionSystemLib.SymbolicSystem.Parser.parseSymbolicSystem s with 
            | Result.Ok x -> x 
            | Result.Error msg -> 
                raise <| AutoHyperQException $"The %i{i}th symbolic system could not be parsed: %s{msg}"
            )

    let propContent = 
        try 
            File.ReadAllText formulaInputPath
        with 
        | _ -> raise <| AutoHyperQException $"Could not open/read file %s{formulaInputPath}"

    let formula = 
        match HyperQPTLVariants.Parser.parseSymbolicSystemHyperQPTL propContent with
        | Result.Ok x -> x
        | Result.Error err -> 
            raise <| AutoHyperQException $"The HyperQPTL formula could not be parsed: %s{err}"

    systemList, formula


let readAndParseBooleanProgramInstance systemInputPaths formulaInputPath =
    let programList = 
        systemInputPaths 
        |> List.map (fun x -> 
                try 
                    File.ReadAllText  x
                with 
                    | _ -> 
                        raise <| AutoHyperQException $"Could not open/read file %s{x}"
            )
        |> List.mapi (fun i s -> 
            match TransitionSystemLib.BooleanProgramSystem.Parser.parseBooleanProgram s with 
                | Result.Ok x -> x 
                | Result.Error msg -> raise <| AutoHyperQException $"The %i{i}th boolean program could not be parsed: %s{msg}"
            )

    let propContent = 
        try 
            File.ReadAllText formulaInputPath
        with 
        | _ -> raise <| AutoHyperQException $"Could not open/read file %s{formulaInputPath}"


    let formula = 
        match HyperQPTLVariants.Parser.parseBooleanProgramHyperQPTL propContent with
        | Result.Ok x -> x
        | Result.Error err -> 
            raise <| AutoHyperQException $"The HyperQPTL formula could not be parsed: %s{err}"

    programList, formula

let readAndParseExplicitInstance systemInputPaths formulaInputPath =
    let explicitTsList = 
        systemInputPaths 
        |> List.map (fun x -> 
                try 
                    File.ReadAllText  x
                with 
                    | _ -> 
                        raise <| AutoHyperQException $"Could not open/read file %s{x}"
            )
        |> List.mapi (fun i s -> 
            match TransitionSystemLib.TransitionSystem.Parser.parseTransitionSystem s with 
                | Result.Ok x -> x 
                | Result.Error msg -> 
                    raise <| AutoHyperQException $"The %i{i}th explicit-state transition system could not be parsed: %s{msg}"
            )

    let propContent = 
        try 
            File.ReadAllText formulaInputPath
        with 
        | _ -> raise <| AutoHyperQException $"Could not open/read file %s{formulaInputPath}"


    let formula = 
        match HyperQPTLVariants.Parser.parseExplictStateHyperQPTL propContent with
        | Result.Ok x -> x
        | Result.Error err -> 
            raise <| AutoHyperQException $"The HyperQPTL formula could not be parsed: %s{err}"

    explicitTsList, formula