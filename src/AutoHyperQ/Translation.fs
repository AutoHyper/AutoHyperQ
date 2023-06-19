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

module AutoHyperQ.Translation

open System 

open FsOmegaLib.LTL
open FsOmegaLib.SAT
open FsOmegaLib.AutomatonSkeleton
open FsOmegaLib.GNBA
open FsOmegaLib.Operations

open TransitionSystemLib.TransitionSystem
open TransitionSystemLib.SymbolicSystem
open TransitionSystemLib.BooleanProgramSystem

open AutoHyperQCore.HyperQPTL
open AutoHyperQCore.SolverConfiguration

open Util
open HyperQPTLVariants


let private convertTransitionSystemToGNBA (ts : TransitionSystem<'T>) = 
    {
        GNBA.Skeleton =
            {
                AutomatonSkeleton.States = ts.States
                APs = ts.APs
                Edges =
                    ts.Edges
                    |> Map.map (fun s sucs ->
                        let apEvalDnf : DNF<int> = 
                            [0..ts.APs.Length - 1]
                            |> List.map (fun i -> 
                                if Set.contains i ts.ApEval.[s] then 
                                    PL i 
                                else    
                                    NL i
                                )
                            |> List.singleton 

                        sucs
                        |> Set.toList
                        |> List.map (fun t -> apEvalDnf, t)
                        )
            }
        InitialStates = ts.InitialStates
        AcceptanceSets =
            ts.States
            |> Set.toSeq
            |> Seq.map (fun x -> x, Set.empty)
            |> Map.ofSeq
        NumberOfAcceptingSets = 0
    }

let convertSymbolicSystemInstanceToGNBA (plist : list<SymbolicSystem>) (formula : SymbolicSystemHyperQPTL) = 
    match SymbolicSystemHyperQPTL.findError formula with 
    | None -> () 
    | Some msg -> 
        raise <| AutoHyperQException $"Error in the specification: %s{msg}"

    plist 
    |> List.iteri (fun i p -> 
        match SymbolicSystem.findError p with 
        | None -> () 
        | Some msg -> 
            raise <| AutoHyperQException $"Error in the %i{i}th system: %s{msg}"
        )

    if plist.Length <> 1 && plist.Length <> formula.QuantifierPrefix.Length then 
        raise <| AutoHyperQException $"Invalid number of programs"

    let nameMapping = 
        SymbolicSystemHyperQPTL.quantifiedTraceVariables formula
        |> List.mapi (fun i x -> x, i)
        |> Map.ofList

    let unfoldRelationPredicate (atom : SymbolicSystemExpressionAtom)  = 
        match atom with 
        | UnaryPredicate (e, n) -> 
            LTL.Atom ((e, n))
        | RelationalEqualityPredicate(e1, n1, e2, n2) -> 

            let getSystem index = 
                if plist.Length = 1 then plist.[0] else plist.[index]

            let t1 = 
                try 
                    SymbolicSystem.inferTypeOfExpression (getSystem nameMapping.[n1]) e1
                with 
                | TypeInferenceException err -> 
                    raise <| AutoHyperQException $"Error when infering the type of expression %s{Expression.print e1} in relation equality atom: %s{err}"

            let t2 = 
                try 
                    SymbolicSystem.inferTypeOfExpression (getSystem nameMapping.[n2]) e2
                with 
                | TypeInferenceException err -> 
                    raise <| AutoHyperQException $"Error when infering the type of expression %s{Expression.print e2} in relation equality atom: %s{err}"
                
            let t = 
                match VariableType.intersectTypes t1 t2 with 
                    | Some x -> x 
                    | None -> 
                        raise <| AutoHyperQException $"Error during unfolding: Could not intersect types %s{VariableType.print t1} and %s{VariableType.print t2} of expressions %s{Expression.print e1} and %s{Expression.print e2}."

            t
            |> VariableType.allValues
            |> List.map (fun v -> 
                LTL.And(
                    LTL.Atom((Expression.Eq(e1, v |> VariableValue.toConstant |> Expression.Const), n1)),
                    LTL.Atom((Expression.Eq(e2, v |> VariableValue.toConstant |> Expression.Const), n2))
                )
            )
            |> fun l -> if List.isEmpty l then LTL.False else l |> List.reduce (fun x y -> LTL.Or(x, y)) 

    let unfoldedHyperQPTL = 
        {
            HyperQPTL.QuantifierPrefix = formula.QuantifierPrefix
            HyperQPTL.LTLMatrix = 
                formula.LTLMatrix 
                |> LTL.bind (fun x -> 
                    match x with 
                    | SymbolicPropAtom q -> PropAtom q |> LTL.Atom
                    | SymbolicTraceAtom x -> 
                        x
                        |> unfoldRelationPredicate
                        |> LTL.map TraceAtom
                )
        }

    let tsList = 
        if plist.Length = 1 then 
            // A single system where all traces are resolved on
            let atomList = 
                unfoldedHyperQPTL.LTLMatrix
                |> LTL.allAtoms
                |> Set.toList
                |> List.choose (fun x -> 
                    match x with 
                    | TraceAtom (x, _) -> Some x 
                    | PropAtom _ -> None
                    )
                |> List.distinct
                
            atomList
            |> List.iter (fun (e : Expression) ->
                try
                    match SymbolicSystem.inferTypeOfExpression plist.[0] e with 
                    | BoolType -> ()
                    | t -> 
                        raise <| AutoHyperQException $"Expression %s{Expression.print e} used in the HyperQPTL formula has non-boolean type %s{VariableType.print t}"
                with 
                | TypeInferenceException err -> 
                    raise <| AutoHyperQException $"Error when infering type of expression %s{Expression.print e} used in the HyperQPTL formula: %s{err}"
            )

            SymbolicSystem.convertSymbolicSystemToTransitionSystem plist.[0] atomList
            |> List.singleton
        else 
            // Multiple systems, so each is resolved on a separate syste
            HyperQPTL.quantifiedTraceVariables unfoldedHyperQPTL
            |> List.map (fun n -> 
                let atomList = 
                    unfoldedHyperQPTL.LTLMatrix
                    |> LTL.allAtoms
                    |> Set.toList
                    |> List.choose (fun x -> 
                        match x with 
                        | TraceAtom (x, m) when n = m -> Some x 
                        | TraceAtom _ -> None
                        | PropAtom _ -> None
                        )
                    |> List.distinct

                // Check that all atoms are well typed
                atomList
                |> List.iter (fun (e : Expression) ->
                    try
                        match SymbolicSystem.inferTypeOfExpression plist.[nameMapping[n]] e with 
                        | BoolType -> ()
                        | t -> 
                            raise <| AutoHyperQException $"Expression %s{Expression.print e} used in the HyperQPTL formula has non-boolean type %s{VariableType.print t}"
                    with 
                    | TypeInferenceException err -> 
                        raise <| AutoHyperQException $"Error when infering type of expression %s{Expression.print e} used in the HyperQPTL formula: %s{err}"
                )

                SymbolicSystem.convertSymbolicSystemToTransitionSystem plist.[nameMapping[n]] atomList
            )

    let renamingMap = 
        unfoldedHyperQPTL.LTLMatrix
        |> LTL.allAtoms
        |> Set.toList
        |> List.choose (fun x -> 
            match x with 
            | TraceAtom (x, _) -> Some x 
            | PropAtom _ -> None
            )
        |> List.distinct
        |> List.mapi (fun i x -> x, "a" + string(i))
        |> Map.ofList

    let mappedTs = 
        tsList
        |> List.map convertTransitionSystemToGNBA
        |> List.map (FsOmegaLib.GNBA.GNBA.mapAPs (fun x -> renamingMap[x]))
            
    let mappedFormula = 
        unfoldedHyperQPTL
        |> HyperQPTL.map (fun x -> renamingMap[x])

    mappedTs, mappedFormula


let convertBooleanProgramInstanceToGNBA (progList : list<BooleanProgram>) (formula : BooleanProgramHyperQPTL) = 
    match BooleanProgramHyperQPTL.findError formula with 
    | None -> () 
    | Some msg -> 
        raise <| AutoHyperQException $"Error in the specification: %s{msg}"

    progList 
    |> List.iteri (fun i p -> 
        match BooleanProgram.findError p with 
        | None -> () 
        | Some msg -> 
            raise <| AutoHyperQException $"Error in the %i{i}th system: %s{msg}"
        )

    if progList.Length <> 1 && progList.Length <> formula.QuantifierPrefix.Length then 
        raise <| AutoHyperQException $"Invalid number of programs"

    let nameMapping =
        formula
        |> BooleanProgramHyperQPTL.quantifiedTraceVariables 
        |> List.mapi (fun i x -> x, i)
        |> Map.ofList


    let unfoldedHyperQPTL = 
        {
            HyperQPTL.QuantifierPrefix = formula.QuantifierPrefix
            HyperQPTL.LTLMatrix = 
                formula.LTLMatrix 
                |> LTL.map (fun x -> 
                    match x with 
                    | BooleanProgramPropAtom q -> PropAtom q
                    | BooleanProgramTraceAtom (x, i, n) -> TraceAtom ((x, i), n)
                )
        }

    let tsList = 
        if progList.Length = 1 then 
            let prog = progList[0]

            let relevantAps = 
                unfoldedHyperQPTL.LTLMatrix
                |> LTL.allAtoms
                |> Set.toList
                |> List.choose (fun x -> 
                    match x with 
                    | TraceAtom (x, _) -> Some x 
                    | PropAtom _ -> None
                    )
                |> List.distinct
                
            relevantAps
            |> List.iter (fun (v, i) ->
                if prog.DomainMap.ContainsKey v && prog.DomainMap.[v] > i |> not then
                    raise <| AutoHyperQException $"AP (%A{v}, %i{i}) is used in the HyperQPTL property but variable %A{v} does not exists or does not have not the required bit width"
                )

            BooleanProgram.convertBooleanProgramToTransitionSystem prog relevantAps
            |> List.singleton
        else 
            unfoldedHyperQPTL
            |> HyperQPTL.quantifiedTraceVariables
            |> List.map (fun n ->   
                let relevantAps = 
                    unfoldedHyperQPTL.LTLMatrix
                    |> LTL.allAtoms
                    |> Set.toList
                    |> List.choose (fun x -> 
                        match x with 
                        | TraceAtom (x, m) when n = m -> Some x 
                        | TraceAtom _ -> None
                        | PropAtom _ -> None
                        )
                    |> List.distinct
                    
                relevantAps
                |> List.iter (fun (v, j) ->
                    if progList.[nameMapping[n]].DomainMap.ContainsKey v && progList.[nameMapping[n]].DomainMap.[v] > j |> not then
                        raise <| AutoHyperQException $"AP (%A{v}, %i{j}) is used in the HyperQPTL property but variable %A{v} does not exists or does not have not the required bit width"
                    )
                
                BooleanProgram.convertBooleanProgramToTransitionSystem progList.[nameMapping[n]] relevantAps
            )
  
    let mappedTs = 
        tsList
        |> List.map convertTransitionSystemToGNBA
        |> List.map (GNBA.mapAPs (fun (x, i) -> x + "@" + string(i)))
            
    let mappedFormula = 
        unfoldedHyperQPTL
        |> HyperQPTL.map (fun (x, i) -> x + "@" + string(i))

    mappedTs, mappedFormula

let convertExplictSystemInstanceToGNBA (systemList : list<TransitionSystem<String>>) (formula : ExplictSystemHyperQPTL) = 
    let unfoldedHyperQPTL = 
        {
            HyperQPTL.QuantifierPrefix = formula.QuantifierPrefix
            HyperQPTL.LTLMatrix = 
                formula.LTLMatrix 
                |> LTL.map (fun x -> 
                    match x with 
                    | ExplictSystemPropAtom q -> PropAtom q
                    | ExplictSystemTraceAtom (x, n) -> TraceAtom (x, n)
                )
        }

    let tsList = 
        systemList 
        |> List.map convertTransitionSystemToGNBA
     
    tsList, unfoldedHyperQPTL

let convertHanoiSystemInstanceToGNBA (config : SolverConfiguration) (autoHyperQOptions : AutoHyperQOptions) (systemStringList : list<String>) (formula : HyperQPTL<String>) = 
    let tsList = 
        systemStringList
        |> List.map (fun x -> 
            match FsOmegaLib.Operations.AutomatonFromString.convertHoaStringToGNBA autoHyperQOptions.RaiseExceptions config.GetMainPath config.GetAutfiltPath Effort.LOW x with 
            | Success x -> x 
            | Fail msg -> raise <| AutoHyperQException $"Failure when obtaining GNBA for system: %s{msg}"
        )
     
    tsList, formula