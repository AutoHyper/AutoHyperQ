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

module AutoHyperQ.HyperQPTLVariants 

open System

open FsOmegaLib.LTL

open TransitionSystemLib.SymbolicSystem

open AutoHyperQCore.HyperQPTL

exception private NotWellFormedException of String

// #####################################################################
// HyperQPTL for explicit-state system

type ExplictSystemHyperQPTLAtom = 
    | ExplictSystemTraceAtom of String * TraceVariable
    | ExplictSystemPropAtom of PropVariable


type ExplictSystemHyperQPTL = 
    {
        QuantifierPrefix : list<HyperQPTLQuantifier>
        LTLMatrix : LTL<ExplictSystemHyperQPTLAtom>
    }

module ExplictSystemHyperQPTL = 
    let quantifiedTraceVariables (formula : ExplictSystemHyperQPTL) =
        formula.QuantifierPrefix
        |> List.choose (fun x ->
            match x with
            | ForallTrace pi | ExistsTrace pi -> Some pi
            | _ -> None)
        
    let quantifiedPropVariables (formula : ExplictSystemHyperQPTL) =
        formula.QuantifierPrefix
        |> List.choose (fun x ->
            match x with
            | ForallProp p | ExistsProp p -> Some p
            | _ -> None)

    

    let findError (formula : ExplictSystemHyperQPTL) = 
        let propVars = quantifiedPropVariables formula
            
        let traceVars = quantifiedTraceVariables formula

        try 
            if propVars |> set |> Set.count <> List.length propVars then 
                raise <| NotWellFormedException $"Some propositional variable is used more than once."

            if traceVars |> set |> Set.count <> List.length traceVars then 
                raise <| NotWellFormedException $"Some trace variable is used more than once."

            LTL.allAtoms formula.LTLMatrix
            |> Set.iter (fun x -> 
                match x with 
                | ExplictSystemPropAtom q -> 
                    if List.contains q propVars |> not then 
                        raise <| NotWellFormedException $"Propositional Variable %s{q} is used but not defined in the prefix"

                | ExplictSystemTraceAtom (_, n) -> 
                    if List.contains n traceVars |> not then 
                        raise <| NotWellFormedException $"Trace Variable %s{n} is used but not defined in the prefix"
                )
            None 
        with 
            | NotWellFormedException msg -> Some msg

// #####################################################################
// HyperQPTL for boolean programs

type BooleanProgramHyperQPTLAtom = 
    | BooleanProgramTraceAtom of String * int * TraceVariable
    | BooleanProgramPropAtom of PropVariable


type BooleanProgramHyperQPTL = 
    {
        QuantifierPrefix : list<HyperQPTLQuantifier>
        LTLMatrix : LTL<BooleanProgramHyperQPTLAtom>
    }

module BooleanProgramHyperQPTL = 
    let quantifiedTraceVariables (formula : BooleanProgramHyperQPTL) =
        formula.QuantifierPrefix
        |> List.choose (fun x ->
            match x with
            | ForallTrace pi | ExistsTrace pi -> Some pi
            | _ -> None)
        
    let quantifiedPropVariables (formula : BooleanProgramHyperQPTL) =
        formula.QuantifierPrefix
        |> List.choose (fun x ->
            match x with
            | ForallProp p | ExistsProp p -> Some p
            | _ -> None)


    let findError (formula : BooleanProgramHyperQPTL) = 
        let propVars = quantifiedPropVariables formula
            
        let traceVars = quantifiedTraceVariables formula

        try 
            if propVars |> set |> Set.count <> List.length propVars then 
                raise <| NotWellFormedException $"Some propositional variable is used more than once."

            if traceVars |> set |> Set.count <> List.length traceVars then 
                raise <| NotWellFormedException $"Some trace variable is used more than once."

            LTL.allAtoms formula.LTLMatrix
            |> Set.iter (fun x -> 
                match x with 
                | BooleanProgramPropAtom q -> 
                    if List.contains q propVars |> not then 
                        raise <| NotWellFormedException $"Propositional Variable %s{q} is used but not defined in the prefix"

                | BooleanProgramTraceAtom (_, _, n) -> 
                    if List.contains n traceVars |> not then 
                        raise <| NotWellFormedException $"Trace Variable %s{n} is used but not defined in the prefix"
                )
            None 
        with 
            | NotWellFormedException msg -> Some msg



// #####################################################################
// HyperQPTL for symbolic system


type SymbolicSystemExpressionAtom = 
    | UnaryPredicate of Expression * TraceVariable
    | RelationalEqualityPredicate  of Expression * TraceVariable * Expression * TraceVariable

type SymbolicHyperQPTLAtom = 
    | SymbolicTraceAtom of SymbolicSystemExpressionAtom
    | SymbolicPropAtom of PropVariable


type SymbolicSystemHyperQPTL = 
    {
        QuantifierPrefix : list<HyperQPTLQuantifier>
        LTLMatrix : LTL<SymbolicHyperQPTLAtom>
    }

module SymbolicSystemHyperQPTL = 
    let quantifiedTraceVariables (formula : SymbolicSystemHyperQPTL) =
        formula.QuantifierPrefix
        |> List.choose (fun x ->
            match x with
            | ForallTrace pi | ExistsTrace pi -> Some pi
            | _ -> None)
        
    let quantifiedPropVariables (formula : SymbolicSystemHyperQPTL) =
        formula.QuantifierPrefix
        |> List.choose (fun x ->
            match x with
            | ForallProp p | ExistsProp p -> Some p
            | _ -> None)


    let findError (formula : SymbolicSystemHyperQPTL) = 
        let propVars = quantifiedPropVariables formula
            
        let traceVars = quantifiedTraceVariables formula

        try 
            if propVars |> set |> Set.count <> List.length propVars then 
                raise <| NotWellFormedException $"Some propositional variable is used more than once."

            if traceVars |> set |> Set.count <> List.length traceVars then 
                raise <| NotWellFormedException $"Some trace variable is used more than once."

            LTL.allAtoms formula.LTLMatrix
            |> Set.iter (fun x -> 
                match x with 
                | SymbolicPropAtom q -> 
                    if List.contains q propVars |> not then 
                        raise <| NotWellFormedException $"Propositional Variable %s{q} is used but not defined in the prefix"

                | SymbolicTraceAtom y -> 
                    match y with 
                    | UnaryPredicate (_, n) -> 
                        if List.contains n traceVars |> not then 
                            raise <| NotWellFormedException $"Trace Variable %s{n} is used but not defined in the prefix"

                    | RelationalEqualityPredicate (_, n1, _, n2) -> 
                        if List.contains n1 traceVars |> not then 
                            raise <| NotWellFormedException $"Trace Variable %s{n1} is used but not defined in the prefix"

                        if List.contains n2 traceVars |> not then 
                            raise <| NotWellFormedException $"Trace Variable %s{n2} is used but not defined in the prefix"  
                )
            None 
        with 
            | NotWellFormedException msg -> Some msg

// #####################################################################
// HyperQPTL for explicit-state system

module Parser = 
    open FParsec

    // ####################################################
    // Parsing for HyperQPTL for explict-state systems

    let private explicitStateHyperQPTLAtomParser =
        attempt (
            (Util.ParserUtil.escapedStringParser .>> pchar '_' .>>. AutoHyperQCore.HyperQPTL.Parser.traceVarParser)
            |>> ExplictSystemTraceAtom
        )
        <|>
        (Parser.propVarParser |>> ExplictSystemPropAtom)

    let private explicitStateHyperQPTLParser = 
        pipe2 
            Parser.hyperQPTLQuantifierPrefixParser 
            (FsOmegaLib.LTL.Parser.ltlParser explicitStateHyperQPTLAtomParser)
            (fun x y -> {ExplictSystemHyperQPTL.QuantifierPrefix = x; LTLMatrix = y})

    let parseExplictStateHyperQPTL s =    
        let full = explicitStateHyperQPTLParser .>> spaces .>> eof
        let res = run full s
        match res with
        | Success (res, _, _) -> Result.Ok res
        | Failure (err, _, _) -> Result.Error err


    // ####################################################
    // Parsing for HyperQPTL for boolean programs systems

    let private relVarParserBit : Parser<(String * int), unit>= 
        pstring "{" >>. 
            pipe3
                (spaces >>. many1Chars letter)
                (pchar '_')
                (pint32 .>> pstring "}")
                (fun x _ y  -> (x, y))

    let private booleanProgramHyperQPTLAtomParser =
        attempt (
            (relVarParserBit .>> pchar '_' .>>. AutoHyperQCore.HyperQPTL.Parser.traceVarParser)
            |>> fun ((x, i), n) -> BooleanProgramTraceAtom(x, i, n)
        )
        <|>
        (Parser.propVarParser |>> BooleanProgramPropAtom)

    let private booleanProgramHyperQPTLParser = 
        pipe2 
            Parser.hyperQPTLQuantifierPrefixParser 
            (FsOmegaLib.LTL.Parser.ltlParser booleanProgramHyperQPTLAtomParser)
            (fun x y -> {BooleanProgramHyperQPTL.QuantifierPrefix = x; BooleanProgramHyperQPTL.LTLMatrix = y})

    let parseBooleanProgramHyperQPTL s =    
        let full = booleanProgramHyperQPTLParser .>> spaces .>> eof
        let res = run full s
        match res with
        | Success (res, _, _) -> Result.Ok res
        | Failure (err, _, _) -> Result.Error err


    // ####################################################
    // Parsing for HyperQPTL for symbolic systems

    let private symbolicSystemExpressionAtomParser = 
        let indexedExpressionParser =   
            tuple2
                (skipChar '{' >>. spaces >>. TransitionSystemLib.SymbolicSystem.Parser.expressionParser .>> spaces .>> skipChar '}')
                (spaces >>. skipChar '_' >>. spaces >>. AutoHyperQCore.HyperQPTL.Parser.traceVarParser) 

        let unaryAtomParser = 
            indexedExpressionParser
            |>> UnaryPredicate

        let relationalAtomParser = 
            pipe2
                (indexedExpressionParser .>> spaces .>> skipChar '=')
                (spaces >>. indexedExpressionParser)
                (fun (e1, pi1) (e2, pi2) -> RelationalEqualityPredicate(e1, pi1, e2, pi2))

        attempt(relationalAtomParser) <|> unaryAtomParser

    let private symbolicSystemHyperQPTLAtomParser =
        attempt (
            symbolicSystemExpressionAtomParser
            |>> SymbolicTraceAtom
        )
        <|>
        (Parser.propVarParser |>> SymbolicPropAtom)

    let private symbolicSystemHyperQPTLParser = 
        pipe2 
            Parser.hyperQPTLQuantifierPrefixParser
            (FsOmegaLib.LTL.Parser.ltlParser symbolicSystemHyperQPTLAtomParser)
            (fun x y -> {SymbolicSystemHyperQPTL.QuantifierPrefix = x; SymbolicSystemHyperQPTL.LTLMatrix = y})

    let parseSymbolicSystemHyperQPTL s =    
        let full = symbolicSystemHyperQPTLParser .>> spaces .>> eof
        let res = run full s
        match res with
        | Success (res, _, _) -> Result.Ok res
        | Failure (err, _, _) -> Result.Error err
