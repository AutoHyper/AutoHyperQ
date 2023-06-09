# AutoHyperQ: A Model Checker for HyperQPTL

This repository contains AutoHyperQ - a fully-automatic model checker for HyperQPTL on finite-state systems.
Clone this repository by running

```shell
git clone https://github.com/autohyper/autohyperq
cd autohyperq
git submodule init
git submodule update
```

## Structure 

- `src/` contains the source code of AutoHyperQ (written in F#). 
- `app/` is the target folder for the build. The final AutoHyperQ executable will be placed here.
- `examples/` contains a few examples which can be used as a smoke test

The following sections contain instructions on how to build AutoHyperQ and how to use it. 

## Build AutoHyperQ

### Dependencies

We require the following dependencies:

- [.NET 7 SDK](https://dotnet.microsoft.com/en-us/download) (tested with version 7.0.203)
- [spot](https://spot.lrde.epita.fr/) (tested with version 2.11.5)

Install the .NET 7 SDK (see [here](https://dotnet.microsoft.com/en-us/download) for details).
Download and build spot (details can be found [here](https://spot.lrde.epita.fr/)). 
You can place the spot executables in any location of your choosing. 
AutoHyperQ requires the *absolute* path to spot (see details below).

### Build AutoHyperQ

To build AutoHyperQ run the following (when in the main directory of this repository).

```shell
cd src/AutoHyperQ
dotnet build -c "release" -o ../../app
cd ../..
```

Afterward, the AutoHyperQ executable is located in the `app/` folder.

### Connect spot to AutoHyperQ

AutoHyperQ requires the *autfilt* and *ltl2tgba* tools from the spot library.
AutoHyperQ is designed such that it only needs the **absolute** path to these executables, so they can be installed and placed at whatever locations fits best.
The absolute paths are specified in a `paths.json` configuration file. 
This file must be located in the *same* directory as the AutoHyperQ executables (this convention makes it easy to find the config file, independent of the relative path AutoHyperQ is called from). 
We already provide a template file `app/paths.json` that *needs to be modified*. 
After having built spot, paste the absolute path to the *autfilt* and *ltl2tgba* executables to the `paths.json` file. 
For example, if `/usr/bin/autfilt` and `/usr/bin/ltl2tgba` are the *autfilt* and *ltl2tgba* executables, the content of `app/paths.json` should be

```json
{
    "autfilt": "/usr/bin/autfilt",
    "ltl2tgba": "/usr/bin/ltl2tgba"
}
```

### A First Example

To test that the paths have been setup correctly, we can verify our first HyperQPTL property by running the following (from the main directory of this repository)
```shell
app/AutoHyperQ --explicit ./examples/example_system.txt ./examples/example_hyperqptl.txt
```
which should output `UNSAT`.


## Model Checking with AutoHyperQ

In this section, we first discuss the command-line options of AutoHyperQ, followed by the structure of supported input. 

### Command-line Arguments

AutoHyperQ supports several command-line options.
We focus on the verification of explicit-state systems by calling
```shell
app/AutoHyperQ --explicit <systemPath(s)> <propPath>
```
where `<systemPath(s)>` is either a single path to the system or multiple such paths and `<propPath>` is the path to the property.
In case  `<systemPath(s)>` is only a single path, we use the system at this path to resolve all quantifiers. 
In case `<systemPath(s)>` are multiple paths, their number must match the trace quantifier prefix in the HyperQPTL property.

In addition to explicit state systems, AutoHyperQ also features support for NuSMV models and automata as models.

For details on how the system and property are specified, we refer to the following sections.   

Additional (optional) command-line options include

- `--log` logs intermediate information to the console


### Specifying HyperQPTL Properties

The specifications checked by AutoHyperQ are written in HyperQPTL.
A HyperQPTL formula consists of an LTL-like body, preceded by a quantifier prefix. 
Formulas have the form `<prefix> <body>`.

Here `<body>` can be one of the following:
- `1`: specifies the boolean constant true
- `0`: specifies the boolean constant false
- `"<AP>"_<TVAR>` where `<AP>` is an atomic proposition (any sequence of letters that does not contain `"`) and `<TVAR>` is a trace variable which is any string consisting of letters, digits, and `-` (starting with a letter). This atomic formula holds if the atomic proposition `<AP>` holds in the current step on trace `<TVAR>`. Note that atomic proposition name is escaped in `"`s.
- `<PVAR>` where `<PVAR>` is a propositional variable which is any string consisting of letters, digits, `-`, `_`, and `@` (starting with a letter).
- `(<body>)`: Parenthesis
- `<body> & <body>`: Conjunction
- `<body> | <body>`: Disjunction
- `<body> -> <body>`: Implication
- `<body> <-> <body>`: Equivalence
- `<body> U <body>`: Until
- `<body> W <body>`: Weak Until
- `<body> R <body>`: Release
- `F <body>`: Eventually
- `G <body>`: Globally
- `X <body>`: Next
- `! <body>`: Negation

The operators follow the usual operator precedences. To avoid ambiguity, we recommend always placing parenthesis around each construct. 

The quantifier prefix `<prefix>` can be one of the following:
- The empty string
- Universal trace quantification `forall <TVAR>. <prefix>`
- Existential trace quantification `exists <TVAR>. <prefix>`
- Universal propositional quantification `A <PVAR>. <prefix>`
- Existential propositional quantification `E <PVAR>. <prefix>`

where `<TVAR>` is a trace variable and `<PVAR>` is a propositional variable.

### Specifying Explicit-State Transition Systems

An explicit-state system has the form 

```
AP: "<AP>" ... "<AP>"
Init: <stateID> ... <stateID> 
--BODY-- 
<stateDefinition>
...
<stateDefinition>
--END--
```

Here, `<AP>` is an atomic proposition. This can be any string not containing `"`. Note that all atomic propositions are escaped in '"'.
`<stateID>` is any natural number specifying a state. 
The header specifies which states are initial (there must be at least one initial state) and which APs are used in the system.

A `<stateDefinition>` has the form 
```
State: <stateID> <apEval>
<stateID> ... <stateID>
```

It specifies which state we are defining and the evaluation of the atomic propositions in that state. 
The `<apEval>` has the form `{<apIndex> ... <apIndex>}` where `<apIndex>` is a natrual number that identifies one of the provided APs. 
The second line lists all successors of that state.
Every state must have at least one successor state.

Consider the following example:

```
AP: "x" "y"
Init: 0 1
--BODY-- 
State: 0 {}
0 2 3
State: 1 {1}
0 1 2
State: 2 {0}
0 2 3
State: 3 {0 1}
2 3
--END--
```

This specification declares states `0` and  `1` as initial states and `"x"` and `"y"` as APs.
For each state, we give the evaluation of the atomic propositions by listing the indices of all APs which hold in the given state.
For example, in state `1`, AP `"x"` (index 0) does not hold but `"y"` (index 1) does.
Each state lists all successors of that node. 
For example, the successor states of state `0` are states `0`, `2`, and `3`.

HyperLTL properties on explicit-state systems are specified by using atomic propositions of the form `"<AP>"_<Var>` where `<AP>` is the AP as given in the system, and `<VAR>` is a trace variable from the quantifier prefix. 
This atomic proposition holds if, in the trace bound to `<VAR>`, the AP `<AP>` holds. 

An example property on the above example system would be:

```
forall A. exists B. X (G ("x"_A <-> "y"_B))
```
