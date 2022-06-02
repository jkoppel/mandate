# Contents of this repository

* src: The core source code for Mandate
* app: Code for the driver, the syntax and semantics of the included languages, and the sample static analyzers
* gallery: Graphs generated by Mandate in interpreted mode. See README in that folder.
* graph_patterns: Example graph patterns for some of the more interesting constructs.
* ghci_scripts: Used for starting a GHCI session from which Mandate can be controlled.
* Tiger: Sample Tiger-files, and a Haskell implementation of a Tiger compiler (+ a C runtime). The parser is included by Mandate's Tiger implementation as a library.
* MITScript: A modified implementation of MITScript in C++. The `main.cpp` file is a small program that reads an MITScript program from stdin and outputs the AST for that term in a Haskell-readable format.

# Installation

## Dependencies

### Core Mandate

The only dependency is `stack` ( https://www.haskellstack.org/ ).

    curl -sSL https://get.haskellstack.org/ | sh

Alternatively, on Ubuntu/Debian:

    sudo apt-get install haskell-stack


### MITScript

If you wish to parse MITScript files, you will also need the dependencies for the included MITScript implementation:

* flex
* bison
* g++ or clang
* make

On Ubuntu or Debian:

    sudo apt-get install g++ flex bison make


## Building

### Core Mandate

With stack:

    stack build

### MITScript

    cd ./MITScript
    make

To test the MITScript build: From the MITScript directory, run:

    ./a.out < tests/good1.mit
    
Expected output:

    (Block [(Assign (Var (Name "x")) (NumConst 5)), (Assign (Var (Name "y")) (NumConst 7)), (Assign (Var (Name "z")) (FunDecl [(Name "x")] (Block [(Return (Var (Name "x")))])))])

## Running and using

Mandate has a very minimal command-line driver, only capable of generating interpreted-mode CFGs from an input file. All other functionality is accessed by entering commands in GHCI.

### Driver instructions

    stack exec derive-cfg <name-of-language> <path-to-source-file>
    
Valid options for <name-of-language>: mitscript, tiger, imp
  
Example usage:

    stack exec derive-cfg tiger Tiger/testcases/test1.tig

Example Tiger files are in `Tiger/testcases`. Example MITScript files are in `MITScript/tests`. For Imp, there are no files. Instead, the names term1, term3, term4, and termBalanceParens are hardcoded to refer to programs defined in `app/Languages/Imp/Imp.hs`.

### REPL instructions

Run "stack ghci".

Use the scripts defined in `ghci-scripts` to load an environment from which you can enter commands for a language. They import necessary modules and generate abstract machines from the SOS rules defined in the corresponding file, and print instructions for how to generate a graph.

Example:

To work in Tiger, from GHCI, run:

    > :script ghci-scripts/run-tiger

The scripts `ghci-scripts/run-tiger` and `ghci-scripts/run-mitscript` will further define the following four functions:

* `parse`: Takes a string giving the path of a Tiger or MITScript file; returns the parsed program as a `Term`
* `eval`: Takes a `Term`; prints an execution trace of the term according to the SOS rules
* `interpret`: Composition of `parse` and `eval`
* `showGraph`: Inputs a generated graph; prints the graph in DOT (GraphViz) format

### Viewing the generated PAM and AM rules

After running the GHCI script for the language of interest, the generated Phased Abstract Machine and Abstract Machine rules will be stored in variables called `pamRules` and `amRules`, respectively. The default `Show` instance will print them in a human-readable format.

    > print amRules
    
Or simply:

    > amRules


### Generating an interpreted-mode CFG

Most of the language scripts print instructions for doing this when loaded.

General command:

    >  abstractAmCfg <function abstraction> <term abstraction> amRules <term>

See the section on abstractions for an explanation of the available abstractions.

For example, to generate a statement-level CFG for the file `MITScript/tests/what-works.mit`:

    > t <- parse "MITScript/tests/what-works.mit"
    > abstractAmCfg (irrelevance (SortIrr \"Exp\")) (irrelevance (SortIrr \"Exp\")) amRules t

### Generating the compiled-mode CFG-generators

After running the GHCI script for the language of interest, use the following commands to generate the graph patterns and the CFG generator :

    >  gs <- makeGraphPatterns <function abstraction> <term abstraction> amRules signature
    >  graphPatternsToCode gs
    
For example, to generate an expression-level CFG-generator for Tiger:

    >  gs <- makeGraphPatterns (irrelevance ValueIrr) (irrSkippingFunScope ValueIrr) amRules signature
    >  graphPatternsToCode gs

For MITScript:

    >  gs <- makeGraphPatterns (irrelevance ValueIrr) (irrSkippingScope ValueIrr) amRules signature
    >  graphPatternsToCode gs

Note that the variable `amRules` is defined by the GHCI script, while `signature :: Lang l => Signature l` is a member of the `Lang` typeclass. Be warned that `graphPatternsToCode` can take several minutes to run for some cases seen in Tiger, due to inefficient graph algorithms.

Different kinds of CFG generators are enabled by the different abstractions passed to `makeGraphPatterns`. More instructions in the abstractions section below.

### Using the various abstractions

Currently, the only abstractions are various instances of the `Irrelevance` typeclass, defined in `src/Semantics/Abstraction.hs`. They are:

`irrelevance ValueIrr`: Abstracts all values to ValStar. Generates expression-level CFGs.
`irrelevance (SortIrr <sort>)`: Abstracts all terms of the given sort to ValStar. For example, for MITScript, `irrelevance (SortIrr "Exp")` abstracts all expresisons to ValStar, generating statement-level CFGs.
`irrelevance (VarNotIrr <variable name>)`: Abstracts all terms to ValStar except the value of the given variable in the environment. 

Tiger and MITScript also define the `irrSkippingFunScope` and `irrSkippingScope` combinators respectively (example usage: `irrSkippingFunScope ValueIrr`) which produces a modified abstraction which also abstracts the inner evaluation of functions to ValStar.

Abstractions of the built-in semantic functions are defined per-language in the `Semantics.hs` file for that language. For example, for Tiger, the code begins:

```haskell
instance Irrelevance (CompFunc Tiger) where
    irrelevance _ ReadField    = AbsReadField
    irrelevance _ WriteField   = AbsWriteField
    ...
```

The execution of `AbsReadFile` and friends is then defined in the `runExternalComputation` function, e.g.:

    runExternalComputation func state [GStar  _] = return $ emptyConf ValStar

### Using the generated CFG generators

Pre-generated code for the expression-level CFG-generators is already included, in `app/Languages/Imp/CfgGen.hs`, `app/Languages/MITScript/CfgGen.hs`, and `app/Languages/Tiger/CfgGen.hs`. Each file defines a `makeExpCfg` function, and is commented with the command that generated the code.

Example session:

    > :script ghci-scripts/run-tiger
    > import Languages.Tiger.CfgGen
    > t <- parse "Tiger/testcases/queens.tig"
    > showGraph (makeExpCfg t)

### Running a static analysis

For the constant propagation analysis (instructions shown for Tiger; trivial tweak for other languages):

    > :script ghci-scripts/run-tiger
    > import Main
    > t <- parse "Tiger/testcases/queens.tig"
    > analyzeConstPropTiger t

To run the paren-balancing analysis on the example from the paper in the intro:


    > :script ghci-scripts/run-imp
    > import Languages.Imp.Analysis
    > analyzeParenBalance "b" termBalanceParens

### Advanced usages

Here we give cursory instructions for some other things a user may wish to do, discussed less in the paper:

Executing a term using the PAM or AM rules: Use the functions `pamEvaluationSequence`, `amEvaluationSequence`, and their variants, defined in `Semantics.AbstractMachine` and `Semantics.Pam`.

# Viewing generated graphs

To generate graphs, you'll need to install Graphviz, which provides the `dot` command line utility for genrating `.svg`'s from dot files.

Alternatively, you can use a browser-based Graphviz viewer such as http://viz-js.com/ .
