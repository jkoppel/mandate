# Docker image

For a Docker image with this repository already running, see https://zenodo.org/record/6689803

# Contents of this repository

* src: The core source code for Mandate
* app: Code for the driver, the syntax and semantics of the included languages, and the sample static analyzers
* gallery: CFGs and CFG-generators generated by Mandate. See README in that folder.
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


### MITScript parser

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

### MITScript parser

    cd ./MITScript
    make

To test the MITScript build: From the MITScript directory, run:

    ./a.out < tests/good1.mit
    
Expected output:

    (Block [(Assign (Var (Name "x")) (NumConst 5)), (Assign (Var (Name "y")) (NumConst 7)), (Assign (Var (Name "z")) (FunDecl [(Name "x")] (Block [(Return (Var (Name "x")))])))])
    
### Flags

Mandate supports two flags:

* `debug-step`: Causes Mandate to verbose print its work during matching, unification, and (attempted) rule application
* `short-node-name`: Causes the names of CFG nodes in interpreted-mode CFG generation to be printed in short form. See the gallery for examples of short vs. long form.

Example usage:

    > stack build --flag derive-cfg:debug-step 

## Running and using

Mandate has a very limited command-line driver, capable of a few tasks. Most functionality is accessed by entering commands in GHCI.

### Driver instructions

Things the driver can do:

* Generate a CFG from a source file in interpreted-mode. (Choice of abstraction is hardcoded.)
* Generate a CFG from a source file using a pre-generated compiled-mode CFG generator
* Run the constant-propagation analysis on a source file

Usage:

    stack exec derive-cfg <command> <name-of-language> <path-to-source-file>
    
Run `stack exec derive-cfg` for details about the available options. Note that `<path-to-source-file>` is special-cased for Imp, as there is no concrete syntax for Imp.
  
Example usage:

    stack exec derive-cfg interpreted-cfg tiger Tiger/testcases/test1.tig

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
    
Example output:

```
Begin Rules:

pexp-cong-2-pexp-done-1:
<(10624v; (441,442)) | [\('1; ('2,'3)) -> (PExp('1); ('2,'3))].436>  ---->  <(10624v; (441,442)) | 436>

pdecs-cong-2-pdecs-done-1:
<(10753v; (450,451)) | [\('1; ('2,'3)) -> (PDecs('1); ('2,'3))].445>  ---->  <(10753v; (450,451)) | 445>
...
# Over 100 more rules for Tiger
```

Some notes on this output: The AM-rule names are generated from the SOS rule names in `Semantics.hs`. A rule called `pexp-cong` may generate rules `pexp-cong-1` and `pexp-cong-2`; if `pexp-cong-2` is fused with the rule `pexp-done-1`, the final rule will be called `pexp-cong-2-pexp-done-1`.

The integers in the output are all named variables (where the names are integers). They may be postfixed for a match type: a "v" postfix (as in the "10624v" variable in the example) means it matches only values; a "t" postfix (for "term") matches only nonvalues. Variables without a postfix match anything; though not discussed in the paper, this is required for terms that do not take part in execution, such as the reduction state. A variable precided by a single quote, e.g.: `'1` is locally bound in a continuation stack frame.

The PAM can be printed similarly:

    > pamRules
   
   
Example output:

```
Begin Rules:

pexp-cong-1:
<(PExp(4t); (0,1)) | 436> down  ---->  <(4t; (0,1)) | [\('1; ('2,'3)) -> (PExp('1); ('2,'3))].436> down

pexp-cong-2:
<(440; (441,442)) | [\('1; ('2,'3)) -> (PExp('1); ('2,'3))].436> up  ---->  <(PExp(440); (441,442)) | 436> up

....
# Many, many, many more
```

We can see that PAM states differ from AM states in that they additionally contain the up or down phase.

### Generating an interpreted-mode CFG

Most of the language scripts print instructions for doing this when loaded.

General command:

    >  abstractAmCfg <function abstraction> <term abstraction> amRules <term>

See the section on abstractions for an explanation of the available abstractions.

For example, to generate a statement-level CFG for the file `MITScript/tests/what-works.mit`:

    > :script ghci-scripts/run-mitscript
    > t <- parse "MITScript/tests/what-works.mit"
    > g <- abstractAmCfg (irrelevance (SortIrr "Exp")) (irrelevance (SortIrr "Exp")) amRules t
    > showGraph g

### Generating the compiled-mode CFG-generators

After running the GHCI script for the language of interest, use the following commands to generate the graph patterns and the CFG generator :

    >  gs <- makeGraphPatterns <function abstraction> <term abstraction> amRules signature
    >  graphPatternsToCode gs
    
For example, to generate an expression-level CFG-generator for Tiger:

    > :script ghci-scripts/run-tiger
    > gs <- makeGraphPatterns (irrelevance ValueIrr) (irrSkippingFunScope ValueIrr) amRules signature
    > graphPatternsToCode gs

For MITScript:

    > :script ghci-scripts/run-mitscript
    >  gs <- makeGraphPatterns (irrelevance ValueIrr) (irrSkippingScope ValueIrr) amRules signature
    >  graphPatternsToCode gs

Note that the variable `amRules` is defined by the GHCI script, while `signature :: Lang l => Signature l` is a member of the `Lang` typeclass. Be warned that `graphPatternsToCode` can take several minutes to run for some cases seen in Tiger, due to inefficient graph algorithms.

Different kinds of CFG generators are enabled by the different abstractions passed to `makeGraphPatterns`. More instructions in the abstractions section below.

### Using the various abstractions

Currently, the only abstractions are various instances of the `Irrelevance` typeclass, defined in `src/Semantics/Abstraction.hs`. They are:

* `irrelevance ValueIrr`: Abstracts all values to ValStar. Generates expression-level CFGs.
* `irrelevance (SortIrr <sort>)`: Abstracts all terms of the given sort to ValStar. For example, for MITScript, `irrelevance (SortIrr "Exp")` abstracts all expresisons to ValStar, generating statement-level CFGs.
* `irrelevance (VarNotIrr <variable name>)`: Abstracts all terms to ValStar except the value of the given variable in the environment. 

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


### Advanced usages

Here we give cursory instructions for some other things a user may wish to do, discussed less in the paper:

Executing a term using the PAM or AM rules: Use the functions `pamEvaluationSequence`, `amEvaluationSequence`, and their variants, defined in `Semantics.AbstractMachine` and `Semantics.Pam`.

To run the paren-balancing analysis on the example from the paper in the intro:

    > :l Languages.Imp.Analyze
    > :set -XOverloadedStrings
    > import Languages.Imp.Imp
    > analyzeParenBalance "b" termBalanceParens

# Viewing generated graphs

To generate graphs, you'll need to install Graphviz, which provides the `dot` command line utility for genrating `.svg`'s from dot files.

Alternatively, you can use a browser-based Graphviz viewer such as http://viz-js.com/ .
