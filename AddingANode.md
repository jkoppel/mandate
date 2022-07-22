In this exercise, we will add a new node for exponentiation to `app/languages/AddMul.hs`, the simplest example language.

This will give you experience with the entire pipeline of creating a language, from the initial node construction to abstraction.

## Adding the Exponentiation Node

1. Modify `addMulLangSig` to add a new node for exponentiation. Follow the example of the others.
   
   NodeSig is define in `Term.hs` as

```haskell
data SigNode = NodeSig !Symbol [Sort] Sort
             | <other cases>
```

2. Following the examples, add a new pattern synonym so that you can easily construct or pattern match a new Exponentiation node

3. In GHCI, check that you can create a new node with your pattern

```
    > :s ghci-scripts/run-muladd
    > Exp (EVal $ Const 2) (Plus (EVal $ Const 1) (EVal $ Const 1))
```

## Define the semantics

1. Add the two congruence rules to `addMulLangRules`.  These should be equivalent to the following two rules in traditional
   paper notation, but encoded into the Mandate datatype. Look at the SOS section of the Mandate paper
   for an explanation of this format, and in `SOS.hs` for the definition of the datatype defining the semantics DSL.
   Alternatively, it should be easy to follow the examples of the other congruence rules in this file.

```
     e1 ~> e1'
-------------------- exp-cong-1
   e1^e2 ~> e1'^e2

   
      e2 ~> e2'
-------------------- exp-cong-2
    v^e2 ~> v^e2'
    
```

2. Test your congruence rules in GHCI


```
    > :s ghci-scripts/run-muladd
    > myTerm = Exp (Plus (EVal $ Const 1) (EVal $ Const 1)) (Plus (EVal $ Const 1) (EVal $ Const 1))
    > evaluationSequenceL (initConf myTerm) >>= mapM_ (putStrLn.show)
```

   You should see it run, and then get stuck at the time to evaluate the exponentiation

3.  Following the other examples, add a case for `RunExp` to the `CompFunc AddMulLang` data type.
    Add the name of this function to `compFuncName`.

4.  Following the other examples, add a case to `runExternalComputation` that actually computes the exponentiation.

5.  Edit `addMulLangRules` again to add an encoding of the following rule into the semantics DSL:

```
      n = n1^n2
-------------------- exp-eval
      n1^n2 ~> n
```

6. Repeat step 2 of this section. It should now run exponentiation to completion.

## Define the abstraction

The default abstraction will already run on the AST with the new node,
but the external semantic function we added in the previous section needs to be abstracted manually.

1. Add an `AbsRunExp` case to `CompFunc AddMulLang`. Add cases for `RunExp` and `AbsRunExp` to the `Irrelevance` instance in this file.

2. Test that you can generate CFGs and a CFG-generator for a language with exponentiation nodes.

```
    > :s ghci-scripts/run-muladd
    > myTerm = Exp (Plus (EVal $ Const 1) (EVal $ Const 1)) (Plus (EVal $ Const 1) (EVal $ Const 1))
    > abstractAmCfg (irrelevance ValueIrr) (irrelevance ValueIrr) amRules myTerm
    > gs <- makeGraphPatterns (irrelevance ValueIrr) (irrelevance ValueIrr) amRules addMulLangSig
    > graphPatternsToCode gs
```
