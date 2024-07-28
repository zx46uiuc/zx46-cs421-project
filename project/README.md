# ZX46-CS421 Project

## How to Run

The implementation can be found in `main.hs`. To run the code, use the following command:

```bash
# run this in terminal
stack ghci ./main.hs
# run this in stack ghci. this will trigger the metric calculation.
main
```

## Explanation of Files
- example_haskell_code_to_be_test_simple.hs: A simple Haskell code example that will be analyzed. This analysis includes converting the code into an Abstract Syntax Tree (AST) and calculating metrics.
- example_haskell_code_to_be_test_complex.hs: A more complex Haskell code example for analysis.
- main.hs: The main file where the metrics calculation and analysis are implemented.

## details.
Feel free to change `demoAST = demoASTSimple` to `demoAST = demoASTComplex` for switching between examples.