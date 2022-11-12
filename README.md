# GCL Verification Tool
This program can be used to verify gcl file's. It will output the total paths, the evaluated paths, the amount of atoms, the computation time. When invalidated the program also returns the counter example, the failed path and the failed wlp.

## Running the program
Make sure Z3 is installed on the path ``D:/z3-binding/z3-4.8.14-x64-win``. Otherwise the program wont build.
- ``cabal install`` (To install dependencies)
- ``cabal v2-run programverification`` (Builds and runs the program)
## Inputs
The program requires multiple inputs.
- **File Name:**, this is the name of the file to be ran. It instantly takes a file from the ``gclparser/examples/`` path. So its only necessary to type ``E`` to run the file ``gclparser/examples/E.gcl``. When running benchmarks you can use `benchmark/pullUp` as example to run. When the path contains benchmark it will automatically run the program for all ***N in range [2..10]***.
- **Maximum depth:**, tells the verification tool how deep it should go within the program to find paths.
- **Heuristics? (y/n):**, tells the verification tool if it should use branch pruning and simplifying to improve performance of verifying.
- **Mutate? (y/n):**, tells the verification tool if it should run the program for all  ***N in range [2..10]*** for all possible mutations. ``WARNING: Might take very long and is only possible for benchmark files``.

## Output
- **Normal Files**, the data mentioned earlier will be printed within the console.
- **Benchmark files** the data mentioned earlier will be printed in the console and will generate a csv file for all values ***N in range [2..10]***.
- **Mutations**, Creates an csv file with the amount of mutations that proved invalid. And prints all runs within the console. ``WARNING: output only counts failed attempts in the mutations, not what went wrong and no the total amount of mutations``.