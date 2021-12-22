# master-minds
```
stack build
stack exec -- master-minds-exe
```

Change "playMastermindChunkStrategy 32" to "playMastermindChunkStrategy 64", "playMastermind", "playMastermindParMap", etc. to test different parallelization strategies.

Note that the solution code should be input with whitespace separation.

Sample Test Cases:
- 4 colors, 4 holes, code "1 2 3 4"
- 4 colors, 4 holes, code "3 3 3 3"
- 10 colors, 4 holes, code "8 7 6 5"