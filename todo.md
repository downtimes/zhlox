- Write test suite that runs all the files in scripts folder and compares them
with known good output. It's hard at the moment to know if we accidentally broke
something.
- Implement the optional anonymous function feature
- make function definitions and class definitions global interpreter objects and the
  interpreter values of them just point to these global definitions.
    - to get this to work think of a better way to 'clone' part of an AST.
    - essentially get rid of most of the memory management by thinking of a better program structure.