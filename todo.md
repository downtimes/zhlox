- Write test suite that runs all the files in scripts folder and compares them
with known good output. It's hard at the moment to know if we accidentally broke
something.
- Implement the optional anonymous function feature
- Improve the ref counted nature of environments and values.
    - https://www.reddit.com/r/rust/comments/1j1rvuh/should_i_use_rcrefcellt/
    - having to be careful on final cleanup when environments clean up values
      which try to clean up environments (functions) is strange. 