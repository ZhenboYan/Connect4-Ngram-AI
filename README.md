# Connect 4 Ngram AI 
Opam dependencies
- Need to download dream. This can be done through an opam command "opam install dream.1.0.0~alpha2"

Build and Test
- Standard "dune build" to build the files
- To execute the dream website run the command "dune exec --root . ./src/main.exe" in the top-level directory
- If a dune-project file appears in the src folder, please delete before running tests as that will mess up the project dependencies
- For testing, "dune test" runs the test suites (not including I/O functions)
- For test coverage, first run "dune test" then "bisect-ppx-report html". To view the coverage report "open _coverage/index.html".

Tranning AI
- training.ml file contains the functions needed to train N-gram AI
- After setting the parameters instructed in training.ml file, "dune exec ./src/training.exe" generates the AI and stored in the file given
