## Goal
- Grade homework on per question basis, and analyze the grade progression per question to explore the relation of program correctness and type-check status.

## Approach
1. For each homework, extract the functions on per question basis for each student submission.
   - Uses ocaml `compiler-lib` library to generate AST to facilitate extraction of functions on per question basis.
   - Extraction includes dependent functions.
   - Python script `bin/question_split.py` facilitates connection to ssh remote database, structure outputs, and navigate data processing traffics between Python script and Ocaml script (`bin/main.ml`)
      - See output project structure in [section](#project-structure) below.
2. Run ocaml grader on per question basis. The command follows the pattern `learn-ocaml grade --exercises="./exercises/hw1/question1/exercise" --grade-student="./exercises/hw1/question1/student_submissions/stu.ml" --timeout=60 --dump-reports grade_report`. The exercises and student submissions can be stored anywhere on the file system provided the correct path is passed to the `--exercises` and `--grade-student` flags.

## Usage
Python script calls for the execution of the dune project. Before running, set up the environment:
```
eval $(opam env)
```
To execute the project, in dune project `ocaml/`, run:
```
python3 bin/question_split.py
```
The resulting files generated will be produced under directory `/analysis/out/`. See project structure below.


## Project structure
`bin/`

Where the main scripts are located.

- `question_split.py`
  - Extracts functions by question.
  - Setup the structure of and organize outputs under `analysis/output/`.

- `main.ml`
  - Process given Ocaml files into `ast_out` and `pretty_ast_out` in analysis/out/.

`analysis/`

Where analysis related data and output are located.
- `info/` (_*Do not edit_)
   - `consentID2021fall.csv`: Student IDs of those that approved the usage of their homework data.
   - `fq.json`: List of function names for related to each question in each homework.
- `out/`

   Where the generated output files are located.

   See full structure below:
```
├── hw1/
│   ├── stats/
│   │   ├── 260950000.json
│   │   ├── ... 
│   ├── exercise/
│   │   ├── description.html
│   │   ├── meta.json
│   │   ├── prelude.ml
│   │   ├── prepare.ml
│   │   ├── solution.ml
│   │   ├── template.ml
│   │   ├── test.ml
│   ├── q1/
│   │   ├── student_submissions/ (only contains q1)
│   │       ├── 260950000/
│   │          ├── Sep_08_2021_03:12:55.ml
│   │       ├── 260950001/
│   │       ├── ...
│   ├── q2/
│   │   ├── student_submissions/ (only contains q2)
│   │       ├── 260950000.ml
│   │       ├── ...
│   ├── q3/
│       ├── ...
├── hw2/
│   ├── q1
│   │   ├── exercise
│   │   │   ├── ...
│   │   ├── student_submissions/ (only contains q1)
│   │       ├── 260950000.ml
            ├── ...
```

`stats/`:
- contains json for each student of their submission statistics, including syntax error counts (AST can't be generated)


## Data analysis
- how many type errors & how many syntax errors
- how long students worked on a particular problem
   - Number of submission
   - time
- grade progression per question
  - once type checked how quickly a student gets 100
