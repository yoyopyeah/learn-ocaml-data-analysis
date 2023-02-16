### Goal
- Grade homwwork on per question basis, and analyze the grade progression per question to explore the relation of program correctness and type-check status.

### Approach
1. Extract the functions for each question from `template.ml` and `solution.ml` in each homework.
   - Uses ocaml `compiler-lib` library to generate AST to facilitate extraction of functions on per question basis.
2. Extract student submission of functions for each question, as well as any dependent functions.
3. Run ocaml grader on per question basis.

## Scripts
`main.ml`
- Process given .ml files into `ast_out` and `pretty_ast_out` in analysis/out/.

`question_split.py`
- Extracts functions by question.


## Usage
This is a dune project. Build the project in the project_name/ directory, in this case, `ocaml/`, and run:
```
eval $(opam env)
dune build
```
Execute the project
```
dune exec ocaml arg1
```
where `arg1` is the hw/file path of the file to process. For example, `dune exec ocaml "hw2/solution.ml"`.

The resulting files generated will be produced under directory `/analysis/out/`.

## Project structure
```
├── hw1
│   ├── description.html
│   ├── meta.json
│   ├── prelude.ml
│   ├── prepare.ml
│   ├── solution.ml
│   ├── template.ml
│   ├── test.ml
│   ├── sol1.ml (solution for q1)
│   ├── sol2.ml (solution for q2)
│   ├── sol3.ml ...
│   ├── student_submissions (only contains q1)
│       ├── 260950000.ml
│       ├── ...
├── hw2
│   ├── question 1
│   │   ├── exercise
│   │   │   ├── ...
│   │   ├── student_submissions (only contains q1)
│   │       ├── 260950000.ml
            ├── ...
```
```
├── hw1
│   ├── question 1
│   │   ├── exercise
│   │   │   ├── description.html
│   │   │   ├── meta.json
│   │   │   ├── prelude.ml
│   │   │   ├── prepare.ml
│   │   │   ├── solution.ml (extracted by question)
│   │   │   ├── template.ml
│   │   │   ├── test.ml
│   │   ├── student_submissions (only contains q1)
│   │       ├── 260950000.ml
│   │       ├── ...
│   ├── question 2
│   │   ├── exercise
│   │   │   ├── ...
│   │   ├── student_submissions (only contains q2)
│   │       ├── 260950000.ml
│   │       ├── ...
│   ├── question 3
│       ├── ...
├── hw2
│   ├── question 1
│   │   ├── exercise
│   │   │   ├── ...
│   │   ├── student_submissions (only contains q1)
│   │       ├── 260950000.ml
            ├── ...
```


### Data analysis
- how many type errors & how many syntax errors
- how long students worked on a particular problem
- grade progression per question
  - once type checked how quickly a student gets 100