### Goal
- Grade homwwork on per question basis, and analyze the grade progression per question to explore the relation of program correctness and type-check status.

### Approach
1. Extract the functions for each question from `template.ml` in each homework.
2. Generate the AST of student submissions to extract student submission of functions for each question, as well as any dependent functions.
3. Run ocaml grader on per question basis.

### Scripts
`main.ml`
- Uses ocaml `compiler-lib` library to generate AST of exercises and student submissions.