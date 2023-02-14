### Goal
- Grade homwwork on per question basis, and analyze the grade progression per question to explore the relation of program correctness and type-check status.

### Approach
1. Extract the functions for each question from `template.ml` and `solution.ml` in each homework.
   - Uses ocaml `compiler-lib` library to generate AST to facilitate extraction of functions on per question basis.
2. Extract student submission of functions for each question, as well as any dependent functions.
3. Run ocaml grader on per question basis.

### Scripts
`main.ml`
- Splits given .ml files in "`process_files/`" by question.


-- hw1
  |-- question 1
    |-- template.ml
    |-- solution.ml
    |-- student submissions
      |-- 260953238.ml