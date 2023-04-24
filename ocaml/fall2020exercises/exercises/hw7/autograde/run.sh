#!/usr/bin/env bash

SCRIPT_DIR=$(dirname ${BASH_SOURCE[0]})
GRADER_DIR="${SCRIPT_DIR}/proj"
PRELUDE_PATH="${SCRIPT_DIR}/../prelude.ml"
PREPARE_PATH="${SCRIPT_DIR}/../prepare.ml"
STUDENT_SOLUTIONS_DIR=${1:?"Solution dir should be given"}

STUDENT_SOLUTIONS_DONE="${SCRIPT_DIR}/done"
STUDENT_SOLUTIONS_FAIL="${SCRIPT_DIR}/fail"
STUDENT_SOLUTIONS_TIMEOUT="${SCRIPT_DIR}/timeout"

mkdir -p ${STUDENT_SOLUTIONS_DONE}
mkdir -p ${STUDENT_SOLUTIONS_FAIL}

touch "${STUDENT_SOLUTIONS_DONE}/grade.txt"
for student in "${STUDENT_SOLUTIONS_DIR}"/*; do
    cat "${PRELUDE_PATH}" > "${GRADER_DIR}/work.ml"
    cat "${PREPARE_PATH}" >> "${GRADER_DIR}/work.ml"
    cat "${student}"/*.ml >> "${GRADER_DIR}/work.ml"
    student_name=`basename "$student"`

    (
        cd "${GRADER_DIR}"
        echo -n "${student_name}," > "grade.txt"
        dune build 2>&1 > /dev/null
        if [[ $? -ne 0 ]]; then
            exit "111"
        else
            timeout 30s dune exec ./grade.exe > "report.txt"
            exitcode=$?
            tail -n 1 report.txt >> grade.txt
            ocaml -w "-a@8@9@10@11@12" "work.ml" 2>&1 > /dev/null
            if [[ $? -ne 0 ]]; then
                sed -i 's/$/,true/' grade.txt
            else
                sed -i 's/$/,false/' grade.txt
            fi
            exit "${exitcode}"
        fi
    )

    exitcode=$?

    if [[ "${exitcode}" -eq 0 ]]; then
        mkdir -p "${STUDENT_SOLUTIONS_DONE}/${student_name}"
        cp "${student}"/*.ml "${STUDENT_SOLUTIONS_DONE}/${student_name}/code.ml"
        cp "${GRADER_DIR}/report.txt" "${STUDENT_SOLUTIONS_DONE}/${student_name}/report.txt"
        cat "${GRADER_DIR}/grade.txt" >> "${STUDENT_SOLUTIONS_DONE}/grade.txt"
    elif [[ "${exitcode}" -eq 111 ]]; then
        mkdir -p "${STUDENT_SOLUTIONS_FAIL}/${student_name}"
        cp "${student}"/*.ml "${STUDENT_SOLUTIONS_FAIL}/${student_name}/code.ml"
    else
        mkdir -p "${STUDENT_SOLUTIONS_TIMEOUT}/${student_name}"
        cp "${student}"/*.ml "${STUDENT_SOLUTIONS_TIMEOUT}/${student_name}/code.ml"
    fi

    # rm "${GRADER_DIR}/work.ml"
    rm -f "${GRADER_DIR}/report.txt"
    rm -f "${GRADER_DIR}/grade.txt"
done
