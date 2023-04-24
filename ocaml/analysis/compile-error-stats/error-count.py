# id, timestamp, error_type, event_key
import os
import csv
import json
import subprocess

def main():
    os.system("mkdir -p gradeHW1_compile_error")
    for q_dir in os.listdir("gradeHW1_question_based"):
        csvf = open(f"HW1-{q_dir}.csv", "w}")
        writer = csv.writer(csvf)
        writer.writerow(["id", "timestamp", "error_type", "event_key"])

        if "err.json" in q_dir: 
          with open(os.path.join("gradeHW1_question_based", "err.json"), "r") as f:
            err_dict = json.load(f)
            for id in err_dict:
               for err in err_dict[id]:
                  for time in err_dict[id][err]:
                    if "syntax" in err: err = "Syntax error"
                    elif "type" in err: err = "Type error"
                    else: err = "Other error"
                    writer.writerow([id, time, err, "grade"])

        if "q" not in q_dir: continue

        for id_dir in os.listdir(os.path.join("gradeHW1_question_based", q_dir), "student_submissions"):
            for ml_file in os.listdir(os.path.join("gradeHW1_question_based", q_dir, "student_submissions", id_dir)):
              compile_cmd = f"ocamlc -c gradeHW1_question_based/{q_dir}/student_submissions/{id_dir}/{ml_file}"                
              process = subprocess.run(compile_cmd, shell=True, timeout=30, text=True, stdout=subprocess.PIPE,stderr=subprocess.PIPE)
              if (process.stderr):
                 #TODO: need to append prelude to the submission
                        


if __name__ == "__main__":
    main()