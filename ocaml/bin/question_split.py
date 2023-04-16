# regex to extract functions: (?s)(?<=\(\*\*\).)(.*?)(?=\(\*\*\))
# regex to extract function name: (?<=(^let\s)|(^let\srec\s))([A-Za-z_]+)
# (?<=(^let\srec\s)|(^let\s(?!rec)))([A-Za-z_]+)

import json
import re
import sys
import pymongo
import os
import subprocess
import shutil

# global variables
fq = json.load(open("analysis/info/fq.json", "r"))
with open('analysis/info/consentID2021fall.txt', 'r') as file:
  consent_students = [line.rstrip() for line in file]

########## extract question ##########
def extract_function_name(fun, verbose=False):
  # fun_name = re.search("(?<=(^let\srec\s)|(^let\s(?!rec)))([A-Za-z_]+)", fun, re.MULTILINE).group(0)
  split = fun.split(" ")
  fun_name = split[1]
  if split[1] == "rec": fun_name = split[2]
  if verbose:
    print("=====" + fun_name + "=====")
    print(fun)
  return fun_name

def split_submission_by_question(hw, submission, studentId, timestamp):
  dune_cmd = f"dune exec ocaml \"{submission}\""                
  process = subprocess.run(dune_cmd, shell=True, timeout=30, text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
  if (process.stderr):
    f = open(f"analysis/out/{hw}/err.json", "r")
    data = json.load(f)
    if "Lexer.Error" in process.stderr:
      data[studentId]["lexer_errors"].append(timestamp)
    elif "Syntaxerr" in process.stderr:
      data[studentId]["syntax_errors"].append(timestamp)
    else :
      data[studentId]["other_err"].append(process.stderr)
    f.close()
    with open(f"analysis/out/{hw}/err.json", "w") as jsonFile:
      json.dump(data, jsonFile)
    return

  f = open("analysis/pretty_ast_out", "r")
  pretty_ast = f.read()
  fun_list = re.findall("(?s)(?<=\(\*\*\).)(.*?)(?=\(\*\*\))", pretty_ast)

  out_name = '_'.join(timestamp.split(" ")[1:5])

  # make a copy of fq.json, and add the dependent function names into json
  # then read that udpated json
  # and do the for loop with q in updated_fq[hw]
  
  for q in fq[hw]:
    out_f = open(f"analysis/out/{hw}/{q}/student_submissions/{studentId}/{out_name}.ml", "w")
    for fun in fun_list:
      fun_name = extract_function_name(fun)
      if fun_name in fq[hw][q]:
        out_f.write(fun)
    out_f.close()  
  
  f.close()


########## set up output structure ##########
# See structure in README.md
def setup_output_structure():
  # remove old outputs directories
  if os.path.exists(f"analysis/out"):
    os.system("rm -rf analysis/out")
  if os.path.exists("analysis/zips"):
    os.system("rm -rf analysis/zips")

  # make new outputs directories
  os.system("mkdir -p analysis/out")
  os.system("mkdir -p analysis/zips")


########## create output structure for given hw ##########
def make_hw_outdir(hw):
  os.system("mkdir -p analysis/out/" + hw)
  os.system("mkdir -p analysis/out/" + hw + "/exercise")
  f = open(f"analysis/out/{hw}/err.json", "a")  
  f.write("{}")
  f.close()
  for q in fq[hw]:
    os.system("mkdir -p analysis/out/" + hw + "/" + q)
    os.system("mkdir -p analysis/out/" + hw + "/" + q + "/student_submissions")
  os.system(f"cp -a fall2021exercises/{hw}/. analysis/out/{hw}/exercise")


########## bulk data processing ##########
from ssh_pymongo import MongoSession

# connect to mongo DB
def get_db():
  session = MongoSession(
    host='winter2021-comp302-backup.cs.mcgill.ca',
    port=22,
    user='ocaml',
    key='/Users/yoyooolo/.ssh/id_rsa',
    to_port=27017,
    to_host='127.0.0.1'
  )
  return session.connection["sanitized-anon"]


########## main ##########
def main():
  # TODO: only do up to 5 ids
  ids = set()

  # set ups
  os.system("dune clean && dune build")
  setup_output_structure()
  db = get_db()
  collections = db.list_collection_names()

  for collection in collections:
    if "HW1" not in collection: continue #TODO: only process HW1
    print("\n>>> processing collection " + collection)

    hw = collection.split("_")[0][-3:].lower()
    hw_collection_submission_count = 0
    make_hw_outdir(hw)

    records = db[collection].find({}, batch_size=15)
    for record in records:
      if not record['consent']: continue

      hw_collection_submission_count += 1

      studentId = record['studentId']
      # if len(ids) < 5: #TODO: only do up to 5 ids
      #   ids.add(studentId)
      # else:
      #   if (studentId not in ids):
      #     continue
      
      # create studentId entry in err.json file
      with open(f"analysis/out/{hw}/err.json", "r") as f:
        data = json.load(f)
        if (studentId not in data):
          newStudent = {studentId: {
            "syntax_errors": [],
            "lexer_errors": [],
            "other_err": []
          }}
          data.update(newStudent)
      with open(f"analysis/out/{hw}/err.json", "w") as f:
        json.dump(data, f, indent=2)

      with open(f"analysis/submission_temp", "w") as f:
        f.write(record["solution"])
      for q in fq[hw]:
        if not os.path.exists(f"analysis/out/{hw}/{q}/student_submissions/{studentId}"):
          os.makedirs(f"analysis/out/{hw}/{q}/student_submissions/{studentId}")
      
      split_submission_by_question(hw, f"analysis/submission_temp", studentId, record['readableTimestamp'])
    
    print(f">>> processed {hw_collection_submission_count} submissions in {collection} for {hw}\n")
    output_filename = collection.split("_")[0] + "_question_based"
    shutil.make_archive(output_filename, 'zip', f"analysis/out/{hw}")
    os.system(f"mv {output_filename}.zip analysis/zips")

    os.system("rm -r analysis/out/" + hw)

  # Clean up
  # os.system("rm -rf analysis/out")
  os.system("rm analysis/submission_temp")
  os.system("rm analysis/pretty_ast_out")
  os.system("rm analysis/ast_out")
  os.system("dune clean")


if __name__ == "__main__":
    main()