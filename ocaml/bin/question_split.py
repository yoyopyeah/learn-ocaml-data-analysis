# regex to extract functions: (?s)(?<=\(\*\*\).)(.*?)(?=\(\*\*\))
# regex to extract function name: (?<=(^let\s)|(^let\srec\s))([A-Za-z_]+)
# (?<=(^let\srec\s)|(^let\s(?!rec)))([A-Za-z_]+)

import json
import re
import sys
import pymongo
import os
import subprocess

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
# Create directories, split exercises
# See structure in README.md
def setup_output_structure():
  if os.path.exists(f"analysis/out"):
    os.system("rm -rf analysis/out")
  os.system("mkdir -p analysis/out")
  for hw in fq:
    os.system("mkdir -p analysis/out/" + hw)
    os.system("mkdir -p analysis/out/" + hw + "/exercise")
    f = open(f"analysis/out/{hw}/err.json", "a")  
    f.write("{}")
    f.close()
    for q in fq[hw]:
      os.system("mkdir -p analysis/out/" + hw + "/" + q)
      os.system("mkdir -p analysis/out/" + hw + "/" + q + "/student_submissions")
    
    break #TODO: only do hw1 for now


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
  # set ups
  os.system("dune clean && dune build")
  setup_output_structure()
  db = get_db()
  collections = db.list_collection_names()

  # pickedId = "d54baff9c3e9e5467505601b4b370289" # TODO: pick one id to run everything

  for hw in fq:
    # hw = "hw1" # TODO: only do hw1 for now
    # copy exercise files
    os.system(f"cp -a fall2021exercises/{hw}/. analysis/out/{hw}/exercise")
    hw_all_submission_count = 0

    print("\n=== starting process of "  + hw + " ===")
    for collection in collections:
      hw_collection_submission_count = 0
      if hw.upper() not in collection:
        # print("-- >> skip collection " + collection)
        continue
      print("\n>>> processing collection " + collection)
      records = db[collection].find({}, batch_size=15)
      for record in records:
        if not record['consent']: continue
        # if pickedId != record['studentId']: continue #TODO: pick one id to run everything

        hw_all_submission_count += 1
        hw_collection_submission_count += 1

        studentId = record['studentId']
        
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
    # break

  # Clean up
  os.system("rm analysis/submission_temp")
  os.system("rm analysis/pretty_ast_out")
  os.system("rm analysis/ast_out")


if __name__ == "__main__":
    main()