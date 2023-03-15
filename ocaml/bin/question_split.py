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
  # try:
  #     result = subprocess.run([f"dune exec ocaml \"{submission}\""], stdout=subprocess.PIPE)
  #     print(result.stdout)
  # except subprocess.CalledProcessError as e:
  #     print(e.output)
  
  exitcode = os.system(f"dune exec ocaml \"{submission}\"")
  # TODO: assuming any exitcode != 0 is syntax error
  if exitcode != 0:
    with open(f"analysis/out/{hw}/stats/{studentId}.json", "r") as jsonFile:
      data = json.load(jsonFile)
      data["syntax_err_count"] += 1
    with open(f"analysis/out/{hw}/stats/{studentId}.json", "w") as jsonFile:
      json.dump(data, jsonFile)
    return

  f = open("analysis/pretty_ast_out", "r")
  pretty_ast = f.read()
  fun_list = re.findall("(?s)(?<=\(\*\*\).)(.*?)(?=\(\*\*\))", pretty_ast)

  out_name = '_'.join(timestamp.split(" ")[1:5])

  for q in fq[hw]:
    if not os.path.exists(f"analysis/out/{hw}/{q}/student_submissions/{studentId}"):
      os.makedirs(f"analysis/out/{hw}/{q}/student_submissions/{studentId}")
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
  os.system("mkdir -p analysis/out")
  for hw in fq:
    os.system("mkdir -p analysis/out/" + hw)
    # wipe stats/ and recreate
    os.system("rm -rf analysis/out/" + hw + "/stats")
    os.system("mkdir -p analysis/out/" + hw + "/stats")
    os.system("mkdir -p analysis/out/" + hw + "/exercise")
    
    for q in fq[hw]:
      os.system("mkdir -p analysis/out/" + hw + "/" + q)
      os.system("mkdir -p analysis/out/" + hw + "/" + q + "/student_submissions")


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
  return session.connection["2021fallcode"]


########## main ##########
def main():
  # set ups
  os.system("dune clean && dune build")
  setup_output_structure()
  db = get_db()
  collections = db.list_collection_names()

  for hw in fq:
    # copy exercise files
    os.system(f"cp -a fall2021exercises/{hw}/. analysis/out/{hw}/exercise")
    hw_submission_count = 0

    print("\n=== starting process of "  + hw + " ===\n")
    for collection in collections:
      if hw not in collection:
        # print("-- >> skip collection " + collection)
        continue
      print("-- >> processing collection " + collection)
      records = db[collection].find({}, batch_size=15)
      for record in records:
        if hw_submission_count >= 5: break
        studentId = record['studentId']
        if studentId not in consent_students: continue
        hw_submission_count += 1
        # print(studentId + " " + record['timestamp'])
        if not os.path.isfile(f"analysis/out/{hw}/stats/{studentId}.json"):
          with open(f"analysis/out/{hw}/stats/{studentId}.json", "w") as jsonFile:
            print(f"created stats/{studentId}.json")
            data = {"syntax_err_count": 0}
            json.dump(data, jsonFile)
        with open(f"analysis/submission_temp", "w") as f:
          f.write(record["solution"])
        split_submission_by_question(hw, f"analysis/submission_temp", studentId, record['timestamp'])


if __name__ == "__main__":
    main()