# regex to extract functions: (?s)(?<=\(\*\*\).)(.*?)(?=\(\*\*\))
# regex to extract function name: (?<=(^let\s)|(^let\srec\s))([A-Za-z_]+)
# (?<=(^let\srec\s)|(^let\s(?!rec)))([A-Za-z_]+)

import json
import re
import os
import subprocess
import shutil

# global variables
fq = json.load(open("analysis/info/fq.json", "r"))

########## extract question ##########
def extract_function_name(fun, verbose=False):
  # fun_name = re.search("(?<=(^let\srec\s)|(^let\s(?!rec)))([A-Za-z_]+)", fun, re.MULTILINE).group(0)
  split = fun.split(" ")
  fun_name = split[1]
  if split[1] == "rec": fun_name = split[2]
  if verbose:
    print("=====" + fun_name + "=====")
    print(fun)
  return fun_name.rstrip()

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
      json.dump(data, jsonFile, indent=2)
    return False

  f = open("analysis/pretty_ast_out", "r")
  pretty_ast = f.read()
  fun_list = re.findall("(?s)(?<=\(\*\*\).)(.*?)(?=\(\*\*\))", pretty_ast)

  out_name = '_'.join(timestamp.split(" ")[1:5])

  if os.path.exists(f"analysis/submission_temp_dep.json"):
    with open(f"analysis/submission_temp_dep.json", "r") as jsonFile:
      dep = json.load(jsonFile)
  else: dep = {}

  for q in fq[hw]: 
    #add the dependent function names
    question_fnames = fq[hw][q].copy()
    for fname in question_fnames:
      if fname not in dep: continue # has to check cuz 2021 and 2020 has different questions
      for dep_fname in dep[fname]:
        if dep_fname not in question_fnames:
          question_fnames.append(dep_fname)
    # print(f"** {fq[hw][q]} + {dep.get(fname)} = {question_fnames}")

    out_f = open(f"analysis/out/{hw}/{q}/student_submissions/{studentId}/{out_name}.ml", "w")

    for fun in fun_list:
      fun_name = extract_function_name(fun)
      if fun_name in question_fnames:
        out_f.write(fun)
    out_f.close()  
  
  f.close()
  if os.path.exists(f"analysis/submission_temp_dep.json"):
    os.remove('analysis/submission_temp_dep.json')
  return True


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
  # set ups
  os.system("dune clean && dune build")
  setup_output_structure()
  db = get_db()
  collections = db.list_collection_names()

  for collection in collections:
    count = 0
    if "HW5" not in collection: continue #TODO: only process HW5 for now
    # if "grade" not in collection: continue #TODO: only process gradeHW5
    print("\n>>> processing collection " + collection)
    clean_collection_name = collection.split("_")[0]

    hw = clean_collection_name[-3:].lower()
    collection_submission_count = 0
    make_hw_outdir(hw)

    records = db[collection].find({}, batch_size=15)
    for record in records:
      # if count == 15: break #TODO: limit num of records to process
      if not record['consent']: continue

      collection_submission_count += 1
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
      
      if split_submission_by_question(hw, "analysis/submission_temp", studentId, record['readableTimestamp']): count += 1
      print(".", end="", flush=True)
    
    print(f">>> processed {collection_submission_count} submissions in {collection}\n")
    output_filename = clean_collection_name + "_question_based"
    shutil.make_archive(output_filename, 'zip', f"analysis/out/{hw}")
    os.system(f"mv {output_filename}.zip analysis/zips")
    os.system("rm -r analysis/out/" + hw)

  # Clean up
  os.system("rm -rf analysis/out")
  os.system("rm analysis/submission_temp")
  os.system("rm analysis/pretty_ast_out")
  os.system("rm analysis/ast_out")
  os.system("dune clean")


if __name__ == "__main__":
    main()