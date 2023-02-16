# regex to extract functions: (?s)(?<=\(\*\*\).)(.*?)(?=\(\*\*\))
# regex to extract function name: (?<=(^let\s)|(^let\srec\s))([A-Za-z_]+)
# (?<=(^let\srec\s)|(^let\s(?!rec)))([A-Za-z_]+)

import json
import re
import sys

f = open("analysis/pretty_ast_out", "r")
pretty_ast = f.read()

fun_list = re.findall("(?s)(?<=\(\*\*\).)(.*?)(?=\(\*\*\))", pretty_ast)
fq = json.load(open("analysis/fq.json", "r"))

def extract_function(fun_list, hw, verbose=False):
    for q in fq[hw]:
      out_f = open("analysis/out/" + hw + "_" + q + ".ml", "w")
      for fun in fun_list:
          # fun_name = re.search("(?<=(^let\srec\s)|(^let\s(?!rec)))([A-Za-z_]+)", fun, re.MULTILINE).group(0)
          split = fun.split(" ")
          fun_name = split[1]
          if split[1] == "rec": fun_name = split[2]
          if verbose:
            print("=====" + fun_name + "=====")
            print(fun)
          if fun_name in fq[hw][q]:
            out_f.write(fun)
      out_f.close()
      
    
extract_function(fun_list, sys.argv[1], verbose=False)

f.close();