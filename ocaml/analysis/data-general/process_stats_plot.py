import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

# collect data into lists
compile = [0] * 6
eval = [0] * 6
grade = [0] * 6

file = open("../info/process_stats_log.txt", "r")
lines = file.readlines()
for line in lines:
    if "HW" not in line: continue
    num = line.split(" ")[2]
    hw = line.split("_")[0][-1:]
    # print(f"hw: {hw}, num: {num}")
    if "compile" in line:
        compile[int(hw)-1] = int(num)
    elif "eval" in line:
        eval[int(hw)-1] = int(num)
    elif "grade" in line:
        grade[int(hw)-1] = int(num)

hw_str = ["HW1", "HW2", "HW3", "HW4", "HW5", "HW6"]

# plot
    # np.c_[compile, eval, grade], index=hw_str

df = pd.DataFrame({'compile': compile, 'eval': eval,'grade': grade}, index = hw_str)
ax = df.plot.bar(rot=0, title="Number of submissions per homework", width=0.8, figsize=(10,5))

for container in ax.containers:
    ax.bar_label(container)

plt.savefig(f"submission_per_hw_numbered.png")
plt.close()
