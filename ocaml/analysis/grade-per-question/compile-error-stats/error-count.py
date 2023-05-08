import pandas as pd

import matplotlib.pyplot as plt
import numpy as np

def autopct_format(values):
  def my_format(pct):
      total = sum(values)
      val = int(round(pct*total/100.0))
      return '{:.1f}% ({v:d})'.format(pct, v=val)
  return my_format

def plot_error_type_percentage(hw, df):
  y = df.iloc[:, 1]
  # mylabels = ["Arguments Application", "Exception error", "If Else Error", "Not a Function", "PM not exhaustive", "Pattern Matching Error", "Syntax error", "Type error", "Unbound value", "Unused variable"]
  mylabels = df.iloc[:,0]
  myexplode = [0,0,0,0,0,0,0,0.2, 0, 0]
  plt.pie(y, labels = mylabels, explode = myexplode, autopct=autopct_format(y))
  plt.title(f"Percentage of type of errors in HW{hw}")
  plt.savefig(f"error_percent_hw{hw}.png")
  plt.close()

def main():
  hw_num = 1
  while hw_num <= 6:
    print(f"\n=== hw{hw_num} ===")
    filename = f"out-hw{hw_num}.csv"
    df = pd.read_csv(filename)
    df2 = df.groupby(['error_category'])['error_category'].count().reset_index(name="count")
    plot_error_type_percentage(hw_num, df2)
    print(df2)
    hw_num+=1



if __name__ == "__main__":
    main()
