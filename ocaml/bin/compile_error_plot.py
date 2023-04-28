import matplotlib.pyplot as plt
import numpy as np

def autopct_format(values):
  def my_format(pct):
      total = sum(values)
      val = int(round(pct*total/100.0))
      return '{:.1f}% ({v:d})'.format(pct, v=val)
  return my_format

def plot_hw5_error_type_percentage():
  y = np.array([469, 54, 231, 336, 1038, 308, 3946, 8290, 1578, 4975])
  mylabels = ["Arguments Application", "Exception error", "If Else Error", "Not a Function", "PM not exhaustive", "Pattern Matching Error", "Syntax error", "Type error", "Unbound value", "Unused variable"]
  myexplode = [0,0,0,0,0,0,0,0.2, 0, 0]
  plt.pie(y, labels = mylabels, explode = myexplode, autopct=autopct_format(y))
  plt.title("Percentage of type of errors in HW5")
  plt.show()



def main():
  plot_hw5_error_type_percentage()


if __name__ == "__main__":
  main()