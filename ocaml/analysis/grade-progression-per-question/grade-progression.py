from dateutil.parser import parse
from datetime import datetime, time
import matplotlib.dates as mdates
import numpy as np
import matplotlib.pyplot as plt
import argparse
import os
import pandas as pd
from pathlib import Path

parser = argparse.ArgumentParser()

# parser.add_argument('-source_db', required=False, default=db_env_name,
#                     help='Specify the source database. By default, it uses the one specified in .env.')

# parse boolean input


def str2bool(v):
    if isinstance(v, bool):
        return v
    if v.lower() in ('yes', 'true', 't', 'y', '1'):
        return True
    elif v.lower() in ('no', 'false', 'f', 'n', '0'):
        return False
    else:
        raise argparse.ArgumentTypeError('Boolean value expected.')


args = parser.parse_args()


def main():
    date_fmt = '%b_%d_%Y_%H:%M:%S.ml'
    os.system("rm -rf figs")
    os.system("mkdir -p figs")
    cnt = 0
    
    for grade_file in os.listdir("hw1-graded-per-question"):
      df = pd.read_csv(os.path.join("hw1-graded-per-question", grade_file))
      df['timestamp'] = pd.to_datetime(df['timestamp'], format=date_fmt)

      # Sort the data by id and timestamp columns
      df = df.sort_values(by=['id', 'timestamp'])

      # Group the data by id and event_key
      groups = df.groupby(['id', 'event_key'])

      # Create one plot per id
      for id_value in df['id'].unique():
          # Create a DataFrame for the current id
          id_df = df[df['id'] == id_value]

          # Create a figure and axes for the current id
          fig, ax = plt.subplots()

          # Create a scatter plot for each group in the current id DataFrame
          for name, group in id_df.groupby('event_key'):
              ax.scatter(group['timestamp'], group['grade'], label=name, marker='x', s=50)
              ax.plot(group['timestamp'], group['grade'], label=None)

          # Set the plot title to the current id value
          ax.set_title(f'Grade progression for id {id_value}')

          # Set the x and y axis labels
          ax.set_xlabel('Timestamp')
          ax.set_ylabel('Grade')

          # Set the legend and show the plot
          ax.legend()
          plt.savefig(f"""figs/{grade_file.split(".")[0]}-{id_value}.png""")

          # flush old graph
          plt.clf()

      print(f'\nSaved {cnt} graphs for collection {grade_file}.')


if __name__ == "__main__":
    main()
