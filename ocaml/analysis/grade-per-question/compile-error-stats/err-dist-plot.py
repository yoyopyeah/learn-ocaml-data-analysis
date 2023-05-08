# importing package
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import os
import json
from datetime import timedelta
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

# errors per 10min?

date_formats = ['%b_%d_%Y_%H:%M:%S.ml', '%a %b %d %Y %H:%M:%S GMT%z']

def main():
  # merge_errors("hw5")
  plot_by_submission_count("hw1")

  return


def plot_by_submission_count(hw):
  os.system(f"mkdir -p plots-all-{hw}")

  # Step 1: Load the CSV file into a pandas DataFrame
  df = pd.read_csv(f"{hw}_err_combined.csv")
  selected_df = df[df['error_category'] == 'Syntax error']
  # df['timestamp'] = pd.to_datetime(df['timestamp'], format=date_fmt)
  date_fmt = '%b_%d_%Y_%H:%M:%S.ml'
  # df = pd.read_csv(f"{hw}_err_combined.csv", parse_dates=['timestamp'], format=date_fmt)

  try :
    selected_df['timestamp'] = pd.to_datetime(selected_df['timestamp'], format=date_fmt)
  except ValueError:
    selected_df['timestamp'] = pd.to_datetime(selected_df['timestamp'], format=date_formats[1])

  # Step 2: Sort the DataFrame by student and timestamp
  df.set_index('timestamp', inplace=True)


  # Loop through each unique value in the 'id' column
  for student_id in selected_df['id'].unique():
      if student_id != "2e8b67ff6ec42ca2a35065901de07638": continue
      # Filter the dataframe to only include errors with the current 'id' value
      student_id_df = selected_df[selected_df['id'] == student_id]
      
      # Resample the data every 10 minutes and count the number of errors
      resampled_df = student_id_df.resample('5T').count()['id']
      
      # Create a new figure
      fig, ax = plt.subplots()
      
      # Set the title of the figure
      ax.set_title(f'Student ID {student_id} errors')
      
      # Plot the resampled data
      ax.plot(resampled_df)
      
      # Set the labels of the x-axis and y-axis
      ax.set_xlabel('Time')
      ax.set_ylabel('Number of errors')
      
      # Save the figure as a PNG file
      plt.savefig(f"{hw}-syntax-error-counts-{student_id}.png")
      os.system(f"mv {hw}-syntax-error-counts-{student_id}.png plots-{hw}")

      # Close the figure
      plt.close(fig)
        


def merge_errors(hw):
  # first merge all errors into one csv
  # id, timestamp, error_category (Type error, Syntax error, Unbound error, Other error), event_key
  combined_df = pd.DataFrame(columns=['id', 'timestamp', 'error_category', 'event_key'])

  for csvf in os.listdir(f"../{hw}"):
    if "error" not in csvf: continue
    df = pd.read_csv(os.path.join(f"../{hw}", csvf))
    for index, row in df.iterrows():
      if len(row['error_category']) > 20:
        if "Error" not in row['error_category']: continue
        row['error_category'] = "Other error"
      new_row = { 
        'id': row['id'],
        'timestamp': row['timestamp'],
        'error_category': row['error_category'],
        'event_key': row['event_key']
      }
      combined_df = pd.concat([combined_df, pd.DataFrame([new_row])], ignore_index=True)

  for jsonf in os.listdir(f"syntax-err/err-{hw}"):
    event_key = jsonf.split("_")[1].split(".")[0]
    js = json.load(open(os.path.join(f"syntax-err/err-{hw}", jsonf)))
    for id_value in js:
      for timestamp in js[id_value]["syntax_errors"]:
        new_row = {
          'id': id_value,
          'timestamp': timestamp,
          'error_category': "Syntax error",
          'event_key': event_key
        }
        combined_df = pd.concat([combined_df, pd.DataFrame([new_row])], ignore_index=True)


  combined_df.to_csv(f"{hw}_err_combined.csv", index=False)


if __name__ == "__main__":
    main()