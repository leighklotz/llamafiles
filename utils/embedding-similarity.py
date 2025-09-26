#!/usr/bin/env python3

# requirements: pandas, numpy, tabulate, colorama, scikit-learn

from typing import Union
import argparse
import os
import sys
import argparse
import pathlib
import glob
import requests
import subprocess

from json import JSONDecodeError
from tabulate import tabulate
from colorama import Fore, Style

import pandas as pd
import numpy as np
from sklearn.metrics import pairwise_distances

EMBEDDING_ENDPOINT="http://tensor-psy.klotz.me:5000/v1/embeddings"

def calculate_embedding_for_text(text):
    url = EMBEDDING_ENDPOINT
    headers = {
        "Content-Type": "application/json",
    }
    data = {
        "input": text,
        "encoding_format": "float"
    }

    try:
        response = requests.post(url, headers=headers, json=data)
        response.raise_for_status()
        return response.json()
    except JSONDecodeError as e:
        print(f"Error decoding JSON: {e}")
        return None
    except requests.exceptions.HTTPError as e:
        print(f"HTTP Error: {e}")
        return None

def calculate_embedding(file_name):
    if not pathlib.Path(file_name).exists():
        print(f"File '{file_name}' does not exist")
        return None

    try:
        with open(file_name, 'r') as f:
            text = f.read()
    except Exception as e:
        print(f"Error reading file '{file_name}': {e}")
        return None

    embeddings = calculate_embedding_for_text(text)
    if embeddings is not None:
        return embeddings['data'][0]['embedding']
    return None

def calculate_embeddings(file_names):
    print(f"* {file_names=}")
    return { file_name : calculate_embedding(file_name) for file_name in file_names }

def get_files(files):
   return sorted(f for f in files if os.path.isfile(f) and os.access(f, os.R_OK))

#def calculate_cosine_distance_matrix(embeddings):
#    file_names, embedding_vectors = zip(*embeddings.items())
#    embedding_vectors = np.array(embedding_vectors)
#    import pdb;pdb.set_trace()
#    distance_matrix = pairwise_distances(embedding_vectors, metric='cosine')
#    distance_matrix = distance_matrix.astype(np.float64)
#    distances = distance_matrix.tolist()
#    return distances

def calculate_cosine_distance_matrix(embeddings):
    # Extract the file names and embedding vectors
    file_names, embedding_vectors = zip(*embeddings.items())
    # Convert embedding vectors to a numpy array
    embedding_vectors = np.array(embedding_vectors)
    # Calculate the cosine distance matrix
    distance_matrix = pairwise_distances(embedding_vectors, metric='cosine')
    # Convert the distances to np.float64
    distance_matrix = distance_matrix.astype(np.float64)
    # Convert to list of lists for the required output format
    distances = distance_matrix.tolist()
    return distances

def print_cosine_distance_matrix(cosine_distance_matrix, files):
    # Get the column names from the first row
    column_names = files
    # Add the row names (file names) to the matrix
    cosine_distance_matrix = list(zip(column_names, cosine_distance_matrix))
    # Transpose the matrix so that the file names are in the first column
    cosine_distance_matrix = list(map(list, zip(*cosine_distance_matrix)))
    # Print the matrix as a table
    print(tabulate(cosine_distance_matrix, headers="firstrow", tablefmt="plain"))

def make_cosine_distance_list(cosine_distance_matrix, files, threshold=0.1, process=lambda f1,f2: f2):
    related_files = {}
    # Sort the distances before threshold check, increasing distance
    sorted_distances = sorted(enumerate(cosine_distance_matrix[0]), key=lambda x: x[1])
    for i, file1 in enumerate(files):
        related = []
        for idx, distance in sorted_distances:
            if cosine_distance_matrix[i][idx] <= threshold and files[idx] != file1:
                related.append(process(file1, files[idx]))
        related_files[file1] = related
    return related_files

def print_cosine_distance_list(d):
   for k, v in d.items():
       print(k)
       for vv in v:
           print(f"\t-> {vv}")

def print_cosine_distance_heatmap(cosine_distance_matrix, files):
    # Convert to numpy array for easier manipulation
    cosine_distance_matrix = np.array(cosine_distance_matrix)
    
    # Normalize the cosine distance matrix
    min_val = np.min(cosine_distance_matrix)
    max_val = np.max(cosine_distance_matrix)
    normalized_matrix = (cosine_distance_matrix - min_val) / (max_val - min_val)
    
    # Define color map
    color_map = [Fore.GREEN, Fore.YELLOW, Fore.RED]
    
    # Calculate the maximum width required for the first column
    max_first_col_width = max(len(str(file)) for file in files)

    # Print the header row
    print(" " * (max_first_col_width + 1) + "\t".join(files))
    
#    # Print the header row
#    print("\t" + "\t".join(files))
    for i, row in enumerate(normalized_matrix):
        print(files[i].ljust(max_first_col_width), end="\t")    
        # print(files[i], end="\t")
        for value in row:
            # Determine the color based on normalized value
            if value < 0.33:
                color_code = color_map[0]
            elif value < 0.66:
                color_code = color_map[1]
            else:
                color_code = color_map[2]
            print(color_code + f"{value:.2f}", end="\t")
        print(Style.RESET_ALL)

def usage(msg):
    print(f"usage: {sys.argv[0]} [--matrix][--heatmap][--list] file1...")
    print(msg)



def parse_args() -> argparse.Namespace:
   parser = argparse.ArgumentParser(description='Some helpful description for your tool')
   parser.add_argument('--matrix', action='store_true')
   parser.add_argument('--heatmap', action='store_true')
   parser.add_argument('--list', action='store_true')
   parser.add_argument('--diff', action='store_true')
   parser.add_argument('--threshold', type=float, default=0.1)
   parser.add_argument('files', nargs='*', type=str)
   return parser.parse_args()

# def diff_process(f1, f2):
#     return run_shell_pipeline(f"diff {f2} {f2} | diffstat")
# 
# def run_shell_pipeline(cmd):
#     process = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
#     output, error = process.communicate()
#     return output
# 
def diff_process(f1, f2):
    p1 = subprocess.Popen(['diff', '-u', f1, f2], stdout=subprocess.PIPE)
    p2 = subprocess.Popen(['diffstat', '-q', '-b', '-f', '1'], stdin=p1.stdout, stdout=subprocess.PIPE)
    p3 = subprocess.Popen(['head', '-1'], stdin=p2.stdout, stdout=subprocess.PIPE)
    r = p3.communicate()
    return f2 + ' ' + r[0].decode().strip()

def main(args):
    files = args.files
    file_embeddings = calculate_embeddings(files)
    files = list(file_embeddings.keys())
    # Calculate cosine distance for all pairs
    cosine_distance_matrix = calculate_cosine_distance_matrix(file_embeddings)

    if args.matrix:
        print_cosine_distance_matrix(cosine_distance_matrix, files)
       
    if args.heatmap:
        print_cosine_distance_heatmap(cosine_distance_matrix, files)

    if args.list:
        d = make_cosine_distance_list(cosine_distance_matrix, files, threshold=args.threshold)
        print_cosine_distance_list(d)

    if args.diff:
        d = make_cosine_distance_list(cosine_distance_matrix, files, threshold=args.threshold, process=diff_process)
        print_cosine_distance_list(d)

if __name__ == "__main__":
    args = parse_args()
    main(args)
