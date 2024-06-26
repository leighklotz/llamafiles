#!/usr/bin/env python3

# requirements: pandas, numpy, tabulate, colorama, scikit-learn

import os
import sys
import pathlib
import glob
import requests

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

def main(files):
    file_embeddings = calculate_embeddings(files)
    files = list(file_embeddings.keys())
    # Calculate cosine distance for all pairs
    cosine_distance_matrix = calculate_cosine_distance_matrix(file_embeddings)
    if False:
        # print cosine_distance_matrix as a table
        print_cosine_distance_matrix(cosine_distance_matrix, files)
    if True:
        # print cosine_distance_matrix as a heatmap
        print_cosine_distance_heatmap(cosine_distance_matrix, files)

if __name__ == "__main__":
    if len(sys.argv) > 1:
        main(get_files(sys.argv[1:]))
    else:
        print(f"usage: {sys.argv[0]} file1...")
        sys.exit(1)
