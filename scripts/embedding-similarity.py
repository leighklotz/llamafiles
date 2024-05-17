#!/usr/bin/env python3

import requests
import os
import sys
import pathlib
import glob
from json import JSONDecodeError
from scipy.spatial.distance import cosine
import pandas as pd
import numpy as np
from tabulate import tabulate
from colorama import Fore, Style


def calculate_embedding_for_text(text):
    url = "http://localhost:5000/v1/embeddings"
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
    return { file_name : calculate_embedding(file_name) for file_name in file_names }

def get_files(files):
   return sorted(f for f in files if os.path.isfile(f) and os.access(f, os.R_OK))
# 
# This function calculates the cosine distance between every pair of embeddings in the input dictionary `embeddings`. The cosine distance
# is a measure of similarity between two non-zero vectors of an inner product space that measures the cosine of the angle between them. The
# cosine of 0° is 1, and it is less than 1 for any other angle. It gives a measure that is useful in various domains, such as text
# analysis, where it can be used to represent documents as vectors of identified terms and to measure the similarity between documents.
# 
# The function uses nested loops to iterate over the keys and values of the `embeddings` dictionary. It first extracts the embeddings for
# the current key `file1` and initializes an empty list `row` to store the distances to the embeddings for the other keys `file2`. It then
# checks if `file1` is less than or equal to `file2` to avoid duplicates and calculates the cosine distance between the two embeddings
# using the formula:
# 
# This implementation first gets the column names from the first row of the matrix. Then, it adds the row names (file names) to the matrix
# and transposes it so that the file names are in the first column. Finally, it removes the first element (column name) from the first row
# and prints the matrix as a table using the `tabulate` library. The `headers="firstrow"` argument tells `tabulate` to use the first row as
# the column names, and the `tablefmt="grid"` argument specifies the table format. You can change the format

def calculate_cosine_distance_matrix(embeddings):
    distances = []
    files = list(embeddings.keys())
    for i, file1 in enumerate(files):
        embedding1 = embeddings[file1]
        row = []
        for j, file2 in enumerate(files):
            if i <= j: # avoid duplicates
                embedding2 = embeddings[file2]
                # Calculate the cosine distance
                dot_product = np.dot(embedding1, embedding2)
                norm_embedding1 = np.linalg.norm(embedding1)
                norm_embedding2 = np.linalg.norm(embedding2)
                cosine_distance = 1 - (dot_product / (norm_embedding1 * norm_embedding2))
                row.append(cosine_distance)
            else:
                row.append(distances[j][i]) # duplicate the value for symmetry
        distances.append(row)
    return distances

def print_cosine_distance_matrix(cosine_distance_matrix, files):
    # Get the column names from the first row
    column_names = files
    # Add the row names (file names) to the matrix
    cosine_distance_matrix = list(zip(column_names, cosine_distance_matrix))
    # Transpose the matrix so that the file names are in the first column
    cosine_distance_matrix = list(map(list, zip(*cosine_distance_matrix)))
    # Remove the first element (column name) from the first row
    # cosine_distance_matrix[0].pop(0)
    # Print the matrix as a table
    print(tabulate(cosine_distance_matrix, headers="firstrow", tablefmt="github"))

def print_cosine_distance_heatmap(cosine_distance_matrix, files):
    # Convert to numpy array for easier manipulation
    cosine_distance_matrix = np.array(cosine_distance_matrix)
    
    # Normalize the cosine distance matrix
    min_val = np.min(cosine_distance_matrix)
    max_val = np.max(cosine_distance_matrix)
    normalized_matrix = (cosine_distance_matrix - min_val) / (max_val - min_val)
    
    # Define color map
    color_map = [Fore.GREEN, Fore.YELLOW, Fore.RED]
    
    # Print the header row
    print("\t" + "\t".join(files))
    
    for i, row in enumerate(normalized_matrix):
        print(files[i], end="\t")
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

def main(directory_name):
    file_embeddings = calculate_embeddings(directory_name)
    files = list(file_embeddings.keys())
    # calculate cosine distance for all pairs
    cosine_distance_matrix = calculate_cosine_distance_matrix(file_embeddings)
    # print cosine_distance_matrix as a table
    if False:
        print_cosine_distance_matrix(cosine_distance_matrix, files)
    # print cosine_distance_matrix as a heatmap
    print_cosine_distance_heatmap(cosine_distance_matrix, files)

if __name__ == "__main__":
    if len(sys.argv) > 1:
        files = sys.argv[1:]
        files = get_files(files)
        main(files)
    else:
        print(f"usage: {sys.argv[0]} dirname")
        sys.exit(1)
